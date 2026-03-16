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
use crate::source_analysis::Span;

// ─── ThreadingPlan ────────────────────────────────────────────────────────────

/// Selects the naming convention for state-map keys.
#[derive(Clone, Debug)]
pub(super) enum KeyStyle {
    /// `__local__x` prefix (actor and value-type methods).
    LocalPrefixed,
    /// Plain variable name (REPL mode).
    ReplPlain,
}

/// BT-1343: Reason why a loop fell back to `StateAcc` threading instead of an optimized mode.
#[derive(Clone, Debug)]
pub(super) enum StateAccFallbackReason {
    /// No fallback — an optimized convention was selected.
    None,
    /// Body contains self-sends (async dispatch requires `gen_server` state).
    SelfSendInBody,
    /// Nested list op with cross-scope mutations incompatible with direct-params.
    NestedListOpCrossScope,
    /// Tier-2 value call on a threaded local (returns `{Result, StateAcc}` tuple).
    Tier2ValueCallOnThreaded,
    /// Inline conditional writes to a threaded local.
    InlineConditionalThreadedWrite,
    /// Condition block has state effects.
    ConditionStateEffects,
    /// Control-flow sub-expression with mutations (e.g. `ifTrue:` with field writes).
    ControlFlowMutations,
    /// No threaded locals (nothing to optimize).
    NoThreadedLocals,
    /// `ValueType` context (no actor State to thread).
    ValueTypeContext,
    /// Not a letrec loop (foldl loops don't support direct-params).
    NotLetrec,
    /// Destructure assignment as last expression (incompatible with tuple-acc).
    DestructureAsLastExpr,
}

impl std::fmt::Display for StateAccFallbackReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::None => write!(f, "none"),
            Self::SelfSendInBody => write!(f, "self-send in loop body"),
            Self::NestedListOpCrossScope => {
                write!(f, "nested list op with cross-scope mutation")
            }
            Self::Tier2ValueCallOnThreaded => {
                write!(f, "tier-2 value call on threaded local")
            }
            Self::InlineConditionalThreadedWrite => {
                write!(f, "inline conditional writing to threaded local")
            }
            Self::ConditionStateEffects => write!(f, "condition has state effects"),
            Self::ControlFlowMutations => {
                write!(f, "control-flow sub-expression with mutations")
            }
            Self::NoThreadedLocals => write!(f, "no threaded locals"),
            Self::ValueTypeContext => write!(f, "ValueType context"),
            Self::NotLetrec => write!(f, "not a letrec loop"),
            Self::DestructureAsLastExpr => {
                write!(f, "destructure assignment as last expression")
            }
        }
    }
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
    /// BT-1326/BT-1342: When `true`, use full-extract direct-params for letrec loops.
    ///
    /// Set when the loop body has BOTH local variable mutations AND actor field mutations
    /// (but no self-sends). The loop fun signature becomes
    /// `fun(I, Local1, ..., RField1, ..., MField1, ...)` with locals, read-only fields,
    /// and mutated fields all as direct parameters. No `State` parameter.
    ///
    /// Eliminates ALL per-iteration `maps:get`/`maps:put` — field writes become simple
    /// variable rebindings, repacked into the state map only at loop exit.
    /// Mutually exclusive with `use_direct_params`.
    pub use_hybrid_params: bool,
    /// BT-1326: Actor fields that are read but never written in the loop body.
    ///
    /// In hybrid mode, read-only fields are pre-extracted before the letrec with a single
    /// `maps:get` and passed as direct fun parameters — eliminating per-iteration
    /// `maps:get` calls for fields that never change during the loop.
    ///
    /// Empty when `use_hybrid_params` is false (sorted for deterministic codegen).
    pub readonly_fields: Vec<String>,
    /// BT-1343: Why `StateAcc` fallback was chosen (if no optimized mode was selected).
    pub fallback_reason: StateAccFallbackReason,
    /// BT-1342: Actor fields that are written (mutated) inside the loop body.
    ///
    /// In full-extract mode, mutated fields are pre-extracted before the letrec via
    /// `maps:get` and passed as direct fun parameters. Inside the loop, field writes
    /// become simple variable rebindings instead of `maps:put` on State. At loop exit,
    /// mutated fields are repacked into the initial State map via `maps:put`.
    ///
    /// Empty when `use_hybrid_params` is false (sorted for deterministic codegen).
    pub mutated_fields: Vec<String>,
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

    #[allow(clippy::too_many_lines)]
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
            // BT-1329: Check for nested list ops with cross-scope mutations whose inner
            // blocks can't use tuple-acc. These fall back to map-acc which references
            // StateAcc — incompatible with direct-params mode.
            let body_has_non_tuple_safe_list_op = body.body.iter().any(|s| {
                CoreErlangGenerator::list_op_needs_stateacc_fallback_recursive(
                    &s.expression,
                    &generator.semantic_facts,
                )
            });
            !body_analysis.has_state_effects()
                && !cond_has_state_effects
                && !body_has_tier2_threaded_assign
                && !body_has_non_tuple_safe_list_op
        };

        // BT-1276: Tuple accumulator optimization for foldl list-ops.
        //
        // Eligible when the body has only simple local var mutations — no field writes,
        // no self-sends, no tier-2 assignments, and no complex control-flow subexpressions
        // that generate `StateAcc`-dependent code (e.g. `ifTrue:` with inner mutations).
        //
        // ValueType methods have no `State` gen_server variable in scope; `collect:`,
        // `select:`, and `inject:` tuple paths call `append_repack_stateacc` which seeds
        // from `initial_state_var` (= the current `State` name). Using tuple-acc in
        // ValueType context would reference an unbound variable — always fall through to
        // the map-acc path which uses its own fresh `maps:new()` seed instead.
        let use_tuple_acc = if !allow_tuple_acc
            || threaded_locals.is_empty()
            || matches!(generator.context, CodeGenContext::ValueType)
        {
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
                    &generator.semantic_facts,
                )
            });
            // DestructureAssignment as the last expr is not supported in tuple-acc mode:
            // `emit_destructure_last_expr` always emits the map-shaped StateAcc path.
            let body_last_is_destructure = body
                .body
                .last()
                .is_some_and(|s| matches!(s.expression, Expression::DestructureAssignment { .. }));
            !body_analysis.has_state_effects()
                && !body_has_tier2_threaded_assign_tuple
                && !body_has_cf_mutations
                && !body_has_conditional_threaded_writes
                && !body_last_is_destructure
        };

        // BT-1326: Hybrid direct-params + State threading for letrec loops.
        //
        // Eligible when:
        // - caller opts in via `allow_direct_params` (letrec loops only)
        // - there are local vars to thread
        // - body has field mutations (but NOT self-sends — those need async dispatch)
        // - condition has no state effects
        // - no tier-2 threaded-local assignments
        // - actor context only (ValueType has no actor State to thread)
        // - NOT already using use_direct_params (mutually exclusive)
        let use_hybrid_params = if !allow_direct_params
            || threaded_locals.is_empty()
            || use_direct_params
            || !matches!(context, CodeGenContext::Actor)
        {
            false
        } else {
            let cond_has_state_effects_for_hybrid = condition.is_some_and(|c| {
                if let Expression::Block(cb) = c {
                    block_analysis::analyze_block(cb).has_state_effects()
                } else {
                    false
                }
            });
            let body_has_tier2_threaded_assign_hybrid = body.body.iter().any(|s| {
                if let Expression::Assignment { target, value, .. } = &s.expression {
                    if let Expression::Identifier(id) = target.as_ref() {
                        if threaded_locals.contains(&id.name.to_string()) {
                            return generator.is_tier2_value_call(value);
                        }
                    }
                }
                false
            });
            // Guard: if any body expression is nested control flow with mutations (e.g.
            // `flag ifTrue: [self.n := ...]`), fall back to StateAcc mode.
            // `emit_non_assign_expr` for BodyKind::Letrec hardcodes `StateAccN` naming;
            // hybrid mode uses `StateN`. Mixed naming causes unbound variable errors.
            let body_has_cf_mutations_hybrid = body
                .body
                .iter()
                .any(|s| generator.control_flow_has_mutations(&s.expression));
            // Guard: inline conditionals that write threaded locals (e.g.
            // `each > max ifTrue: [max := each]`). `control_flow_has_mutations` only
            // catches field writes; this catches the pure-overwrite-local pattern that
            // `emit_non_assign_expr` would thread via StateAcc naming.
            let body_has_conditional_threaded_writes_hybrid = body.body.iter().any(|s| {
                CoreErlangGenerator::inline_conditional_writes_threaded(
                    &s.expression,
                    &threaded_locals,
                    &generator.semantic_facts,
                )
            });
            // BT-1329: Check for nested list ops incompatible with direct-params.
            let body_has_non_tuple_safe_list_op_hybrid = body.body.iter().any(|s| {
                CoreErlangGenerator::list_op_needs_stateacc_fallback_recursive(
                    &s.expression,
                    &generator.semantic_facts,
                )
            });
            // Eligible when body has field mutations but no self-sends:
            // - self-sends require async gen_server dispatch; hybrid mode only handles
            //   direct field-map mutations which produce simple maps:put code.
            !body_analysis.field_writes.is_empty()
                && !body_analysis.has_self_sends
                && !cond_has_state_effects_for_hybrid
                && !body_has_tier2_threaded_assign_hybrid
                && !body_has_cf_mutations_hybrid
                && !body_has_conditional_threaded_writes_hybrid
                && !body_has_non_tuple_safe_list_op_hybrid
        };

        // BT-1326: In hybrid mode, collect fields that are read but never written.
        // These will be pre-extracted before the letrec (one maps:get total) and
        // passed as direct fun parameters, eliminating per-iteration maps:get calls.
        let readonly_fields = if use_hybrid_params {
            let mut fields: Vec<String> = body_analysis
                .field_reads
                .difference(&body_analysis.field_writes)
                .cloned()
                .collect();
            fields.sort(); // deterministic codegen
            fields
        } else {
            vec![]
        };

        // BT-1343: Determine fallback reason when no optimized convention was selected.
        let fallback_reason =
            if use_direct_params || use_tuple_acc || use_hybrid_params {
                StateAccFallbackReason::None
            } else if threaded_locals.is_empty() {
                StateAccFallbackReason::NoThreadedLocals
            } else if !allow_direct_params && !allow_tuple_acc {
                StateAccFallbackReason::NotLetrec
            } else if body_analysis.has_self_sends {
                StateAccFallbackReason::SelfSendInBody
            } else if !body_analysis.field_writes.is_empty() {
                // Field writes present (self-sends already excluded above) but not eligible
                // for hybrid — check specific guards
                if body.body.iter().any(|s| {
                    CoreErlangGenerator::list_op_needs_stateacc_fallback_recursive(
                        &s.expression,
                        &generator.semantic_facts,
                    )
                }) {
                    StateAccFallbackReason::NestedListOpCrossScope
                } else if body.body.iter().any(|s| {
                    if let Expression::Assignment { target, value, .. } = &s.expression {
                        if let Expression::Identifier(id) = target.as_ref() {
                            if threaded_locals.contains(&id.name.to_string()) {
                                return generator.is_tier2_value_call(value);
                            }
                        }
                    }
                    false
                }) {
                    StateAccFallbackReason::Tier2ValueCallOnThreaded
                } else if condition.is_some_and(|c| {
                    if let Expression::Block(cb) = c {
                        block_analysis::analyze_block(cb).has_state_effects()
                    } else {
                        false
                    }
                }) {
                    StateAccFallbackReason::ConditionStateEffects
                } else if body
                    .body
                    .iter()
                    .any(|s| generator.control_flow_has_mutations(&s.expression))
                {
                    StateAccFallbackReason::ControlFlowMutations
                } else if body.body.iter().any(|s| {
                    CoreErlangGenerator::inline_conditional_writes_threaded(
                        &s.expression,
                        &threaded_locals,
                        &generator.semantic_facts,
                    )
                }) {
                    StateAccFallbackReason::InlineConditionalThreadedWrite
                } else if body.body.last().is_some_and(|s| {
                    matches!(s.expression, Expression::DestructureAssignment { .. })
                }) {
                    StateAccFallbackReason::DestructureAsLastExpr
                } else {
                    // Generic fallback — body has state effects but doesn't match
                    // any specific guard above. Re-check for self-sends first.
                    if body_analysis.has_self_sends {
                        StateAccFallbackReason::SelfSendInBody
                    } else {
                        StateAccFallbackReason::ControlFlowMutations
                    }
                }
            } else if matches!(generator.context, CodeGenContext::ValueType) {
                StateAccFallbackReason::ValueTypeContext
            } else {
                // Check the specific guards that prevented optimization
                if body.body.iter().any(|s| {
                    CoreErlangGenerator::list_op_needs_stateacc_fallback_recursive(
                        &s.expression,
                        &generator.semantic_facts,
                    )
                }) {
                    StateAccFallbackReason::NestedListOpCrossScope
                } else if body.body.iter().any(|s| {
                    if let Expression::Assignment { target, value, .. } = &s.expression {
                        if let Expression::Identifier(id) = target.as_ref() {
                            if threaded_locals.contains(&id.name.to_string()) {
                                return generator.is_tier2_value_call(value);
                            }
                        }
                    }
                    false
                }) {
                    StateAccFallbackReason::Tier2ValueCallOnThreaded
                } else if condition.is_some_and(|c| {
                    if let Expression::Block(cb) = c {
                        block_analysis::analyze_block(cb).has_state_effects()
                    } else {
                        false
                    }
                }) {
                    StateAccFallbackReason::ConditionStateEffects
                } else if body
                    .body
                    .iter()
                    .any(|s| generator.control_flow_has_mutations(&s.expression))
                {
                    StateAccFallbackReason::ControlFlowMutations
                } else if body.body.iter().any(|s| {
                    CoreErlangGenerator::inline_conditional_writes_threaded(
                        &s.expression,
                        &threaded_locals,
                        &generator.semantic_facts,
                    )
                }) {
                    StateAccFallbackReason::InlineConditionalThreadedWrite
                } else if body.body.last().is_some_and(|s| {
                    matches!(s.expression, Expression::DestructureAssignment { .. })
                }) {
                    StateAccFallbackReason::DestructureAsLastExpr
                } else {
                    // Generic fallback — body has state effects but doesn't match
                    // any specific guard above. Re-check for self-sends first.
                    if body_analysis.has_self_sends {
                        StateAccFallbackReason::SelfSendInBody
                    } else {
                        StateAccFallbackReason::ControlFlowMutations
                    }
                }
            };

        // BT-1342: In hybrid mode, collect fields that are written (mutated).
        // These will be pre-extracted before the letrec and passed as direct
        // fun parameters, eliminating per-iteration maps:get/maps:put overhead.
        let mutated_fields = if use_hybrid_params {
            let mut fields: Vec<String> = body_analysis.field_writes.iter().cloned().collect();
            fields.sort(); // deterministic codegen
            fields
        } else {
            vec![]
        };

        Self {
            threaded_locals,
            initial_state_var,
            key_style,
            context,
            use_direct_params,
            use_tuple_acc,
            use_hybrid_params,
            readonly_fields,
            fallback_reason,
            mutated_fields,
        }
    }

    /// BT-1343: Returns a human-readable label for the selected calling convention.
    pub fn convention_label(&self) -> &'static str {
        if self.use_direct_params {
            "direct-params"
        } else if self.use_tuple_acc {
            "tuple-acc"
        } else if self.use_hybrid_params {
            "hybrid"
        } else {
            "StateAcc"
        }
    }

    /// BT-1343: Returns the total number of extracted parameters (locals + readonly fields).
    pub fn total_extracted_params(&self) -> usize {
        self.threaded_locals.len() + self.readonly_fields.len()
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
        if self.threaded_locals.is_empty() || self.use_direct_params || self.use_hybrid_params {
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
            if !self.use_direct_params && !self.use_hybrid_params {
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

    /// BT-1342: Generates the exit `{'nil', ExitSA}` expression for the false arm of a
    /// full-extract loop (no State parameter).
    ///
    /// In full-extract mode, ALL fields (both read-only and mutated) are direct params,
    /// and there is no `State` fun parameter. At loop exit, mutated fields are packed
    /// back into the initial state map (from the enclosing scope), then locals are packed.
    ///
    /// `local_param_names` — Core Erlang names of each threaded local in the current iteration.
    /// `mutated_field_param_names` — Core Erlang names of each mutated field param in the
    ///   current iteration (the fun parameter names, which may have been rebound by writes).
    /// `initial_state` — the State variable from the enclosing scope (before the letrec).
    pub fn generate_exit_stateacc_full_extract(
        &self,
        local_param_names: &[String],
        mutated_field_param_names: &[String],
        initial_state: &str,
        generator: &mut CoreErlangGenerator,
    ) -> Document<'static> {
        let mut docs: Vec<Document<'static>> = Vec::new();
        let mut current = initial_state.to_string();

        // First, repack mutated fields into the state map.
        for (field_name, param) in self
            .mutated_fields
            .iter()
            .zip(mutated_field_param_names.iter())
        {
            let next_var = generator.fresh_temp_var("ExitSA");
            docs.push(docvec![
                "let ",
                Document::String(next_var.clone()),
                " = call 'maps':'put'('",
                Document::String(field_name.clone()),
                "', ",
                Document::String(param.clone()),
                ", ",
                Document::String(current),
                ") in ",
            ]);
            current = next_var;
        }

        // Then, pack locals into the state map.
        for (var_name, param) in self.threaded_locals.iter().zip(local_param_names.iter()) {
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

        docs.push(docvec!["{'nil', ", Document::String(current), "}"]);
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

    /// Returns the current Core Erlang bindings of all threaded locals as a `Document`.
    ///
    /// Example: `threaded_locals = ["sum", "count"]` → `Sum1, Count`.
    /// Returns `Document::Nil` when `threaded_locals` is empty.
    pub fn current_vars_doc(&self, generator: &CoreErlangGenerator) -> Document<'static> {
        join(
            self.threaded_locals.iter().map(|v| {
                Document::String(
                    generator
                        .lookup_var(v)
                        .cloned()
                        .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v)),
                )
            }),
            &Document::Str(", "),
        )
    }

    /// Returns the initial-values tuple `Document` constructed from outer-scope bindings.
    ///
    /// Call this **before** `push_scope()` so the bindings reflect the state before
    /// the loop.  Example: `threaded_locals = ["sum"]` → `{Sum}`.
    /// Returns `Document::Str("{}")` when `threaded_locals` is empty.
    pub fn initial_vars_tuple_doc(&self, generator: &CoreErlangGenerator) -> Document<'static> {
        if self.threaded_locals.is_empty() {
            return Document::Str("{}");
        }
        docvec!["{", self.current_vars_doc(generator), "}"]
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

    /// Returns element-extraction code after foldl completes for tuple mode as a `Document`.
    ///
    /// Generates `let V = call 'erlang':'element'(idx, acc) in ...` using the
    /// outer-scope binding names (from `lookup_var`) as targets.
    ///
    /// `index_offset` — same as in `generate_tuple_unpack_docs`.
    pub fn generate_tuple_extract_suffix_doc(
        &self,
        final_acc_var: &str,
        index_offset: usize,
        generator: &CoreErlangGenerator,
    ) -> Document<'static> {
        let mut docs: Vec<Document<'static>> = Vec::new();
        for (i, var_name) in self.threaded_locals.iter().enumerate() {
            let core_var = generator
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(var_name));
            let idx = index_offset + i;
            docs.push(docvec![
                "let ",
                Document::String(core_var),
                " = call 'erlang':'element'(",
                Document::String(idx.to_string()),
                ", ",
                Document::String(final_acc_var.to_string()),
                ") in ",
            ]);
        }
        Document::Vec(docs)
    }

    /// BT-1276: Re-packs updated locals back into the `StateAcc` map after a tuple-acc loop,
    /// returning a `(Document, final_var_name)` pair instead of mutating a `String`.
    ///
    /// The returned `Document` contains the `let PkSt1 = maps:put(...) in ...` chain.
    /// The returned `String` is the name of the final packed-state variable (`PkStN`).
    ///
    /// Must be called AFTER `generate_tuple_extract_suffix_doc` so that the Core Erlang
    /// variable names (e.g. `Total`) refer to the extracted (updated) values.
    pub fn append_repack_stateacc_doc(
        &self,
        generator: &mut CoreErlangGenerator,
    ) -> (Document<'static>, String) {
        let mut current = self.initial_state_var.clone();
        let mut docs: Vec<Document<'static>> = Vec::new();
        for var_name in &self.threaded_locals {
            let core_var = generator
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(var_name));
            let key = self.state_key(var_name);
            let pack_var = generator.fresh_temp_var("PkSt");
            docs.push(docvec![
                "let ",
                Document::String(pack_var.clone()),
                " = call 'maps':'put'('",
                Document::String(key),
                "', ",
                Document::String(core_var),
                ", ",
                Document::String(current),
                ") in ",
            ]);
            current = pack_var;
        }
        (Document::Vec(docs), current)
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
    /// BT-1343: Emits a codegen diagnostic for the calling convention chosen for a loop.
    ///
    /// Reports which optimization mode was selected (direct-params, tuple-acc, hybrid,
    /// or `StateAcc` fallback with reason). Also emits a large-arity warning when >8 params
    /// are extracted. Gated by `BEAMTALK_CODEGEN_DIAGNOSTICS=1`.
    pub(super) fn emit_loop_convention_diagnostic(&mut self, plan: &ThreadingPlan, span: Span) {
        if !self.codegen_diagnostics_enabled {
            return;
        }
        let line_info = self
            .span_to_line(span)
            .map_or(String::new(), |l| format!(" at line {l}"));
        let n_locals = plan.threaded_locals.len();
        let n_readonly = plan.readonly_fields.len();
        let convention = plan.convention_label();

        if matches!(plan.fallback_reason, StateAccFallbackReason::None) {
            // Optimized convention chosen
            let detail = match convention {
                "direct-params" => {
                    format!("{n_locals} locals, 0 field mutations")
                }
                "tuple-acc" => {
                    format!("{n_locals} locals in tuple accumulator")
                }
                "hybrid" => {
                    format!("{n_locals} locals + {n_readonly} read-only fields as direct params")
                }
                _ => String::new(),
            };
            self.emit_codegen_diagnostic(
                format!("Loop{line_info}: using {convention} ({detail})"),
                span,
            );
        } else {
            // StateAcc fallback
            let reason = &plan.fallback_reason;
            self.emit_stateacc_fallback_diagnostic(
                format!("Loop{line_info}: StateAcc fallback — {reason}"),
                span,
            );
        }

        // BT-1343: Large extracted arity diagnostic (>8 direct fun params)
        let total = plan.total_extracted_params();
        if total > 8 && (plan.use_direct_params || plan.use_hybrid_params) {
            self.emit_codegen_diagnostic(
                format!("Loop{line_info}: {total} extracted params"),
                span,
            );
        }
    }

    /// BT-1343: Emits a diagnostic for synchronous self-send detected in a loop body.
    pub(super) fn emit_self_send_in_loop_diagnostic(&mut self, expr: &Expression, span: Span) {
        if !self.codegen_diagnostics_enabled {
            return;
        }
        // Extract selector name from the message send
        if let Expression::MessageSend { selector, .. } = expr {
            let sel_name = selector.to_erlang_atom();
            let line_info = self
                .span_to_line(span)
                .map_or(String::new(), |l| format!(" at line {l}"));
            self.emit_codegen_diagnostic(
                format!(
                    "Self-send 'self {sel_name}' inside loop{line_info}: \
                     synchronous call to own mailbox, potential deadlock"
                ),
                span,
            );
        }
    }
}

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
                // BT-1343: Emit diagnostic for synchronous self-send in loop body.
                self.emit_self_send_in_loop_diagnostic(expr, expr.span());
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
                } else if plan.use_direct_params || plan.use_tuple_acc || plan.use_hybrid_params {
                    // BT-1275/BT-1276/BT-1326: Direct-params, tuple-acc, or hybrid mode —
                    // emit `let NewVar = value in` without a StateAcc map. The var binding
                    // is updated so the final repack references the latest version.
                    has_mutations = true;
                    let (assign_doc, new_var) = self.generate_direct_var_update_in_loop(expr)?;
                    docs.push(assign_doc);
                    if is_last {
                        self.emit_local_assign_last_expr(
                            &mut docs,
                            kind,
                            pred_var.as_ref(),
                            plan,
                            new_var.as_deref(),
                        );
                    }
                } else {
                    has_mutations = true;
                    let assign_doc = self.generate_local_var_assignment_in_loop(expr)?;
                    docs.push(assign_doc);
                    if is_last {
                        self.emit_local_assign_last_expr(
                            &mut docs,
                            kind,
                            pred_var.as_ref(),
                            plan,
                            None,
                        );
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
                    || Self::inline_conditional_writes_threaded(
                        expr,
                        &plan.threaded_locals,
                        &self.semantic_facts,
                    ))
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
                let condition_doc: Document<'static> = if *negate {
                    docvec!["call 'erlang':'not'(", Document::String(pv.clone()), ")",]
                } else {
                    Document::String(pv.clone())
                };
                if plan.use_tuple_acc {
                    // BT-1276: Tuple mode — repack current var bindings into the result tuple.
                    let vars_doc = plan.current_vars_doc(self);
                    docs.push(docvec![
                        "case ",
                        condition_doc,
                        " of <'true'> when 'true' -> {[",
                        Document::String(item_var.clone()),
                        " | AccList], ",
                        vars_doc.clone(),
                        "} <'false'> when 'true' -> {AccList, ",
                        vars_doc,
                        "} end",
                    ]);
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    docs.push(docvec![
                        "case ",
                        condition_doc,
                        " of <'true'> when 'true' -> {[",
                        Document::String(item_var.clone()),
                        " | AccList], ",
                        Document::String(final_state.clone()),
                        "} <'false'> when 'true' -> {AccList, ",
                        Document::String(final_state),
                        "} end",
                    ]);
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
                docs.push(docvec![
                    "{[_Val | AccList], ",
                    Document::String(self.current_state_var()),
                    "}",
                ]);
            }
            BodyKind::FoldlFilter { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(docvec!["let ", Document::String(pv.clone()), " = _Val in ",]);
                }
            }
            BodyKind::FoldlInject => {
                docs.push(docvec![
                    "{_Val, ",
                    Document::String(self.current_state_var()),
                    "}",
                ]);
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
                    docs.push(docvec![
                        "let ",
                        Document::String(ir.clone()),
                        " = call 'erlang':'element'(1, ",
                        Document::String(dv),
                        ") in {[",
                        Document::String(ir),
                        " | AccList], ",
                        Document::String(fs),
                        "}",
                    ]);
                } else {
                    docs.push(docvec!["{['nil' | AccList], ", Document::String(fs), "}",]);
                }
            }
            BodyKind::FoldlFilter { .. } => {
                if let Some(pv) = pred_var {
                    if let Some(dv) = self.last_dispatch_var.clone() {
                        docs.push(docvec![
                            "let ",
                            Document::String(pv.clone()),
                            " = call 'erlang':'element'(1, ",
                            Document::String(dv),
                            ") in ",
                        ]);
                    } else {
                        docs.push(docvec![
                            "let ",
                            Document::String(pv.clone()),
                            " = 'nil' in ",
                        ]);
                    }
                }
            }
            BodyKind::FoldlInject => {
                let fs = self.current_state_var();
                if let Some(dv) = self.last_dispatch_var.clone() {
                    let ar = self.fresh_temp_var("AccResult");
                    docs.push(docvec![
                        "let ",
                        Document::String(ar.clone()),
                        " = call 'erlang':'element'(1, ",
                        Document::String(dv),
                        ") in {",
                        Document::String(ar),
                        ", ",
                        Document::String(fs),
                        "}",
                    ]);
                } else {
                    docs.push(docvec!["{'nil', ", Document::String(fs), "}",]);
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
        last_val: Option<&str>,
    ) {
        if plan.use_tuple_acc {
            // BT-1276: Tuple mode — repack current bindings as tuple accumulator.
            // `last_val` is the newly-bound variable name from `generate_direct_var_update_in_loop`
            // (e.g. `"Sum1"`). Used for FoldlCollect/FoldlFilter/FoldlInject where the loop
            // result value must be referenced explicitly; falls back to `"_Val"` when not set.
            let val = last_val.unwrap_or("_Val");
            let vars_doc = plan.current_vars_doc(self);
            match kind {
                BodyKind::Letrec => {}
                BodyKind::FoldlDo => {
                    docs.push(docvec![" {", vars_doc, "}"]);
                }
                BodyKind::FoldlCollect => {
                    docs.push(docvec![
                        " {[",
                        Document::String(val.to_string()),
                        " | AccList], ",
                        vars_doc,
                        "}",
                    ]);
                }
                BodyKind::FoldlFilter { .. } => {
                    if let Some(pv) = pred_var {
                        docs.push(docvec![
                            " let ",
                            Document::String(pv.clone()),
                            " = ",
                            Document::String(val.to_string()),
                            " in ",
                        ]);
                    }
                }
                BodyKind::FoldlInject => {
                    docs.push(docvec![
                        " {",
                        Document::String(val.to_string()),
                        ", ",
                        vars_doc,
                        "}",
                    ]);
                }
            }
            return;
        }
        match kind {
            BodyKind::Letrec => {
                // Nothing; caller appends recursive call.
            }
            BodyKind::FoldlDo => {
                docs.push(docvec![" ", Document::String(self.current_state_var())]);
            }
            BodyKind::FoldlCollect => {
                docs.push(docvec![
                    " {[_Val | AccList], ",
                    Document::String(self.current_state_var()),
                    "}",
                ]);
            }
            BodyKind::FoldlFilter { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(docvec![
                        " let ",
                        Document::String(pv.clone()),
                        " = _Val in ",
                    ]);
                }
            }
            BodyKind::FoldlInject => {
                docs.push(docvec![
                    " {_Val, ",
                    Document::String(self.current_state_var()),
                    "}",
                ]);
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
                docs.push(docvec!["{['nil' | AccList], ", Document::String(fs), "}",]);
            }
            BodyKind::FoldlFilter { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(docvec![
                        "let ",
                        Document::String(pv.clone()),
                        " = 'false' in ",
                    ]);
                }
            }
            BodyKind::FoldlInject => {
                let fs = if has_mutations {
                    self.current_state_var()
                } else {
                    "StateAcc".to_string()
                };
                docs.push(docvec!["{'nil', ", Document::String(fs), "}"]);
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
                if self.in_direct_params_loop {
                    // BT-1329: In direct-params mode, list ops omit the trailing 'nil' and
                    // leave their let-chain open. We emit the expression directly so that
                    // variable rebindings (e.g. `let Count = element(1, FoldResult) in`)
                    // escape to the outer scope where the loop recursion can see them.
                    let expr_code = self.expression_doc(expr)?;
                    docs.push(expr_code);
                } else if is_last && !has_direct_field_assignments {
                    // BT-478/BT-483: Mutations come from nested constructs.
                    // Extract updated state via element(2).
                    let next_version = self.state_version() + 1;
                    let next_var = format!("StateAcc{next_version}");
                    let tuple_var = format!("_NestTuple{next_version}");
                    let expr_code = self.expression_doc(expr)?;
                    self.set_state_version(next_version);
                    docs.push(docvec![
                        "let ",
                        Document::String(tuple_var.clone()),
                        " = ",
                        expr_code,
                        " in let ",
                        Document::String(next_var),
                        " = call 'erlang':'element'(2, ",
                        Document::String(tuple_var),
                        ") in",
                    ]);
                } else {
                    let expr_code = self.expression_doc(expr)?;
                    docs.push(docvec!["let _ = ", expr_code, " in"]);
                }
            }
            BodyKind::FoldlDo => {
                // BT-1290: When preceding let-bindings exist (has_mutations/has_plain_lets),
                // the last expression must also be bound with `let _ =` before `in StateAcc`.
                // Without this, `let Y = ... in <expr> in StateAcc` is invalid Core Erlang
                // (the `in StateAcc` has no corresponding `let`).
                if !is_last || (has_mutations || has_plain_lets) {
                    docs.push(Document::Str("let _ = "));
                }
                let doc = self.generate_expression(expr)?;
                docs.push(doc);
                if is_last && (has_mutations || has_plain_lets) {
                    if plan.use_tuple_acc {
                        // BT-1276: Repack threaded locals as tuple.
                        docs.push(docvec![" in {", plan.current_vars_doc(self), "}"]);
                    } else {
                        docs.push(docvec![" in ", Document::String(self.current_state_var())]);
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
                        let vars_doc = plan.current_vars_doc(self);
                        docs.push(docvec![
                            "let ",
                            Document::String(result_var.clone()),
                            " = ",
                            expr_code,
                            " in {[",
                            Document::String(result_var),
                            " | AccList], ",
                            vars_doc,
                            "}",
                        ]);
                    } else {
                        let fs = if has_mutations {
                            self.current_state_var()
                        } else {
                            "StateAcc".to_string()
                        };
                        docs.push(docvec![
                            "let ",
                            Document::String(result_var.clone()),
                            " = ",
                            expr_code,
                            " in {[",
                            Document::String(result_var),
                            " | AccList], ",
                            Document::String(fs),
                            "}",
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
                        docs.push(docvec![
                            "let ",
                            Document::String(pv.clone()),
                            " = ",
                            expr_code,
                            " in ",
                        ]);
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
                        let vars_doc = plan.current_vars_doc(self);
                        docs.push(docvec![
                            "let ",
                            Document::String(acc_var.clone()),
                            " = ",
                            expr_code,
                            " in {",
                            Document::String(acc_var),
                            ", ",
                            vars_doc,
                            "}",
                        ]);
                    } else {
                        let fs = if has_mutations {
                            self.current_state_var()
                        } else {
                            "StateAcc".to_string()
                        };
                        docs.push(docvec![
                            "let ",
                            Document::String(acc_var.clone()),
                            " = ",
                            expr_code,
                            " in {",
                            Document::String(acc_var),
                            ", ",
                            Document::String(fs),
                            "}",
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
        if plan.use_hybrid_params {
            return self.generate_counted_stateful_loop_hybrid(frame, body, plan);
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

        // Body — set in_direct_params_loop so nested list ops skip StateAcc repack (BT-1329).
        let prev_direct_params_loop = self.in_direct_params_loop;
        self.in_direct_params_loop = true;
        let (body_doc, _) = self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec)?;
        self.in_direct_params_loop = prev_direct_params_loop;
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

    /// BT-1326/BT-1342: Full-extract variant of `generate_counted_stateful_loop`.
    ///
    /// Uses `fun (I, Var1, ..., VarN, RField1, ..., MField1, ...)` — locals, read-only fields,
    /// AND mutated fields as direct fun parameters. No `State` parameter.
    ///
    /// Field reads resolve to direct parameters. Field writes become simple variable
    /// rebindings (no `maps:put` per iteration). At loop exit, mutated fields are repacked
    /// into the initial State map.
    #[allow(clippy::too_many_lines)]
    fn generate_counted_stateful_loop_hybrid(
        &mut self,
        frame: &CountedLoopFrame,
        body: &crate::ast::Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        // Collect initial arg values from the outer scope (before push_scope overwrites them).
        let initial_local_args = plan.initial_direct_args(self);
        let initial_state = plan.initial_state_var.clone();

        // Pre-extract ALL fields (readonly + mutated) before the letrec.
        // Each field is read once from the outer state via maps:get.
        let mut pre_extract_docs: Vec<Document<'static>> = Vec::new();

        // BT-1326: Read-only fields.
        let readonly_params: Vec<(String, String)> = plan
            .readonly_fields
            .iter()
            .map(|field| {
                let var_name = self.fresh_temp_var(&format!(
                    "{}Field",
                    CoreErlangGenerator::to_core_erlang_var(field)
                ));
                pre_extract_docs.push(docvec![
                    " let ",
                    Document::String(var_name.clone()),
                    " = call 'maps':'get'('",
                    Document::String(field.clone()),
                    "', ",
                    Document::String(initial_state.clone()),
                    ") in",
                ]);
                (field.clone(), var_name)
            })
            .collect();

        // BT-1342: Mutated fields — also pre-extracted to direct params.
        let mutated_params: Vec<(String, String)> = plan
            .mutated_fields
            .iter()
            .map(|field| {
                let var_name = self.fresh_temp_var(&format!(
                    "{}Field",
                    CoreErlangGenerator::to_core_erlang_var(field)
                ));
                pre_extract_docs.push(docvec![
                    " let ",
                    Document::String(var_name.clone()),
                    " = call 'maps':'get'('",
                    Document::String(field.clone()),
                    "', ",
                    Document::String(initial_state.clone()),
                    ") in",
                ]);
                (field.clone(), var_name)
            })
            .collect();

        // Fun param names: locals + readonly fields + mutated fields (NO State param).
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
            1 + local_param_names.len() + readonly_param_names.len() + mutated_param_names.len();

        // Build param list doc: (I, Var1, ..., VarN, RField1, ..., MField1, ...)
        let param_list_doc = join(
            std::iter::once(Document::Str("I"))
                .chain(
                    local_param_names
                        .iter()
                        .map(|v| Document::String(v.clone())),
                )
                .chain(
                    readonly_param_names
                        .iter()
                        .map(|v| Document::String(v.clone())),
                )
                .chain(
                    mutated_param_names
                        .iter()
                        .map(|v| Document::String(v.clone())),
                ),
            &Document::Str(", "),
        );

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(frame.preamble.clone());
        docs.extend(pre_extract_docs);
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

        // Bind block counter param if any (e.g. to:do: [:i | ...] → bind "i" → "I")
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, "I");
        }

        // Register local var bindings (no unpack docs emitted in hybrid mode).
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        debug_assert!(
            unpack_docs.is_empty(),
            "hybrid params: unpack should emit no code"
        );

        // Condition + true arm
        docs.push(frame.continue_header.clone());

        // Run body with in_hybrid_loop = true and hybrid_mutated_fields set.
        // Field reads resolve to direct parameters via hybrid_readonly_field_params.
        // Field writes rebind the param variable via hybrid_mutated_fields.
        let prev_hybrid = self.in_hybrid_loop;
        let prev_direct_params_loop = self.in_direct_params_loop;
        // Build combined field params map: readonly + mutated fields.
        let mut all_field_params: std::collections::HashMap<String, String> =
            readonly_params.iter().cloned().collect();
        for (field, var) in &mutated_params {
            all_field_params.insert(field.clone(), var.clone());
        }
        let prev_readonly_field_params =
            std::mem::replace(&mut self.hybrid_readonly_field_params, all_field_params);
        let prev_mutated_fields = std::mem::replace(
            &mut self.hybrid_mutated_fields,
            plan.mutated_fields.iter().cloned().collect(),
        );
        self.in_hybrid_loop = true;
        self.in_direct_params_loop = true; // BT-1329: nested list ops skip StateAcc repack
        let body_result = self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec);

        // BT-1342: Capture final mutated field var names BEFORE restoring maps.
        // Field writes in the body updated hybrid_readonly_field_params with new var names.
        let final_mutated_field_args: Vec<String> = plan
            .mutated_fields
            .iter()
            .map(|field| {
                self.hybrid_readonly_field_params
                    .get(field)
                    .cloned()
                    .unwrap_or_else(|| {
                        // No write happened — use the original param name.
                        mutated_params
                            .iter()
                            .find(|(f, _)| f == field)
                            .map(|(_, v)| v.clone())
                            .unwrap_or_default()
                    })
            })
            .collect();

        self.hybrid_mutated_fields = prev_mutated_fields;
        self.hybrid_readonly_field_params = prev_readonly_field_params;
        self.in_hybrid_loop = prev_hybrid;
        self.in_direct_params_loop = prev_direct_params_loop;
        let (body_doc, _final_state_version) = match body_result {
            Ok(result) => result,
            Err(err) => {
                self.pop_scope();
                return Err(err);
            }
        };
        docs.push(body_doc);

        // Final local var args after body (updated bindings from scope).
        let final_local_args: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| {
                self.lookup_var(v)
                    .cloned()
                    .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(v))
            })
            .collect();

        // Exit StateAcc: uses initial param names (current iteration's starting values).
        // In the exit arm (false branch), the body hasn't executed, so params are unchanged.
        let exit_stateacc = plan.generate_exit_stateacc_full_extract(
            &local_param_names,
            &mutated_param_names,
            &initial_state,
            self,
        );

        self.pop_scope();

        // Recursive call args: next_counter, updated locals, readonly fields (unchanged), updated mutated fields
        let recursive_args_doc = join(
            std::iter::once(Document::String(frame.next_counter.clone()))
                .chain(final_local_args.into_iter().map(Document::String))
                .chain(
                    readonly_param_names
                        .iter()
                        .map(|v| Document::String(v.clone())),
                )
                .chain(final_mutated_field_args.into_iter().map(Document::String)),
            &Document::Str(", "),
        );

        // Initial apply args: initial_counter, initial locals, initial readonly vals, initial mutated vals
        let initial_args_doc = join(
            std::iter::once(Document::String(frame.initial_counter.clone()))
                .chain(initial_local_args.into_iter().map(Document::String))
                .chain(
                    readonly_param_names
                        .iter()
                        .map(|v| Document::String(v.clone())),
                )
                .chain(
                    mutated_param_names
                        .iter()
                        .map(|v| Document::String(v.clone())),
                ),
            &Document::Str(", "),
        );

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
        self.emit_loop_convention_diagnostic(&plan, body.span);
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

        // BT-1329: Include variables captured and mutated by nested list op blocks.
        // `analyze_block` doesn't propagate local_writes from nested (non-conditional) blocks,
        // so variables mutated inside `do:`, `collect:`, `inject:into:`, `select:`, `reject:`
        // blocks are invisible to the outer loop's threaded_locals computation.
        // Scan the body for list op message sends and include their cross-scope mutations.
        let mut list_op_cross_scope_writes = std::collections::HashSet::new();
        for stmt in &body.body {
            Self::collect_list_op_cross_scope_mutations_recursive(
                &stmt.expression,
                &self.semantic_facts,
                &mut list_op_cross_scope_writes,
            );
        }

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
                // BT-1329: Add cross-scope list op mutations to both reads and writes.
                // These vars are both read and written in the nested block, so they need
                // threading through the outer loop.
                all_captured_reads = all_captured_reads
                    .union(&list_op_cross_scope_writes)
                    .cloned()
                    .collect();
                all_writes = all_writes
                    .union(&list_op_cross_scope_writes)
                    .cloned()
                    .collect();
                // BT-1329: Also include outer-scope variables that are written in the loop
                // body but not read (write-only). These variables need their final value
                // to escape the loop via StateAcc. We detect them by checking if the
                // variable already has a binding in the generator's scope (meaning it was
                // defined before the loop in the method body).
                for v in &all_writes {
                    if !block_params.contains(v.as_str())
                        && !all_captured_reads.contains(v)
                        && self.lookup_var(v).is_some()
                    {
                        all_captured_reads.insert(v.clone());
                    }
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
                let mut captured = analysis.captured_reads.clone();
                let mut writes = analysis.local_writes.clone();
                captured = captured
                    .union(&list_op_cross_scope_writes)
                    .cloned()
                    .collect();
                writes = writes.union(&list_op_cross_scope_writes).cloned().collect();
                // BT-1329: Include outer-scope write-only variables (same as Actor above).
                for v in &writes {
                    if !block_params.contains(v.as_str())
                        && !captured.contains(v)
                        && self.lookup_var(v).is_some()
                    {
                        captured.insert(v.clone());
                    }
                }
                captured
                    .intersection(&writes)
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
    ///
    /// Returns `(doc, Some(new_var_name))` so callers (e.g. `emit_local_assign_last_expr`)
    /// can reference the newly-bound variable by name (e.g. for FoldlCollect/FoldlInject).
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
                        Document::String(new_var.clone()),
                        " = ",
                        Document::String(result_var),
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
                    Document::String(new_var.clone()),
                    " = ",
                    value_code,
                    " in ",
                ];
                return Ok((doc, Some(new_var)));
            }
        }
        Ok((Document::Nil, None))
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
                // BT-1397: Clear before generating to detect open-scope results from
                // class method self-sends in the value expression.
                self.last_open_scope_result = None;
                let value_code = self.expression_doc(value)?;

                // Increment state version for the new state
                let _ = self.next_state_var();
                let new_state = if self.in_loop_body {
                    self.current_state_var()
                } else {
                    format!("State{}", self.state_version())
                };

                // BT-1397: If the RHS produced an open scope (class method self-send),
                // emit the open scope first, then bind the variable to the result.
                if let Some(open_scope_result) = self.last_open_scope_result.take() {
                    self.last_open_scope_result = Some(val_var.clone());
                    return Ok(docvec![
                        value_code,
                        "let ",
                        Document::String(val_var.clone()),
                        " = ",
                        Document::String(open_scope_result),
                        " in let ",
                        Document::String(new_state),
                        " = call 'maps':'put'('",
                        state_key.to_string(),
                        "', ",
                        Document::String(val_var.clone()),
                        ", ",
                        current_state,
                        ") in ",
                    ]);
                }

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
        facts: &crate::semantic_analysis::SemanticFacts,
    ) -> bool {
        use crate::ast::MessageSelector;
        if let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            if crate::state_threading_selectors::is_conditional_selector(sel.as_str()) {
                for arg in arguments {
                    if let Expression::Block(block) = arg {
                        let analysis = facts
                            .block_profile(&block.span)
                            .cloned()
                            .unwrap_or_else(|| block_analysis::analyze_block(block));
                        if analysis.local_writes.iter().any(|v| threaded.contains(v)) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// BT-1329: Collects variables that are captured and mutated by nested list op blocks.
    ///
    /// Scans a body expression for list op message sends (do:, collect:, etc.) with literal
    /// blocks, and adds any variables that are captured from the outer scope and written
    /// inside the block to `out`. These variables need threading through the outer loop.
    pub(in crate::codegen::core_erlang) fn collect_list_op_cross_scope_mutations(
        expr: &Expression,
        facts: &crate::semantic_analysis::SemanticFacts,
        out: &mut std::collections::HashSet<String>,
    ) {
        use crate::ast::MessageSelector;
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let body_block = match sel.as_str() {
            "do:" | "collect:" | "select:" | "reject:" => {
                if let Some(Expression::Block(block)) = arguments.first() {
                    block
                } else {
                    return;
                }
            }
            "inject:into:" => {
                if arguments.len() == 2 {
                    if let Expression::Block(block) = &arguments[1] {
                        block
                    } else {
                        return;
                    }
                } else {
                    return;
                }
            }
            _ => return,
        };

        let analysis = facts
            .block_profile(&body_block.span)
            .cloned()
            .unwrap_or_else(|| block_analysis::analyze_block(body_block));

        let block_params: std::collections::HashSet<String> = body_block
            .parameters
            .iter()
            .map(|p| p.name.to_string())
            .collect();

        for v in analysis.captured_reads.intersection(&analysis.local_writes) {
            if !block_params.contains(v.as_str()) {
                out.insert(v.clone());
            }
        }
    }

    /// BT-1329: Returns `true` if `expr` is a list op (do:, collect:, select:, reject:,
    /// inject:into:) whose block captures and mutates outer-scope locals but whose inner
    /// block is NOT eligible for tuple-acc optimization.
    ///
    /// When this returns `true`, the list op would fall back to map-accumulator mode which
    /// references `StateAcc` — incompatible with direct-params loops. The outer loop must
    /// fall back to `StateAcc` mode.
    fn list_op_needs_stateacc_fallback(
        expr: &Expression,
        facts: &crate::semantic_analysis::SemanticFacts,
    ) -> bool {
        use crate::ast::MessageSelector;
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();

        // Identify list ops and their body block argument
        let body_block = match sel.as_str() {
            "do:" | "collect:" | "select:" | "reject:" => {
                if let Some(Expression::Block(block)) = arguments.first() {
                    block
                } else {
                    return false;
                }
            }
            "inject:into:" => {
                if arguments.len() == 2 {
                    if let Expression::Block(block) = &arguments[1] {
                        block
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            _ => return false,
        };

        let analysis = facts
            .block_profile(&body_block.span)
            .cloned()
            .unwrap_or_else(|| block_analysis::analyze_block(body_block));

        // Check if inner block captures and mutates outer-scope locals
        let block_params: std::collections::HashSet<String> = body_block
            .parameters
            .iter()
            .map(|p| p.name.to_string())
            .collect();
        let has_cross_scope_mutations = analysis
            .captured_reads
            .intersection(&analysis.local_writes)
            .any(|v| !block_params.contains(v.as_str()));

        if !has_cross_scope_mutations {
            return false;
        }

        // Inner block has cross-scope mutations. Check if tuple-acc would be blocked.
        // These mirror the guards in ThreadingPlan::new_impl for use_tuple_acc.
        if analysis.has_state_effects() {
            // Field mutations — outer direct-params is already blocked by has_state_effects
            // propagation through analyze_block's nested Block handling. But be safe.
            return true;
        }

        // Check for conditional writes to threaded locals within the inner block body
        let inner_threaded: Vec<String> = analysis
            .captured_reads
            .intersection(&analysis.local_writes)
            .filter(|v| !block_params.contains(v.as_str()))
            .cloned()
            .collect();
        for stmt in &body_block.body {
            if Self::inline_conditional_writes_threaded(&stmt.expression, &inner_threaded, facts) {
                return true;
            }
        }

        // Check for destructure as last expression
        if body_block
            .body
            .last()
            .is_some_and(|s| matches!(s.expression, Expression::DestructureAssignment { .. }))
        {
            return true;
        }

        false
    }

    /// BT-1329: Recursive wrapper for `list_op_needs_stateacc_fallback` that also
    /// looks inside Assignment values. Without this, `result := items collect: [...]`
    /// inside a counted loop body would not be detected by the top-level scan.
    fn list_op_needs_stateacc_fallback_recursive(
        expr: &Expression,
        facts: &crate::semantic_analysis::SemanticFacts,
    ) -> bool {
        match expr {
            Expression::Assignment { value, .. } => {
                Self::list_op_needs_stateacc_fallback_recursive(value, facts)
            }
            Expression::MessageSend { .. } => Self::list_op_needs_stateacc_fallback(expr, facts),
            _ => false,
        }
    }
}
