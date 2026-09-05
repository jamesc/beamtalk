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
//! - **Dictionary iteration**: `do:`, `doWithKey:`
//! - **While loops**: `whileTrue:`, `whileFalse:`
//! - **Counted loops**: `repeat`, `timesRepeat:`, `to:do:`, `to:by:do:`
//!
//! Submodules organize the code by domain:
//! - [`list_ops`] — List iteration constructs
//! - [`dict_ops`] — Dictionary iteration constructs
//! - [`while_loops`] — While loop constructs
//! - [`counted_loops`] — Counted loop constructs

mod conditionals;
mod counted_loops;
mod dict_ops;
mod exception_handling;
mod list_ops;
mod while_loops;

use super::threaded_ir::{self, ThreadedStmt};
use super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, block_analysis};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};

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
///
/// BT-3129: `PartialEq`/`Eq` added so `threaded_ir::ThreadingMode` (which wraps
/// this in its `StateAcc` variant) can derive them too — needed for verifier
/// unit-test assertions comparing `VerifyError`s.
#[derive(Clone, Debug, PartialEq, Eq)]
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

/// BT-3147 (ADR 0111 Phase C completion): classifies which family of
/// `TupleAcc`-mode accumulator shape a foldl list-op uses, each with its own
/// canonical leading gate-slot count — the reserved tuple positions ahead of
/// the threaded locals that hold the op's own in-flight result/continuation
/// state. This is the "per-op declaration" / lowering-time source
/// [`threaded_ir::VerifyError::EarlyExitGateSlotMismatch`] cross-checks
/// against the unpack node's own rendering-time `gate_slots` (each call
/// site's own `index_offset - 1`, passed to
/// [`ThreadingPlan::generate_tuple_unpack_docs`] unchanged since BT-3133) —
/// see [`threaded_ir::build_tuple_acc_unpack`]'s doc comment for the full
/// independent-derivation rationale.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(super) enum ListOpKind {
    /// `do:` / dict `do:`/`doWithKey:` — no gate slots: `{Var1, ..., VarN}`.
    Do,
    /// `collect:`/`select:`/`reject:`/`inject:into:`/`anySatisfy:`/
    /// `allSatisfy:`/`count:`/`flatMap:`/`groupBy:` — one gate slot
    /// (`{Acc, Var1, ...}`).
    Accumulate,
    /// `detect:`/`takeWhile:`/`dropWhile:`/`partition:` — two gate slots
    /// (`{Result1, Result2, Var1, ...}`): either an early-exit found-item/
    /// continue-flag pair, or (`partition:`) two result lists — same shape,
    /// different semantics, so the SAME gate-slot count applies even though
    /// `partition:` never early-exits.
    TwoSlot,
}

impl ListOpKind {
    /// The canonical leading gate-slot count for this op family — the
    /// `mode_gate_slots` half of BT-3147's independent derivation.
    pub(super) const fn gate_slots(self) -> usize {
        match self {
            Self::Do => 0,
            Self::Accumulate => 1,
            Self::TwoSlot => 2,
        }
    }
}

/// Pre-computed contract for threading mutable state through a loop body.
///
/// Created once per loop and shared across pack / unpack / extract steps,
/// eliminating the copy-paste that previously existed in 7+ generators.
#[allow(
    clippy::struct_excessive_bools,
    reason = "each bool is an independent, mutually-orthogonal threading-mode flag \
              (direct-params / tuple-acc / hybrid-params / class-vars), not encodable \
              as a single state machine — mirrors CoreErlangGenerator's own allow"
)]
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
    /// BT-3147: the `TupleAcc` mode's canonical leading gate-slot count,
    /// declared at lowering time from the constructing call site's
    /// [`ListOpKind`] (`0` for plain `Do`; see [`ListOpKind::gate_slots`]).
    /// Only meaningful when `use_tuple_acc` is `true`; independent of each
    /// unpack call's own `index_offset - 1` (`generate_tuple_unpack_docs`'s
    /// `node_gate_slots`) — see
    /// [`threaded_ir::build_tuple_acc_unpack`]'s doc comment.
    pub tuple_acc_gate_slots: usize,
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
    /// BT-3168/BT-3169 (ADR 0111 Addendum 9, Questions 3/4/6): `true` when
    /// this loop/fold body threads a `ClassVars` mutation. Two mutually
    /// exclusive shapes, distinguished by `allow_direct_params` at
    /// construction time (never both true for the same plan):
    ///
    /// * **Letrec** (`new_for_letrec`, `allow_direct_params: true`, BT-3168):
    ///   `true` when the body has a direct class-var field write or a
    ///   same-class self-send (`generator.loop_body_threads_class_vars`).
    ///   Threads through the loop's own recursive tail call as an extra,
    ///   explicit trailing fun parameter, never folded into `StateAcc`'s own
    ///   map (Question 3). Per Question 4 Part A, any body shape that sets
    ///   this also always has `use_direct_params`/`use_tuple_acc`/
    ///   `use_hybrid_params` all `false`, so only the `StateAcc` base-path
    ///   loop generators (`while_loops.rs`, `counted_loops.rs`) ever consult
    ///   it in this shape.
    /// * **`Foldl*`** (`new`/`new_for_foldl_list_op`, `allow_direct_params:
    ///   false`, BT-3169): `true` when this is a class-method loop/fold body
    ///   (`generator.in_class_method()`, `context != Actor` — see this
    ///   field's own construction site for why the `Actor`-context exclusion
    ///   matters) that contains a self-send (`body_analysis.has_self_sends`)
    ///   — the only shape Question 4 Part A found reachable for `ClassVars`
    ///   mutation via a class-method self-send, since `has_self_sends`
    ///   already unconditionally forces `StateAcc`/plain-map-fold mode
    ///   whenever it's true. When `true`, the fold's own accumulator must
    ///   carry an extra `ClassVars` slot (a leading tuple position, Question
    ///   6) so a class-var mutation made by the self-send survives the fold
    ///   instead of being silently discarded — the exact BT-3151 gap BT-3169
    ///   closes.
    ///
    /// A bare class-var field write (not a self-send) inside a threaded
    /// `Foldl*` body is unaffected by the `Foldl*` shape above —
    /// `reject_class_var_field_assignment` already rejects that at compile
    /// time, unchanged by this field.
    pub threads_class_vars: bool,
    /// BT-3169: the class-var version name (`generator.current_class_var()`)
    /// in effect immediately before this loop/fold begins — mirrors
    /// `initial_state_var`'s own capture-at-construction-time discipline.
    /// Only meaningful for the `Foldl*` shape of `threads_class_vars`
    /// (`allow_direct_params: false`) — the Letrec shape threads `ClassVars`
    /// via its own recursive-call fun parameter instead, never consulting
    /// this field.
    pub initial_class_var: String,
}

/// Pre-computed body-effect predicates for threading strategy selection.
///
/// Analyzing the loop body for various effects (tier-2 calls, control-flow mutations,
/// conditional writes, etc.) requires iterating over all body statements. This struct
/// computes each predicate once and reuses the results across strategy selection and
/// fallback-reason diagnosis.
#[allow(clippy::struct_excessive_bools)]
struct BodyEffects {
    /// Condition block has state effects (field writes or self-sends).
    cond_has_state_effects: bool,
    /// Body has a tier-2 value call assigned to a threaded local.
    has_tier2_threaded_assign: bool,
    /// Body has nested list ops incompatible with direct-params (BT-1329).
    has_non_tuple_safe_list_op: bool,
    /// BT-2363: Body has a nested counted loop (`timesRepeat:`/`to:do:`/`to:by:do:`)
    /// that mutates a threaded outer local. The inner loop returns a `{value, StateAcc}`
    /// tuple that must be unpacked via `element(2, …)` to thread the local back out —
    /// incompatible with direct-params mode (which has no `StateAcc` to rebuild into).
    has_nested_counted_loop_mutation: bool,
    /// Body has control-flow sub-expressions with field mutations.
    has_cf_mutations: bool,
    /// Body has inline conditionals writing to threaded locals.
    has_conditional_threaded_writes: bool,
    /// Last expression in body is a `DestructureAssignment`.
    last_is_destructure: bool,
}

/// Whether a `whileTrue:`/`whileFalse:` `condition` expression has state
/// effects (a field write or self-send) that need to thread through the
/// loop. Factored out of [`BodyEffects::analyze`]'s own `cond_has_state_effects`
/// so `while_loops.rs`'s mode-SELECTION check (ADR 0118 phase 3, BT-3419:
/// a condition-only mutation must route a trivially-pure body to the
/// mutation-threading path too — see `generate_while_true`/
/// `generate_while_false`) shares one implementation with `ThreadingPlan`'s
/// own gate, per CLAUDE.md's no-duplicate-implementations rule.
pub(super) fn condition_has_state_effects(condition: &Expression) -> bool {
    if let Expression::Block(cond_block) = condition {
        block_analysis::analyze_block(cond_block).has_state_effects()
    } else {
        false
    }
}

impl BodyEffects {
    /// Analyze the loop body and condition to compute all effect predicates.
    fn analyze(
        generator: &CoreErlangGenerator,
        body: &beamtalk_core::ast::Block,
        condition: Option<&Expression>,
        threaded_locals: &[String],
    ) -> Self {
        let cond_has_state_effects = condition.is_some_and(condition_has_state_effects);

        // Guard: if any threaded-local assignment's RHS is a Tier-2 block call,
        // fall back to StateAcc mode so `generate_local_var_assignment_in_loop`
        // can properly unpack the {Result, NewStateAcc} tuple.
        let has_tier2_threaded_assign = body.body.iter().any(|s| {
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
        let has_non_tuple_safe_list_op = body.body.iter().any(|s| {
            CoreErlangGenerator::list_op_needs_stateacc_fallback_recursive(
                &s.expression,
                &generator.semantic_facts,
            )
        });

        // BT-2363: Detect a nested counted loop that mutates a threaded outer local.
        // Such an inner loop returns a `{value, StateAcc}` tuple; the outer loop must
        // unpack `element(2, …)` to propagate the local — only possible in StateAcc mode.
        let has_nested_counted_loop_mutation = body.body.iter().any(|s| {
            generator.expr_has_nested_counted_loop_threading(&s.expression, threaded_locals)
        });

        // Guard: control-flow sub-expressions with field mutations (e.g.
        // `flag ifTrue: [self.n := ...]`). These generate `StateAcc`-dependent code.
        let has_cf_mutations = body
            .body
            .iter()
            .any(|s| generator.control_flow_has_mutations(&s.expression));

        // Guard: inline conditionals that write threaded locals (e.g.
        // `each > max ifTrue: [max := each]`). `control_flow_has_mutations` only
        // catches field writes; this catches the pure-overwrite-local pattern.
        let has_conditional_threaded_writes = body.body.iter().any(|s| {
            CoreErlangGenerator::inline_conditional_writes_threaded(
                &s.expression,
                threaded_locals,
                &generator.semantic_facts,
            )
        });

        // DestructureAssignment as the last expr is not supported in tuple-acc mode:
        // `emit_destructure_last_expr` always emits the map-shaped StateAcc path.
        let last_is_destructure = body
            .body
            .last()
            .is_some_and(|s| matches!(s.expression, Expression::DestructureAssignment { .. }));

        Self {
            cond_has_state_effects,
            has_tier2_threaded_assign,
            has_non_tuple_safe_list_op,
            has_nested_counted_loop_mutation,
            has_cf_mutations,
            has_conditional_threaded_writes,
            last_is_destructure,
        }
    }
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
        body: &beamtalk_core::ast::Block,
        condition: Option<&Expression>,
    ) -> Self {
        Self::new_impl(generator, body, condition, false, None)
    }

    /// Creates a `ThreadingPlan` for a letrec-based loop body (whileTrue:, timesRepeat:, etc.).
    ///
    /// BT-1275: Sets `use_direct_params = true` when the body has no field mutations or
    /// self-sends, eliminating per-iteration `maps:get`/`maps:put` overhead.
    ///
    /// Only valid for letrec loops where each variable can be passed as a fun parameter.
    pub fn new_for_letrec(
        generator: &mut CoreErlangGenerator,
        body: &beamtalk_core::ast::Block,
        condition: Option<&Expression>,
    ) -> Self {
        Self::new_impl(generator, body, condition, true, None)
    }

    /// BT-1276: Creates a `ThreadingPlan` for a foldl list-op body with tuple accumulator
    /// optimization (`do:`, `collect:`, `select:`/`reject:`, `inject:into:`).
    ///
    /// Sets `use_tuple_acc = true` when eligible: body has only simple local variable
    /// mutations (no field writes, no self-sends, no complex control flow, no tier-2
    /// assignments to threaded locals). Replaces per-iteration `StateAcc` map operations
    /// with a flat tuple accumulator.
    ///
    /// BT-3147: `kind` declares this call site's canonical `TupleAcc` gate-slot
    /// count ([`ListOpKind::gate_slots`]) at construction time — independent
    /// of whatever `index_offset` the caller later passes to
    /// [`Self::generate_tuple_unpack_docs`], see that method's doc comment.
    pub fn new_for_foldl_list_op(
        generator: &mut CoreErlangGenerator,
        body: &beamtalk_core::ast::Block,
        kind: ListOpKind,
    ) -> Self {
        Self::new_impl(generator, body, None, false, Some(kind))
    }

    fn new_impl(
        generator: &mut CoreErlangGenerator,
        body: &beamtalk_core::ast::Block,
        condition: Option<&Expression>,
        allow_direct_params: bool,
        tuple_acc_kind: Option<ListOpKind>,
    ) -> Self {
        let allow_tuple_acc = tuple_acc_kind.is_some();
        if generator.is_repl_mode() {
            generator.set_repl_loop_mutated(true);
        }
        let key_style = if generator.is_repl_mode() {
            KeyStyle::ReplPlain
        } else {
            KeyStyle::LocalPrefixed
        };
        let context = generator.context;
        let threaded_locals = generator.compute_threaded_locals_for_loop(body, condition);
        let initial_state_var = generator.current_state_var();

        // Pre-analyze body once — reused across all strategy decisions.
        let body_analysis = block_analysis::analyze_block(body);

        // Pre-compute all body-effect predicates once to avoid repeated iteration.
        let effects = BodyEffects::analyze(generator, body, condition, &threaded_locals);

        // BT-1275: Direct fun parameters for letrec loops.
        let use_direct_params = Self::select_direct_params(
            allow_direct_params,
            &threaded_locals,
            &body_analysis,
            &effects,
        );

        // BT-1276: Tuple accumulator for foldl list-ops.
        let use_tuple_acc = Self::select_tuple_acc(
            allow_tuple_acc,
            &threaded_locals,
            context,
            &body_analysis,
            &effects,
        );

        // BT-3133 (ADR 0111 Phase C, invariant class 2) / BT-3147: no runtime
        // check here anymore — `select_tuple_acc`'s own `matches!(context,
        // CodeGenContext::ValueType)` early return (above) already makes
        // `use_tuple_acc && context_is_value_type` unconditionally
        // unreachable BY INSPECTION of that one function, the same
        // already-structural shape BT-3154 found for
        // `ThreadingModeUnpackMismatch`. `VerifyError::TupleAccInValueTypeContext`,
        // `threaded_ir::verify_tuple_acc_value_type_exclusion`, and their
        // hand-built-IR unit tests remain as regression pins (ADR 0111
        // §Verifier honesty) — only this now-tautological production call
        // site is gone.

        // BT-3133 (ADR 0111 Phase C, invariant class 3) / BT-3147: likewise
        // no runtime check here — `select_direct_params`'s own
        // `!effects.has_non_tuple_safe_list_op` conjunct (above) already
        // makes `use_direct_params && effects.has_non_tuple_safe_list_op`
        // unconditionally unreachable by the same reasoning.
        // `VerifyError::NestedStateAccFallbackUnderDirectParams`,
        // `threaded_ir::verify_nested_list_op_stateacc_compat`, and their
        // hand-built-IR unit tests remain as regression pins.

        // BT-1326: Hybrid direct-params + State threading for letrec loops.
        // BT-3169 (ADR 0111 Addendum 9, Question 4 Part B): also excluded for
        // any class-method loop/fold body — a class method has no instance
        // `State` map to amortize `Hybrid`'s pre-extraction against, and
        // `field_writes` inside a class method is, by construction, 100%
        // class-var names (a class method has no `self.field` instance
        // field at all), so `Hybrid`'s pre-extraction was latently reachable
        // there for exactly the wrong-pre-extraction shape this addendum
        // found — see `select_hybrid_params`'s own doc comment.
        let use_hybrid_params = Self::select_hybrid_params(
            allow_direct_params,
            &threaded_locals,
            context,
            use_direct_params,
            &body_analysis,
            &effects,
            generator,
        );

        // BT-1326: In hybrid mode, collect fields that are read but never written.
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
        let optimized_selected = use_direct_params || use_tuple_acc || use_hybrid_params;
        let any_optimization_allowed = allow_direct_params || allow_tuple_acc;
        let fallback_reason = Self::determine_fallback_reason(
            optimized_selected,
            any_optimization_allowed,
            &threaded_locals,
            context,
            &body_analysis,
            &effects,
        );

        // BT-1342: In hybrid mode, collect fields that are written (mutated).
        let mutated_fields = if use_hybrid_params {
            let mut fields: Vec<String> = body_analysis.field_writes.iter().cloned().collect();
            fields.sort(); // deterministic codegen
            fields
        } else {
            vec![]
        };

        // BT-3147: the mode's canonical gate-slot count, declared here at
        // lowering time from the caller's `ListOpKind` — independent of
        // whatever `index_offset` a later `generate_tuple_unpack_docs` call
        // computes its own `node_gate_slots` from.
        let tuple_acc_gate_slots = tuple_acc_kind.map_or(0, ListOpKind::gate_slots);

        // BT-3168 (ADR 0111 Addendum 9, Question 3/4): whether this Letrec
        // loop body threads a `ClassVars` mutation through its own recursive
        // tail call. Only ever true for `new_for_letrec`-constructed plans
        // (`allow_direct_params`).
        //
        // BT-3169 (ADR 0111 Addendum 9, Questions 3/4/6): a class-method
        // `Foldl*` body containing a self-send needs to thread `ClassVars`
        // through the fold's own accumulator. Excluded for `Actor` context:
        // `is_actor_self_send` (checked before any class-method-self-send
        // path in `generate_threaded_loop_body_inner`) unconditionally wins
        // for a `self <msg>` send whenever `context == Actor`, regardless of
        // `in_class_method()` — an Actor subclass's class-method self-send
        // never reaches the `emit_class_var_result_unwrap`/`class_bump` path
        // this field's threading exists to support, so claiming
        // `threads_class_vars` there would build a fun signature/accumulator
        // shape the body never actually populates. Scoped to the addendum's
        // own confirmed-reachable repros (`ValueType`/`Object subclass:`
        // class methods) — not a general fix for that separate, pre-existing
        // Actor-class-method-self-send gap, out of this issue's scope.
        // `!allow_direct_params` restricts this to Foldl-shaped constructors
        // (`new_for_foldl_list_op` and the plain `new` compat-shim variant) —
        // `new_for_letrec` passes `allow_direct_params: true` unconditionally,
        // so a `whileTrue:`/`timesRepeat:`/`to:do:` (`BodyKind::Letrec`) plan
        // never sets this field, regardless of self-sends. This is a hard
        // safety boundary, not merely an optimization: BT-3169's own
        // `generate_threaded_loop_body_inner` wrap (below, guarded on this
        // same field) is Foldl-only by design (Question 6's `{ClassVars,
        // StateAcc}` accumulator shape has no Letrec analogue — Letrec's own
        // `ClassVars` threading is BT-3168's parallel, independent migration,
        // via an extra `letrec` fun parameter, never this accumulator wrap).
        // A direct top-level self-send statement inside a real Letrec body is
        // already unconditionally rejected before reaching this wrap
        // (`ClassMethodSelfSendInThreadedLoopBody`, this file's `else if
        // matches!(kind, BodyKind::Letrec) && self.is_class_method_self_send`
        // arm) — but a self-send nested inside a DEEPER block within a
        // Letrec body (e.g. `whileTrue: [ i := i + 1. aList do: [:x | self
        // bump] ]`) would not trip that direct-statement check, since
        // `body_analysis.has_self_sends` recurses into nested blocks while
        // `is_class_method_self_send` only inspects the top-level statement
        // expression — this gate is what keeps that shape from reaching the
        // Foldl-only wrap on the OUTER Letrec plan (the nested `do:`'s own,
        // separately-constructed Foldl plan still threads correctly on its
        // own terms).
        let threads_class_vars = if allow_direct_params {
            // Letrec shape (BT-3168): `new_for_letrec`-constructed plans only.
            generator.loop_body_threads_class_vars(body)
        } else {
            // Foldl* shape (BT-3169): `new`/`new_for_foldl_list_op`-constructed
            // plans only.
            !matches!(context, CodeGenContext::Actor)
                && generator.in_class_method()
                && body_analysis.has_self_sends
        };
        let initial_class_var = generator.current_class_var();

        Self {
            threaded_locals,
            initial_state_var,
            key_style,
            context,
            use_direct_params,
            use_tuple_acc,
            tuple_acc_gate_slots,
            use_hybrid_params,
            readonly_fields,
            fallback_reason,
            mutated_fields,
            threads_class_vars,
            initial_class_var,
        }
    }

    /// BT-1275: Select direct fun parameters for letrec loops when the body has no
    /// field mutations, self-sends, tier-2 threaded assignments, or nested list ops
    /// incompatible with direct-params mode.
    fn select_direct_params(
        allow_direct_params: bool,
        threaded_locals: &[String],
        body_analysis: &block_analysis::BlockMutationAnalysis,
        effects: &BodyEffects,
    ) -> bool {
        if !allow_direct_params || threaded_locals.is_empty() {
            return false;
        }
        !body_analysis.has_state_effects()
            && !effects.cond_has_state_effects
            && !effects.has_tier2_threaded_assign
            && !effects.has_non_tuple_safe_list_op
            && !effects.has_nested_counted_loop_mutation
    }

    /// BT-1276: Select tuple accumulator for foldl list-ops when eligible: body has
    /// only simple local var mutations — no field writes, self-sends, tier-2 assignments,
    /// complex control flow, conditional threaded writes, or destructure-as-last-expr.
    ///
    /// `ValueType` methods have no `State` `gen_server` variable in scope; tuple-acc would
    /// reference an unbound variable — always fall through to the map-acc path.
    fn select_tuple_acc(
        allow_tuple_acc: bool,
        threaded_locals: &[String],
        context: CodeGenContext,
        body_analysis: &block_analysis::BlockMutationAnalysis,
        effects: &BodyEffects,
    ) -> bool {
        if !allow_tuple_acc
            || threaded_locals.is_empty()
            || matches!(context, CodeGenContext::ValueType)
        {
            return false;
        }
        !body_analysis.has_state_effects()
            && !effects.has_tier2_threaded_assign
            && !effects.has_cf_mutations
            && !effects.has_conditional_threaded_writes
            && !effects.last_is_destructure
    }

    /// BT-1326: Select hybrid direct-params + State threading for letrec loops.
    ///
    /// Eligible when body has field mutations but NOT self-sends, and no guards
    /// (tier-2 assignments, control-flow mutations, conditional writes, nested list ops)
    /// prevent it. Actor context only (`ValueType` has no actor State to thread).
    ///
    /// BT-3168 (ADR 0111 Addendum 9, Question 4 Part B): also excluded for ANY
    /// class-method loop/fold body (`generator.in_class_method()`), not just
    /// `ValueType` ones. A class method has no instance `self.field` at all —
    /// `field_writes` inside a class-method body is, by construction, 100%
    /// class-var names — and Hybrid mode's entire premise (amortizing the
    /// actor `State` map's per-iteration `maps:get`/`maps:put` cost by
    /// pre-extracting mutated fields as direct fun params) has no `State` to
    /// amortize against inside a class method. Before this guard, an `Actor`
    /// subclass's class-method loop body mutating only a class var could
    /// latently select Hybrid mode (its own `CodeGenContext::Actor` check
    /// alone doesn't distinguish "instance method on an Actor class" from
    /// "class method on an Actor class") — never manifesting as a visible bug
    /// only because `reject_class_var_field_assignment` fired downstream
    /// regardless of the selected mode; now that class-var writes thread
    /// through `StateAcc` mode instead of being rejected, mode selection must
    /// route them there correctly rather than latently into Hybrid.
    fn select_hybrid_params(
        allow_direct_params: bool,
        threaded_locals: &[String],
        context: CodeGenContext,
        use_direct_params: bool,
        body_analysis: &block_analysis::BlockMutationAnalysis,
        effects: &BodyEffects,
        generator: &CoreErlangGenerator,
    ) -> bool {
        if !allow_direct_params
            || threaded_locals.is_empty()
            || use_direct_params
            || !matches!(context, CodeGenContext::Actor)
            || generator.in_class_method()
        {
            return false;
        }
        !body_analysis.field_writes.is_empty()
            && !body_analysis.has_self_sends
            && !effects.cond_has_state_effects
            && !effects.has_tier2_threaded_assign
            && !effects.has_cf_mutations
            && !effects.has_conditional_threaded_writes
            && !effects.has_non_tuple_safe_list_op
    }

    /// BT-1343: Determine why `StateAcc` fallback was chosen (if no optimized mode was selected).
    ///
    /// `optimized_selected` is true when any optimized convention was chosen.
    /// `any_optimization_allowed` is true when the caller allows direct-params or tuple-acc.
    fn determine_fallback_reason(
        optimized_selected: bool,
        any_optimization_allowed: bool,
        threaded_locals: &[String],
        context: CodeGenContext,
        body_analysis: &block_analysis::BlockMutationAnalysis,
        effects: &BodyEffects,
    ) -> StateAccFallbackReason {
        if optimized_selected {
            return StateAccFallbackReason::None;
        }
        if threaded_locals.is_empty() {
            return StateAccFallbackReason::NoThreadedLocals;
        }
        if !any_optimization_allowed {
            return StateAccFallbackReason::NotLetrec;
        }
        if body_analysis.has_self_sends {
            return StateAccFallbackReason::SelfSendInBody;
        }
        if !body_analysis.field_writes.is_empty() {
            // Field writes present (self-sends already excluded above) but not eligible
            // for hybrid — check specific guards.
            return Self::diagnose_guard_failure(body_analysis, effects);
        }
        if matches!(context, CodeGenContext::ValueType) {
            return StateAccFallbackReason::ValueTypeContext;
        }
        // No field writes, not ValueType — check the specific guards that prevented optimization.
        Self::diagnose_guard_failure(body_analysis, effects)
    }

    /// Identify the specific guard that prevented an optimized threading convention.
    fn diagnose_guard_failure(
        body_analysis: &block_analysis::BlockMutationAnalysis,
        effects: &BodyEffects,
    ) -> StateAccFallbackReason {
        if effects.has_non_tuple_safe_list_op {
            StateAccFallbackReason::NestedListOpCrossScope
        } else if effects.has_tier2_threaded_assign {
            StateAccFallbackReason::Tier2ValueCallOnThreaded
        } else if effects.cond_has_state_effects {
            StateAccFallbackReason::ConditionStateEffects
        } else if effects.has_cf_mutations {
            StateAccFallbackReason::ControlFlowMutations
        } else if effects.has_conditional_threaded_writes {
            StateAccFallbackReason::InlineConditionalThreadedWrite
        } else if effects.last_is_destructure {
            StateAccFallbackReason::DestructureAsLastExpr
        } else if body_analysis.has_self_sends {
            StateAccFallbackReason::SelfSendInBody
        } else {
            StateAccFallbackReason::ControlFlowMutations
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
    /// For class methods (BT-3055), also starts from a fresh `maps:new()`: a class
    /// method's signature is `(ClassSelf, ClassVars, Args...)` — there is no `State`
    /// parameter to pack from, even when `self.context` is `Actor` (an actor class's
    /// class methods still run with `context == Actor`, since the enclosing class is
    /// an actor even though the *method* itself has no per-instance state).
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
        let mut pack_docs: Vec<Document<'static>> = Vec::new();
        // BT-1053/BT-3055: Value-type methods and class methods have no actor State
        // to pack from — start from a fresh empty map instead.
        let mut current =
            if matches!(self.context, CodeGenContext::ValueType) || generator.in_class_method() {
                let init_map_var = generator.fresh_temp_var("InitMap");
                pack_docs.push(docvec![
                    "let ",
                    leaf::var(init_map_var.clone()),
                    " = call 'maps':'new'() in ",
                ]);
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
            pack_docs.push(docvec![
                "let ",
                leaf::var(packed_var.clone()),
                " = call 'maps':'put'(",
                leaf::atom(key),
                ", ",
                leaf::var(core_var),
                ", ",
                leaf::var(current),
                ") in ",
            ]);
            current = packed_var;
        }
        (Document::Vec(pack_docs), current)
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
                    leaf::var(core_var),
                    " = call 'maps':'get'(",
                    leaf::atom(key),
                    ", StateAcc) in ",
                ]);
            }
        }
        docs
    }

    /// BT-3169 (ADR 0111 Addendum 9, Question 6): returns the fold fun's own
    /// second (accumulator) parameter name to print at the `fun (Item, <here>) ->`
    /// position, plus a prelude `Document` binding `real_param_name` (and,
    /// when threading, the loop-entry `ClassVars` name) from it.
    ///
    /// When `threads_class_vars` is `false`, returns `(real_param_name,
    /// Document::Nil)` unchanged — the caller's existing `fun (Item,
    /// <real_param_name>) -> ...` continues to bind the accumulator directly,
    /// byte-identical to before this field existed.
    ///
    /// When `true`, the fold's own accumulator is wrapped one level deeper as
    /// `{ClassVars, <original accumulator>}` (Question 6's "`gate_slots=0`"
    /// shape — the only reachable one per Question 4 Part A). This method
    /// mints a fresh raw parameter name to receive that 2-tuple and returns a
    /// prelude that unwraps it: `let <initial_class_var> = element(1, Raw) in
    /// let <real_param_name> = element(2, Raw) in`. Every existing line of
    /// code downstream of the fun header that references `real_param_name`
    /// (however it further destructures that value — a bare `StateAcc`, or a
    /// `{AccList, StateAcc}` pair for `collect:`/`inject:into:`-shaped
    /// bodies) needs no change: after this prelude, `real_param_name` is
    /// bound to exactly the same value it always was.
    pub fn class_var_fun_param(
        &self,
        generator: &mut CoreErlangGenerator,
        real_param_name: &str,
    ) -> (String, Document<'static>) {
        if !self.threads_class_vars {
            return (real_param_name.to_string(), Document::Nil);
        }
        let raw = generator.fresh_temp_var("AccCV");
        let doc = docvec![
            "let ",
            leaf::var(self.initial_class_var.clone()),
            " = call 'erlang':'element'(1, ",
            leaf::var(raw.clone()),
            ") in let ",
            leaf::var(real_param_name.to_string()),
            " = call 'erlang':'element'(2, ",
            leaf::var(raw.clone()),
            ") in ",
        ];
        (raw, doc)
    }

    /// BT-3169 (ADR 0111 Addendum 9, Question 6): builds
    /// `" in let <fold_result> = call 'lists':'foldl'(<lambda>, <init_acc>,
    /// <list>) in "` — transparently wrapping `init_acc` with a leading
    /// `ClassVars` slot, and unwrapping the fold's own result back out
    /// immediately after the call, whenever `threads_class_vars`. Every call
    /// site's existing post-fold code keeps referencing `fold_result` by the
    /// same name, bound to exactly the same (unwrapped) shape it always was —
    /// only the freshly-minted post-fold `ClassVars` version name differs,
    /// silently making the mutated value visible to subsequent statements in
    /// the calling method via the generator's own class-var version counter
    /// (`next_class_var`).
    ///
    /// When `threads_class_vars` is `false`, this is exactly the `" in let
    /// <fold_result> = call 'lists':'foldl'(...) in "` text every call site
    /// built by hand before this method existed — byte-identical.
    pub fn foldl_call_doc(
        &self,
        generator: &mut CoreErlangGenerator,
        lambda_var: &str,
        init_acc: Document<'static>,
        safe_list_var: &str,
        fold_result: &str,
    ) -> Document<'static> {
        if !self.threads_class_vars {
            return docvec![
                " in let ",
                leaf::var(fold_result.to_string()),
                " = call 'lists':'foldl'(",
                leaf::var(lambda_var.to_string()),
                ", ",
                init_acc,
                ", ",
                leaf::var(safe_list_var.to_string()),
                ") in ",
            ];
        }
        let raw = generator.fresh_temp_var("RawFoldCV");
        // BT-3169: fast-forward past whatever peak the fold body's own
        // closure reached internally (already restored by now) before
        // minting — otherwise this mint can collide with an
        // already-used-inside-the-closure name (Core Erlang requires
        // globally unique variable names across nested `fun` scopes within
        // one compiled function) — see `last_foldl_class_var_peak`'s doc
        // comment.
        generator.catch_up_class_var_version_to_foldl_peak();
        let cv_after = generator.next_class_var();
        docvec![
            " in let ",
            leaf::var(raw.clone()),
            " = call 'lists':'foldl'(",
            leaf::var(lambda_var.to_string()),
            ", {",
            leaf::var(self.initial_class_var.clone()),
            ", ",
            init_acc,
            "}, ",
            leaf::var(safe_list_var.to_string()),
            ") in let ",
            leaf::var(cv_after),
            " = call 'erlang':'element'(1, ",
            leaf::var(raw.clone()),
            ") in let ",
            leaf::var(fold_result.to_string()),
            " = call 'erlang':'element'(2, ",
            leaf::var(raw),
            ") in ",
        ]
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
    ///
    /// BT-3055: mirrors `generate_pack_prefix`'s `ValueType`/`in_class_method` check —
    /// this is the direct-params fast path's own `StateAcc` rebuild, and class methods
    /// have no `State` to rebuild from here either.
    pub fn generate_exit_stateacc(
        &self,
        param_names: &[String],
        generator: &mut CoreErlangGenerator,
    ) -> Document<'static> {
        let starts_from_fresh_map =
            matches!(self.context, CodeGenContext::ValueType) || generator.in_class_method();
        if self.threaded_locals.is_empty() && !starts_from_fresh_map {
            return docvec!["{'nil', ", leaf::var(self.initial_state_var.clone()), "}",];
        }
        let mut docs: Vec<Document<'static>> = Vec::new();
        // BT-1053/BT-3055: Value-type methods and class methods have no actor State
        // to rebuild from — start from a fresh empty map instead.
        let mut current = if starts_from_fresh_map {
            let exit_var = generator.fresh_temp_var("ExitSA");
            docs.push(docvec![
                "let ",
                leaf::var(exit_var.clone()),
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
                leaf::var(next_var.clone()),
                " = call 'maps':'put'(",
                leaf::atom(key),
                ", ",
                leaf::var(param.clone()),
                ", ",
                leaf::var(current),
                ") in ",
            ]);
            current = next_var;
        }
        docs.push(docvec!["{'nil', ", leaf::var(current), "}",]);
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
                leaf::var(next_var.clone()),
                " = call 'maps':'put'(",
                leaf::atom(field_name.clone()),
                ", ",
                leaf::var(param.clone()),
                ", ",
                leaf::var(current),
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
                leaf::var(next_var.clone()),
                " = call 'maps':'put'(",
                leaf::atom(key),
                ", ",
                leaf::var(param.clone()),
                ", ",
                leaf::var(current),
                ") in ",
            ]);
            current = next_var;
        }

        docs.push(docvec!["{'nil', ", leaf::var(current), "}"]);
        Document::Vec(docs)
    }

    /// Generates `let X = maps:get(key, FinalState) in` for each threaded local
    /// to extract updated values after the loop completes.
    ///
    /// Returns the extract code as a `Document` (BT-2216: replaces the legacy
    /// `String` variant that used `format!` to produce CE syntax).
    pub fn generate_extract_suffix_doc(
        &self,
        final_state_var: &str,
        generator: &CoreErlangGenerator,
    ) -> Document<'static> {
        let mut docs: Vec<Document<'static>> = Vec::new();
        for var_name in &self.threaded_locals {
            let core_var = generator
                .lookup_var(var_name)
                .cloned()
                .unwrap_or_else(|| CoreErlangGenerator::to_core_erlang_var(var_name));
            let key = self.state_key(var_name);
            docs.push(docvec![
                "let ",
                leaf::var(core_var),
                " = call 'maps':'get'(",
                leaf::atom(key),
                ", ",
                leaf::var(final_state_var.to_string()),
                ") in ",
            ]);
        }
        Document::Vec(docs)
    }

    // ─── BT-1276: Tuple accumulator helpers ───────────────────────────────────

    /// Returns the current Core Erlang bindings of all threaded locals as a `Document`.
    ///
    /// Example: `threaded_locals = ["sum", "count"]` → `Sum1, Count`.
    /// Returns `Document::Nil` when `threaded_locals` is empty.
    pub fn current_vars_doc(&self, generator: &CoreErlangGenerator) -> Document<'static> {
        join(
            self.threaded_locals.iter().map(|v| {
                leaf::var(
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
    ///
    /// BT-3147: real `ThreadedIr` emission input now — this builds
    /// [`threaded_ir::build_tuple_acc_unpack`]'s `ThreadedStmt`, `verify()`s
    /// it, and [`threaded_ir::render`]s it directly; the pre-BT-3147
    /// hand-rolled `let`-chain loop and the separate verification-only
    /// fixture it sat alongside are both gone (see `threaded_ir`'s module
    /// docs §Status). `self.tuple_acc_gate_slots` (declared at
    /// `ThreadingPlan` construction, from the caller's [`ListOpKind`]) and
    /// `index_offset - 1` (this call's own, unchanged) are genuinely
    /// independent sources for [`VerifyError::EarlyExitGateSlotMismatch`]
    /// to cross-check — no span is available at this call depth
    /// (`ThreadingPlan` carries none); this is a compiler-internal
    /// invariant, not user-facing, so `Span::default()` is an acceptable
    /// diagnostic-location gap here (mirrors `verify`'s own `produces`
    /// check, which does the same).
    pub fn generate_tuple_unpack_docs(
        &self,
        generator: &mut CoreErlangGenerator,
        source_var: &str,
        index_offset: usize,
    ) -> Document<'static> {
        let (stmt, targets) = threaded_ir::build_tuple_acc_unpack(
            source_var,
            self.tuple_acc_gate_slots,
            index_offset.saturating_sub(1),
            &self.threaded_locals,
            Span::default(),
        );

        let errors = threaded_ir::verify(std::slice::from_ref(&stmt));
        generator.report_threaded_ir_verify_errors(
            &errors,
            "tuple-acc positional-unpack mode/shape mismatch",
            Span::default(),
        );

        for (var_name, target) in self.threaded_locals.iter().zip(&targets) {
            generator.bind_var(var_name, &target.render_name());
        }

        let mut ctx = threaded_ir::RenderCtx::new(generator);
        threaded_ir::render(std::slice::from_ref(&stmt), &mut ctx)
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
                leaf::var(core_var),
                " = call 'erlang':'element'(",
                leaf::int_lit(i64::try_from(idx).unwrap_or(0)),
                ", ",
                leaf::var(final_acc_var.to_string()),
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
                leaf::var(pack_var.clone()),
                " = call 'maps':'put'(",
                leaf::atom(key),
                ", ",
                leaf::var(core_var),
                ", ",
                leaf::var(current),
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

    /// Foldl `anySatisfy:`/`allSatisfy:` body: last expression becomes a predicate;
    /// a `case` expression updates a boolean accumulator.
    /// Accumulator is `{BoolAcc, StateAcc{N}}`.
    FoldlBoolPredicate {
        /// When `true`, semantics = `allSatisfy:` (start `true`, set `false` on failure).
        /// When `false`, semantics = `anySatisfy:` (start `false`, set `true` on match).
        is_all: bool,
    },

    /// BT-1486: Foldl `detect:` / `detect:ifNone:` body: last expression becomes a predicate;
    /// a `case` expression updates the found-item accumulator on first match.
    /// Accumulator is `{FoundItem, FoundFlag, StateAcc{N}}`.
    FoldlDetect {
        /// The item variable (element being iterated).
        item_var: String,
    },

    /// BT-1486: Foldl `count:` body: last expression becomes a predicate;
    /// a `case` expression increments the count accumulator on match.
    /// Accumulator is `{Count, StateAcc{N}}`.
    FoldlCount,

    /// BT-1487: Foldl `takeWhile:` body: last expression becomes a predicate;
    /// a `case` expression includes the item only while the predicate holds.
    /// Once the predicate returns false, all subsequent elements are excluded.
    /// Accumulator is `{ResultList, StillTaking, StateVars...}`.
    FoldlTakeWhile {
        /// The item variable (element being iterated).
        item_var: String,
    },

    /// BT-1487: Foldl `dropWhile:` body: last expression becomes a predicate;
    /// a `case` expression drops elements while the predicate holds.
    /// Once the predicate returns false, all subsequent elements are included.
    /// Accumulator is `{ResultList, StillDropping, StateVars...}`.
    FoldlDropWhile {
        /// The item variable (element being iterated).
        item_var: String,
    },

    /// BT-1487: Foldl `partition:` body: last expression becomes a predicate;
    /// a `case` expression routes the item to one of two lists.
    /// Accumulator is `{MatchList, NoMatchList, StateVars...}`.
    FoldlPartition {
        /// The item variable (element being iterated).
        item_var: String,
    },

    /// BT-1487: Foldl `groupBy:` body: last expression is the key function result;
    /// each element is grouped by its key into a map.
    /// Accumulator is `{Map, StateVars...}`.
    FoldlGroupBy {
        /// The item variable (element being iterated).
        item_var: String,
    },

    /// BT-1487: `sort:` with state threading via process dictionary.
    /// The comparator block's state mutations are threaded through the process dictionary.
    /// Not constructed directly — sort: generates its own body inline. Listed for
    /// exhaustive match coverage in body-kind dispatchers.
    #[allow(dead_code)]
    FoldlSort,
}

/// BT-3172: which family a [`Self::nested_loop_or_fold_body`] match belongs
/// to — `ThreadingPlan::threads_class_vars` uses a genuinely different
/// formula for each (see that field's doc comment), so
/// [`Self::nested_loop_lost_class_var_mutation`] must apply the matching
/// one rather than a single one-size-fits-all check.
#[derive(Clone, Copy, PartialEq, Eq)]
enum NestedLoopShape {
    /// `whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/`to:by:do:` — the
    /// `BodyKind::Letrec` shapes, gated by the narrow, top-level-only
    /// `loop_body_threads_class_vars`.
    Letrec,
    /// `do:`/`collect:`/`select:`/... — the `BodyKind::Foldl*` shapes,
    /// gated by the recursive, Actor-excluded `has_self_sends` formula.
    Foldl,
}

// ─── CountedLoopFrame ─────────────────────────────────────────────────────────

/// Describes the loop-type-specific structure of a counted (`letrec`-based) loop.
///
/// Each `generate_*_with_mutations` for counted loops becomes a thin wrapper
/// that builds a `CountedLoopFrame` and calls `generate_counted_stateful_loop`.
///
/// ADR 0118 phase 3 (BT-3419) scope note: this is a plain `Document`-level
/// struct, unrelated to [`threaded_ir::ThreadedStmt::ConditionalLoop`]'s own
/// `condition`/`condition_value` fields, despite the field-name overlap
/// (`continue_header` here vs. that node's own, now-split `continue_arm`) —
/// `counted_loops.rs` never constructs a `ConditionalLoop` node (no counted
/// loop does; see that variant's own `#[allow(dead_code)]` status). A
/// counted loop's `continue_header` is always a pure counter compare (e.g.
/// `Counter =&lt; N`) built once from the receiver/limit/step, captured
/// before the letrec — never itself state-effecting — so had this frame
/// been unified onto `ConditionalLoop`, its `condition` would always be
/// empty and `condition_value` the bare compare, exactly the "pure counter
/// compare" case that variant's own field docs already name.
pub(super) struct CountedLoopFrame {
    /// Variable bindings emitted before the `letrec` (e.g. `let N = recv in`).
    pub preamble: Document<'static>,
    /// Name of the letrec function (e.g. `"repeat"` or `"loop"`).
    pub fn_name: String,
    /// The condition header up to `<'true'> when 'true' ->` for the continue arm.
    pub continue_header: Document<'static>,
    /// Expression used as the next counter in the recursive call
    /// (e.g. `"call 'erlang':'+'(I, 1)"`).
    pub next_counter: Document<'static>,
    /// Initial counter argument for the first `apply` call: an integer literal
    /// (e.g. `1`) for `timesRepeat:`, or a variable (e.g. `StartVar`) for `to:do:`.
    pub initial_counter: Document<'static>,
    /// The `false` arm and `end` (e.g. `"<'false'> when 'true' -> {'nil', StateAcc} end"`).
    pub false_arm: Document<'static>,
    /// Optional Beamtalk block-parameter name to bind to the gensym'd counter.
    pub body_param: Option<String>,
    /// BT-2354: gensym'd Core Erlang counter variable name (e.g. `_loopidx3`).
    ///
    /// Used as the loop fun's first parameter and threaded through
    /// `continue_header`/`next_counter`. Produced by `fresh_temp_var` (leading
    /// underscore + a monotonic counter), so it does not collide with the common
    /// case of a user local named `i`, which maps to `I` via `to_core_var`. The
    /// unique suffix also keeps it distinct from underscore-prefixed user
    /// identifiers (which `to_core_var` passes through verbatim).
    pub counter: String,
    /// BT-3168 (ADR 0111 Addendum 9, Question 3): the pre-loop `ClassVars`
    /// name (`current_class_var()`, captured before body generation runs),
    /// when the body threads a `ClassVars` mutation through the loop's own
    /// recursive tail call. `None` when it doesn't. Used, verbatim, as both
    /// the letrec fun's extra trailing formal parameter and the initial
    /// `apply`'s trailing argument — see [`class_var_arg_doc`].
    pub class_var_param: Option<String>,
}

/// BT-3168 (ADR 0111 Addendum 9, Question 3): renders `", <name>"` for a
/// threaded `ClassVars` fun-argument slot, or nothing when the loop doesn't
/// thread class vars. Shared by `while_loops.rs`'s and `counted_loops.rs`'s
/// (via `generate_counted_stateful_loop`) Letrec base-path `ClassVars`
/// plumbing — the letrec fun signature, both `apply` call sites, and the
/// exit arm all need the identical "extra trailing arg, or nothing" shape,
/// so it is written once rather than copy-evolved per call site.
pub(super) fn class_var_arg_doc(name: Option<&String>) -> Document<'static> {
    name.map_or(Document::Nil, |v| docvec![", ", leaf::var(v.clone())])
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

    /// Shared failure-reporting path for every `ThreadedIr` production
    /// invariant check in this module (ADR 0111 §The verifier /
    /// CLAUDE.md's "never panic on user input" rule): hard-fails in debug/CI
    /// via `debug_assert!`, exactly as the deleted `debug_assert!`s this
    /// migration's checks replace did; in release builds (where
    /// `debug_assert!` is compiled out), degrades to an internal-error
    /// diagnostic on the compile result instead of silently doing nothing —
    /// the compile still succeeds with the generator's (unverified) output.
    /// Shared by every BT-3132/BT-3133/BT-3134/BT-3135 check in this module
    /// and in `expressions.rs`/`dispatch_codegen.rs`/`gen_server/methods.rs`/
    /// `mod.rs` — BT-3134 and BT-3135 each deliberately dropped their own
    /// independently-added copy of this helper (CLAUDE.md's
    /// no-duplicate-implementations rule) in favor of this one, already on
    /// `main` from BT-3133. `pub(super)` makes it visible to the rest of
    /// `codegen::core_erlang`, not just this module.
    pub(super) fn report_threaded_ir_verify_errors(
        &mut self,
        errors: &[threaded_ir::VerifyError],
        invariant_label: &str,
        span: Span,
    ) {
        if errors.is_empty() {
            return;
        }
        debug_assert!(
            false,
            "ThreadedIr verify found a {invariant_label}: {errors:?}"
        );
        self.add_codegen_warning(
            Diagnostic::error(format!("internal: {invariant_label}: {errors:?}"), span)
                .with_category(DiagnosticCategory::Type),
        );
    }

    /// ADR 0111 Addendum 9 (BT-3168), Questions 3/4: whether a Letrec loop
    /// body threads a `ClassVars` mutation through the loop's own recursive
    /// tail call. True exactly when the body is compiled inside a class
    /// method AND has a direct class-var field write or a same-class
    /// self-send — per Question 4 Part A, either shape already
    /// unconditionally forces `StateAcc` mode for the loop's own
    /// local-variable threading (`has_state_effects()`/`has_self_sends`
    /// exclude `DirectParams`/`TupleAcc`/`Hybrid`), so `ClassVars`
    /// composition only needs to be designed against that one shape.
    ///
    /// Shared by [`ThreadingPlan::new_impl`] (Letrec-only, via
    /// `allow_direct_params`) and the value-type/class-method loop-open
    /// consumers (`value_type_codegen.rs`) so the routing decision and the
    /// tuple-shape decision can never independently drift out of sync
    /// (CLAUDE.md's no-duplicate-implementations rule).
    pub(super) fn loop_body_threads_class_vars(&self, body: &beamtalk_core::ast::Block) -> bool {
        self.find_class_var_mutating_stmt(body).is_some()
    }

    /// Shared predicate behind [`Self::loop_body_threads_class_vars`] and
    /// [`Self::nested_loop_lost_class_var_mutation`] (BT-3172) — returns the
    /// first top-level statement of `body` that is a bare class-var
    /// assignment or class-method self-send, or `None` if there isn't one.
    ///
    /// Deliberately narrower than `block_analysis::analyze_block`'s own
    /// (recursive) `field_writes`/`has_self_sends` — those also count a
    /// class-var write or self-send NESTED inside a conditional, a binary
    /// op, or any other sub-expression position, which is exactly right
    /// for THEIR job (deciding whether the body needs `StateAcc` fallback
    /// at all) but wrong for this one: `generate_threaded_loop_body_inner`
    /// only ever threads `ClassVars` through the loop's tail call for a
    /// BARE, top-level class-var-assignment or class-method-self-send
    /// STATEMENT (the two shapes it has real Bind-construction branches
    /// for) — never for one buried inside a larger expression, whose own
    /// `ClassVarsN` rebind is scoped to that expression's own nested
    /// `let`, not threaded out to the loop body's own statement sequence.
    /// Confirmed by a real regression while validating this issue: a
    /// pre-existing, previously-compiling fixture
    /// (`class_var_subexpr.bt`'s `tickInLoopConditional`, a self-send
    /// nested inside a `to:do:` body's `ifTrue:` *condition*) started
    /// emitting `unbound variable 'ClassVars1'` once this predicate used
    /// the recursive analysis — the self-send's own internally-minted
    /// rebind was correctly scoped to its own conditional's nested `let`,
    /// but this predicate's resulting extra loop-level `ClassVars` fun
    /// parameter/tail-call argument then referenced that same
    /// already-out-of-scope name.
    fn find_class_var_mutating_stmt<'a>(
        &self,
        body: &'a beamtalk_core::ast::Block,
    ) -> Option<&'a Expression> {
        if !self.in_class_method() {
            return None;
        }
        let filtered_body = super::util::collect_body_exprs(&body.body);
        filtered_body.into_iter().find(|expr| {
            (Self::is_field_assignment(expr) && self.is_class_var_assignment(expr))
                || self.is_class_method_self_send(expr)
        })
    }

    /// BT-3172: if `expr` is itself a nested `Letrec`- or `Foldl*`-shaped
    /// loop (per [`Self::nested_loop_or_fold_body`]) whose own body would
    /// thread a `ClassVars` mutation through its own recursive tail call or
    /// fold accumulator, returns a short description of that mutation for
    /// use in [`CodeGenError::ClassVarMutationLostAcrossNestedLoop`]'s
    /// message. Returns `None` for anything else, including a nested
    /// loop/fold whose own body has no class-var mutation to lose in the
    /// first place.
    ///
    /// Two independent triggers, matching each shape's own real threading
    /// gate:
    /// * [`Self::loop_body_threads_class_vars`] — a BARE, top-level
    ///   class-var field write or class-method self-send (the `Letrec`
    ///   gate, `ThreadingPlan::threads_class_vars`'s `allow_direct_params`
    ///   branch).
    /// * `block_analysis::analyze_block(body).has_self_sends` — ANY
    ///   same-class self-send anywhere in the body, however deeply nested
    ///   in a conditional or another block (the `Foldl*` gate, that same
    ///   field's `else` branch) — deliberately recursive here, unlike the
    ///   first trigger, because that IS how `Foldl*`'s own
    ///   `ThreadingPlan::new_impl` decides `threads_class_vars`. A bare
    ///   class-var field write inside a `Foldl*` body needs no matching
    ///   trigger here: `generate_field_assignment_open` never threads one
    ///   regardless of nesting (`loop_threads_class_vars` stays scoped to
    ///   `BodyKind::Letrec`), so it is already unconditionally rejected by
    ///   `reject_class_var_field_assignment` at any depth.
    ///
    /// This is a detection-only predicate, deliberately separate from
    /// `ThreadingPlan::threads_class_vars`, which stays scoped to the OUTER
    /// body's own top-level statements (see that field's doc comment)
    /// rather than being extended to also thread the inner construct's
    /// `ClassVars` value through — no code path currently unpacks a nested
    /// loop/fold's `ClassVars` tuple element back into an enclosing body
    /// (confirmed empirically for the `Foldl*`-in-`Foldl*` shape: the
    /// nested fold's own `next_class_var()` mint permanently advances the
    /// generator's single, unscoped class-var-name counter even though the
    /// resulting name is never surfaced to the enclosing body, producing an
    /// `erlc` "unbound variable" compile crash rather than a clean
    /// diagnostic) — so this predicate exists purely to reject the shape,
    /// not to make it work.
    pub(super) fn nested_loop_lost_class_var_mutation(&self, expr: &Expression) -> Option<String> {
        let (body, shape) = Self::nested_loop_or_fold_body(expr)?;
        if let Some(mutating_stmt) = self.find_class_var_mutating_stmt(body) {
            if Self::is_field_assignment(mutating_stmt)
                && self.is_class_var_assignment(mutating_stmt)
            {
                if let Expression::Assignment { target, .. } = mutating_stmt {
                    if let Expression::FieldAccess { field, .. } = target.as_ref() {
                        return Some(format!("class variable '{}'", field.name));
                    }
                }
            } else if let Expression::MessageSend { selector, .. } = mutating_stmt {
                return Some(format!("'self {}'", selector.name()));
            }
        }
        // BT-3172 review: the recursive self-send fallback must match
        // `ThreadingPlan::new_impl`'s OWN per-shape gate exactly, not apply
        // uniformly to both shapes. `Letrec`'s real gate
        // (`loop_body_threads_class_vars`, already checked above) is
        // deliberately top-level-only — recursing into a conditional
        // buried inside a `Letrec` body is EXACTLY the shape that predicate
        // was narrowed to exclude (the `class_var_subexpr.bt`
        // `tickInLoopConditional` regression), and it's also the shape
        // `class_var_subexpr_test.bt`'s `testTickInLoopConditionalCompilesAndRuns`
        // pins as already-accepted, out-of-scope, silently-non-threading
        // behavior (BT-2308) at a single loop level — rejecting only the
        // nested-loop variant of that exact same shape would be an
        // inconsistent, surprising new restriction this predicate has no
        // business introducing. Only `Foldl*`'s own real gate
        // (`!Actor && in_class_method() && body_analysis.has_self_sends`)
        // is genuinely recursive, so the fallback below applies only when
        // `shape` is `Foldl` — matching `context` too.
        if matches!(shape, NestedLoopShape::Foldl)
            && !matches!(self.context, CodeGenContext::Actor)
            && self.in_class_method()
        {
            let analysis = block_analysis::analyze_block(body);
            // `self_send_selectors` is a `HashSet` (default `RandomState`) —
            // pick the lexicographically-smallest selector so the
            // diagnostic text is reproducible across runs for identical
            // source, rather than depending on hash-iteration order. Which
            // selector is named doesn't affect the accept/reject decision,
            // only the message.
            if let Some(selector) = analysis.self_send_selectors.iter().min() {
                return Some(format!("'self {selector}'"));
            }
        }
        None
    }

    /// BT-3175: Canonical "selector → body-block-argument position" table.
    /// Shared by every "given a keyword-selector `MessageSend`, extract its
    /// loop/fold body block" call site in this module
    /// ([`Self::nested_loop_or_fold_body`],
    /// [`Self::collect_list_op_cross_scope_mutations`],
    /// [`Self::list_op_needs_stateacc_fallback`],
    /// [`Self::expr_has_nested_counted_loop_threading`]) — before this, each
    /// independently re-matched selector strings against
    /// `arguments.first()`/`arguments.last()`/`arguments[N]`, and could
    /// silently drift out of sync (see BT-3175).
    ///
    /// This is the canonical/maximal selector set: the `BodyKind::Letrec`
    /// shapes (`whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/
    /// `to:by:do:`, see this module's `//!` doc comment) plus the
    /// `BodyKind::Foldl*` shapes (`do:`/`collect:`/`select:`/`reject:`/
    /// `anySatisfy:`/`allSatisfy:`/`inject:into:`/`detect:`/`count:`/
    /// `takeWhile:`/`dropWhile:`/`partition:`/`groupBy:`) — matching
    /// [`Self::nested_loop_or_fold_body`]'s pre-BT-3175 coverage, the most
    /// complete of the four (BT-3172 added the predicate-based shapes
    /// there only). `detect:ifNone:` is intentionally excluded: its second
    /// (`ifNone:`) block argument is a separate, not-yet-analyzed risk
    /// surface no call site here attempts to cover.
    ///
    /// [`Self::list_op_needs_stateacc_fallback`]/
    /// [`Self::collect_list_op_cross_scope_mutations`]/
    /// [`Self::expr_has_nested_counted_loop_threading`] each cover only a
    /// narrower subset of this table (their own deliberate optimization
    /// scopes, verified against each site's pre-refactor selector list
    /// rather than broadened here) — they filter the returned selector
    /// down to their own subset after calling this.
    ///
    /// Takes the already-extracted `sel` (concatenated keyword parts, e.g.
    /// `"to:by:do:"`) and `arguments` rather than the raw `Expression`, so
    /// callers that already destructured a `MessageSend` for their own
    /// purposes (e.g. reading `receiver` for `ensure:`/`on:do:` handling)
    /// don't have to re-match. Does not itself unwrap parens or an
    /// assignment RHS — callers that need that (e.g.
    /// [`Self::nested_loop_or_fold_body`]'s `unwrap_parens`,
    /// [`Self::expr_has_nested_counted_loop_threading`]'s assignment-RHS
    /// unwrap) do it before calling, matching each site's pre-existing
    /// behavior exactly.
    ///
    /// Position eligibility is delegated to
    /// `beamtalk_core::ast::is_state_threaded_block_arg` — the single
    /// source of truth for this table, shared with `beamtalk-lint`'s
    /// `DeadAssignment` check (BT-3385) so the two can never silently
    /// drift (CLAUDE.md's "No duplicate implementations" rule). That
    /// shared table also covers `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`
    /// (threaded via dedicated codegen elsewhere, not this loop/fold
    /// table) — explicitly excluded below so a conditional's block is
    /// never misclassified as a nested loop/fold body by this function's
    /// callers (e.g. [`Self::nested_loop_or_fold_body`], which calls this
    /// with whatever keyword selector it finds, unfiltered).
    fn block_arg_for_selector<'a>(
        sel: &str,
        arguments: &'a [Expression],
    ) -> Option<&'a beamtalk_core::ast::Block> {
        if matches!(sel, "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:") {
            return None;
        }
        arguments.iter().enumerate().find_map(|(idx, arg)| {
            if beamtalk_core::ast::is_state_threaded_block_arg(sel, idx) {
                match arg {
                    Expression::Block(block) => Some(block),
                    _ => None,
                }
            } else {
                None
            }
        })
    }

    /// Extracts the body block of `expr`, and which family it belongs to,
    /// if it is a nested loop/fold send. Selector coverage and argument
    /// position come from [`Self::block_arg_for_selector`]; the returned
    /// [`NestedLoopShape`] tells [`Self::nested_loop_lost_class_var_mutation`]
    /// which of `ThreadingPlan`'s two `threads_class_vars` gates applies.
    fn nested_loop_or_fold_body(
        expr: &Expression,
    ) -> Option<(&beamtalk_core::ast::Block, NestedLoopShape)> {
        use beamtalk_core::ast::MessageSelector;
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr.unwrap_parens()
        else {
            return None;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let block = Self::block_arg_for_selector(&sel, arguments)?;
        let shape = match sel.as_str() {
            "whileTrue:" | "whileFalse:" | "timesRepeat:" | "to:do:" | "to:by:do:" => {
                NestedLoopShape::Letrec
            }
            _ => NestedLoopShape::Foldl,
        };
        Some((block, shape))
    }

    /// BT-1343: Emits a diagnostic for synchronous self-send detected in a loop body.
    pub(super) fn emit_self_send_in_loop_diagnostic(&mut self, expr: &Expression, span: Span) {
        if !self.codegen_diagnostics_enabled {
            return;
        }
        // Extract selector name from the message send
        if let Expression::MessageSend { selector, .. } = expr {
            let sel_name = selector.name().to_string();
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
    ///
    /// BT-3168: also scopes `loop_threads_class_vars` to this exact frame —
    /// set from `plan.threads_class_vars` right after `with_branch_context`
    /// resets it to `false` on entry (mirroring `state_version`'s
    /// reset-on-entry discipline, not `class_var_version`'s
    /// restore-without-reset one), so a nested construct that calls
    /// `generate_threaded_loop_body` again (or any other `with_branch_context`
    /// user — a conditional, `sort:`'s manually-inlined body, …) always sees
    /// the flag correctly reflecting ITS OWN body, never a leaked `true` from
    /// an enclosing Letrec loop. On success, also stashes the loop's final
    /// in-body `current_class_var()` name into `last_loop_class_var` — read
    /// by `while_loops.rs`/`counted_loops.rs` right after this call returns
    /// (`with_branch_context`'s guard restores `class_var_version` to the
    /// pre-loop value on drop, so this is the only chance to capture it).
    pub(super) fn generate_threaded_loop_body(
        &mut self,
        body: &beamtalk_core::ast::Block,
        plan: &ThreadingPlan,
        kind: &BodyKind,
    ) -> Result<(Document<'static>, usize)> {
        // BT-3168/BT-3169 merge: `plan.threads_class_vars` is `true` for two
        // mutually exclusive shapes (see `ThreadingPlan::threads_class_vars`'s
        // own doc comment) — `loop_threads_class_vars` and
        // `last_loop_class_var` are exclusively the Letrec shape's own
        // consumer-facing signals (`dispatch_codegen.rs`'s direct-field-write
        // Bind bypass, `while_loops.rs`/`counted_loops.rs`'s recursive-tail-
        // call `ClassVars` argument), so both must stay scoped to
        // `BodyKind::Letrec` — never set for a `Foldl*` plan, whose own
        // ClassVars threading is the `{ClassVars, StateAcc}` accumulator wrap
        // in `generate_threaded_loop_body_inner` instead. Letting either leak
        // true for a Foldl* body would wrongly bypass
        // `reject_class_var_field_assignment` for a bare (non-self-send)
        // class-var field write there, or leave a stale `last_loop_class_var`
        // for a later, unrelated Letrec loop's own `.take()` to pick up.
        let is_letrec = matches!(kind, BodyKind::Letrec);
        self.with_branch_context(|this| {
            this.loop_threads_class_vars = is_letrec && plan.threads_class_vars;
            let result = this.generate_threaded_loop_body_inner(body, plan, kind);
            if is_letrec && plan.threads_class_vars {
                this.last_loop_class_var = Some(this.current_class_var());
            }
            result
        })
    }

    /// Inner implementation of `generate_threaded_loop_body`, called inside
    /// `with_branch_context` so that `in_loop_body = true` and `state_version = 0`.
    #[allow(clippy::too_many_lines)]
    fn generate_threaded_loop_body_inner(
        &mut self,
        body: &beamtalk_core::ast::Block,
        plan: &ThreadingPlan,
        kind: &BodyKind,
    ) -> Result<(Document<'static>, usize)> {
        // Needed for Letrec: detect whether body has direct field assignments.
        let has_direct_field_assignments = body
            .body
            .iter()
            .any(|s| Self::is_field_assignment(&s.expression));

        let filtered_body = super::util::collect_body_exprs(&body.body);

        let mut docs: Vec<Document<'static>> = Vec::new();
        let mut has_mutations = false;
        let mut has_plain_lets = false;

        // For predicate-based body kinds, allocate a pred_var upfront.
        let pred_var: Option<String> = if matches!(
            kind,
            BodyKind::FoldlFilter { .. }
                | BodyKind::FoldlBoolPredicate { .. }
                | BodyKind::FoldlDetect { .. }
                | BodyKind::FoldlCount
                | BodyKind::FoldlTakeWhile { .. }
                | BodyKind::FoldlDropWhile { .. }
                | BodyKind::FoldlPartition { .. }
                | BodyKind::FoldlGroupBy { .. }
        ) {
            Some(self.fresh_temp_var("Pred"))
        } else {
            None
        };

        for (i, expr) in filtered_body.iter().enumerate() {
            let is_last = i == filtered_body.len() - 1;

            // BT-3172: `expr` is a top-level statement of THIS loop/fold's
            // own body — if it's itself a nested loop/fold whose own body
            // threads a `ClassVars` mutation, no downstream branch (this
            // function's own field-assignment/self-send/tier2/local-var/
            // destructure/control-flow-has-mutations dispatch above
            // `emit_non_assign_expr`'s generic fallback, direct-params, the
            // `is_last`/`element(2)` unpacks, or `push_discarded_stmt`'s
            // open-scope tracking) unpacks that nested construct's
            // `ClassVars` value, so the mutation would be silently lost or
            // (for a nested `Foldl*`, confirmed empirically) crash `erlc`
            // with an unbound-variable error — regardless of which branch
            // below would otherwise handle `expr`, `is_last`, or
            // `plan.threads_class_vars`. Checked once here, at the very top
            // of the per-statement loop (ahead of every dispatch branch,
            // not just `emit_non_assign_expr`'s fallback) because a nested
            // `do:`/`collect:`/etc. statement is classified
            // `DispatchKind::ControlFlow` (`is_state_threading_keyword_selector`
            // includes the `Foldl*` selectors, not just conditionals) and so
            // is actually intercepted by the `control_flow_has_mutations`
            // branch below, never reaching `emit_non_assign_expr` at all —
            // confirmed empirically when this check lived only there and
            // silently failed to catch the `Foldl*`-in-`Foldl*` repro.
            // Reject rather than emit code that's silently wrong or
            // malformed — see `CodeGenError::ClassVarMutationLostAcrossNestedLoop`'s
            // doc comment.
            if let Some(mutation) = self.nested_loop_lost_class_var_mutation(expr) {
                let location = self.span_to_line(expr.span()).map_or_else(
                    || format!("offset {}", expr.span().start()),
                    |line| format!("line {line}"),
                );
                return Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop {
                    mutation,
                    location,
                });
            }

            // Letrec body uses a space separator between statements.
            if i > 0 && matches!(kind, BodyKind::Letrec) {
                docs.push(Document::Str(" "));
            }

            if Self::is_field_assignment(expr) {
                has_mutations = true;
                // ADR 0118 phase 2b (BT-3418): thread every state-effecting
                // sub-expression nested in the RHS ahead of
                // `generate_field_assignment_open`'s own compile of it —
                // `self.count := self.count + (self bump)` no longer
                // silently drops `bump`'s mutation. Mirrors
                // `lower_field_assignment_bind`'s identical `thread_ahead`
                // step (`conditionals.rs`), and is safe to run unconditionally
                // ahead of every one of `generate_field_assignment_open`'s
                // own three internal branches (plain, hybrid-full-extract,
                // class-var) — all three eventually compile `value` via
                // `expression_doc`, which is exactly the route
                // `precompiled_subexprs` substitution reaches, so a `value`
                // that needs no threading (the overwhelmingly common case)
                // leaves this a no-op.
                let Expression::Assignment { value, .. } = expr else {
                    unreachable!("is_field_assignment guarantees an Assignment expr");
                };
                let frame = self.current_frame();
                let mut prelude_stmts: Vec<ThreadedStmt> = Vec::new();
                let thread_scope = self.thread_ahead(value, &mut prelude_stmts, frame)?;
                if !prelude_stmts.is_empty() {
                    docs.push(self.threaded_prelude_doc(&prelude_stmts));
                }
                let (doc, _val_var) = self.generate_field_assignment_open(expr)?;
                self.finish_precompiled_scope(thread_scope)?;
                docs.push(doc);
                if is_last {
                    self.emit_field_assign_last_expr(&mut docs, kind, pred_var.as_ref());
                }
            } else if self.is_actor_self_send(expr) {
                has_mutations = true;
                // BT-1343: Emit diagnostic for synchronous self-send in loop body.
                self.emit_self_send_in_loop_diagnostic(expr, expr.span());
                let (doc, dispatch_var) = self.generate_self_dispatch_open(expr)?;
                docs.push(doc);
                if is_last {
                    self.emit_self_send_last_expr(
                        &mut docs,
                        kind,
                        pred_var.as_ref(),
                        &dispatch_var,
                    );
                }
            } else if matches!(kind, BodyKind::Letrec) && self.is_class_method_self_send(expr) {
                // BT-3150/BT-3168 (ADR 0111 Addendum 9, Questions 3/5): a
                // self-send to a same-class class method inside a
                // whileTrue:/timesRepeat:/to:do:/to:by:do: loop body routes
                // through `emit_class_var_result_unwrap`, which leaves an
                // *open* let-chain ending in `... in ` and rebinds
                // `ClassVarsN` from the callee's own `{class_var_result,
                // Result, ClassVars}` reply — exactly like a top-frame
                // self-send. `loop_threads_class_vars` (set by
                // `generate_threaded_loop_body` from this call's own
                // `ThreadingPlan::threads_class_vars`) is `true` here by
                // construction whenever this branch is reached (the same
                // body-analysis formula that decided `threads_class_vars`
                // found this exact self-send) — so the loop's own
                // ClassVars fun parameter/tail-call argument
                // (while_loops.rs/counted_loops.rs) picks up the resulting
                // `current_class_var()` name the same way any other
                // in-body class-var mutation does. Reading `ClassSelf` and
                // the loop-entry `ClassVars` value needs no extra plumbing
                // here (Question 5: ordinary Core Erlang closure scoping —
                // both are free variables at this `letrec` nesting depth).
                //
                // ADR 0118 phase 5b (BT-3422): `generate_expression(expr)`
                // used to reach this same producer through
                // `try_handle_class_method_self_send`/`try_handle_class_reference`,
                // which left its `ThreadedValue` open (the pre-migration
                // open-scope protocol this whole phase deletes). Both call
                // sites now CLOSE it via `close_threaded_value_doc` (a
                // self-contained `let ... in Value` expression, no longer
                // ending in an open `in `) — appending that closed
                // `Document` here and then appending MORE statements after
                // it left a dangling, disconnected `Document` with no `in`
                // joining them (`erlc`'s "syntax error before: 'let'").
                // `threaded_expression` reaches the identical producer
                // ([`Self::class_method_prelude_producer`]) but returns its
                // prelude un-rendered — splicing `tv.prelude` here restores
                // the open chain this branch has always required, and the
                // producer's own result (`tv.value`) is intentionally never
                // referenced, matching "Letrec's own body value is *always*
                // discarded" below.
                if self.loop_threads_class_vars {
                    has_mutations = true;
                    let frame = self.current_frame();
                    let tv = self.threaded_expression(expr, frame)?;
                    docs.push(self.threaded_prelude_doc(&tv.prelude));
                    // `Letrec`'s own body value is *always* discarded
                    // regardless of the last statement (a `whileTrue:`/
                    // `timesRepeat:` unconditionally evaluates to `nil`,
                    // mirroring `emit_self_send_last_expr`'s Letrec arm —
                    // "nothing; caller appends recursive call") — so
                    // `is_last` needs no special handling here.
                } else {
                    // Defensive fallback: `loop_threads_class_vars` should
                    // always be `true` whenever this branch is reached (see
                    // above), so this path is not expected to be live in
                    // practice — kept as a conservative rejection rather
                    // than an `unreachable!()`, per CLAUDE.md's "never panic
                    // on user input" rule, in case a future divergence
                    // between this predicate and `loop_body_threads_class_vars`
                    // is ever introduced. Confirmed empirically: without this
                    // guard a mutating count stayed at 0 across 3 iterations
                    // instead of accumulating — reject at compile time rather
                    // than emit code that's silently wrong, the same
                    // "can't thread this state shape back correctly here"
                    // category as BT-2792's `FieldAssignmentInUnsupportedBlock`.
                    //
                    // Deliberately scoped to `Letrec` only, NOT any `Foldl*`
                    // kind (`do:`/`collect:`/`select:`/`inject:into:`/...) —
                    // tried and reverted after two rounds of CI failure. The
                    // identical class-var-mutation-loss bug IS reachable via
                    // `Foldl*` bodies too (confirmed empirically for `do:`),
                    // but unlike `Letrec`, `Foldl*` bodies routinely use a
                    // self-send's return value as (or within) the fold's own
                    // output, AND — per a pre-existing, intentionally-supported
                    // BT-2350 pattern — even a *discarded*, non-last self-send
                    // statement inside `do:`/`inject:into:`/`collect:` is
                    // common and expected to compile (see
                    // `stdlib/test/fixtures/class_method_block.bt`, which uses
                    // pure self-sends like `self double:`/`self logIt:` in
                    // exactly these positions). Neither "is the return value
                    // used" nor "is the statement last" reliably distinguishes
                    // safe from unsafe there, so a position-based rejection
                    // breaks real code. Closing the `Foldl*` gap needs real
                    // `ClassVars` threading through fold accumulators —
                    // tracked as BT-3169.
                    let selector = if let Expression::MessageSend { selector, .. } = expr {
                        selector.name().to_string()
                    } else {
                        unreachable!("is_class_method_self_send only matches MessageSend")
                    };
                    let location = self.span_to_line(expr.span()).map_or_else(
                        || format!("offset {}", expr.span().start()),
                        |line| format!("line {line}"),
                    );
                    return Err(CodeGenError::ClassMethodSelfSendInThreadedLoopBody {
                        selector,
                        location,
                    });
                }
            } else if !matches!(kind, BodyKind::Letrec) && self.is_tier2_value_call(expr) {
                // BT-2813: a bare (non-assigned) Tier 2 `value(:...)` statement
                // (field-stored or local-var-stored block) inside a foldl-based
                // loop body (do:/collect:/select:/etc). Before this fix, such a
                // statement fell through to `emit_non_assign_expr`, which treats
                // it as an ordinary expression and emits a Tier-1-only apply —
                // crashing with badarity for a genuinely Tier 2 (state-threading)
                // stored block. `generate_tier2_value_call_doc` always returns a
                // {Result, NewState} tuple — unpack it and thread the state
                // forward, mirroring the already-working `Tier2ValueCall`
                // handling in conditionals.rs and gen_server/methods.rs.
                //
                // Excluded for Letrec (whileTrue:/timesRepeat:) loop bodies:
                // out of scope for BT-2813, whose repro and matrix coverage are
                // do:/collect:/nested-do: only.
                has_mutations = true;
                let tuple_var = self.fresh_temp_var("T2LoopTuple");
                let expr_doc = self.generate_tier2_value_call_doc(expr)?;
                let new_state = self.next_state_var();
                docs.push(docvec![
                    "let ",
                    leaf::var(tuple_var.clone()),
                    " = ",
                    expr_doc,
                    " in let ",
                    leaf::var(new_state),
                    " = call 'erlang':'element'(2, ",
                    leaf::var(tuple_var.clone()),
                    ") in ",
                ]);
                if is_last {
                    self.emit_tier2_value_call_last_expr(
                        &mut docs,
                        kind,
                        pred_var.as_ref(),
                        &tuple_var,
                    );
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
                    let (assign_doc, _val_var) =
                        self.generate_local_var_assignment_in_loop(expr)?;
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
            } else if !matches!(kind, BodyKind::Letrec)
                && (self.control_flow_has_mutations(expr)
                    || Self::inline_conditional_writes_threaded(
                        expr,
                        &plan.threaded_locals,
                        &self.semantic_facts,
                    ))
            {
                // BT-1053/BT-1477: Inline conditional with mutations returns {Result, NewStateAcc}.
                // Unpack element(2) so subsequent iterations see the updated StateAcc.
                // This applies to ALL foldl body kinds (do:, collect:, select:, reject:,
                // inject:into:), not just do: — otherwise mutations inside conditionals
                // nested within collect:/select:/etc. are silently lost.
                has_mutations = true;
                let tuple_var = self.fresh_temp_var("CondResult");
                // BT-3173: the vars THIS construct itself threads, read before
                // `generate_expression` below (which may push/pop scopes) so
                // the lookup reflects this statement's own captured set.
                let inner_threaded_vars = self.get_control_flow_threaded_vars(expr);
                let doc = self.generate_expression(expr)?;
                let new_state = self.next_state_var();
                docs.push(docvec![
                    "let ",
                    leaf::var(tuple_var.clone()),
                    " = ",
                    doc,
                    " in let ",
                    leaf::var(new_state.clone()),
                    " = call 'erlang':'element'(2, ",
                    leaf::var(tuple_var.clone()),
                    ") in ",
                ]);
                // BT-3173: a non-last (or last) ensure:/on:do:/ifNotNil:/nested-loop
                // statement here only bumps the StateAcc *version pointer* above —
                // it does NOT rebind the specific local vars it threads. Both
                // StateAcc (map) mode and tuple-acc mode bind each threaded local
                // to a FIXED Core Erlang variable once per iteration (see
                // `generate_unpack_at_iteration_start`/`generate_tuple_unpack_docs`),
                // not via a live re-lookup through the state pointer — so without
                // this, any read of the var later in the SAME block invocation
                // (e.g. the next statement) would see the stale pre-statement
                // value instead of what this construct just wrote. Shares
                // `rebind_threaded_vars_from_state` with `conditionals.rs`'s
                // `push_control_flow_threaded_var_rereads`, which does the same
                // rebind for the `ThreadedIr`-rendered conditional-arm path.
                if let Some(inner_vars) = inner_threaded_vars {
                    docs.extend(self.rebind_threaded_vars_from_state(&inner_vars, &new_state));
                }
                if is_last {
                    match kind {
                        BodyKind::FoldlDo => {
                            // do: discards the result value, returns state only
                            docs.push(leaf::var(self.current_state_var()));
                        }
                        BodyKind::FoldlCollect => {
                            // collect: needs the result value for the list
                            let result_var = self.fresh_temp_var("CondVal");
                            docs.push(docvec![
                                "let ",
                                leaf::var(result_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var),
                                ") in {[",
                                leaf::var(result_var),
                                " | AccList], ",
                                leaf::var(self.current_state_var()),
                                "}",
                            ]);
                        }
                        BodyKind::FoldlFilter { .. }
                        | BodyKind::FoldlBoolPredicate { .. }
                        | BodyKind::FoldlDetect { .. }
                        | BodyKind::FoldlCount
                        | BodyKind::FoldlTakeWhile { .. }
                        | BodyKind::FoldlDropWhile { .. }
                        | BodyKind::FoldlPartition { .. }
                        | BodyKind::FoldlGroupBy { .. } => {
                            // predicate-based selectors — bind predicate result
                            if let Some(pv) = pred_var.as_ref() {
                                let result_var = self.fresh_temp_var("CondVal");
                                docs.push(docvec![
                                    "let ",
                                    leaf::var(result_var.clone()),
                                    " = call 'erlang':'element'(1, ",
                                    leaf::var(tuple_var),
                                    ") in let ",
                                    leaf::var(pv.clone()),
                                    " = ",
                                    leaf::var(result_var),
                                    " in ",
                                ]);
                            }
                        }
                        BodyKind::FoldlInject => {
                            // inject:into: — result is the new accumulator
                            let result_var = self.fresh_temp_var("CondVal");
                            docs.push(docvec![
                                "let ",
                                leaf::var(result_var.clone()),
                                " = call 'erlang':'element'(1, ",
                                leaf::var(tuple_var),
                                ") in {",
                                leaf::var(result_var),
                                ", ",
                                leaf::var(self.current_state_var()),
                                "}",
                            ]);
                        }
                        BodyKind::Letrec => {
                            unreachable!("Letrec excluded by guard above");
                        }
                        BodyKind::FoldlSort => {
                            unreachable!("FoldlSort does not use generate_threaded_loop_body");
                        }
                    }
                }
            } else {
                // Non-assignment expression: handling depends on BodyKind.
                self.emit_non_assign_expr(
                    &mut docs,
                    expr,
                    i,
                    is_last,
                    &mut has_mutations,
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
                    docvec!["call 'erlang':'not'(", leaf::var(pv.clone()), ")",]
                } else {
                    leaf::var(pv.clone())
                };
                if plan.use_tuple_acc {
                    // BT-1276: Tuple mode — repack current var bindings into the result tuple.
                    let vars_doc = plan.current_vars_doc(self);
                    docs.push(docvec![
                        "case ",
                        condition_doc,
                        " of <'true'> when 'true' -> {[",
                        leaf::var(item_var.clone()),
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
                        leaf::var(item_var.clone()),
                        " | AccList], ",
                        leaf::var(final_state.clone()),
                        "} <'false'> when 'true' -> {AccList, ",
                        leaf::var(final_state),
                        "} end",
                    ]);
                }
            }
        }

        // BT-1481: FoldlBoolPredicate — update boolean accumulator based on predicate result.
        // Match only 'true'/'false' explicitly (consistent with FoldlFilter and lists:any/all).
        if let BodyKind::FoldlBoolPredicate { is_all } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    if *is_all {
                        // allSatisfy: pred=false → set BoolAcc to false; pred=true → keep
                        docs.push(docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'false'> when 'true' -> {'false', ",
                            vars_doc.clone(),
                            "} <'true'> when 'true' -> {BoolAcc, ",
                            vars_doc,
                            "} end",
                        ]);
                    } else {
                        // anySatisfy: pred=true → set BoolAcc to true; pred=false → keep
                        docs.push(docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {'true', ",
                            vars_doc.clone(),
                            "} <'false'> when 'true' -> {BoolAcc, ",
                            vars_doc,
                            "} end",
                        ]);
                    }
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    if *is_all {
                        docs.push(docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'false'> when 'true' -> {'false', ",
                            leaf::var(final_state.clone()),
                            "} <'true'> when 'true' -> {BoolAcc, ",
                            leaf::var(final_state),
                            "} end",
                        ]);
                    } else {
                        docs.push(docvec![
                            "case ",
                            leaf::var(pv.clone()),
                            " of <'true'> when 'true' -> {'true', ",
                            leaf::var(final_state.clone()),
                            "} <'false'> when 'true' -> {BoolAcc, ",
                            leaf::var(final_state),
                            "} end",
                        ]);
                    }
                }
            }
        }

        // BT-1486: FoldlDetect — update found-item accumulator on first match.
        // Accumulator is {FoundItem, FoundFlag, StateVars...}.
        // Only update FoundItem when pred=true AND FoundFlag='false' (first match only).
        if let BodyKind::FoldlDetect { item_var } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    docs.push(docvec![
                        "case ",
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> case FoundFlag of <'false'> when 'true' -> {",
                        leaf::var(item_var.clone()),
                        ", 'true', ",
                        vars_doc.clone(),
                        "} <'true'> when 'true' -> {FoundItem, 'true', ",
                        vars_doc.clone(),
                        "} end <'false'> when 'true' -> {FoundItem, FoundFlag, ",
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
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> case FoundFlag of <'false'> when 'true' -> {",
                        leaf::var(item_var.clone()),
                        ", 'true', ",
                        leaf::var(final_state.clone()),
                        "} <'true'> when 'true' -> {FoundItem, 'true', ",
                        leaf::var(final_state.clone()),
                        "} end <'false'> when 'true' -> {FoundItem, FoundFlag, ",
                        leaf::var(final_state),
                        "} end",
                    ]);
                }
            }
        }

        // BT-1486: FoldlCount — increment count accumulator on predicate match.
        // Accumulator is {Count, StateVars...}.
        if matches!(kind, BodyKind::FoldlCount) {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    docs.push(docvec![
                        "case ",
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> {call 'erlang':'+'(CountAcc, 1), ",
                        vars_doc.clone(),
                        "} <'false'> when 'true' -> {CountAcc, ",
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
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> {call 'erlang':'+'(CountAcc, 1), ",
                        leaf::var(final_state.clone()),
                        "} <'false'> when 'true' -> {CountAcc, ",
                        leaf::var(final_state),
                        "} end",
                    ]);
                }
            }
        }

        // BT-1487: FoldlTakeWhile — include item while predicate holds.
        // Once predicate returns false, StillTaking flips to false and all subsequent
        // elements are excluded. Accumulator: {ResultList, StillTaking, StateVars...}.
        if let BodyKind::FoldlTakeWhile { item_var } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    docs.push(docvec![
                        "case StillTaking of \
                         <'false'> when 'true' -> {AccList, 'false', ",
                        vars_doc.clone(),
                        "} <'true'> when 'true' -> case ",
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> {[",
                        leaf::var(item_var.clone()),
                        " | AccList], 'true', ",
                        vars_doc.clone(),
                        "} <'false'> when 'true' -> {AccList, 'false', ",
                        vars_doc,
                        "} end end",
                    ]);
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    docs.push(docvec![
                        "case StillTaking of \
                         <'false'> when 'true' -> {AccList, 'false', ",
                        leaf::var(final_state.clone()),
                        "} <'true'> when 'true' -> case ",
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> {[",
                        leaf::var(item_var.clone()),
                        " | AccList], 'true', ",
                        leaf::var(final_state.clone()),
                        "} <'false'> when 'true' -> {AccList, 'false', ",
                        leaf::var(final_state),
                        "} end end",
                    ]);
                }
            }
        }

        // BT-1487: FoldlDropWhile — drop items while predicate holds.
        // Once predicate returns false, StillDropping flips to false and all subsequent
        // elements are included. Accumulator: {ResultList, StillDropping, StateVars...}.
        if let BodyKind::FoldlDropWhile { item_var } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    docs.push(docvec![
                        "case StillDropping of \
                         <'false'> when 'true' -> {[",
                        leaf::var(item_var.clone()),
                        " | AccList], 'false', ",
                        vars_doc.clone(),
                        "} <'true'> when 'true' -> case ",
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> {AccList, 'true', ",
                        vars_doc.clone(),
                        "} <'false'> when 'true' -> {[",
                        leaf::var(item_var.clone()),
                        " | AccList], 'false', ",
                        vars_doc,
                        "} end end",
                    ]);
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    docs.push(docvec![
                        "case StillDropping of \
                         <'false'> when 'true' -> {[",
                        leaf::var(item_var.clone()),
                        " | AccList], 'false', ",
                        leaf::var(final_state.clone()),
                        "} <'true'> when 'true' -> case ",
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> {AccList, 'true', ",
                        leaf::var(final_state.clone()),
                        "} <'false'> when 'true' -> {[",
                        leaf::var(item_var.clone()),
                        " | AccList], 'false', ",
                        leaf::var(final_state),
                        "} end end",
                    ]);
                }
            }
        }

        // BT-1487: FoldlPartition — route item to one of two lists based on predicate.
        // Accumulator: {MatchList, NoMatchList, StateVars...}.
        if let BodyKind::FoldlPartition { item_var } = kind {
            if let Some(pv) = &pred_var {
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    docs.push(docvec![
                        "case ",
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> {[",
                        leaf::var(item_var.clone()),
                        " | MatchList], NoMatchList, ",
                        vars_doc.clone(),
                        "} <'false'> when 'true' -> {MatchList, [",
                        leaf::var(item_var.clone()),
                        " | NoMatchList], ",
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
                        leaf::var(pv.clone()),
                        " of <'true'> when 'true' -> {[",
                        leaf::var(item_var.clone()),
                        " | MatchList], NoMatchList, ",
                        leaf::var(final_state.clone()),
                        "} <'false'> when 'true' -> {MatchList, [",
                        leaf::var(item_var.clone()),
                        " | NoMatchList], ",
                        leaf::var(final_state),
                        "} end",
                    ]);
                }
            }
        }

        // BT-1487: FoldlGroupBy — group item by key.
        // The pred_var holds the key result. Each element is added to the key's list in a map.
        // Accumulator: {Map, StateVars...}.
        if let BodyKind::FoldlGroupBy { item_var } = kind {
            if let Some(pv) = &pred_var {
                // Use maps:get/3 to get current list for key (default []), prepend item, put back.
                let key_var = pv;
                if plan.use_tuple_acc {
                    let vars_doc = plan.current_vars_doc(self);
                    let existing_var = self.fresh_temp_var("ExistingList");
                    let new_list_var = self.fresh_temp_var("NewList");
                    let new_map_var = self.fresh_temp_var("NewMap");
                    docs.push(docvec![
                        "let ",
                        leaf::var(existing_var.clone()),
                        " = call 'maps':'get'(",
                        leaf::var(key_var.clone()),
                        ", GroupMap, []) in let ",
                        leaf::var(new_list_var.clone()),
                        " = [",
                        leaf::var(item_var.clone()),
                        " | ",
                        leaf::var(existing_var),
                        "] in let ",
                        leaf::var(new_map_var.clone()),
                        " = call 'maps':'put'(",
                        leaf::var(key_var.clone()),
                        ", ",
                        leaf::var(new_list_var),
                        ", GroupMap) in {",
                        leaf::var(new_map_var),
                        ", ",
                        vars_doc,
                        "}",
                    ]);
                } else {
                    let final_state = if has_mutations {
                        self.current_state_var()
                    } else {
                        "StateAcc".to_string()
                    };
                    let existing_var = self.fresh_temp_var("ExistingList");
                    let new_list_var = self.fresh_temp_var("NewList");
                    let new_map_var = self.fresh_temp_var("NewMap");
                    docs.push(docvec![
                        "let ",
                        leaf::var(existing_var.clone()),
                        " = call 'maps':'get'(",
                        leaf::var(key_var.clone()),
                        ", GroupMap, []) in let ",
                        leaf::var(new_list_var.clone()),
                        " = [",
                        leaf::var(item_var.clone()),
                        " | ",
                        leaf::var(existing_var),
                        "] in let ",
                        leaf::var(new_map_var.clone()),
                        " = call 'maps':'put'(",
                        leaf::var(key_var.clone()),
                        ", ",
                        leaf::var(new_list_var),
                        ", GroupMap) in {",
                        leaf::var(new_map_var),
                        ", ",
                        leaf::var(final_state),
                        "}",
                    ]);
                }
            }
        }

        let final_state_version = self.state_version();

        // BT-3169 (ADR 0111 Addendum 9, Question 6): whenever this fold body
        // threads `ClassVars`, wrap its returned TAIL VALUE — regardless of
        // which `BodyKind` arm above produced it, and regardless of that
        // arm's own internal shape (a bare `StateAcc`, `{[Result|AccList],
        // StateAcc}`, `{AccOut, StateAcc}`, a filter/predicate tuple, …) —
        // as `{ClassVars, <original tail value>}`. This is the single choke
        // point every `Foldl*` exit arm's tail value flows through
        // (`generate_threaded_loop_body`'s only call site into this
        // function), so it closes BT-3151's silent-loss gap uniformly
        // without touching any of the ~15 individual exit-arm branches
        // above: each keeps building exactly the value it always did.
        //
        // Deliberately `docs.pop()` + re-push, NOT `let FoldTail = <all of
        // docs> in {ClassVars, FoldTail}` (an earlier, rejected version of
        // this fix): `docs` is an OPEN Core Erlang let-chain — every element
        // but the last ends in `in `, and the last is a bare tail
        // expression, still lexically inside every preceding `let`'s scope.
        // Wrapping the WHOLE chain as the RHS of a fresh `let` closes that
        // scope at the chain's own tail expression, making any name a
        // mid-chain statement bound (e.g. a self-send's own `ClassVarsN`
        // rebind, `emit_class_var_result_unwrap`) unreachable from outside
        // — confirmed the hard way: `erlc` rejected it with "unbound
        // variable", not a scoping warning. Popping and rewrapping only the
        // last element leaves every earlier `let`'s scope untouched and
        // still open, so `cv` (itself possibly bound by one of those
        // `let`s) stays visible at the exact point it's used.
        //
        // Read AFTER the loop body is fully generated (not before) so a
        // class-method self-send's own `ClassVarsN` rebind inside this
        // iteration (`emit_class_var_result_unwrap`, frame-scoped to this
        // loop body's `current_branch_frame()` per Question 2) is reflected.
        //
        // BT-3168/BT-3169 merge: `plan.threads_class_vars` is now `true` for
        // TWO mutually exclusive shapes (see `ThreadingPlan::threads_class_vars`'s
        // own doc comment) — this `{ClassVars, tail}` accumulator wrap is
        // the `Foldl*` shape's own mechanism (Question 6) and must NOT also
        // fire for a `BodyKind::Letrec` plan: `while_loops.rs`/
        // `counted_loops.rs` already build their OWN, textually-different
        // `{ClassVars1, <tail>}` true-arm shape via the loop's extra
        // recursive-tail-call fun parameter (Question 3) as part of the
        // `BodyKind::Letrec` arms above, and popping+rewrapping that
        // half-built `docs` entry a second time here would splice the
        // closing `}` in before the arm's own trailing `apply` call —
        // confirmed the hard way via `erlc`'s "syntax error before: '}'"
        // on `loop_class_var_mutation.bt`.
        if !matches!(kind, BodyKind::Letrec) && plan.threads_class_vars {
            let cv = self.current_class_var();
            // BT-3169: record this closure's peak class-var version (BEFORE
            // `with_branch_context`'s guard restores it on drop, right after
            // this function returns) so `ThreadingPlan::foldl_call_doc` can
            // fast-forward past it — see `last_foldl_class_var_peak`'s own
            // doc comment for why a naive post-fold `next_class_var()` call
            // would otherwise mint an already-used name.
            self.set_foldl_class_var_peak(self.class_var_version());
            let tail = docs
                .pop()
                .expect("a Foldl* body must push at least one tail-expression Document");
            docs.push(docvec!["{", leaf::var(cv), ", ", tail, "}"]);
        }
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
                docs.push(leaf::var(self.current_state_var()));
            }
            BodyKind::FoldlCollect => {
                docs.push(docvec![
                    "{[_Val | AccList], ",
                    leaf::var(self.current_state_var()),
                    "}",
                ]);
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(docvec!["let ", leaf::var(pv.clone()), " = _Val in ",]);
                }
            }
            BodyKind::FoldlInject => {
                docs.push(docvec!["{_Val, ", leaf::var(self.current_state_var()), "}",]);
            }
            BodyKind::FoldlSort => {
                // sort: uses process dictionary, not generate_threaded_loop_body.
                unreachable!("FoldlSort does not use generate_threaded_loop_body");
            }
        }
    }

    fn emit_self_send_last_expr(
        &mut self,
        docs: &mut Vec<Document<'static>>,
        kind: &BodyKind,
        pred_var: Option<&String>,
        dispatch_var: &str,
    ) {
        match kind {
            BodyKind::Letrec => {
                // Nothing; caller appends recursive call.
            }
            BodyKind::FoldlDo => {
                docs.push(leaf::var(self.current_state_var()));
            }
            BodyKind::FoldlCollect => {
                let fs = self.current_state_var();
                let ir = self.fresh_temp_var("ItemResult");
                docs.push(docvec![
                    "let ",
                    leaf::var(ir.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(dispatch_var.to_string()),
                    ") in {[",
                    leaf::var(ir),
                    " | AccList], ",
                    leaf::var(fs),
                    "}",
                ]);
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(docvec![
                        "let ",
                        leaf::var(pv.clone()),
                        " = call 'erlang':'element'(1, ",
                        leaf::var(dispatch_var.to_string()),
                        ") in ",
                    ]);
                }
            }
            BodyKind::FoldlInject => {
                let fs = self.current_state_var();
                let ar = self.fresh_temp_var("AccResult");
                docs.push(docvec![
                    "let ",
                    leaf::var(ar.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(dispatch_var.to_string()),
                    ") in {",
                    leaf::var(ar),
                    ", ",
                    leaf::var(fs),
                    "}",
                ]);
            }
            BodyKind::FoldlSort => {
                unreachable!("FoldlSort does not use generate_threaded_loop_body");
            }
        }
    }

    /// BT-2813: emits the `is_last`-position tail for a bare Tier 2 `value(:...)`
    /// loop-body statement, per `BodyKind`. `tuple_var` holds the full
    /// `{Result, NewState}` tuple returned by `generate_tier2_value_call_doc`;
    /// `self.current_state_var()` already reflects `NewState` (bound by the
    /// caller before this is invoked). Mirrors `emit_self_send_last_expr`.
    fn emit_tier2_value_call_last_expr(
        &mut self,
        docs: &mut Vec<Document<'static>>,
        kind: &BodyKind,
        pred_var: Option<&String>,
        tuple_var: &str,
    ) {
        match kind {
            BodyKind::Letrec => {
                unreachable!("Letrec excluded by guard at the call site");
            }
            BodyKind::FoldlDo => {
                docs.push(leaf::var(self.current_state_var()));
            }
            BodyKind::FoldlCollect => {
                let fs = self.current_state_var();
                let ir = self.fresh_temp_var("T2LoopVal");
                docs.push(docvec![
                    "let ",
                    leaf::var(ir.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(tuple_var.to_string()),
                    ") in {[",
                    leaf::var(ir),
                    " | AccList], ",
                    leaf::var(fs),
                    "}",
                ]);
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(docvec![
                        "let ",
                        leaf::var(pv.clone()),
                        " = call 'erlang':'element'(1, ",
                        leaf::var(tuple_var.to_string()),
                        ") in ",
                    ]);
                }
            }
            BodyKind::FoldlInject => {
                let fs = self.current_state_var();
                let ar = self.fresh_temp_var("T2LoopVal");
                docs.push(docvec![
                    "let ",
                    leaf::var(ar.clone()),
                    " = call 'erlang':'element'(1, ",
                    leaf::var(tuple_var.to_string()),
                    ") in {",
                    leaf::var(ar),
                    ", ",
                    leaf::var(fs),
                    "}",
                ]);
            }
            BodyKind::FoldlSort => {
                unreachable!("FoldlSort does not use generate_threaded_loop_body");
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
                        leaf::var(val.to_string()),
                        " | AccList], ",
                        vars_doc,
                        "}",
                    ]);
                }
                BodyKind::FoldlFilter { .. }
                | BodyKind::FoldlBoolPredicate { .. }
                | BodyKind::FoldlDetect { .. }
                | BodyKind::FoldlCount
                | BodyKind::FoldlTakeWhile { .. }
                | BodyKind::FoldlDropWhile { .. }
                | BodyKind::FoldlPartition { .. }
                | BodyKind::FoldlGroupBy { .. } => {
                    if let Some(pv) = pred_var {
                        docs.push(docvec![
                            " let ",
                            leaf::var(pv.clone()),
                            " = ",
                            leaf::var(val.to_string()),
                            " in ",
                        ]);
                    }
                }
                BodyKind::FoldlInject => {
                    docs.push(docvec![
                        " {",
                        leaf::var(val.to_string()),
                        ", ",
                        vars_doc,
                        "}",
                    ]);
                }
                BodyKind::FoldlSort => {
                    unreachable!("FoldlSort does not use generate_threaded_loop_body");
                }
            }
            return;
        }
        match kind {
            BodyKind::Letrec => {
                // Nothing; caller appends recursive call.
            }
            BodyKind::FoldlDo => {
                docs.push(docvec![" ", leaf::var(self.current_state_var())]);
            }
            BodyKind::FoldlCollect => {
                docs.push(docvec![
                    " {[_Val | AccList], ",
                    leaf::var(self.current_state_var()),
                    "}",
                ]);
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(docvec![" let ", leaf::var(pv.clone()), " = _Val in ",]);
                }
            }
            BodyKind::FoldlInject => {
                docs.push(docvec![
                    " {_Val, ",
                    leaf::var(self.current_state_var()),
                    "}",
                ]);
            }
            BodyKind::FoldlSort => {
                unreachable!("FoldlSort does not use generate_threaded_loop_body");
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
                docs.push(leaf::var(self.current_state_var()));
            }
            BodyKind::FoldlCollect => {
                let fs = if has_mutations {
                    self.current_state_var()
                } else {
                    "StateAcc".to_string()
                };
                docs.push(docvec!["{['nil' | AccList], ", leaf::var(fs), "}",]);
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if let Some(pv) = pred_var {
                    docs.push(docvec!["let ", leaf::var(pv.clone()), " = 'false' in ",]);
                }
            }
            BodyKind::FoldlInject => {
                let fs = if has_mutations {
                    self.current_state_var()
                } else {
                    "StateAcc".to_string()
                };
                docs.push(docvec!["{'nil', ", leaf::var(fs), "}"]);
            }
            BodyKind::FoldlSort => {
                unreachable!("FoldlSort does not use generate_threaded_loop_body");
            }
        }
    }

    /// BT-3169 (ADR 0111 Addendum 9, Question 6): builds `"let <result_var> =
    /// <expr's value> in "` — the exact prelude every `is_last` `Foldl*`
    /// exit arm below builds by hand.
    ///
    /// ADR 0118 phase 5b (BT-3422): `expr`'s own top-level class-var
    /// producer (a same-class self-send or a class-var assignment) is now
    /// threaded ahead of this call by the caller's own `thread_ahead`
    /// (`emit_non_assign_expr`'s first statement), which splices a real
    /// `Bind` into the fold body's own frame — visible to every subsequent
    /// statement (including this function's own final `{ClassVars, tail}`
    /// wrap) without needing a scope to be kept open and re-paired here.
    /// The 2-tuple-pairing dance this replaced was the pre-ADR-0118
    /// mechanism for keeping a self-send's `ClassVarsN` rebind visible past
    /// its own closed-expression boundary — `plan.threads_class_vars`
    /// itself no longer changes what this function builds, since there is
    /// no separate scope left to pair.
    fn bind_closed_expr_threading_class_vars(
        &mut self,
        expr: &Expression,
        result_var: &str,
        _plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        // `expr` may dispatch a class-method self-send (locally declared or,
        // per BT-2007, inherited) that rebinds `ClassVarsN` opaquely, closed
        // by the time this call returns — `refresh_class_var_after_opaque_scope`
        // recovers the live value via the ADR 0110 shadow write (rather than
        // relying on lexical scope) so the fold's own `{ClassVars, tail}`
        // wrap, built from `current_class_var()` after this call, sees it
        // regardless of nesting depth.
        let cv_version_before = self.class_var_version();
        let expr_code = self.expression_doc(expr)?;
        let refresh = self
            .refresh_class_var_after_opaque_scope(cv_version_before)
            .unwrap_or(Document::Nil);
        Ok(docvec![
            "let ",
            leaf::var(result_var.to_string()),
            " = ",
            expr_code,
            " in ",
            refresh,
        ])
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
        has_mutations: &mut bool,
        has_plain_lets: bool,
        has_direct_field_assignments: bool,
        kind: &BodyKind,
        pred_var: Option<&String>,
        plan: &ThreadingPlan,
    ) -> Result<()> {
        // ADR 0118 phase 2b (BT-3418): thread every state-effecting
        // sub-expression nested in `expr` (e.g. `1 + (self bumpCount)`)
        // ahead of `expr`'s own compile, via the sequencing rule
        // (`Self::thread_ahead`) — the drop-in replacement for BT-3403's
        // planner-based emission (now deleted). Every branch below compiles `expr` through
        // `expression_doc`/`closed_expression_doc`/
        // `bind_closed_expr_threading_class_vars`/`push_discarded_stmt` —
        // all four route through `generate_expression`, so whichever one
        // fires consults the registered substitution; `finish_precompiled_scope`
        // is therefore called once, after the whole `match` below, rather
        // than duplicated at every one of those call sites. Must run before
        // every arm below reads `state_version()`/`current_state_var()`/
        // `*has_mutations` — a threaded prelude is exactly as
        // state-advancing as any other mutation this function already
        // detects, so `*has_mutations` is set here BEFORE any arm's own
        // `current_state_var()` vs `"StateAcc"` choice reads it (both the
        // ones inside this function and the caller's own post-loop
        // `FoldlFilter`/`FoldlBoolPredicate`-family wrap, which reads the
        // same accumulator after this call returns).
        let frame = self.current_frame();
        let mut prelude_stmts: Vec<ThreadedStmt> = Vec::new();
        let thread_scope = self.thread_ahead(expr, &mut prelude_stmts, frame)?;
        let hoisted_anything = !prelude_stmts.is_empty();
        if hoisted_anything {
            docs.push(self.threaded_prelude_doc(&prelude_stmts));
            *has_mutations = true;
        }
        let has_mutations = *has_mutations;

        // BT-3172: the nested-loop/fold `ClassVars`-loss check runs once, at
        // the top of `generate_threaded_loop_body_inner`'s per-statement
        // loop — ahead of every dispatch branch, not just this function's
        // own fallback — since a nested `do:`/`collect:`/etc. statement is
        // classified `DispatchKind::ControlFlow` and is actually intercepted
        // by that loop's `control_flow_has_mutations` branch before ever
        // reaching here. See that check's own comment for why.
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
                    // BT-478/BT-483: Mutations come from nested constructs
                    // (a bare nested loop/fold statement, whose own compile
                    // is itself a `{Result, State}` tuple) — extract updated
                    // state via element(2).
                    //
                    // BT-3403: that assumption does NOT hold when THIS
                    // statement's own mutation came entirely from a self-send
                    // threaded ahead above (`hoisted_anything`) — `expr`'s own
                    // compile is then just a plain value (the threaded
                    // dispatch's result was substituted in via
                    // `precompiled_subexprs`), not a tuple, and wrapping
                    // it in a phantom `element(2, ...)` unwrap crashes
                    // (`badarg`, confirmed empirically for `N timesRepeat: [1
                    // + (self bumpCount)]`). The threaded prelude's own let-chain already
                    // rebound `current_state_var()` to the post-dispatch
                    // state, so just discard `expr`'s plain value and use it
                    // directly — the same pattern the `else` branch below
                    // uses for a non-last statement.
                    //
                    // ADR 0118 phase 3 (BT-3419): that assumption ALSO does
                    // not hold for a genuinely PURE last statement (e.g.
                    // bare `nil`) — before this phase, this Letrec-mode
                    // fallback was only ever reached because SOMETHING in
                    // the body itself (a nested loop/fold, or an inline
                    // `ifTrue:`/`and:`/`or:` with mutations) forced
                    // StateAcc mode, so `expr` was always one of those two
                    // tuple-producing shapes. Now a `whileTrue: [nil]`
                    // whose CONDITION alone needs threading also compiles
                    // its (otherwise-pure) body here — `expr`'s own compile
                    // is then just `'nil'`, not a tuple, and the same
                    // phantom unwrap crashes (`badarg`, confirmed
                    // empirically for `[i := i + 1. (self bumpCount) + i <
                    // 5] whileTrue: [nil]`). Verify `expr` actually IS one
                    // of the two tuple-producing shapes before unwrapping.
                    let produces_tuple = !hoisted_anything
                        && (self.get_control_flow_threaded_vars(expr).is_some()
                            || self.control_flow_has_mutations(expr));
                    if produces_tuple {
                        let next_var = self.peek_next_state_var();
                        let tuple_var = format!("_NestTuple{}", self.state_version() + 1);
                        let expr_code = self.expression_doc(expr)?;
                        let _ = self.next_state_var();
                        docs.push(docvec![
                            "let ",
                            leaf::var(tuple_var.clone()),
                            " = ",
                            expr_code,
                            " in let ",
                            leaf::var(next_var),
                            " = call 'erlang':'element'(2, ",
                            leaf::var(tuple_var),
                            ") in",
                        ]);
                    } else {
                        let expr_code = self.expression_doc(expr)?;
                        docs.push(docvec!["let _ = ", expr_code, " in"]);
                    }
                } else {
                    let expr_code = self.expression_doc(expr)?;
                    docs.push(docvec!["let _ = ", expr_code, " in"]);
                }
            }
            BodyKind::FoldlDo => {
                if is_last {
                    // BT-1290: When preceding let-bindings exist (has_mutations/
                    // has_plain_lets), the last expression must also be bound with
                    // `let _ =` before `in StateAcc`. Without this,
                    // `let Y = ... in <expr> in StateAcc` is invalid Core Erlang
                    // (the `in StateAcc` has no corresponding `let`).
                    //
                    // BT-2350/BT-3169: close any class self-send open scope so
                    // the trailing `… in {vars}` / `… in StateAcc` does not
                    // dangle a second `in` — `bind_closed_expr_threading_class_vars`
                    // also threads a self-send's own `ClassVarsN` rebind
                    // forward past this `let _ = …` boundary when
                    // `plan.threads_class_vars` (see its own doc comment).
                    //
                    // BT-3169 review fix: this arm must take the same
                    // unconditional-threading path as `FoldlCollect`/
                    // `FoldlInject`/the predicate arms whenever
                    // `plan.threads_class_vars` — not just when
                    // `has_mutations || has_plain_lets`. A bare, last-statement
                    // self-send with no co-occurring local mutation (e.g.
                    // `aList do: [:x | self bump]`) previously fell to
                    // `closed_expression_doc`, which closes the self-send's own
                    // `ClassVarsN` rebind out of scope — but the generic
                    // `{ClassVars, tail}` wrap in `generate_threaded_loop_body_inner`
                    // (fired whenever `plan.threads_class_vars`, independent of
                    // `has_mutations`) then referenced that now-out-of-scope name,
                    // an `erlc` "unbound variable" regression confirmed empirically.
                    let threads_here = has_mutations || has_plain_lets || plan.threads_class_vars;
                    if threads_here {
                        docs.push(self.bind_closed_expr_threading_class_vars(expr, "_", plan)?);
                    } else {
                        let doc = self.expression_doc(expr)?;
                        docs.push(doc);
                    }
                    if threads_here {
                        if plan.use_tuple_acc {
                            // BT-1276: Repack threaded locals as tuple.
                            docs.push(docvec!["{", plan.current_vars_doc(self), "}"]);
                        } else {
                            let fs = if has_mutations {
                                self.current_state_var()
                            } else {
                                "StateAcc".to_string()
                            };
                            docs.push(leaf::var(fs));
                        }
                    }
                } else {
                    // BT-2350: ClassVars-visible discard for non-last statements
                    // (a class self-send leaves an open let-chain whose ClassVarsN
                    // must stay visible to following statements).
                    docs.push(docvec!["let _ = ", self.expression_doc(expr)?, " in "]);
                }
            }
            BodyKind::FoldlCollect => {
                if is_last {
                    let result_var = self.fresh_temp_var("CollectItem");
                    // BT-3169: threads a self-send's own `ClassVarsN` rebind
                    // forward past this `let` boundary when
                    // `plan.threads_class_vars` — see
                    // `bind_closed_expr_threading_class_vars`'s doc comment.
                    // BT-3169: pushed as its OWN `docs` entry, separate from
                    // the tuple-construction push below — this function's
                    // own final `{ClassVars, tail}` wrap only pops the LAST
                    // `docs` entry, so a self-send's `ClassVarsN` rebind
                    // inside `bind_doc` (an open, not-yet-closed chain) must
                    // stay a strictly EARLIER entry, not fused into the same
                    // one as the tuple it precedes — fusing them would place
                    // the wrap's `{ClassVars, …}` reference to `ClassVarsN`
                    // BEFORE the `let` that defines it (confirmed empirically
                    // — `erlc` "unbound variable", the same failure mode this
                    // whole helper exists to avoid).
                    docs.push(self.bind_closed_expr_threading_class_vars(
                        expr,
                        &result_var,
                        plan,
                    )?);
                    if plan.use_tuple_acc {
                        // BT-1276: Tuple mode — repack current vars.
                        let vars_doc = plan.current_vars_doc(self);
                        docs.push(docvec![
                            "{[",
                            leaf::var(result_var),
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
                            "{[",
                            leaf::var(result_var),
                            " | AccList], ",
                            leaf::var(fs),
                            "}",
                        ]);
                    }
                } else {
                    // BT-2350: a non-last statement may be a class self-send that
                    // emits an open let-chain; close it (keeping ClassVarsN visible)
                    // so the surrounding sequencing does not dangle a second `in`.
                    docs.push(docvec!["let _ = ", self.expression_doc(expr)?, " in "]);
                }
            }
            BodyKind::FoldlFilter { .. }
            | BodyKind::FoldlBoolPredicate { .. }
            | BodyKind::FoldlDetect { .. }
            | BodyKind::FoldlCount
            | BodyKind::FoldlTakeWhile { .. }
            | BodyKind::FoldlDropWhile { .. }
            | BodyKind::FoldlPartition { .. }
            | BodyKind::FoldlGroupBy { .. } => {
                if is_last {
                    if let Some(pv) = pred_var {
                        // BT-3169: threads a self-send's own `ClassVarsN`
                        // rebind forward past this `let` boundary when
                        // `plan.threads_class_vars` — see
                        // `bind_closed_expr_threading_class_vars`'s doc
                        // comment. This is the exact shape a `select:`
                        // predicate self-send needs (BT-3151's own repro).
                        docs.push(self.bind_closed_expr_threading_class_vars(expr, pv, plan)?);
                    }
                } else {
                    // BT-2350: see FoldlCollect — close a non-last open scope while
                    // keeping ClassVarsN visible for following statements.
                    docs.push(docvec!["let _ = ", self.expression_doc(expr)?, " in "]);
                }
            }
            BodyKind::FoldlInject => {
                if is_last {
                    let acc_var = self.fresh_temp_var("AccOut");
                    // BT-3169: pushed as its OWN `docs` entry, separate from
                    // the tuple-construction push below — see the identical
                    // `FoldlCollect` comment above for why fusing them is
                    // wrong (confirmed empirically, `erlc` "unbound
                    // variable").
                    docs.push(self.bind_closed_expr_threading_class_vars(expr, &acc_var, plan)?);
                    if plan.use_tuple_acc {
                        // BT-1276: Tuple mode — repack current vars.
                        let vars_doc = plan.current_vars_doc(self);
                        docs.push(docvec!["{", leaf::var(acc_var), ", ", vars_doc, "}",]);
                    } else {
                        let fs = if has_mutations {
                            self.current_state_var()
                        } else {
                            "StateAcc".to_string()
                        };
                        docs.push(docvec!["{", leaf::var(acc_var), ", ", leaf::var(fs), "}",]);
                    }
                } else {
                    // BT-2350: see FoldlCollect — close a non-last open scope while
                    // keeping ClassVarsN visible for following statements.
                    docs.push(docvec!["let _ = ", self.expression_doc(expr)?, " in "]);
                }
            }
            BodyKind::FoldlSort => {
                unreachable!("FoldlSort does not use generate_threaded_loop_body");
            }
        }
        self.finish_precompiled_scope(thread_scope)?;
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
        body: &beamtalk_core::ast::Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        if plan.use_direct_params {
            return self.generate_counted_stateful_loop_direct(frame, body, plan);
        }
        if plan.use_hybrid_params {
            return self.generate_counted_stateful_loop_hybrid(frame, body, plan);
        }

        let (pack_doc, init_state) = plan.generate_pack_prefix(self);

        // BT-3168 (ADR 0111 Addendum 9, Question 3): an extra, explicit
        // trailing fun parameter when the body threads `ClassVars` — the
        // arity grows to 3 (`counter, StateAcc, ClassVars`) instead of 2.
        // `frame.class_var_param` was captured pre-loop by the
        // `counted_loops.rs` constructor that built this frame.
        let arity = if frame.class_var_param.is_some() {
            3
        } else {
            2
        };
        let cv_param_doc = class_var_arg_doc(frame.class_var_param.as_ref());

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(pack_doc);
        docs.push(frame.preamble.clone());
        docs.push(docvec![
            " letrec ",
            leaf::fname(frame.fn_name.clone(), arity),
            " = fun (",
            leaf::var(frame.counter.clone()),
            ", StateAcc",
            cv_param_doc.clone(),
            ") -> ",
        ]);

        self.push_scope();

        // Bind the block counter param if any (e.g. to:do: [:i | ...] → bind "i" → counter)
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, &frame.counter);
        }

        // Unpack threaded locals at the top of each iteration
        let unpack_docs = plan.generate_unpack_at_iteration_start(self);
        docs.extend(unpack_docs);

        // Condition + true arm
        docs.push(frame.continue_header.clone());

        // Body
        let (body_doc, final_state_version) =
            self.generate_threaded_loop_body(body, plan, &BodyKind::Letrec)?;
        let final_class_var = self.last_loop_class_var.take();
        docs.push(body_doc);
        let final_state_var = super::util::versioned_var("StateAcc", final_state_version);
        let recur_cv_doc = final_class_var
            .as_ref()
            .map_or(Document::Nil, |v| docvec![", ", leaf::var(v.clone())]);

        self.pop_scope();

        // Recursive call + false arm + initial apply
        docs.push(docvec![
            " apply ",
            leaf::fname(frame.fn_name.clone(), arity),
            " (",
            frame.next_counter.clone(),
            ", ",
            leaf::var(final_state_var),
            recur_cv_doc,
            ") ",
            frame.false_arm.clone(),
            docvec![
                "in apply ",
                leaf::fname(frame.fn_name.clone(), arity),
                " (",
                frame.initial_counter.clone(),
                ", ",
                leaf::var(init_state),
                cv_param_doc,
                ")",
            ],
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
        body: &beamtalk_core::ast::Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        // Collect initial arg values from the outer scope (before push_scope overwrites them).
        let initial_direct_args = plan.initial_direct_args(self);

        // Build the fun parameter list: (<counter>, Var1, ..., VarN)
        let param_names: Vec<String> = plan
            .threaded_locals
            .iter()
            .map(|v| CoreErlangGenerator::to_core_erlang_var(v))
            .collect();
        let arity = 1 + param_names.len();
        let param_list_doc = join(
            std::iter::once(leaf::var(frame.counter.clone()))
                .chain(param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        );

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(frame.preamble.clone());
        docs.push(docvec![
            " letrec ",
            leaf::fname(frame.fn_name.clone(), arity),
            " = fun (",
            param_list_doc,
            ") -> ",
        ]);

        self.push_scope();

        // Bind the block counter param if any (e.g. to:do: [:i | ...] → bind "i" → counter)
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, &frame.counter);
        }

        // Register var → param bindings (no unpack docs emitted in
        // direct-params mode — structurally guaranteed by
        // `generate_unpack_at_iteration_start`'s own
        // `if !use_direct_params && !use_hybrid_params` guard).
        plan.generate_unpack_at_iteration_start(self);

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
            std::iter::once(frame.next_counter.clone())
                .chain(final_args.into_iter().map(leaf::var)),
            &Document::Str(", "),
        );
        let initial_args_doc = join(
            std::iter::once(frame.initial_counter.clone())
                .chain(initial_direct_args.into_iter().map(leaf::var)),
            &Document::Str(", "),
        );

        // Recursive call + false arm (with rebuilt StateAcc) + initial apply.
        docs.push(docvec![
            " apply ",
            leaf::fname(frame.fn_name.clone(), arity),
            " (",
            recursive_args_doc,
            ") ",
            "<'false'> when 'true' -> ",
            exit_stateacc,
            " end ",
            "in apply ",
            leaf::fname(frame.fn_name.clone(), arity),
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
    fn generate_counted_stateful_loop_hybrid(
        &mut self,
        frame: &CountedLoopFrame,
        body: &beamtalk_core::ast::Block,
        plan: &ThreadingPlan,
    ) -> Result<Document<'static>> {
        // Collect initial arg values from the outer scope (before push_scope overwrites them).
        let initial_local_args = plan.initial_direct_args(self);
        let initial_state = plan.initial_state_var.clone();

        // Pre-extract ALL fields (readonly + mutated) before the letrec.
        // Each field is read once from the outer state via maps:get.
        // The leading space before "let" matches the surrounding Core Erlang formatting.
        let (pre_extract_docs, readonly_params, mutated_params) =
            self.pre_extract_hybrid_fields(plan, &initial_state, (" ", ""));

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

        // Build param list doc: (<counter>, Var1, ..., VarN, RField1, ..., MField1, ...)
        let param_list_doc = join(
            std::iter::once(leaf::var(frame.counter.clone()))
                .chain(local_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(mutated_param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        );

        let mut docs: Vec<Document<'static>> = Vec::new();
        docs.push(frame.preamble.clone());
        docs.extend(pre_extract_docs);
        docs.push(docvec![
            " letrec ",
            leaf::fname(frame.fn_name.clone(), arity),
            " = fun (",
            param_list_doc,
            ") -> ",
        ]);

        self.push_scope();

        // Bind block counter param if any (e.g. to:do: [:i | ...] → bind "i" → counter)
        if let Some(ref bt_name) = frame.body_param {
            self.bind_var(bt_name, &frame.counter);
        }

        // Register local var bindings (no unpack docs emitted in hybrid mode
        // — structurally guaranteed by `generate_unpack_at_iteration_start`'s
        // own `if !use_direct_params && !use_hybrid_params` guard).
        plan.generate_unpack_at_iteration_start(self);

        // Condition + true arm
        docs.push(frame.continue_header.clone());

        // BT-1326/BT-1342: Run body with hybrid field params active; pop scope on error.
        let (body_doc, final_mutated_field_args) =
            self.run_counted_hybrid_body(body, plan, &readonly_params, &mutated_params)?;
        docs.push(body_doc);

        // Final local var args after body (updated bindings from scope).
        let final_local_args = self.collect_final_local_args(plan);

        // Exit StateAcc: uses initial param names (current iteration's starting values).
        // In the exit arm (false branch), the body hasn't executed, so params are unchanged.
        let exit_stateacc = plan.generate_exit_stateacc_full_extract(
            &local_param_names,
            &mutated_param_names,
            &initial_state,
            self,
        );

        self.pop_scope();

        Self::append_counted_hybrid_loop_tail(
            &mut docs,
            frame,
            arity,
            final_local_args,
            &readonly_param_names,
            &mutated_param_names,
            initial_local_args,
            final_mutated_field_args,
            exit_stateacc,
        );

        Ok(Document::Vec(docs))
    }

    /// Appends the recursive call, exit arm, and initial apply call to the counted hybrid loop docs.
    /// Executes the counted hybrid loop body with hybrid-mode field params active.
    ///
    /// Sets up `hybrid_readonly_field_params` and `hybrid_mutated_fields` from the
    /// pre-extracted params, runs the threaded body, captures final mutated field arg names,
    /// and restores all hybrid state. Calls `pop_scope` and returns an error if body fails.
    fn run_counted_hybrid_body(
        &mut self,
        body: &beamtalk_core::ast::Block,
        plan: &ThreadingPlan,
        readonly_params: &[(String, String)],
        mutated_params: &[(String, String)],
    ) -> Result<(Document<'static>, Vec<String>)> {
        let prev_hybrid = self.in_hybrid_loop;
        let prev_direct_params_loop = self.in_direct_params_loop;
        let mut all_field_params: std::collections::HashMap<String, String> =
            readonly_params.iter().cloned().collect();
        for (field, var) in mutated_params {
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
        let final_mutated_field_args: Vec<String> = plan
            .mutated_fields
            .iter()
            .map(|field| {
                self.hybrid_readonly_field_params
                    .get(field)
                    .cloned()
                    .unwrap_or_else(|| {
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
        let (body_doc, _) = match body_result {
            Ok(result) => result,
            Err(err) => {
                self.pop_scope();
                return Err(err);
            }
        };
        Ok((body_doc, final_mutated_field_args))
    }

    #[allow(clippy::too_many_arguments)]
    fn append_counted_hybrid_loop_tail(
        docs: &mut Vec<Document<'static>>,
        frame: &CountedLoopFrame,
        arity: usize,
        final_local_args: Vec<String>,
        readonly_param_names: &[String],
        mutated_param_names: &[String],
        initial_local_args: Vec<String>,
        final_mutated_field_args: Vec<String>,
        exit_stateacc: Document<'static>,
    ) {
        // Recursive call args: next_counter, updated locals, readonly fields (unchanged), updated mutated fields
        let recursive_args_doc = join(
            std::iter::once(frame.next_counter.clone())
                .chain(final_local_args.into_iter().map(leaf::var))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(final_mutated_field_args.into_iter().map(leaf::var)),
            &Document::Str(", "),
        );

        // Initial apply args: initial_counter, initial locals, initial readonly vals, initial mutated vals
        let initial_args_doc = join(
            std::iter::once(frame.initial_counter.clone())
                .chain(initial_local_args.into_iter().map(leaf::var))
                .chain(readonly_param_names.iter().map(|v| leaf::var(v.clone())))
                .chain(mutated_param_names.iter().map(|v| leaf::var(v.clone()))),
            &Document::Str(", "),
        );

        docs.push(docvec![
            " apply ",
            leaf::fname(frame.fn_name.clone(), arity),
            " (",
            recursive_args_doc,
            ") ",
            "<'false'> when 'true' -> ",
            exit_stateacc,
            " end ",
            "in apply ",
            leaf::fname(frame.fn_name.clone(), arity),
            " (",
            initial_args_doc,
            ")",
        ]);
    }

    // ── Compat shim ───────────────────────────────────────────────────────────

    /// Generates the foldl lambda body for a `do:` loop with state threading.
    ///
    /// This is a forwarding shim used by `value_type_codegen::generate_value_type_do_open`,
    /// which manages its own pack/extract prefix/suffix independently.
    pub(in crate::core_erlang) fn generate_list_do_body_with_threading(
        &mut self,
        body: &beamtalk_core::ast::Block,
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

    /// ADR 0111 Addendum 5 (BT-1213/BT-2355/BT-3173 rebind idiom): rebinds
    /// each of `vars` — a nested control-flow construct's threaded
    /// `__local__` captured vars — from `state_var`, returning one `let V =
    /// maps:get(...) in` `Document` per var and updating each var's own
    /// Core Erlang binding via `bind_var` so later code (in whatever form
    /// the caller assembles) sees the rebound value rather than the stale
    /// pre-statement one.
    ///
    /// Shared leaf helper for two call sites that both need this same
    /// rebind after unpacking a nested construct's `{Result, NewState}`
    /// result, differing only in how they wrap the returned `Document`s:
    /// `conditionals.rs`'s `push_control_flow_threaded_var_rereads` (the
    /// `ThreadedIr`-rendered conditional-arm path, wraps as one
    /// `ThreadedStmt::Statement`) and this module's
    /// `generate_threaded_loop_body_inner` (the foldl loop-body path,
    /// pushes directly onto its `docs` vec).
    pub(super) fn rebind_threaded_vars_from_state(
        &mut self,
        vars: &[String],
        state_var: &str,
    ) -> Vec<Document<'static>> {
        let mut docs = Vec::new();
        for var in vars {
            let core_var = self
                .lookup_var(var)
                .map_or_else(|| Self::to_core_erlang_var(var), String::clone);
            docs.push(docvec![
                "let ",
                leaf::var(core_var.clone()),
                " = call 'maps':'get'(",
                leaf::atom(Self::local_state_key(var)),
                ", ",
                leaf::var(state_var.to_string()),
                ") in ",
            ]);
            self.bind_var(var, &core_var);
        }
        docs
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
        body: &beamtalk_core::ast::Block,
        condition: Option<&Expression>,
    ) -> Vec<String> {
        if self.is_repl_mode() {
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
        // BT-2363: also thread write-only outer locals mutated inside nested counted/list-op
        // loops (those that the read+write `collect_list_op_cross_scope_mutations` misses).
        for stmt in &body.body {
            self.collect_nested_loop_outer_local_writes(
                &stmt.expression,
                &block_params,
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
                        format!("State{}", self.state_version())
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
                        format!("State{}", self.state_version())
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
                let current_state = super::util::versioned_var("StateAcc", self.state_version());

                // Increment state version for the new state
                let _ = self.next_state_var();
                let new_state = if self.in_loop_body {
                    self.current_state_var()
                } else {
                    format!("State{}", self.state_version())
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
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
    ) -> bool {
        use beamtalk_core::ast::MessageSelector;
        if let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            if beamtalk_core::state_threading_selectors::is_conditional_selector(sel.as_str()) {
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
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn collect_list_op_cross_scope_mutations(
        expr: &Expression,
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
        out: &mut std::collections::HashSet<String>,
    ) {
        use beamtalk_core::ast::MessageSelector;
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();

        // BT-3173: ensure:/on:do:/ifNotNil: aren't list-ops/counted-loops
        // themselves, but one may be nested inside one of their blocks —
        // recurse straight through their block(s) (the receiver for
        // ensure:/on:do:, any block arguments for all three) so a list-op's
        // cross-scope mutation buried behind one of these constructs is
        // still found, instead of stopping here (the previous behavior,
        // which silently dropped such a mutation from the outer loop's own
        // threaded-locals computation).
        if beamtalk_core::state_threading_selectors::is_exception_selector(&sel)
            || beamtalk_core::state_threading_selectors::is_conditional_selector(&sel)
        {
            let mut blocks: Vec<&beamtalk_core::ast::Block> = Vec::new();
            if beamtalk_core::state_threading_selectors::is_exception_selector(&sel) {
                if let Expression::Block(b) = receiver.as_ref() {
                    blocks.push(b);
                }
            }
            for arg in arguments {
                if let Expression::Block(b) = arg {
                    blocks.push(b);
                }
            }
            for block in blocks {
                // BT-3173 review follow-up: exclude this wrapping block's own
                // parameters (e.g. `on:do:`'s exception var, `ifNotNil:`'s bound
                // value) before merging into `out` — mirrors
                // `collect_nested_loop_outer_local_writes`'s `all_excluded`
                // threading for the identical construct shape. Without this, a
                // nested loop reporting a write to the wrapping block's own
                // param (e.g. `x ifNotNil: [:v | nested do: [:i | v := v + i]]`)
                // would be misreported as an outer-scope mutation.
                let block_params: std::collections::HashSet<String> = block
                    .parameters
                    .iter()
                    .map(|p| p.name.to_string())
                    .collect();
                let mut nested = std::collections::HashSet::new();
                for stmt in &block.body {
                    Self::collect_list_op_cross_scope_mutations_recursive(
                        &stmt.expression,
                        facts,
                        &mut nested,
                    );
                }
                for v in nested {
                    if !block_params.contains(v.as_str()) {
                        out.insert(v);
                    }
                }
            }
            return;
        }

        // BT-2363: nested counted loops (`timesRepeat:`/`to:do:`/`to:by:do:`)
        // capture and mutate outer locals just like list ops. Including them
        // here makes the *outer* loop's threaded-locals computation see
        // writes buried in an inner counted loop, so the outer loop threads
        // them via StateAcc instead of dropping them.
        let body_block = match Self::block_arg_for_selector(&sel, arguments) {
            Some(block)
                if matches!(
                    sel.as_str(),
                    "do:"
                        | "collect:"
                        | "select:"
                        | "reject:"
                        | "anySatisfy:"
                        | "allSatisfy:"
                        | "timesRepeat:"
                        | "inject:into:"
                        | "to:do:"
                        | "to:by:do:"
                ) =>
            {
                block
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

        // BT-2363: Recurse into the inner block's statements so deeper nesting
        // (a counted/list op nested two or more levels deep) is still detected.
        // `analyze_block` does not propagate writes out of nested non-conditional
        // blocks, so a write buried in a doubly-nested loop is invisible above
        // without this recursion. Block parameters of the inner block are not
        // outer locals, so drop any cross-scope name shadowed by a block param.
        let mut nested = std::collections::HashSet::new();
        for stmt in &body_block.body {
            Self::collect_list_op_cross_scope_mutations_recursive(
                &stmt.expression,
                facts,
                &mut nested,
            );
        }
        for v in nested {
            if !block_params.contains(v.as_str()) {
                out.insert(v);
            }
        }
    }

    /// BT-2363: Returns `true` if `expr` is (or wraps, via assignment RHS or parens) a
    /// nested counted loop (`timesRepeat:`/`to:do:`/`to:by:do:`) whose body mutates one
    /// of the outer loop's `threaded_locals`.
    ///
    /// Such an inner loop returns a `{value, StateAcc}` tuple whose `element(2, …)` must
    /// be unpacked to thread the local back out; that is only possible when the outer loop
    /// uses `StateAcc` mode, so the presence of this pattern disqualifies direct-params.
    pub(super) fn expr_has_nested_counted_loop_threading(
        &self,
        expr: &Expression,
        threaded_locals: &[String],
    ) -> bool {
        use beamtalk_core::ast::MessageSelector;

        let inner = match expr.unwrap_parens() {
            Expression::Assignment { value, .. } => value.unwrap_parens(),
            other => other,
        };
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = inner
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let body_block = match Self::block_arg_for_selector(&sel, arguments) {
            Some(block) if matches!(sel.as_str(), "timesRepeat:" | "to:do:" | "to:by:do:") => block,
            _ => return false,
        };

        // The inner counted loop threads back exactly the outer locals its own body
        // mutates (read+write or write-only). If any of those overlap the threaded set
        // the outer loop must thread, the inner tuple must be unpacked into StateAcc.
        let inner_threaded = self.compute_threaded_locals_for_loop(body_block, None);
        inner_threaded.iter().any(|v| threaded_locals.contains(v))
    }

    /// BT-1329: Returns `true` if `expr` is a list op (do:, collect:, select:, reject:,
    /// anySatisfy:, allSatisfy:, inject:into:) whose block captures and mutates outer-scope locals but whose inner
    /// block is NOT eligible for tuple-acc optimization.
    ///
    /// When this returns `true`, the list op would fall back to map-accumulator mode which
    /// references `StateAcc` — incompatible with direct-params loops. The outer loop must
    /// fall back to `StateAcc` mode.
    fn list_op_needs_stateacc_fallback(
        expr: &Expression,
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
    ) -> bool {
        use beamtalk_core::ast::MessageSelector;
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
        let body_block = match Self::block_arg_for_selector(&sel, arguments) {
            Some(block)
                if matches!(
                    sel.as_str(),
                    "do:"
                        | "collect:"
                        | "select:"
                        | "reject:"
                        | "anySatisfy:"
                        | "allSatisfy:"
                        | "inject:into:"
                ) =>
            {
                block
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
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core_erlang::block_analysis::BlockMutationAnalysis;

    // ─── Helper constructors ────────────────────────────────────────────────────

    /// Creates a bare `CoreErlangGenerator` (not inside a class method,
    /// `Actor` context) — used only to exercise `select_hybrid_params`'s
    /// `generator.in_class_method()` guard (BT-3168) in isolation.
    fn plain_generator() -> CoreErlangGenerator {
        CoreErlangGenerator::new("test")
    }

    /// Creates a `BodyEffects` with all flags set to `false`.
    fn clean_effects() -> BodyEffects {
        BodyEffects {
            cond_has_state_effects: false,
            has_tier2_threaded_assign: false,
            has_non_tuple_safe_list_op: false,
            has_nested_counted_loop_mutation: false,
            has_cf_mutations: false,
            has_conditional_threaded_writes: false,
            last_is_destructure: false,
        }
    }

    /// Creates a `BlockMutationAnalysis` with no mutations.
    fn clean_body_analysis() -> BlockMutationAnalysis {
        BlockMutationAnalysis::new()
    }

    /// Creates a `BlockMutationAnalysis` with self-sends.
    fn body_with_self_sends() -> BlockMutationAnalysis {
        let mut a = BlockMutationAnalysis::new();
        a.has_self_sends = true;
        a
    }

    /// Creates a `BlockMutationAnalysis` with field writes.
    fn body_with_field_writes(fields: &[&str]) -> BlockMutationAnalysis {
        let mut a = BlockMutationAnalysis::new();
        a.field_writes = fields.iter().map(ToString::to_string).collect();
        a
    }

    /// Creates a `BlockMutationAnalysis` with both field writes and self-sends.
    fn body_with_field_writes_and_self_sends(fields: &[&str]) -> BlockMutationAnalysis {
        let mut a = body_with_field_writes(fields);
        a.has_self_sends = true;
        a
    }

    /// Builds a minimal `ThreadingPlan` for testing computed properties.
    fn make_plan(
        threaded_locals: Vec<String>,
        key_style: KeyStyle,
        use_direct_params: bool,
        use_tuple_acc: bool,
        use_hybrid_params: bool,
        readonly_fields: Vec<String>,
        mutated_fields: Vec<String>,
    ) -> ThreadingPlan {
        ThreadingPlan {
            threaded_locals,
            initial_state_var: "State".to_string(),
            key_style,
            context: CodeGenContext::Actor,
            use_direct_params,
            use_tuple_acc,
            tuple_acc_gate_slots: 0,
            use_hybrid_params,
            readonly_fields,
            fallback_reason: StateAccFallbackReason::None,
            mutated_fields,
            threads_class_vars: false,
            initial_class_var: "ClassVars".to_string(),
        }
    }

    // ─── StateAccFallbackReason Display tests ───────────────────────────────────

    #[test]
    fn fallback_reason_display_none() {
        assert_eq!(StateAccFallbackReason::None.to_string(), "none");
    }

    #[test]
    fn fallback_reason_display_self_send() {
        assert_eq!(
            StateAccFallbackReason::SelfSendInBody.to_string(),
            "self-send in loop body"
        );
    }

    #[test]
    fn fallback_reason_display_nested_list_op() {
        assert_eq!(
            StateAccFallbackReason::NestedListOpCrossScope.to_string(),
            "nested list op with cross-scope mutation"
        );
    }

    #[test]
    fn fallback_reason_display_tier2_value_call() {
        assert_eq!(
            StateAccFallbackReason::Tier2ValueCallOnThreaded.to_string(),
            "tier-2 value call on threaded local"
        );
    }

    #[test]
    fn fallback_reason_display_all_variants() {
        // Ensure every variant has a non-empty display string.
        let variants = [
            StateAccFallbackReason::None,
            StateAccFallbackReason::SelfSendInBody,
            StateAccFallbackReason::NestedListOpCrossScope,
            StateAccFallbackReason::Tier2ValueCallOnThreaded,
            StateAccFallbackReason::InlineConditionalThreadedWrite,
            StateAccFallbackReason::ConditionStateEffects,
            StateAccFallbackReason::ControlFlowMutations,
            StateAccFallbackReason::NoThreadedLocals,
            StateAccFallbackReason::ValueTypeContext,
            StateAccFallbackReason::NotLetrec,
            StateAccFallbackReason::DestructureAsLastExpr,
        ];
        for v in &variants {
            assert!(!v.to_string().is_empty(), "variant {v:?} has empty display");
        }
    }

    // ─── convention_label tests ─────────────────────────────────────────────────

    #[test]
    fn convention_label_direct_params() {
        let plan = make_plan(
            vec!["x".into()],
            KeyStyle::LocalPrefixed,
            true,
            false,
            false,
            vec![],
            vec![],
        );
        assert_eq!(plan.convention_label(), "direct-params");
    }

    #[test]
    fn convention_label_tuple_acc() {
        let plan = make_plan(
            vec!["x".into()],
            KeyStyle::LocalPrefixed,
            false,
            true,
            false,
            vec![],
            vec![],
        );
        assert_eq!(plan.convention_label(), "tuple-acc");
    }

    #[test]
    fn convention_label_hybrid() {
        let plan = make_plan(
            vec!["x".into()],
            KeyStyle::LocalPrefixed,
            false,
            false,
            true,
            vec!["f".into()],
            vec!["g".into()],
        );
        assert_eq!(plan.convention_label(), "hybrid");
    }

    #[test]
    fn convention_label_stateacc_fallback() {
        let plan = make_plan(
            vec!["x".into()],
            KeyStyle::LocalPrefixed,
            false,
            false,
            false,
            vec![],
            vec![],
        );
        assert_eq!(plan.convention_label(), "StateAcc");
    }

    // ─── total_extracted_params tests ───────────────────────────────────────────

    #[test]
    fn total_extracted_params_empty() {
        let plan = make_plan(
            vec![],
            KeyStyle::LocalPrefixed,
            false,
            false,
            false,
            vec![],
            vec![],
        );
        assert_eq!(plan.total_extracted_params(), 0);
    }

    #[test]
    fn total_extracted_params_locals_only() {
        let plan = make_plan(
            vec!["a".into(), "b".into()],
            KeyStyle::LocalPrefixed,
            true,
            false,
            false,
            vec![],
            vec![],
        );
        assert_eq!(plan.total_extracted_params(), 2);
    }

    #[test]
    fn total_extracted_params_locals_and_readonly_fields() {
        let plan = make_plan(
            vec!["a".into(), "b".into()],
            KeyStyle::LocalPrefixed,
            false,
            false,
            true,
            vec!["rf1".into(), "rf2".into(), "rf3".into()],
            vec!["mf1".into()],
        );
        assert_eq!(plan.total_extracted_params(), 5);
    }

    // ─── state_key tests ────────────────────────────────────────────────────────

    #[test]
    fn state_key_local_prefixed() {
        let plan = make_plan(
            vec![],
            KeyStyle::LocalPrefixed,
            false,
            false,
            false,
            vec![],
            vec![],
        );
        assert_eq!(plan.state_key("count"), "__local__count");
    }

    #[test]
    fn state_key_repl_plain() {
        let plan = make_plan(
            vec![],
            KeyStyle::ReplPlain,
            false,
            false,
            false,
            vec![],
            vec![],
        );
        assert_eq!(plan.state_key("count"), "count");
    }

    // ─── select_direct_params tests ─────────────────────────────────────────────

    #[test]
    fn select_direct_params_eligible() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        assert!(ThreadingPlan::select_direct_params(
            true, &threaded, &analysis, &effects
        ));
    }

    #[test]
    fn select_direct_params_not_allowed() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_direct_params(
            false, &threaded, &analysis, &effects
        ));
    }

    #[test]
    fn select_direct_params_empty_threaded() {
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_direct_params(
            true,
            &[],
            &analysis,
            &effects
        ));
    }

    #[test]
    fn select_direct_params_blocked_by_state_effects() {
        let threaded = vec!["x".to_string()];
        let analysis = body_with_self_sends();
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_direct_params(
            true, &threaded, &analysis, &effects
        ));
    }

    #[test]
    fn select_direct_params_blocked_by_cond_state_effects() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.cond_has_state_effects = true;
        assert!(!ThreadingPlan::select_direct_params(
            true, &threaded, &analysis, &effects
        ));
    }

    #[test]
    fn select_direct_params_blocked_by_tier2() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.has_tier2_threaded_assign = true;
        assert!(!ThreadingPlan::select_direct_params(
            true, &threaded, &analysis, &effects
        ));
    }

    #[test]
    fn select_direct_params_blocked_by_nested_list_op() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.has_non_tuple_safe_list_op = true;
        assert!(!ThreadingPlan::select_direct_params(
            true, &threaded, &analysis, &effects
        ));
    }

    // ─── select_tuple_acc tests ─────────────────────────────────────────────────

    #[test]
    fn select_tuple_acc_eligible() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        assert!(ThreadingPlan::select_tuple_acc(
            true,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects
        ));
    }

    #[test]
    fn select_tuple_acc_not_allowed() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_tuple_acc(
            false,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects
        ));
    }

    #[test]
    fn select_tuple_acc_empty_threaded() {
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_tuple_acc(
            true,
            &[],
            CodeGenContext::Actor,
            &analysis,
            &effects
        ));
    }

    #[test]
    fn select_tuple_acc_blocked_by_value_type_context() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_tuple_acc(
            true,
            &threaded,
            CodeGenContext::ValueType,
            &analysis,
            &effects
        ));
    }

    #[test]
    fn select_tuple_acc_blocked_by_state_effects() {
        let threaded = vec!["x".to_string()];
        let analysis = body_with_field_writes(&["n"]);
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_tuple_acc(
            true,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects
        ));
    }

    #[test]
    fn select_tuple_acc_blocked_by_cf_mutations() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.has_cf_mutations = true;
        assert!(!ThreadingPlan::select_tuple_acc(
            true,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects
        ));
    }

    #[test]
    fn select_tuple_acc_blocked_by_conditional_threaded_writes() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.has_conditional_threaded_writes = true;
        assert!(!ThreadingPlan::select_tuple_acc(
            true,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects
        ));
    }

    #[test]
    fn select_tuple_acc_blocked_by_destructure() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.last_is_destructure = true;
        assert!(!ThreadingPlan::select_tuple_acc(
            true,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects
        ));
    }

    // ─── select_hybrid_params tests ─────────────────────────────────────────────

    #[test]
    fn select_hybrid_params_eligible() {
        let threaded = vec!["x".to_string()];
        let analysis = body_with_field_writes(&["n"]);
        let effects = clean_effects();
        assert!(ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::Actor,
            false,
            &analysis,
            &effects,
            &plain_generator()
        ));
    }

    #[test]
    fn select_hybrid_params_not_allowed() {
        let threaded = vec!["x".to_string()];
        let analysis = body_with_field_writes(&["n"]);
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_hybrid_params(
            false,
            &threaded,
            CodeGenContext::Actor,
            false,
            &analysis,
            &effects,
            &plain_generator()
        ));
    }

    #[test]
    fn select_hybrid_params_empty_threaded() {
        let analysis = body_with_field_writes(&["n"]);
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_hybrid_params(
            true,
            &[],
            CodeGenContext::Actor,
            false,
            &analysis,
            &effects,
            &plain_generator()
        ));
    }

    #[test]
    fn select_hybrid_params_skipped_when_direct_params() {
        let threaded = vec!["x".to_string()];
        let analysis = body_with_field_writes(&["n"]);
        let effects = clean_effects();
        // use_direct_params = true means hybrid is skipped
        assert!(!ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::Actor,
            true,
            &analysis,
            &effects,
            &plain_generator()
        ));
    }

    #[test]
    fn select_hybrid_params_blocked_by_value_type() {
        let threaded = vec!["x".to_string()];
        let analysis = body_with_field_writes(&["n"]);
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::ValueType,
            false,
            &analysis,
            &effects,
            &plain_generator()
        ));
    }

    #[test]
    fn select_hybrid_params_blocked_by_no_field_writes() {
        let threaded = vec!["x".to_string()];
        let analysis = clean_body_analysis(); // no field_writes
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::Actor,
            false,
            &analysis,
            &effects,
            &plain_generator()
        ));
    }

    #[test]
    fn select_hybrid_params_blocked_by_self_sends() {
        let threaded = vec!["x".to_string()];
        let analysis = body_with_field_writes_and_self_sends(&["n"]);
        let effects = clean_effects();
        assert!(!ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::Actor,
            false,
            &analysis,
            &effects,
            &plain_generator()
        ));
    }

    #[test]
    fn select_hybrid_params_blocked_by_cond_state_effects() {
        let threaded = vec!["x".to_string()];
        let analysis = body_with_field_writes(&["n"]);
        let mut effects = clean_effects();
        effects.cond_has_state_effects = true;
        assert!(!ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::Actor,
            false,
            &analysis,
            &effects,
            &plain_generator()
        ));
    }

    #[test]
    fn select_hybrid_params_blocked_by_class_method() {
        // ADR 0111 Addendum 9 Question 4 Part B (BT-3168): an otherwise-eligible
        // body (identical to `select_hybrid_params_eligible`'s fixture) must be
        // excluded from Hybrid mode once the generator is inside a class method,
        // since Hybrid's per-field pre-extraction doesn't understand ADR 0110's
        // multi-field class-var map + shadow-write semantics.
        let threaded = vec!["x".to_string()];
        let analysis = body_with_field_writes(&["n"]);
        let effects = clean_effects();
        let mut generator = plain_generator();
        generator.set_in_class_method(true);
        assert!(!ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::Actor,
            false,
            &analysis,
            &effects,
            &generator
        ));
    }

    // ─── determine_fallback_reason tests ────────────────────────────────────────

    #[test]
    fn fallback_reason_none_when_optimized_selected() {
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        let reason = ThreadingPlan::determine_fallback_reason(
            true,
            true,
            &["x".to_string()],
            CodeGenContext::Actor,
            &analysis,
            &effects,
        );
        assert!(matches!(reason, StateAccFallbackReason::None));
    }

    #[test]
    fn fallback_reason_no_threaded_locals() {
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        let reason = ThreadingPlan::determine_fallback_reason(
            false,
            true,
            &[],
            CodeGenContext::Actor,
            &analysis,
            &effects,
        );
        assert!(matches!(reason, StateAccFallbackReason::NoThreadedLocals));
    }

    #[test]
    fn fallback_reason_not_letrec() {
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        let reason = ThreadingPlan::determine_fallback_reason(
            false,
            false,
            &["x".to_string()],
            CodeGenContext::Actor,
            &analysis,
            &effects,
        );
        assert!(matches!(reason, StateAccFallbackReason::NotLetrec));
    }

    #[test]
    fn fallback_reason_self_send_in_body() {
        let analysis = body_with_self_sends();
        let effects = clean_effects();
        let reason = ThreadingPlan::determine_fallback_reason(
            false,
            true,
            &["x".to_string()],
            CodeGenContext::Actor,
            &analysis,
            &effects,
        );
        assert!(matches!(reason, StateAccFallbackReason::SelfSendInBody));
    }

    #[test]
    fn fallback_reason_field_writes_delegates_to_diagnose() {
        // Field writes present but no self-sends — should delegate to diagnose_guard_failure.
        let analysis = body_with_field_writes(&["n"]);
        let mut effects = clean_effects();
        effects.has_tier2_threaded_assign = true;
        let reason = ThreadingPlan::determine_fallback_reason(
            false,
            true,
            &["x".to_string()],
            CodeGenContext::Actor,
            &analysis,
            &effects,
        );
        assert!(matches!(
            reason,
            StateAccFallbackReason::Tier2ValueCallOnThreaded
        ));
    }

    #[test]
    fn fallback_reason_value_type_context() {
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        let reason = ThreadingPlan::determine_fallback_reason(
            false,
            true,
            &["x".to_string()],
            CodeGenContext::ValueType,
            &analysis,
            &effects,
        );
        assert!(matches!(reason, StateAccFallbackReason::ValueTypeContext));
    }

    // ─── diagnose_guard_failure tests ───────────────────────────────────────────

    #[test]
    fn diagnose_nested_list_op() {
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.has_non_tuple_safe_list_op = true;
        let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
        assert!(matches!(
            reason,
            StateAccFallbackReason::NestedListOpCrossScope
        ));
    }

    #[test]
    fn diagnose_tier2_threaded_assign() {
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.has_tier2_threaded_assign = true;
        let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
        assert!(matches!(
            reason,
            StateAccFallbackReason::Tier2ValueCallOnThreaded
        ));
    }

    #[test]
    fn diagnose_cond_state_effects() {
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.cond_has_state_effects = true;
        let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
        assert!(matches!(
            reason,
            StateAccFallbackReason::ConditionStateEffects
        ));
    }

    #[test]
    fn diagnose_cf_mutations() {
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.has_cf_mutations = true;
        let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
        assert!(matches!(
            reason,
            StateAccFallbackReason::ControlFlowMutations
        ));
    }

    #[test]
    fn diagnose_conditional_threaded_writes() {
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.has_conditional_threaded_writes = true;
        let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
        assert!(matches!(
            reason,
            StateAccFallbackReason::InlineConditionalThreadedWrite
        ));
    }

    #[test]
    fn diagnose_destructure_as_last_expr() {
        let analysis = clean_body_analysis();
        let mut effects = clean_effects();
        effects.last_is_destructure = true;
        let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
        assert!(matches!(
            reason,
            StateAccFallbackReason::DestructureAsLastExpr
        ));
    }

    #[test]
    fn diagnose_self_sends_fallback() {
        let analysis = body_with_self_sends();
        let effects = clean_effects();
        let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
        assert!(matches!(reason, StateAccFallbackReason::SelfSendInBody));
    }

    #[test]
    fn diagnose_guard_priority_order() {
        // When multiple guards are set, the first in priority order wins.
        let analysis = body_with_self_sends();
        let mut effects = clean_effects();
        effects.has_non_tuple_safe_list_op = true;
        effects.has_tier2_threaded_assign = true;
        effects.cond_has_state_effects = true;
        // nested_list_op has highest priority in diagnose_guard_failure
        let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
        assert!(matches!(
            reason,
            StateAccFallbackReason::NestedListOpCrossScope
        ));
    }

    #[test]
    fn diagnose_default_is_control_flow_mutations() {
        // No specific guard triggered, no self-sends — falls through to default.
        let analysis = clean_body_analysis();
        let effects = clean_effects();
        let reason = ThreadingPlan::diagnose_guard_failure(&analysis, &effects);
        assert!(matches!(
            reason,
            StateAccFallbackReason::ControlFlowMutations
        ));
    }

    // ─── BodyEffects struct tests ───────────────────────────────────────────────

    #[test]
    fn body_effects_default_all_false() {
        let effects = clean_effects();
        assert!(!effects.cond_has_state_effects);
        assert!(!effects.has_tier2_threaded_assign);
        assert!(!effects.has_non_tuple_safe_list_op);
        assert!(!effects.has_cf_mutations);
        assert!(!effects.has_conditional_threaded_writes);
        assert!(!effects.last_is_destructure);
    }

    // ─── Integration: combined strategy selection tests ─────────────────────────

    #[test]
    fn letrec_pure_locals_selects_direct_params() {
        // Letrec with pure local mutations (no field writes, no self-sends)
        // should select direct-params.
        let threaded = vec!["count".to_string()];
        let analysis = clean_body_analysis();
        let effects = clean_effects();

        let direct = ThreadingPlan::select_direct_params(true, &threaded, &analysis, &effects);
        let tuple = ThreadingPlan::select_tuple_acc(
            false,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects,
        );
        let hybrid = ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::Actor,
            direct,
            &analysis,
            &effects,
            &plain_generator(),
        );

        assert!(direct);
        assert!(!tuple);
        assert!(!hybrid);
    }

    #[test]
    fn foldl_pure_locals_selects_tuple_acc() {
        // Foldl list-op with pure local mutations should select tuple-acc.
        let threaded = vec!["sum".to_string()];
        let analysis = clean_body_analysis();
        let effects = clean_effects();

        let direct = ThreadingPlan::select_direct_params(false, &threaded, &analysis, &effects);
        let tuple = ThreadingPlan::select_tuple_acc(
            true,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects,
        );
        let hybrid = ThreadingPlan::select_hybrid_params(
            false,
            &threaded,
            CodeGenContext::Actor,
            direct,
            &analysis,
            &effects,
            &plain_generator(),
        );

        assert!(!direct);
        assert!(tuple);
        assert!(!hybrid);
    }

    #[test]
    fn letrec_field_writes_selects_hybrid() {
        // Letrec with field writes (no self-sends) should select hybrid.
        let threaded = vec!["x".to_string()];
        let analysis = body_with_field_writes(&["counter"]);
        let effects = clean_effects();

        let direct = ThreadingPlan::select_direct_params(true, &threaded, &analysis, &effects);
        let hybrid = ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::Actor,
            direct,
            &analysis,
            &effects,
            &plain_generator(),
        );

        assert!(!direct); // field writes block direct-params
        assert!(hybrid);
    }

    #[test]
    fn letrec_self_sends_falls_back_to_stateacc() {
        // Letrec with self-sends should fall back to StateAcc.
        let threaded = vec!["x".to_string()];
        let analysis = body_with_self_sends();
        let effects = clean_effects();

        let direct = ThreadingPlan::select_direct_params(true, &threaded, &analysis, &effects);
        let tuple = ThreadingPlan::select_tuple_acc(
            false,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects,
        );
        let hybrid = ThreadingPlan::select_hybrid_params(
            true,
            &threaded,
            CodeGenContext::Actor,
            direct,
            &analysis,
            &effects,
            &plain_generator(),
        );

        assert!(!direct);
        assert!(!tuple);
        assert!(!hybrid);

        let reason = ThreadingPlan::determine_fallback_reason(
            false,
            true,
            &threaded,
            CodeGenContext::Actor,
            &analysis,
            &effects,
        );
        assert!(matches!(reason, StateAccFallbackReason::SelfSendInBody));
    }
}
