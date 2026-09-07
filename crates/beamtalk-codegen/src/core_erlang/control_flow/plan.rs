// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ThreadingPlan` — the pre-computed contract for threading mutable state
//! through a loop or fold body, plus the mode-selection predicates that
//! build it.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! BT-3459: split out of `control_flow/mod.rs`, no logic changes.

use super::super::threaded_ir::{self, StateAccFallbackReason};
use super::super::{CodeGenContext, CoreErlangGenerator, block_analysis};
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::{Document, join, leaf};
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

// ─── ThreadingPlan ────────────────────────────────────────────────────────────

/// Selects the naming convention for state-map keys.
#[derive(Clone, Debug)]
pub(in crate::core_erlang) enum KeyStyle {
    /// `__local__x` prefix (actor and value-type methods).
    LocalPrefixed,
    /// Plain variable name (REPL mode).
    ReplPlain,
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
pub(in crate::core_erlang) enum ListOpKind {
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
    pub(in crate::core_erlang) const fn gate_slots(self) -> usize {
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
pub(in crate::core_erlang) struct ThreadingPlan {
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
pub(in crate::core_erlang) fn condition_has_state_effects(condition: &Expression) -> bool {
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

    /// Resolves each threaded local's REAL, currently-bound Core Erlang
    /// variable name in the AMBIENT (pre-loop) scope — e.g. a method
    /// parameter unpacked from `Args` under a gensym'd pattern name like
    /// `_startFlag1`, never the generic `to_core_erlang_var` spelling
    /// (`StartFlag`) a plain `:=`-declared local would get. Needed for a
    /// `DirectParams`/`Hybrid`-mode loop's OUTER initial `apply` argument
    /// list: `param_list`/`body`/`final_args` all correctly use the generic
    /// spelling (the fun's own parameter, always freshly bound to that
    /// name), but the value actually LIVE at the call site is whatever this
    /// Beamtalk name currently resolves to via `lookup_var` — which only
    /// coincides with the generic spelling when the local was itself
    /// declared by a plain `:=` (never a parameter). Falls back to the
    /// generic spelling when `lookup_var` has no entry (never observed in
    /// practice — every threaded local is bound by the time a loop
    /// references it — kept as defense-in-depth, matching this method's own
    /// pre-ADR-0111-Addendum-15 behavior).
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

#[cfg(test)]
mod tests;
