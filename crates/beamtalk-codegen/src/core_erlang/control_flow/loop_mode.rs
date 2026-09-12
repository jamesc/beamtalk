// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! [`LoopMode`]: the generator's loop-body context — nine fields that used
//! to live as separate [`CoreErlangGenerator`](crate::core_erlang::generator::CoreErlangGenerator)
//! fields, now grouped into one `control_flow`-owned value so the state
//! `control_flow` reads and writes while compiling a loop body is scoped to
//! a single field rather than scattered across the generator's own
//! namespace.

/// Generator state that only has meaning while compiling a loop body:
/// hybrid/direct-params mode flags, the `ClassVars`-threading side channels
/// a `Letrec`/`Foldl` loop body hands its caller, and the pre-extracted
/// actor-field variable maps a hybrid/full-extract loop substitutes for
/// `maps:get` reads.
#[derive(Debug, Default)]
#[expect(
    clippy::struct_excessive_bools,
    reason = "Loop-mode flags are context switches, not configuration"
)]
pub(in crate::core_erlang) struct LoopMode {
    /// Whether we're inside a hybrid-params loop body.
    ///
    /// When `true`, `current_state_var()` and `next_state_var()` use `State*` naming
    /// instead of `StateAcc*`, even when `in_loop_body` is also true.
    /// Set by `generate_counted_stateful_loop_hybrid` and `generate_while_loop_hybrid`.
    pub(in crate::core_erlang) in_hybrid_loop: bool,
    /// When `true`, the generator is inside a direct-params (or hybrid) counted
    /// loop body. List ops that thread captured outer-scope locals should skip the
    /// `append_repack_stateacc_doc` step and return just the result value (not
    /// `{Result, StateAcc}`), since there is no `StateAcc` variable in scope.
    pub(in crate::core_erlang) in_direct_params_loop: bool,
    /// ADR 0111 Addendum 9, Questions 2/3/4: whether the generator
    /// is directly inside a Letrec loop body that threads a `ClassVars`
    /// mutation through its own recursive tail call. When `true`,
    /// `generate_field_assignment_open`'s class-var branch threads the write
    /// via a real, `current_branch_frame()`-tagged `Bind` instead of calling
    /// `reject_class_var_field_assignment`, and the `BodyKind::Letrec`
    /// same-class self-send branch in `lower_letrec_body`
    /// emits the self-send (via its own `emit_class_var_result_unwrap` open
    /// chain) instead of raising `ClassMethodSelfSendInThreadedLoopBody`.
    /// Reset to `false` on every `enter_branch_context` entry (mirroring
    /// `state_version`'s reset-on-entry discipline, not `class_var_version`'s
    /// restore-without-reset one) and restored on exit, so it can never leak
    /// from an enclosing Letrec loop into a nested construct
    /// (conditional, `sort:`'s manually-inlined body, a nested Foldl body, …)
    /// that doesn't understand this loop's specific tuple-shape convention.
    /// `generate_threaded_loop_body` is the only place that sets it `true`,
    /// immediately after entry, from that specific call's own
    /// `ThreadingPlan::threads_class_vars`.
    pub(in crate::core_erlang) loop_threads_class_vars: bool,
    /// the current Letrec loop body's final in-body `ClassVars`
    /// name (`current_class_var()`, captured just before
    /// `with_branch_context`'s guard restores `class_var_version` to its
    /// pre-loop value), stashed by `generate_threaded_loop_body` for
    /// `while_loops.rs`/`counted_loops.rs` to read immediately afterward as
    /// the loop's own recursive-tail-call `ClassVars` argument. `Some` only
    /// when that call's `plan.threads_class_vars` was `true`; consumed
    /// (`Option::take`) by the reader so a stale value can never leak into
    /// an unrelated later loop.
    pub(in crate::core_erlang) last_loop_class_var: Option<String>,
    /// When a list op in direct-params mode generates an open let-chain
    /// (omitting the trailing result expression), it stores the result variable name
    /// here so the caller can append `let AssignedVar = <result_var> in` separately.
    /// `None` when no list op result is pending.
    pub(in crate::core_erlang) direct_params_list_op_result: Option<String>,
    /// side channel from `lower_foldl_body`'s
    /// `ClassVars`-threading wrap to `ThreadingPlan::foldl_call_doc` — the
    /// peak `class_var_version` reached *inside* a `Foldl*` body's own
    /// `with_branch_context` scope (captured just before that scope's guard
    /// restores the live counter to its pre-loop value on drop, per
    /// `BranchContextGuard`'s `class_var_version` restore-without-reset
    /// discipline, shared with conditionals/`on:do:`/`ensure:`).
    ///
    /// Needed because Core Erlang requires globally unique variable names
    /// across nested `fun` scopes within one compiled function (confirmed
    /// empirically — `erlc` rejects a reused name with "unbound variable",
    /// not a "shadowing" diagnostic): the fold body's own internal
    /// `emit_class_var_result_unwrap` self-send rebind mints
    /// `ClassVars1`..`ClassVars{peak}` starting from the SAME pre-loop
    /// version the (restored) live counter sits at again once
    /// `generate_threaded_loop_body` returns — so a naive `next_class_var()`
    /// call right after would mint an already-used name.
    /// `ThreadingPlan::foldl_call_doc` consumes (takes) this field to
    /// fast-forward the live counter past the peak before minting the
    /// post-fold rebind, guaranteeing a fresh name. `None` when the fold
    /// body did not thread `ClassVars` (`plan.threads_class_vars == false`)
    /// or hasn't run yet.
    pub(in crate::core_erlang) last_foldl_class_var_peak: Option<usize>,
    /// Map of actor field name → Core Erlang variable name for fields
    /// that have been pre-extracted before a hybrid/full-extract letrec loop.
    ///
    /// When non-empty, `generate_field_access` substitutes the variable name directly
    /// instead of emitting `call 'maps':'get'('field', State)`, eliminating per-iteration
    /// map reads for fields during the loop body.
    /// Contains both read-only fields and mutated fields.
    /// Cleared after the loop body is generated.
    pub(in crate::core_erlang) hybrid_readonly_field_params:
        std::collections::HashMap<String, String>,
    /// Set of actor field names that are mutated inside the current
    /// full-extract loop body. When a field write targets one of these fields,
    /// `generate_field_assignment_open` emits a simple variable rebinding instead
    /// of `maps:put` on State, and updates `hybrid_readonly_field_params` with the
    /// new variable name so subsequent reads see the updated value.
    /// Empty when not in full-extract mode.
    pub(in crate::core_erlang) hybrid_mutated_fields: std::collections::HashSet<String>,
    /// ADR 0118 phase 5b: narrow side-channel — set deep inside
    /// `generate_expression` (`generate_list_do_with_mutations`/
    /// `generate_dict_do_with_mutations`) exactly when a mutation-threaded
    /// `do:`/dict-`do:` nested in a direct-params loop (`in_direct_params_loop`)
    /// leaves its returned `Document` as an open, dangling let-chain that
    /// answers `do:`'s own `nil` contract rather than a single value. Read
    /// by `threaded_expression`'s generic fallback, which converts it into
    /// the `ThreadedValue` shape every other producer already returns
    /// (`value: ValueRef::Literal("'nil'")`), and by the annotation guard in
    /// `generate_expression` to skip line annotations on the open chain.
    /// Reset to `false` by `threaded_expression` before each compile it
    /// wraps this way; never read or written anywhere else.
    pub(in crate::core_erlang) direct_params_do_open_chain: bool,
}

impl LoopMode {
    /// Creates a fresh `LoopMode` with every field at its inactive default
    /// (`false`/`None`/empty) — the state a generator starts in and returns
    /// to between loop bodies.
    pub(in crate::core_erlang) fn new() -> Self {
        Self::default()
    }
}
