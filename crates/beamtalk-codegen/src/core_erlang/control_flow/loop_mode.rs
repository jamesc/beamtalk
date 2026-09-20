// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! [`LoopMode`]: the generator's loop-body context — eight fields that used
//! to live as separate [`CoreErlangGenerator`](crate::core_erlang::generator::CoreErlangGenerator)
//! fields, now grouped into one `control_flow`-owned value so the state
//! `control_flow` reads and writes while compiling a loop body is scoped to
//! a single field rather than scattered across the generator's own
//! namespace.

use super::super::threaded_ir::VersionPrefix;
use super::analysis::ThreadedFamilies;

/// Generator state that only has meaning while compiling a loop body:
/// hybrid/direct-params mode flags, the storage-family threading side
/// channels a `Letrec`/`Foldl` loop body hands its caller, and the
/// pre-extracted actor-field variable maps a hybrid/full-extract loop
/// substitutes for `maps:get` reads.
#[derive(Debug, Default)]
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
    /// ADR 0111 Addendum 9, Questions 2/3/4 / ADR 0122 Decision 5 (BT-3518):
    /// the storage families the generator is directly inside a Letrec loop
    /// body threading through its own recursive tail call, right now. When
    /// this contains `VersionPrefix::ClassVars`, `generate_field_assignment_open`'s
    /// class-var branch threads the write via a real, `current_branch_frame()`-tagged
    /// `Bind` instead of calling `reject_class_var_field_assignment`, and the
    /// `BodyKind::Letrec` same-class self-send branch in `lower_letrec_body`
    /// emits the self-send (via its own `emit_class_var_result_unwrap` open
    /// chain) instead of raising `ClassMethodSelfSendInThreadedLoopBody`.
    /// Reset to empty on every `enter_branch_context` entry (mirroring
    /// `state_version`'s reset-on-entry discipline, not `class_var_version`'s
    /// restore-without-reset one) and restored on exit, so it can never leak
    /// from an enclosing Letrec loop into a nested construct
    /// (conditional, `sort:`'s manually-inlined body, a nested Foldl body, …)
    /// that doesn't understand this loop's specific tuple-shape convention.
    /// `generate_letrec_body_ir` is the only place that populates it,
    /// immediately after entry, from that specific call's own
    /// `ThreadingPlan::threaded_families`.
    ///
    /// Keyed generically by [`VersionPrefix`] (a [`ThreadedFamilies`], the
    /// same canonical-order list every other migrated construct carries)
    /// rather than a `ClassVars`-only `bool`, even though `SelfVt` never
    /// actually populates it in practice: a value-type method has no
    /// same-class self-send that could reach the gates above, so this stays
    /// empty or `[ClassVars]`, never `[SelfVt]` — kept generic anyway so a
    /// future family needs no new field here, per ADR 0122's "one list, one
    /// helper" goal. Before BT-3518 this lived as two separate side
    /// channels: a plain `bool` (this field's direct ancestor) plus a
    /// `last_loop_class_var: Option<String>` the loop-body caller was
    /// supposed to read back out — that second field had no reader left
    /// after BT-3515 moved the loop's own recursive-tail-call `ClassVars`
    /// argument onto `ThreadingPlan::capture_loop_family_params`/
    /// `family_slots::append_family_slots` (see `counted_loops.rs`'s/
    /// `while_loops.rs`'s `family_params`), so it was deleted rather than
    /// folded in here.
    pub(in crate::core_erlang) threading_families: ThreadedFamilies,
    /// When a list op in direct-params mode generates an open let-chain
    /// (omitting the trailing result expression), it stores the result variable name
    /// here so the caller can append `let AssignedVar = <result_var> in` separately.
    /// `None` when no list op result is pending.
    pub(in crate::core_erlang) direct_params_list_op_result: Option<String>,
    /// side channel from `lower_foldl_body`'s storage-family-threading wrap
    /// to `ThreadingPlan::foldl_call_doc` — the peak version, per family,
    /// reached *inside* a `Foldl*` body's own `with_branch_context` scope
    /// (captured just before that scope's guard restores the live counter
    /// to its pre-loop value on drop, per `BranchContextGuard`'s
    /// `class_var_version` restore-without-reset discipline, shared with
    /// conditionals/`on:do:`/`ensure:`).
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
    /// `ThreadingPlan::foldl_call_doc` consumes (removes) each family's
    /// entry here to fast-forward that family's live counter past its peak
    /// before minting the post-fold rebind, guaranteeing a fresh name.
    /// Empty of a given family's key when that fold body did not thread it
    /// (`plan.threaded_families()` doesn't contain it) or hasn't run yet.
    ///
    /// ADR 0122 Decision 5 (BT-3518): keyed by [`VersionPrefix`] rather than
    /// a `ClassVars`-only `Option<usize>` — even though, as of this writing,
    /// `ClassVars` is the only family a `Foldl*` accumulator can ever carry
    /// (a fold has no matching `SelfVt` slot; see
    /// `ThreadingPlan::class_var_fun_param`'s `unreachable!` arm), so this
    /// map only ever holds at most one entry in practice. Kept as its own
    /// map, distinct from [`Self::threading_families`] above, because the
    /// two side channels have genuinely different lifecycles: this one is a
    /// one-shot value set *inside* a `with_branch_context` scope for
    /// `foldl_call_doc` to read back *after* that same scope's guard has
    /// already restored the live counter — folding it into
    /// `threading_families`'s own reset-on-entry/restore-on-exit discipline
    /// would erase it before its one reader ever runs.
    pub(in crate::core_erlang) foldl_peak_versions: std::collections::HashMap<VersionPrefix, usize>,
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
    /// BT-3562: for a non-hybrid direct-params `whileTrue:`/`whileFalse:`/
    /// counted loop (locals-only threading — no `StateAcc` accumulator
    /// parameter is ever bound in its `letrec` fun), the enclosing state
    /// variable name exactly as [`super::super::generator::CoreErlangGenerator::current_state_var`]
    /// reported it immediately BEFORE entering the loop body (before
    /// `with_branch_context` resets `state_version` to 0 and `in_loop_body`
    /// starts rendering that same accessor as `StateAcc*`). A direct-params
    /// loop's body is guaranteed to contain no field WRITES
    /// (`ThreadingPlan::select_direct_params`'s `!body_analysis.has_state_effects()`
    /// guard), so the state map is never rebuilt inside the loop — this
    /// captured name stays correct on every iteration, and (unlike
    /// `StateAcc`/`StateAcc0`) is still a real, lexically-in-scope Core
    /// Erlang variable, since the loop's `letrec` fun is a genuine closure
    /// over its enclosing function. `generate_field_access`/the bare-field
    /// fallback consult this (via
    /// [`super::super::generator::CoreErlangGenerator::current_field_read_state_var`])
    /// for a bare mid-body field READ instead of blindly deriving `StateAcc`
    /// from `in_loop_body` — the gap that made such a read compile to a
    /// reference to a `StateAcc` variable the loop never actually bound.
    /// `None` outside a direct-params loop, or when a direct-params loop
    /// site hasn't been updated to capture it (the accessor then falls back
    /// to the previous `current_state_var()` behaviour, unchanged).
    pub(in crate::core_erlang) direct_params_outer_state_var: Option<String>,
}

impl LoopMode {
    /// Creates a fresh `LoopMode` with every field at its inactive default
    /// (`false`/`None`/empty) — the state a generator starts in and returns
    /// to between loop bodies.
    pub(in crate::core_erlang) fn new() -> Self {
        Self::default()
    }
}
