// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! [`BranchContextGuard`]: RAII guard for [`CoreErlangGenerator::with_branch_context`]'s
//! per-prefix save/reset/restore discipline (ADR 0111 Phase A2).
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::generator::CoreErlangGenerator;

/// RAII guard for [`CoreErlangGenerator::with_branch_context`]'s
/// per-prefix save/reset/restore discipline (ADR 0111 §Phase A2). Replaces the
/// previous manual save-before/restore-after sequencing with a `Drop` impl
/// that restores unconditionally when the guard goes out of scope — including
/// through an early return via `?` inside the branch closure, which the old
/// manual-restore-after-the-call sequencing could not cover.
///
/// Per-prefix branch discipline:
/// - **state**: reset to 0 on entry, restored on exit.
/// - **self**: saved and restored on exit, but **NOT reset to 0 on entry**,
///   unlike `state`. `state`'s reset is
///   safe because `state` inside a loop body renders as `StateAcc{N}` (a
///   context-dependent rename, `in_loop_body`), so a reset only affects that
///   local rendering convention. `Self{N}` has no such rename — `self.field`
///   reads compile directly to `maps:get(field, Self{N})` — and `Self`
///   (version 0, the bare method parameter) is always a syntactically valid
///   Core Erlang variable, so resetting to 0 does not fail to compile: it
///   silently reads the pre-mutation value. `generate_threaded_loop_body`
///   calls `with_branch_context` unconditionally and is shared by
///   `ValueType` contexts (confirmed empirically: a `self.field := ...`
///   assignment followed by a `do:`/conditional body in the same method
///   that reads `self.field` produced `maps:get(field, Self)` instead of
///   `maps:get(field, Self1)` under a reset-on-entry policy). A call site
///   can enter `with_branch_context` with a nonzero `self_version`, so
///   `self_version` is saved and restored unconditionally rather than left
///   untouched.
pub(in crate::core_erlang) struct BranchContextGuard<'a> {
    generator: &'a mut CoreErlangGenerator,
    saved_in_loop: bool,
    saved_state_version: usize,
    saved_self_version: usize,
    saved_active_branch_frame: u32,
}

impl Drop for BranchContextGuard<'_> {
    fn drop(&mut self) {
        self.generator.in_loop_body = self.saved_in_loop;
        self.generator.set_state_version(self.saved_state_version);
        self.generator.set_self_version(self.saved_self_version);
        self.generator.active_branch_frame = self.saved_active_branch_frame;
    }
}

impl CoreErlangGenerator {
    /// Enters a branch context, applying the per-prefix reset policy
    /// documented on [`BranchContextGuard`] and returning a guard that
    /// restores everything (per that same policy) when dropped.
    pub(in crate::core_erlang) fn enter_branch_context(&mut self) -> BranchContextGuard<'_> {
        let saved_state_version = self.state_version();
        let saved_in_loop = self.in_loop_body;
        let saved_self_version = self.self_version();
        let saved_active_branch_frame = self.active_branch_frame;
        self.set_state_version(0);
        self.in_loop_body = true;
        // mint a fresh frame identity for this branch context —
        // see `current_branch_frame`'s doc comment. `branch_frame_counter`
        // itself is never reset/restored (frame identity must stay globally
        // unique across the whole module compile), but `active_branch_frame`
        // — the frame `current_branch_frame()` reports as "the one I'm
        // logically inside right now" — IS saved/restored below (BT-3623):
        // otherwise a sibling branch that opens and closes here would leave
        // a later sibling reading this now-closed branch's stale frame.
        self.branch_frame_counter += 1;
        self.active_branch_frame = self.branch_frame_counter;
        // do NOT reset self_version to 0 here — unlike
        // `state`, a `Self{N}` reference is always a syntactically valid
        // Core Erlang variable (the bare `Self` parameter always exists), so
        // resetting doesn't fail to compile, it silently reads the
        // pre-mutation value. `generate_threaded_loop_body` calls this
        // unconditionally and is shared with ValueType contexts (confirmed:
        // resetting produces `maps:get(field, Self)` instead of
        // `maps:get(field, Self1)` for a `self.field := ...` read inside a
        // `do:`/conditional body that follows an earlier `self.field := ...`
        // in the same method — see `BranchContextGuard`'s doc comment).
        // `self` gets a restore-only discipline instead.
        BranchContextGuard {
            generator: self,
            saved_in_loop,
            saved_state_version,
            saved_self_version,
            saved_active_branch_frame,
        }
    }

    /// Lowers the body of a closed Core Erlang `fun` that threads nothing out
    /// (a pure Tier 1 block, an inlined `inject:into:` fold fun, ...): a
    /// variable scope plus a `state_version` that is restored on exit.
    ///
    /// A `fun` is a separate Core Erlang scope: any `State{N}`/`StateAcc{N}`
    /// its body binds (a conditional with a block-local write threads one
    /// even though nothing consumes it outside) is unreachable once the
    /// `fun` returns. Without the restore the bumped counter leaks into the
    /// enclosing scope, whose next statement then reads (or rebinds from) a
    /// version that was never bound there -- the `UnboundVersion` /
    /// `NonLinearVersion` pair the `ThreadedIr` verifier reported on BT-3737's
    /// generated class-method programs. This is the one place a `fun` whose
    /// body is lowered against the enclosing scope gets that discipline
    /// (BT-3737), so a new one cannot forget half of it; `f` runs with the
    /// guard's invariants even on `Err`. A `fun` whose body starts from its
    /// own `StateAcc` (every fold lambda, including the `sort:` comparator,
    /// and every loop `letrec`/condition fun) gets it from
    /// [`Self::with_branch_context`] instead, whose guard also restores
    /// `state_version` (BT-3771 audited every closed-`fun` lowering; see
    /// `tests/closed_fun_state_version.rs`).
    pub(in crate::core_erlang) fn with_closed_fun_scope<T>(
        &mut self,
        f: impl FnOnce(&mut CoreErlangGenerator) -> T,
    ) -> T {
        self.push_scope();
        let saved_state_version = self.state_version();
        let out = f(self);
        self.set_state_version(saved_state_version);
        self.pop_scope();
        out
    }

    /// Executes `f` inside a branch context where `in_loop_body` is
    /// `true` and `state_version` is reset to 0.  The previous values are
    /// unconditionally restored — via [`BranchContextGuard`]'s `Drop` impl —
    /// once this function returns, including through an early return via
    /// `?` inside `f`.
    ///
    /// Also saves/restores `self_version` (restore-only, no reset) — see
    /// [`BranchContextGuard`]'s doc comment for why a `state`-style reset is
    /// unsafe for `self`.
    pub(in crate::core_erlang) fn with_branch_context<T>(
        &mut self,
        f: impl FnOnce(&mut CoreErlangGenerator) -> T,
    ) -> T {
        let guard = self.enter_branch_context();
        f(guard.generator)
    }
}
