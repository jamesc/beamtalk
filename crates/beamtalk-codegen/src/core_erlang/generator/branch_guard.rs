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
/// - **`class_vars`**: NOT reset on entry (the branch inherits the outer
///   scope's current version) but restored on exit. `class_var_mutated` is
///   intentionally NOT restored: it is a method-level flag that
///   must stay sticky once set.
/// - **self**: saved and restored on exit, but **NOT reset to 0 on entry**:
///   the same discipline as `class_vars`, not `state`. `state`'s reset is
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
///   `self_version` is saved and restored unconditionally, on the same
///   `class_vars` discipline, rather than left untouched.
pub(in crate::core_erlang) struct BranchContextGuard<'a> {
    generator: &'a mut CoreErlangGenerator,
    saved_in_loop: bool,
    saved_state_version: usize,
    saved_class_var_version: usize,
    saved_self_version: usize,
    saved_loop_threads_class_vars: bool,
}

impl Drop for BranchContextGuard<'_> {
    fn drop(&mut self) {
        self.generator.in_loop_body = self.saved_in_loop;
        self.generator.set_state_version(self.saved_state_version);
        self.generator
            .set_class_var_version(self.saved_class_var_version);
        self.generator.set_self_version(self.saved_self_version);
        self.generator.loop_mode.loop_threads_class_vars = self.saved_loop_threads_class_vars;
        // class_var_mutated intentionally NOT restored — sticky.
    }
}

impl CoreErlangGenerator {
    /// Enters a branch context, applying the per-prefix reset policy
    /// documented on [`BranchContextGuard`] and returning a guard that
    /// restores everything (per that same policy) when dropped.
    pub(in crate::core_erlang) fn enter_branch_context(&mut self) -> BranchContextGuard<'_> {
        let saved_state_version = self.state_version();
        let saved_in_loop = self.in_loop_body;
        let saved_class_var_version = self.class_var_version();
        let saved_self_version = self.self_version();
        let saved_loop_threads_class_vars = self.loop_mode.loop_threads_class_vars;
        self.set_state_version(0);
        self.in_loop_body = true;
        // reset-on-entry, like `state_version` — see
        // `loop_threads_class_vars`'s own doc comment for why this must
        // never inherit an enclosing Letrec loop's `true` by default.
        self.loop_mode.loop_threads_class_vars = false;
        // mint a fresh frame identity for this branch context —
        // see `current_branch_frame`'s doc comment. Never reset/restored
        // (unlike the version counters above): frame identity must stay
        // globally unique across the whole module compile.
        self.branch_frame_counter += 1;
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
        // `self` gets `class_vars`' restore-only discipline instead.
        BranchContextGuard {
            generator: self,
            saved_in_loop,
            saved_state_version,
            saved_class_var_version,
            saved_self_version,
            saved_loop_threads_class_vars,
        }
    }

    /// Executes `f` inside a branch context where `in_loop_body` is
    /// `true` and `state_version` is reset to 0.  The previous values are
    /// unconditionally restored — via [`BranchContextGuard`]'s `Drop` impl —
    /// once this function returns, including through an early return via
    /// `?` inside `f`.
    ///
    /// Also saves/restores `class_var_version` (without resetting
    /// it — the branch inherits the outer scope's current version) so that
    /// self-calls inside a conditional branch don't leak `ClassVars{N}`
    /// bindings into the outer scope.  `class_var_mutated` is intentionally
    /// NOT restored — it is a method-level flag that must stay sticky once
    /// set.
    ///
    /// Also saves/restores `self_version`, with the same
    /// restore-only-no-reset discipline as `class_var_version` — see
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
