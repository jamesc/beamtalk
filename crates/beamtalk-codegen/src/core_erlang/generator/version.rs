// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! State/self version-counter helpers: the accessor methods that
//! read and mint fresh `State{N}`/`Self{N}` names, plus the
//! shared prefix-rendering function they (and `threaded_ir::RenderCtx`) both
//! call.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::threaded_ir::{self, VersionCounter, VersionPrefix};
use crate::core_erlang::util;

/// Renders a `VersionPrefix::State` counter value, honoring loop context —
/// the single shared implementation behind `current_state_var`/
/// `next_state_var`/`peek_next_state_var` (this impl block) and
/// `threaded_ir::RenderCtx::resolve_prefix` (ADR 0111 §Addendum, "Renderer
/// design sketch": "prefix rendering is a function of (counter, loop
/// context), decided at Document-construction time, not stored in the
/// IR" — CLAUDE.md's no-duplicate-implementations rule pins that decision
/// to exactly one place instead of leaving the live-generator and
/// `ThreadedIr`-renderer paths to duplicate/drift it independently).
///
/// Hybrid-params loops (`in_hybrid_loop = true`) use `State`/`StateN` —
/// same as normal (non-loop) context — because `State` is an explicit fun
/// parameter there, not a `StateAcc` map. Normal loop bodies
/// (`in_loop_body = true`, `in_hybrid_loop = false`) use `StateAcc`/
/// `StateAccN`.
pub(in crate::core_erlang) fn render_state_prefix(
    in_hybrid_loop: bool,
    in_loop_body: bool,
    version: usize,
) -> String {
    let prefix = if in_hybrid_loop || !in_loop_body {
        "State"
    } else {
        "StateAcc"
    };
    util::versioned_var(prefix, version)
}

impl CoreErlangGenerator {
    /// Returns the current state variable name for state threading.
    ///
    /// When inside a hybrid-params loop (`in_hybrid_loop = true`), returns `State` or `StateN`
    /// (same as normal context) so that field mutations thread through the explicit `State`
    /// parameter instead of a `StateAcc` map.
    ///
    /// When inside a normal loop body (`in_loop_body = true`), returns `StateAcc` or `StateAccN`.
    /// Otherwise returns `State` or `StateN`.
    // widened from `pub(crate)` — `beamtalk-repl` reads the current
    // state variable name while threading REPL bindings.
    pub fn current_state_var(&self) -> String {
        render_state_prefix(
            self.loop_mode.in_hybrid_loop,
            self.in_loop_body,
            self.state_threading.version(),
        )
    }

    /// Increments the state version and returns the new state variable name.
    ///
    /// When inside a hybrid-params loop (`in_hybrid_loop = true`) or normal context,
    /// returns `State1`, `State2`, etc.
    /// When inside a normal loop body (`in_loop_body = true`), returns `StateAcc1`, etc.
    // widened from `pub(crate)` — `beamtalk-repl` advances the
    // state version while threading REPL bindings.
    pub fn next_state_var(&mut self) -> String {
        self.state_threading.next_var(VersionPrefix::State);
        render_state_prefix(
            self.loop_mode.in_hybrid_loop,
            self.in_loop_body,
            self.state_threading.version(),
        )
    }

    /// The state/map variable name for a **field read** (`self.field`, or
    /// the bare-identifier implicit-field-read fallback) in
    /// `CodeGenContext::Actor` — the one place both call sites
    /// (`generate_field_access` and the `Identifier` fallback in
    /// `expressions.rs`) resolve it, so they can't independently drift
    /// (CLAUDE.md: no duplicate "mirrors" implementations).
    ///
    /// Identical to [`Self::current_state_var`] except inside a non-hybrid
    /// direct-params loop with a captured
    /// [`crate::core_erlang::control_flow::loop_mode::LoopMode::direct_params_outer_state_var`]
    /// (BT-3562) — such a loop's `letrec` fun binds no `StateAcc`
    /// accumulator parameter at all, so `current_state_var`'s blanket
    /// `in_loop_body` → `StateAcc` rule names a variable that was never
    /// bound. See that field's own doc comment for why the captured
    /// pre-loop name is safe to reuse for every iteration.
    pub(in crate::core_erlang) fn current_field_read_state_var(&self) -> String {
        if !self.loop_mode.in_hybrid_loop {
            if let Some(captured) = &self.loop_mode.direct_params_outer_state_var {
                return captured.clone();
            }
        }
        self.current_state_var()
    }

    /// Resets the state version to 0.
    pub(in crate::core_erlang) fn reset_state_version(&mut self) {
        self.state_threading.reset();
    }

    /// Gets the current state version.
    pub(in crate::core_erlang) fn state_version(&self) -> usize {
        self.state_threading.version()
    }

    /// Returns the name of the next state variable without advancing the
    /// version counter.  Context-aware: uses `StateAcc*` in loop bodies.
    pub(in crate::core_erlang) fn peek_next_state_var(&self) -> String {
        render_state_prefix(
            self.loop_mode.in_hybrid_loop,
            self.in_loop_body,
            self.state_threading.version() + 1,
        )
    }

    /// Sets the state version.
    pub(in crate::core_erlang) fn set_state_version(&mut self, version: usize) {
        self.state_threading.set_version(version);
    }

    /// ADR 0111 Addendum 5, §Branch-context version discipline,
    /// "`FrameId` allocation is the one missing production mechanism":
    /// returns the [`threaded_ir::FrameId`] for the CURRENT (already
    /// entered) branch context — the frame every real `Bind`/`Threaded` node
    /// a branch-arm lowering constructs must use. Distinct from
    /// [`threaded_ir::FrameId::ROOT`] always: `enter_branch_context` mints
    /// starting at `1`.
    ///
    /// Reads `active_branch_frame` — saved/restored per branch context by
    /// `enter_branch_context`/`BranchContextGuard` — rather than
    /// `branch_frame_counter` directly. `branch_frame_counter` only ever
    /// grows and is never restored (frame identities must stay globally
    /// unique across a whole module compile), so reading it back as "the
    /// frame I'm logically inside right now" is wrong as soon as a sibling
    /// branch context has opened and closed since this one was entered — a
    /// second `ifTrue:` in the same loop body reading the first, already-
    /// closed `ifTrue:`'s frame instead of the loop's own (BT-3623).
    pub(in crate::core_erlang) fn current_branch_frame(&self) -> threaded_ir::FrameId {
        threaded_ir::FrameId::new(self.active_branch_frame)
    }

    /// ADR 0118 phase 2a: the [`threaded_ir::FrameId`] a
    /// `threaded_expression`/`thread_ahead` caller should splice a prelude's
    /// `Bind`s into RIGHT NOW — [`Self::current_branch_frame`] while inside
    /// any `with_branch_context` arm (a conditional branch, an
    /// `on:do:`/`ensure:` body, a Tier 2 stateful-block body, or — since
    /// ADR 0118 phase 2b — a real loop body itself, all of which
    /// set `in_loop_body`), [`FrameId::ROOT`](threaded_ir::FrameId::ROOT)
    /// at the flat method body.
    pub(in crate::core_erlang) fn current_frame(&self) -> threaded_ir::FrameId {
        if self.in_loop_body {
            self.current_branch_frame()
        } else {
            threaded_ir::FrameId::ROOT
        }
    }

    /// Returns the current Self variable name for value type Self-threading.
    ///
    /// Version 0 → `"Self"` (the original method parameter).
    /// Version N → `"Self{N}"` (after N field assignments have threaded a new snapshot).
    pub(in crate::core_erlang) fn current_self_var(&self) -> String {
        self.value_type_context
            .as_ref()
            .map_or(VersionCounter::new(), |ctx| ctx.self_version)
            .current_var(VersionPrefix::SelfVt)
    }

    /// Increments the Self version and returns the new variable name.
    pub(in crate::core_erlang) fn next_self_var(&mut self) -> String {
        self.value_type_context_mut()
            .self_version
            .next_var(VersionPrefix::SelfVt)
    }

    /// Resets the Self version to 0 (call at the start of each value type method).
    pub(in crate::core_erlang) fn reset_self_version(&mut self) {
        self.set_self_version(0);
    }

    /// Returns the value type self-version counter.
    pub(in crate::core_erlang) fn self_version(&self) -> usize {
        self.value_type_context
            .as_ref()
            .map_or(0, |ctx| ctx.self_version.version())
    }

    /// Sets the value type self-version counter.
    pub(in crate::core_erlang) fn set_self_version(&mut self, version: usize) {
        self.value_type_context_mut()
            .self_version
            .set_version(version);
    }
}
