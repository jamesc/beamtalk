// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! State/class-var/self version-counter helpers: the accessor methods that
//! read and mint fresh `State{N}`/`ClassVars{N}`/`Self{N}` names, plus the
//! shared prefix-rendering function they (and `threaded_ir::RenderCtx`) both
//! call.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::threaded_ir::{self, VersionCounter, VersionPrefix};
use crate::core_erlang::util;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

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
    /// "`FrameId` allocation is the one missing production mechanism": mints
    /// and returns the [`threaded_ir::FrameId`] for the CURRENT (already
    /// entered) branch context — the frame every real `Bind`/`Threaded` node
    /// a branch-arm lowering constructs must use. Distinct from
    /// [`threaded_ir::FrameId::ROOT`] always: `enter_branch_context` mints
    /// starting at `1`.
    pub(in crate::core_erlang) fn current_branch_frame(&self) -> threaded_ir::FrameId {
        threaded_ir::FrameId::new(self.branch_frame_counter)
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

    /// Returns the current class variable state variable name.
    pub(in crate::core_erlang) fn current_class_var(&self) -> String {
        self.class_context
            .as_ref()
            .map_or(VersionCounter::new(), |ctx| ctx.class_var_version)
            .current_var(VersionPrefix::ClassVars)
    }

    /// records the peak `class_var_version` reached inside a
    /// `Foldl*` body's own `with_branch_context` scope, for
    /// [`Self::take_foldl_class_var_peak`] to consume once that scope's
    /// guard has restored the live counter — see
    /// `last_foldl_class_var_peak`'s own doc comment for the full rationale.
    pub(in crate::core_erlang) fn set_foldl_class_var_peak(&mut self, version: usize) {
        self.loop_mode.last_foldl_class_var_peak = Some(version);
    }

    /// takes (clears) the peak class-var version recorded by
    /// [`Self::set_foldl_class_var_peak`], if any, and — when it exceeds the
    /// live (already-restored) counter — fast-forwards the live counter to
    /// it, so the next [`Self::next_class_var`] mint is guaranteed not to
    /// collide with a name already used inside the fold body's own closure.
    /// A no-op when no peak was recorded (non-`ClassVars`-threading bodies).
    pub(in crate::core_erlang) fn catch_up_class_var_version_to_foldl_peak(&mut self) {
        if let Some(peak) = self.loop_mode.last_foldl_class_var_peak.take() {
            if peak > self.class_var_version() {
                self.set_class_var_version(peak);
            }
        }
    }

    /// refreshes the live `ClassVars` name after generating an
    /// expression whose caller is about to bind the WHOLE returned
    /// `Document` opaquely (`let X = <expr> in ...`, e.g.
    /// `emit_vt_threaded_local_assignment`'s `{Value, StateAcc}`-tuple
    /// binding), catching up to a class-var mutation the ADR 0110 shadow
    /// write recorded but that opaque compile never surfaced as its own
    /// `ThreadedValue` prelude.
    ///
    /// The gap this closes: a class-method self-send inside a `Foldl*` body
    /// (`do:`/`collect:`/`select:`/`inject:into:`) correctly threads its own
    /// `ClassVarsN` rebind through the fold's `{ClassVars, StateAcc}`
    /// accumulator (ADR 0111 Addendum 9, Question 6) — but that name is
    /// minted, and lexically bound, entirely INSIDE the list-op function's
    /// own returned `Document`. A caller that treats the whole thing as an
    /// opaque value (rather than splicing it into its own open let-chain,
    /// the way `push_discarded_stmt` and the value-type/class-method "open"
    /// loop-body family already do) confines that binding to its own `let`'s
    /// RHS — any LATER code in the same method that references
    /// `self.current_class_var()`'s name (a class-var read, or another
    /// self-send) would then reference a name Core Erlang never actually
    /// bound at that point — confirmed empirically as an `erlc` "unbound
    /// variable" compiler crash, not merely a silently-wrong value.
    ///
    /// Rather than widening every list-op function's own external contract
    /// to also expose `ClassVars` as an explicit extra tuple element (a much
    /// larger, cross-cutting change to every consumer of that contract),
    /// this reaches for the ADR 0110 process-dictionary shadow write that
    /// already exists for the analogous foreign-NLR-relay problem: a
    /// same-class, locally-defined self-send call (`class_bump`-style,
    /// compiled as a direct module call, not a dispatch) unconditionally
    /// writes its own mutation to `{'$bt_class_vars_shadow', element(2,
    /// ClassSelf)}` before returning, and that key is erased only by the
    /// OUTER-MOST dispatch wrapper (`invoke_class_method/7`'s `after`) —
    /// never by the direct call itself — so it is still live, and correct,
    /// for the remainder of the SAME class method's own body. Reading it
    /// back here is a safe, minimal escape hatch scoped to exactly the
    /// opaque-wrap gap above, not a substitute for the accumulator threading
    /// itself (which is still what makes the fold's OWN cross-iteration
    /// threading correct in the first place).
    ///
    /// Returns `Some(prelude_doc)` — a `"let <fresh ClassVarsN> = <shadow
    /// read, falling back to the pre-scope value> in "` binding the caller
    /// should push immediately after its own opaque-value `let` — when
    /// `self.class_var_version()` advanced across generating the just-built
    /// expression (`version_before` is the version read immediately before
    /// generating it); `None` when nothing changed (including, by
    /// construction, every non-class-method context, where no code path
    /// ever advances `class_var_version` at all).
    ///
    /// **Guarded against a false-positive shadow read** (found during
    /// review): `class_var_version` advances on EVERY class-method
    /// self-send (`emit_class_var_result_unwrap` calls `next_class_var()`
    /// unconditionally, whether or not the callee performs a real field
    /// write), but the shadow key is written only by an actual
    /// `self.field := value` (`shadow_write: true`). A pure self-send (e.g.
    /// a `select:`/`collect:` predicate/transform with no field write) would
    /// otherwise read back the atom `'undefined'` and corrupt this class
    /// method's own class-var state. The `case` below treats `'undefined'`
    /// as "nothing new was shadow-written" and falls back to the value that
    /// was already live before this scope, rather than trusting the read.
    /// This also subsumes the narrower, previously-documented INHERITED
    /// self-send gap (`self someInheritedMethod` routing through
    /// `class_self_dispatch`, which may erase the shadow key the same way
    /// `invoke_class_method/7`'s own `after` does) — that path now falls
    /// back safely too, for the same reason.
    pub(in crate::core_erlang) fn refresh_class_var_after_opaque_scope(
        &mut self,
        version_before: usize,
    ) -> Option<Document<'static>> {
        if self.class_var_version() == version_before {
            return None;
        }
        let mut before_counter = VersionCounter::new();
        before_counter.set_version(version_before);
        let cv_before = before_counter.current_var(VersionPrefix::ClassVars);
        let shadow_raw = self.fresh_temp_var("ClassVarsShadow");
        let cv_new = self.next_class_var();
        Some(docvec![
            "let ",
            leaf::var(shadow_raw.clone()),
            " = call 'erlang':'get'({",
            leaf::atom("$bt_class_vars_shadow"),
            ", call 'erlang':'element'(2, ",
            leaf::var("ClassSelf"),
            ")}) in let ",
            leaf::var(cv_new),
            " = case ",
            leaf::var(shadow_raw),
            " of <'undefined'> when 'true' -> ",
            leaf::var(cv_before),
            " <_ShadowVal> when 'true' -> _ShadowVal end in ",
        ])
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

    /// Increments class var version and returns the new variable name.
    pub(in crate::core_erlang) fn next_class_var(&mut self) -> String {
        let name = self
            .class_context_mut()
            .class_var_version
            .next_var(VersionPrefix::ClassVars);
        self.set_class_var_mutated(true);
        name
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
