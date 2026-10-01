// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! State/class-var/self version-counter helpers: the accessor methods that
//! read and mint fresh `State{N}`/`ClassVars{N}`/`Self{N}` names, plus the
//! shared prefix-rendering function they (and `threaded_ir::RenderCtx`) both
//! call.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::control_flow::analysis::ThreadedFamilies;
use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::threaded_ir::{self, VersionCounter, VersionPrefix};
use crate::core_erlang::util;
use beamtalk_cerl_doc::Document;
use beamtalk_cerl_doc::docvec;
use beamtalk_cerl_doc::leaf;

/// One open per-scope class-variable token (BT-3675).
///
/// A scope that cannot thread a `ClassVars` rebind out (a bare block closure, a
/// loop body, a conditional or `match:` arm, an `on:do:`/`ensure:` body) is
/// opened by [`CoreErlangGenerator::class_var_scope_mark`] (or one of the
/// region openers), which pushes one of these. Every `ClassVars` version
/// minted while it is the innermost token (a class-side send, a nested scope's
/// refresh, a construct's family-slot rebind) is committed under `name` — a
/// runtime value (`make_ref()`) bound once per entry of the scope, so a scope
/// that raised leaves an entry nothing will ever read — and the scope's
/// refresh ([`CoreErlangGenerator::refresh_class_var_after_opaque_scope`])
/// consumes only that entry.
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct ClassVarScopeToken {
    /// The Core Erlang variable the token is bound to.
    pub(in crate::core_erlang) name: String,
    /// Whether any send (or nested scope refresh) committed under this token,
    /// i.e. whether the scope needs its `make_ref()` binding and a refresh.
    pub(in crate::core_erlang) used: bool,
    /// Whether a closure body exported into this scope (see
    /// `ClassContext::deferred_scope_tokens`).
    pub(in crate::core_erlang) exported_into: bool,
    /// Length of the deferred-token list when the scope opened.
    deferred_base: usize,
}

/// Handle for [`CoreErlangGenerator::class_var_scope_mark`]: the live
/// `ClassVars{N}` version before the scope, and the depth of the token stack
/// below the token the scope pushed (`None` when the scope opened no token:
/// not in a class method).
#[derive(Debug, Clone, Copy)]
pub(in crate::core_erlang) struct ClassVarScopeMark {
    pub(in crate::core_erlang) version: usize,
    depth: Option<usize>,
}

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

    /// Returns the current class variable state variable name.
    pub(in crate::core_erlang) fn current_class_var(&self) -> String {
        self.class_context
            .as_ref()
            .map_or(VersionCounter::new(), |ctx| ctx.class_var_version)
            .current_var(VersionPrefix::ClassVars)
    }

    /// records the peak version `prefix`'s live counter reached inside a
    /// `Foldl*` body's own `with_branch_context` scope, for
    /// [`Self::catch_up_class_var_version_to_foldl_peak`] to consume once
    /// that scope's guard has restored the live counter — see
    /// `LoopMode::foldl_peak_versions`'s own doc comment for the full
    /// rationale.
    pub(in crate::core_erlang) fn set_foldl_class_var_peak(
        &mut self,
        prefix: VersionPrefix,
        version: usize,
    ) {
        self.loop_mode.foldl_peak_versions.insert(prefix, version);
    }

    /// takes (removes) the peak version recorded by
    /// [`Self::set_foldl_class_var_peak`] for each family in `families`, if
    /// any, and — when it exceeds that family's live (already-restored)
    /// counter — fast-forwards the live counter to it, so the next mint
    /// (e.g. [`Self::next_class_var`]) is guaranteed not to collide with a
    /// name already used inside the fold body's own closure. A no-op for a
    /// family with no recorded peak (a non-threading body, or a family this
    /// generation never populates in practice).
    ///
    /// ADR 0122 Decision 5 (BT-3518): iterates `families` — `ThreadingPlan::foldl_call_doc`
    /// passes its own `threaded_families()` — rather than hardcoding a
    /// single `ClassVars` take, even though `ClassVars` is the only family a
    /// `Foldl*` accumulator can ever carry.
    pub(in crate::core_erlang) fn catch_up_class_var_version_to_foldl_peak(
        &mut self,
        families: &ThreadedFamilies,
    ) {
        for prefix in families.as_slice() {
            let Some(peak) = self.loop_mode.foldl_peak_versions.remove(prefix) else {
                continue;
            };
            match prefix {
                VersionPrefix::ClassVars => {
                    if peak > self.class_var_version() {
                        self.set_class_var_version(peak);
                    }
                }
                other => unreachable!(
                    "a Foldl* body's own peak-tracking only ever carries ClassVars \
                     (never SelfVt — a fold accumulator has no matching slot), \
                     got {other:?}"
                ),
            }
        }
    }

    /// Opens a per-scope class-variable token (BT-3675) for the expression
    /// the caller is about to generate, and snapshots the live `ClassVars`
    /// version. Pair with [`Self::class_var_scope_prefix`] and
    /// [`Self::refresh_class_var_after_opaque_scope`] after generating it.
    ///
    /// The expression is compiled at a site that cannot carry a `ClassVars`
    /// rebind out lexically: a bare block closure, a loop body or conditional
    /// arm whose rebind is rolled back, an `on:do:`/`ensure:` body, or a send
    /// whose prelude is closed into an opaque value. The compile-time purity
    /// gates judge a late-bound class-side send by the base class's own view
    /// of the selector, so they cannot know that a subclass override writes a
    /// class variable. Every class-side send generated while the token is
    /// open therefore commits its returned `ClassVars` under it
    /// ([`Self::class_var_for_send`]), and the refresh binds a fresh
    /// `ClassVars` version from the committed value.
    ///
    /// A commit is made only after the callee returned normally, so a write
    /// by a callee that raised — and a write by a scope that raised and was
    /// caught — is never read back: the scope's own refresh never ran, and no
    /// other scope holds its token (ADR 0110: "a genuine runtime error after
    /// a class-var mutation must still revert the mutation").
    ///
    /// Opens nothing outside a class method (the value-type and actor
    /// boundaries never advance `ClassVars`).
    pub(in crate::core_erlang) fn class_var_scope_mark(&mut self) -> ClassVarScopeMark {
        let version = self.class_var_version();
        ClassVarScopeMark {
            version,
            depth: self.push_class_var_scope(),
        }
    }

    /// Pushes a token; `None` outside a class method.
    fn push_class_var_scope(&mut self) -> Option<usize> {
        if !self.in_class_method() || self.class_context.is_none() {
            return None;
        }
        let ctx = self.class_context_mut();
        ctx.class_var_scope_counter += 1;
        let name = format!("_CVTok{}", ctx.class_var_scope_counter);
        let deferred_base = ctx.deferred_scope_tokens.len();
        let tokens = &mut ctx.class_var_scope_tokens;
        let depth = tokens.len();
        tokens.push(ClassVarScopeToken {
            name,
            used: false,
            exported_into: false,
            deferred_base,
        });
        Some(depth)
    }

    /// Pops the token at `depth` (dropping any inner scope an early return left
    /// open above it) and forgets the deferred tokens opened inside it: their
    /// bindings are not in scope beyond it.
    fn pop_class_var_scope_token(&mut self, depth: usize) -> Option<ClassVarScopeToken> {
        let ctx = self.class_context.as_mut()?;
        if ctx.class_var_scope_tokens.len() <= depth {
            return None;
        }
        ctx.class_var_scope_tokens.truncate(depth + 1);
        let token = ctx.class_var_scope_tokens.pop()?;
        ctx.deferred_scope_tokens.truncate(token.deferred_base);
        Some(token)
    }

    /// Hides the enclosing method's open scopes while a nested method body is
    /// generated (a `ClassBuilder` class-method fun); restore with
    /// [`Self::restore_class_var_scopes`].
    pub(in crate::core_erlang) fn take_class_var_scopes(
        &mut self,
    ) -> (Vec<ClassVarScopeToken>, Vec<String>) {
        self.class_context.as_mut().map_or_else(
            || (Vec::new(), Vec::new()),
            |ctx| {
                (
                    std::mem::take(&mut ctx.class_var_scope_tokens),
                    std::mem::take(&mut ctx.deferred_scope_tokens),
                )
            },
        )
    }

    pub(in crate::core_erlang) fn restore_class_var_scopes(
        &mut self,
        saved: (Vec<ClassVarScopeToken>, Vec<String>),
    ) {
        if let Some(ctx) = self.class_context.as_mut() {
            ctx.class_var_scope_tokens = saved.0;
            ctx.deferred_scope_tokens = saved.1;
        }
    }

    /// Opens a closure-body region (BT-3675): everything minted inside the
    /// closure commits under the region's own token, and the closure hands it
    /// to the enclosing scope as its last step
    /// ([`Self::wrap_closure_region`]) — so a closure that raises after a send
    /// completed (caught by a runtime catcher such as `Result tryDo:`) exports
    /// nothing. `None` when there is no enclosing scope to export to.
    pub(in crate::core_erlang) fn open_closure_region(&mut self) -> Option<usize> {
        if self
            .class_context
            .as_ref()
            .is_none_or(|ctx| ctx.class_var_scope_tokens.is_empty())
        {
            return None;
        }
        self.push_class_var_scope()
    }

    /// Closes the region [`Self::open_closure_region`] opened and wraps the
    /// closure's already-generated `body` (a closed expression): binds the
    /// region's token on entry and exports its commit to the enclosing scope
    /// after the body returned. Unchanged when nothing committed in the
    /// region, so a closure without class-side sends costs nothing.
    pub(in crate::core_erlang) fn wrap_closure_region(
        &mut self,
        region: Option<usize>,
        body: Document<'static>,
    ) -> Document<'static> {
        let Some(depth) = region else {
            return body;
        };
        let Some(token) = self.pop_class_var_scope_token(depth) else {
            return body;
        };
        if !token.used {
            return body;
        }
        // The enclosing scope receives the export, so it must exist at run
        // time; a later statement may invoke the closure after that scope was
        // refreshed, so the scope is remembered as exported-into.
        let Some(parent) = self.mark_innermost_scope_exported_into() else {
            return body;
        };
        let result = self.fresh_temp_var("ClosureRes");
        docvec![
            "let ",
            leaf::var(token.name.clone()),
            " = call 'erlang':'make_ref'() in let ",
            leaf::var(result.clone()),
            " = ",
            body,
            " in ",
            Self::class_var_scope_export_doc(&token.name, &parent),
            leaf::var(result),
        ]
    }

    /// Opens an arm-body region (BT-3675) around an `on:do:`/`ensure:` try or
    /// handler body: its statements are [`Self::class_var_scope_mark`] scopes
    /// nested in it, and the arm's lexical `ClassVars` flows out through the
    /// construct's result slot only when the arm completes.
    pub(in crate::core_erlang) fn open_arm_region(&mut self) -> Option<usize> {
        self.push_class_var_scope()
    }

    /// Closes the region [`Self::open_arm_region`] opened. Returns `None` when
    /// nothing committed, else the `let <token> = make_ref() in ` binding the
    /// arm's statements need (spliced at the start of the arm) and, when an
    /// enclosing scope exists, the export of the arm's newest commit to it
    /// (spliced at the end of the arm). A construct that threads `ClassVars`
    /// through its result slot carries the same value out; one that does not
    /// (an `on:do:` inside a closure, say) would otherwise lose the arm's
    /// writes. The export runs only when the arm completes.
    pub(in crate::core_erlang) fn close_arm_region(
        &mut self,
        region: Option<usize>,
    ) -> Option<(Document<'static>, Option<Document<'static>>)> {
        let depth = region?;
        let token = self.pop_class_var_scope_token(depth)?;
        if !token.used {
            return None;
        }
        let export = self
            .mark_innermost_scope_used()
            .map(|parent| Self::class_var_scope_export_doc(&token.name, &parent));
        let prefix = docvec![
            "let ",
            leaf::var(token.name),
            " = call 'erlang':'make_ref'() in ",
        ];
        Some((prefix, export))
    }

    /// `let _ = call 'beamtalk_class_dispatch':'class_var_scope_export'(ClassSelf,
    /// <from>, <to>) in `.
    fn class_var_scope_export_doc(from: &str, to: &str) -> Document<'static> {
        docvec![
            "let _ = call 'beamtalk_class_dispatch':'class_var_scope_export'(",
            leaf::var("ClassSelf"),
            ", ",
            leaf::var(from.to_string()),
            ", ",
            leaf::var(to.to_string()),
            ") in ",
        ]
    }

    /// The `let <token> = make_ref() in ` binding the scope opened by `mark`
    /// needs, or [`Document::Nil`] when no send committed under it. The caller
    /// splices it BEFORE the scope's expression (its binding must be visible
    /// to every closure the expression builds), in the open let-chain the
    /// refresh also lands in. Call after generating the expression and before
    /// [`Self::refresh_class_var_after_opaque_scope`].
    pub(in crate::core_erlang) fn class_var_scope_prefix(
        &self,
        mark: ClassVarScopeMark,
    ) -> Document<'static> {
        let Some(token) = self.class_var_scope_token(mark) else {
            return Document::Nil;
        };
        if !token.used {
            return Document::Nil;
        }
        docvec![
            "let ",
            leaf::var(token.name.clone()),
            " = call 'erlang':'make_ref'() in ",
        ]
    }

    fn class_var_scope_token(&self, mark: ClassVarScopeMark) -> Option<&ClassVarScopeToken> {
        let depth = mark.depth?;
        self.class_context
            .as_ref()
            .and_then(|ctx| ctx.class_var_scope_tokens.get(depth))
    }

    /// Closes the scope opened by `mark` and, when a send committed under its
    /// token, returns the `let <fresh ClassVarsN> = <committed value> in `
    /// refresh the caller splices right after the scope's expression, so the
    /// writes the scope's sends made are carried on instead of being dropped
    /// with the rolled-back rebinds of the closures and arms that made them.
    /// `None` when nothing committed (the version is untouched).
    ///
    /// The refresh consumes only this scope's own entry
    /// (`beamtalk_class_dispatch:class_var_scope_take/3`), falling back to the
    /// version live before the scope. Its result is itself a mint, so it is
    /// committed to the enclosing scope's token (when there is one): the
    /// refreshed version only exists lexically inside the enclosing scope's own
    /// body, which a closure or an arm that later raises takes with it.
    ///
    /// `ClassSelf` is `nil` in a direct-called `class sealed` method of a
    /// stateless class, which has no class variables: the runtime helper then
    /// answers the fallback.
    pub(in crate::core_erlang) fn refresh_class_var_after_opaque_scope(
        &mut self,
        mark: ClassVarScopeMark,
    ) -> Option<Document<'static>> {
        let token = self.close_scope_for_refresh(mark)?;
        let cv_before = Self::class_var_name_at(mark.version);
        let cv_new = self.next_class_var();
        let take = self.refresh_take_doc(&token, &cv_before);
        self.remember_deferred_scope(&token);
        let commit = self.commit_to_innermost_scope_doc(&cv_new);
        Some(docvec![
            "let ",
            leaf::var(cv_new),
            " = ",
            take,
            " in ",
            commit.unwrap_or(Document::Nil),
        ])
    }

    /// Closes the scope opened by `mark` for a refresh: `Some` when the scope
    /// was used, or when deferred tokens (see
    /// `ClassContext::deferred_scope_tokens`) may hold the writes of a stored
    /// closure invoked by this statement, in which case the returned token is
    /// unused (no binding or own `take`).
    pub(in crate::core_erlang) fn close_scope_for_refresh(
        &mut self,
        mark: ClassVarScopeMark,
    ) -> Option<ClassVarScopeToken> {
        let depth = mark.depth?;
        let token = self.pop_class_var_scope_token(depth)?;
        let has_deferred = self
            .class_context
            .as_ref()
            .is_some_and(|ctx| !ctx.deferred_scope_tokens.is_empty());
        (token.used || has_deferred).then_some(token)
    }

    /// The value a refresh binds: the scope's own entry (when it was used),
    /// then the deferred tokens' entries (a stored closure invoked by this
    /// statement), falling back to the version live before the scope.
    pub(in crate::core_erlang) fn refresh_take_doc(
        &self,
        token: &ClassVarScopeToken,
        cv_before: &str,
    ) -> Document<'static> {
        let mut doc = if token.used {
            Self::class_var_scope_take_doc(&token.name, cv_before)
        } else {
            leaf::var(cv_before.to_string())
        };
        if let Some(ctx) = self.class_context.as_ref() {
            for deferred in &ctx.deferred_scope_tokens {
                doc = docvec![
                    "call 'beamtalk_class_dispatch':'class_var_scope_take'(",
                    leaf::var("ClassSelf"),
                    ", ",
                    leaf::var(deferred.clone()),
                    ", ",
                    doc,
                    ")",
                ];
            }
        }
        doc
    }

    /// After a refresh: a scope a closure exported into stays available to the
    /// statements that follow it (its `make_ref()` binding is in their
    /// let-chain), so they can recover a stored closure's writes.
    pub(in crate::core_erlang) fn remember_deferred_scope(&mut self, token: &ClassVarScopeToken) {
        if token.used && token.exported_into {
            if let Some(ctx) = self.class_context.as_mut() {
                ctx.deferred_scope_tokens.push(token.name.clone());
            }
        }
    }

    /// Pops the scope opened by `mark` (and any inner scope an early return
    /// left open above it); `Some(token)` only when it was used.
    pub(in crate::core_erlang) fn close_class_var_scope(
        &mut self,
        mark: ClassVarScopeMark,
    ) -> Option<ClassVarScopeToken> {
        let depth = mark.depth?;
        let token = self.pop_class_var_scope_token(depth)?;
        token.used.then_some(token)
    }

    /// The `ClassVars{N}` name for version `version` (0 is the bare
    /// `ClassVars` method parameter).
    pub(in crate::core_erlang) fn class_var_name_at(version: usize) -> String {
        let mut counter = VersionCounter::new();
        counter.set_version(version);
        counter.current_var(VersionPrefix::ClassVars)
    }

    /// `call 'beamtalk_class_dispatch':'class_var_scope_take'(ClassSelf,
    /// <token>, <fallback>)`.
    pub(in crate::core_erlang) fn class_var_scope_take_doc(
        token: &str,
        fallback: &str,
    ) -> Document<'static> {
        docvec![
            "call 'beamtalk_class_dispatch':'class_var_scope_take'(",
            leaf::var("ClassSelf"),
            ", ",
            leaf::var(token.to_string()),
            ", ",
            leaf::var(fallback.to_string()),
            ")",
        ]
    }

    /// `call 'beamtalk_class_dispatch':'class_var_scope_read'(ClassSelf,
    /// [<tokens, innermost first>], <fallback>)`.
    pub(in crate::core_erlang) fn class_var_scope_read_doc(
        tokens: &[String],
        fallback: &str,
    ) -> Document<'static> {
        let list = tokens
            .iter()
            .enumerate()
            .map(|(i, token)| {
                if i == 0 {
                    leaf::var(token.clone())
                } else {
                    docvec![", ", leaf::var(token.clone())]
                }
            })
            .collect::<Vec<_>>();
        docvec![
            "call 'beamtalk_class_dispatch':'class_var_scope_read'(",
            leaf::var("ClassSelf"),
            ", [",
            Document::Vec(list),
            "], ",
            leaf::var(fallback.to_string()),
            ")",
        ]
    }

    /// The tokens of every open scope, innermost first, each marked used (a
    /// token's `make_ref()` binding is only emitted for a used one, and the
    /// caller is about to reference all of them); empty outside any scope.
    pub(in crate::core_erlang) fn class_var_scope_chain(&mut self) -> Vec<String> {
        let Some(ctx) = self.class_context.as_mut() else {
            return Vec::new();
        };
        ctx.class_var_scope_tokens
            .iter_mut()
            .rev()
            .map(|token| {
                token.used = true;
                token.name.clone()
            })
            .collect()
    }

    /// `let _ = call 'beamtalk_class_dispatch':'class_var_scope_commit'(ClassSelf,
    /// <innermost token>, <class_vars>) in ` — the commit every `ClassVars`
    /// version minted inside a scope makes, marking the token used; `None`
    /// when no scope is open.
    pub(in crate::core_erlang) fn commit_to_innermost_scope_doc(
        &mut self,
        class_vars: &str,
    ) -> Option<Document<'static>> {
        let name = self.mark_innermost_scope_used()?;
        Some(Self::class_var_scope_commit_doc(&name, class_vars))
    }

    /// The commit for the live `ClassVars` version, after a construct rebound
    /// it from its own result slot (a loop, fold or conditional that threads
    /// `ClassVars` precisely): the rebind is a mint like a send's, and the
    /// enclosing scope's later sends sync from what it committed.
    pub(in crate::core_erlang) fn commit_live_class_var_doc(&mut self) -> Document<'static> {
        let live = self.current_class_var();
        self.commit_to_innermost_scope_doc(&live)
            .unwrap_or(Document::Nil)
    }

    /// BT-3675: the commit a direct class-variable write (`self.x := …`,
    /// `clearField:`) makes after its `Bind` when a scope is open. Such a write
    /// is threaded lexically, but a later send in the scope syncs from the
    /// newest commit, so a write that did not commit would be overwritten by
    /// an older commit. `None` outside any scope: nothing is emitted for
    /// straight-line code.
    pub(in crate::core_erlang) fn class_var_write_commit_doc(
        &mut self,
    ) -> Option<Document<'static>> {
        if !self.in_class_method() {
            return None;
        }
        let live = self.current_class_var();
        self.commit_to_innermost_scope_doc(&live)
    }

    /// Marks the innermost open scope token used and as exported-into by a
    /// closure body, and returns its name.
    pub(in crate::core_erlang) fn mark_innermost_scope_exported_into(&mut self) -> Option<String> {
        let ctx = self.class_context.as_mut()?;
        let token = ctx.class_var_scope_tokens.last_mut()?;
        token.used = true;
        token.exported_into = true;
        Some(token.name.clone())
    }

    /// Marks the innermost open scope token used and returns its name.
    pub(in crate::core_erlang) fn mark_innermost_scope_used(&mut self) -> Option<String> {
        let ctx = self.class_context.as_mut()?;
        let token = ctx.class_var_scope_tokens.last_mut()?;
        token.used = true;
        Some(token.name.clone())
    }

    pub(in crate::core_erlang) fn class_var_scope_commit_doc(
        token: &str,
        class_vars: &str,
    ) -> Document<'static> {
        docvec![
            "let _ = call 'beamtalk_class_dispatch':'class_var_scope_commit'(",
            leaf::var("ClassSelf"),
            ", ",
            leaf::var(token.to_string()),
            ", ",
            leaf::var(class_vars.to_string()),
            ") in ",
        ]
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
