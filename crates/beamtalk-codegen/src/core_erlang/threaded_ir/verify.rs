// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ThreadedIr` verification (ADR 0111 § The verifier) — checks a lowered
//! [`super::ThreadedStmt`] slice against the invariants documented on each
//! [`VerifyError`] variant. Depends only on [`super::ir`]; nothing here
//! renders or builds IR.
//!
//! See `docs/development/debugging.md` § `ThreadedIr` verifier for the
//! variant-by-variant reference, and
//! [`CoreErlangGenerator::report_threaded_ir_verify_errors`] for how callers
//! turn a non-empty result into a debug/CI failure or a release diagnostic.

use std::collections::HashMap;

use super::super::CoreErlangGenerator;
use super::ir::{
    BindOp, CatchClause, CatchStep, FrameId, NlrThrowShape, OnDoCatchVars, StateAccFallbackReason,
    ThreadedStmt, ThreadingMode, ValueRef, VersionPrefix, VersionedVar,
};
use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};

// ─── Verifier ───────────────────────────────────────────────────────────────

/// A violated invariant, found by [`verify`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum VerifyError {
    /// A versioned var referenced with no producing `Bind` in its frame (or
    /// an ancestor frame currently on the frame stack — the explicit
    /// frame-flow rule). Catches the unbound-`StateX` class one layer earlier
    /// than `core_lint`, with a Beamtalk-source-attributable message instead
    /// of erlc's raw "unbound variable 'State3' in myMethod/2". Diagnosis-quality
    /// improvement over an existing backstop, not net-new detection.
    UnboundVersion { var: VersionedVar, at: Span },

    /// Per-frame linearity: within one `FrameId`, each version produced by
    /// exactly one `Bind`, consumed as the source of at most one successor.
    /// Frame-scoped by design — see [`FrameId`].
    NonLinearVersion {
        var: VersionedVar,
        producers: usize,
        consumers: usize,
    },

    /// Replaces the four "unpack should emit no code" `debug_assert!`s as a
    /// structural property: an optimized `ThreadingMode` node cannot contain
    /// an unpack `Bind`. (In release today this failure degrades to a
    /// `core_lint` unbound-variable error; the gain is a correct,
    /// centralized, source-attributed diagnosis.)
    ThreadingModeUnpackMismatch { mode: ThreadingMode, at: Span },

    /// Invariant class 1: a [`ThreadedStmt::TupleAccUnpack`] node
    /// (the flat positional-unpack accumulator discipline) appeared outside
    /// a [`ThreadingMode::TupleAcc`] body. Mirrors `ThreadingModeUnpackMismatch`
    /// for the tuple-shaped (rather than map-shaped) accumulator; in release
    /// today this degrades to a `core_lint` unbound-variable or badarg-on-
    /// `element/2` error, one layer further from the cause.
    TupleAccUnpackModeMismatch { mode: ThreadingMode, at: Span },

    /// Invariant class 4 ("early-exit accumulator liveness"): a
    /// [`ThreadedStmt::TupleAccUnpack`] node's own `gate_slots` disagrees with
    /// its enclosing [`ThreadingMode::TupleAcc`]'s `gate_slots`. Each list-op
    /// family reserves a different number of leading accumulator slots for
    /// its own result/continuation state (`do:`: 0; `collect:`/`select:`/
    /// boolean-predicate ops: 1; `takeWhile:`/`dropWhile:`/`detect:`/
    /// `partition:`-family: 2) — a mismatch here means the unpack would read
    /// threaded-local values from the wrong tuple positions: well-formed Core
    /// Erlang, silently wrong values (the ADR 0110 danger class, not a
    /// `core_lint` failure). A live check, not scaffolding — see
    /// [`build_tuple_acc_unpack`]'s doc comment for the two independent
    /// sources (`ListOpKind::gate_slots` at lowering time vs. each call
    /// site's own `index_offset - 1` at rendering time).
    EarlyExitGateSlotMismatch {
        mode_gate_slots: usize,
        node_gate_slots: usize,
        at: Span,
    },

    /// Invariant class 2: `select_tuple_acc`'s `ValueType`-context
    /// exclusion (`control_flow/mod.rs`'s `select_tuple_acc`), pinned
    /// structurally. `ValueType` methods have no actor `State` `gen_server`
    /// variable to reference — `TupleAcc` mode is unconditionally
    /// unavailable there, and this fires if a future change to
    /// `select_tuple_acc`'s guard ordering ever lets `use_tuple_acc` become
    /// `true` in a `ValueType` context. Regression-pinning (see ADR
    /// §Verifier honesty) — `select_tuple_acc`'s own early-return already
    /// makes this unreachable today.
    ///
    /// `#[cfg(test)]`: this variant's sole constructor
    /// ([`verify_tuple_acc_value_type_exclusion`]) is itself test-only —
    /// see that function's doc comment for why production never reaches
    /// it structurally.
    #[cfg(test)]
    TupleAccInValueTypeContext { at: Span },

    /// Invariant class 3: the recursive inter-construct fallback
    /// invariant `list_op_needs_stateacc_fallback_recursive` encodes
    /// (`control_flow/mod.rs`), pinned structurally. A nested list-op whose
    /// own inner block cannot use tuple-acc (so it falls back to a
    /// `StateAcc`-map accumulator to thread its cross-scope mutation) is
    /// incompatible with an *enclosing* `DirectParams` loop: `DirectParams`
    /// has no `StateAcc` map for the inner loop's `{value, StateAcc}` result
    /// to unpack into. Fires if `select_direct_params`'s
    /// `!effects.has_non_tuple_safe_list_op` guard is ever dropped or
    /// reordered past the point where `DirectParams` is selected.
    ///
    /// `#[cfg(test)]`: this variant's sole constructor
    /// ([`verify_nested_list_op_stateacc_compat`]) is itself test-only —
    /// see that function's doc comment for why production never reaches
    /// it structurally.
    #[cfg(test)]
    NestedStateAccFallbackUnderDirectParams { at: Span },

    /// ADR 0118 §Decision 5: a [`ThreadedValue`] whose prelude
    /// carries a versioned `Bind` for `prefix` was [`ThreadedValue::close`]d
    /// in a context that cannot thread that prefix
    /// ([`CloseContext::Opaque`]) — the closed `Document` scopes the new
    /// version away, so the state effect the expression performed is lost
    /// to everything after it. This is the "silent drop" class of bug
    /// (a nested actor self-send whose `NewState` is discarded) made into
    /// a verifier finding: the fix is for the enclosing consumer to
    /// *splice* the prelude into its own frame (`stmts.extend(tv.prelude)`)
    /// instead of closing it, or — at a genuine boundary such as a Tier 1
    /// closure body — to surface a user-facing diagnostic built from this
    /// error rather than from a second predicate.
    ///
    /// ADR 0118 phase 1a: constructed by [`ThreadedValue::close`].
    ///
    /// No production caller yet — see [`CloseContext`].
    #[allow(dead_code)]
    StateEffectEscapesExpression { prefix: VersionPrefix, at: Span },

    /// ADR 0130 §4: a compiled `on:do:`'s catch is a class-variable catch
    /// boundary, and its [`ThreadedStmt::OnDoCatch`] node does not honour the
    /// obligation — the non-NLR clause does not begin with
    /// `beamtalk_class_vars:restore(Snap)`, or it is missing, or the two
    /// `$bt_nlr` pass-through clauses (3-tuple and actor 4-tuple) are not both
    /// ordered before it. This is wrong-value Core Erlang, not an unbound
    /// variable: an error keeps the writes made inside the protected region,
    /// or a `^` throw discards writes it must keep. See [`CatchRestoreDefect`].
    CatchWithoutClassVarRestore {
        defect: CatchRestoreDefect,
        at: Span,
    },

    /// A compiled `on:do:`'s [`ThreadedStmt::OnDoCatch`] opens the exception
    /// class filter ([`CatchStep::ClassFilter`]) but does not close it with
    /// its handler arm followed by [`CatchStep::FilterMiss`], the `'false'`
    /// re-raise and the exhaustive wildcard clause. A filter `case` with no
    /// wildcard is not provably exhaustive to the Core Erlang compiler, and
    /// when the `on:do:` sits inside another protected region `erlc` rejects
    /// the module with `ambiguous_catch_try_state` (BT-3718). Every `on:do:`
    /// in any nesting position closes its filter, so the check is per node.
    CatchFilterNotClosed { at: Span },

    /// BT-3725 (the verifier backing for the BT-3694 fix): a
    /// [`ScopeKind::ClassMethod`] scope opens a `StateAcc` loop that carries no
    /// threaded local, or threads the actor `State` / value-type `Self` family.
    /// A class method has no `State` parameter and threads no family at all
    /// (ADR 0130 §3: a class-variable write is an in-place `put` into the class
    /// process and a self-send rebinds nothing), so the only thing a
    /// class-method `StateAcc` loop may carry is a map of threaded *locals*,
    /// seeded from a fresh `maps:new()`. A `StateAcc` loop with no locals can
    /// only be seeded from the ambient actor `State` — unbound in a class
    /// method, which `erlc` rejects as `unbound variable 'State'` (BT-3694).
    /// See [`ClassMethodDefect`].
    ActorStateInClassMethod { defect: ClassMethodDefect, at: Span },
}

/// What a [`ThreadedStmt`] tree is lowered *for*: decides which families the
/// scope may thread (the per-scope-kind invariant [`verify_in_scope`]
/// enforces, BT-3725).
///
/// (`State` inside a threading frame is the `StateAcc` map's version,
/// legitimate in a class method — see `VerifyWalk::check_class_method_family`.)
///
/// | scope         | method-level `State` | `SelfVt` family      | `StateAcc` loop with no locals |
/// |---------------|----------------------|----------------------|--------------------------------|
/// | `ClassMethod` | forbidden            | forbidden            | forbidden                      |
/// | `Instance`    | not constrained here | not constrained here | not constrained here           |
///
/// **What enforces it:** exactly the entry points that take a `ScopeKind` —
/// [`verify_in_scope`], `verify_body_with_opaque_version_gaps` and
/// `verify_simple_bind` — which every production lowering site reaches via
/// `CoreErlangGenerator::threaded_scope` / `verify_threaded_ir`. Bare
/// [`verify`] is `Instance`-scoped and enforces nothing here; it is used only
/// by tests and test-local wrappers.
///
/// `Instance` (actor instance, value-type instance, REPL and every non-method
/// fixture) is deliberately unconstrained by this check: which of
/// `State`/`SelfVt` is eligible there is decided by
/// `CoreErlangGenerator::eligible_families` (ADR 0122), not re-derived here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum ScopeKind {
    /// A class-side method body (`CoreErlangGenerator::in_class_method`).
    ClassMethod,
    /// Any other scope.
    Instance,
}

/// What is wrong with a class-method scope
/// ([`VerifyError::ActorStateInClassMethod`]).
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) enum ClassMethodDefect {
    /// A `StateAcc` loop whose fallback reason is
    /// [`StateAccFallbackReason::NoThreadedLocals`]: it carries nothing, so its
    /// seed is the ambient actor `State` — unbound in a class method.
    StateAccLoopWithoutLocals,
    /// A versioned `Bind` / `Return` of the actor `State` family produced at
    /// method level (outside every threading frame, where `State` is not the
    /// `StateAcc` map), or of `SelfVt` anywhere: a class method threads no
    /// family.
    FamilyVersion(VersionPrefix),
}

/// What is wrong with an [`ThreadedStmt::OnDoCatch`] node
/// ([`VerifyError::CatchWithoutClassVarRestore`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum CatchRestoreDefect {
    /// There is no non-NLR clause at all.
    NoNonNlrClause,
    /// The non-NLR clause does not begin with the class-variable restore.
    RestoreNotFirst,
    /// The restore reads a variable other than the node's snapshot variable.
    RestoreReadsWrongSnapshot,
    /// An NLR pass-through shape is missing before the non-NLR clause (it is
    /// absent, or ordered after it): a `^` of that shape would be restored.
    NlrArmNotBeforeRestore(NlrThrowShape),
}

/// Checks `ir` against the invariants documented on each [`VerifyError`]
/// variant. Returns an empty `Vec` when `ir` is well-formed.
///
/// **Failure behavior** (per ADR 0111 §The verifier / CLAUDE.md error-recovery
/// rules): this function never panics — callers in debug/CI treat a non-empty
/// result as a hard failure; release callers must degrade a non-empty result
/// to an internal-error diagnostic, never a panic or a refusal to compile.
/// (No call site does either yet — see module docs §Status.)
///
/// `Instance`-scoped, so it enforces no per-[`ScopeKind`] invariant; no
/// production code calls it (they use [`verify_in_scope`]), hence test-only.
#[cfg(test)]
pub(in crate::core_erlang) fn verify(ir: &[ThreadedStmt]) -> Vec<VerifyError> {
    verify_in_scope(ir, ScopeKind::Instance)
}

/// [`verify`] for IR lowered in `scope`: additionally enforces the
/// per-[`ScopeKind`] family invariant ([`VerifyError::ActorStateInClassMethod`]).
/// Every production caller reaches this with `CoreErlangGenerator::threaded_scope`
/// — via `CoreErlangGenerator::verify_threaded_ir`, or by passing
/// `threaded_scope()` to `verify_body_with_opaque_version_gaps` / `verify_simple_bind`.
pub(in crate::core_erlang) fn verify_in_scope(
    ir: &[ThreadedStmt],
    scope: ScopeKind,
) -> Vec<VerifyError> {
    let mut producers: HashMap<VersionedVar, usize> = HashMap::new();
    let mut consumers: HashMap<VersionedVar, usize> = HashMap::new();
    collect_producer_consumer_counts(ir, &mut producers, &mut consumers);

    let mut errors = Vec::new();
    // NonLinearVersion: iterate every var that was ever produced *or*
    // consumed, since a version consumed zero times but produced twice (or
    // vice-versa) is still a linearity violation. Version-0 vars are each
    // frame's implicit entry parameter — never produced by a `Bind` by
    // design (see `VersionedVar` docs) — so they are exempt from the
    // "exactly one producer" rule entirely; an unproduced, un-owned version-0
    // reference is checked separately by `UnboundVersion`.
    let mut all_vars: Vec<&VersionedVar> = producers.keys().filter(|v| v.version != 0).collect();
    for var in consumers.keys() {
        if var.version != 0 && !producers.contains_key(var) {
            all_vars.push(var);
        }
    }
    // Deterministic order: `producers`/`consumers` are `HashMap`s, whose
    // iteration order is per-process-random — sort so `verify`'s output
    // (and any future diagnostic built from it) is stable across runs.
    all_vars.sort();
    for var in all_vars {
        let produced = producers.get(var).copied().unwrap_or(0);
        let consumed = consumers.get(var).copied().unwrap_or(0);
        if produced != 1 || consumed > 1 {
            errors.push(VerifyError::NonLinearVersion {
                var: var.clone(),
                producers: produced,
                consumers: consumed,
            });
        }
    }

    let mut walk = VerifyWalk {
        producers: &producers,
        frame_stack: vec![FrameId::ROOT],
        mode_stack: Vec::new(),
        scope,
        errors: &mut errors,
    };
    walk.walk(ir);

    errors
}

/// First pass: collects, per [`VersionedVar`], how many `Bind`s produce it
/// (`target`) and how many `Bind`s consume it as their `source`. `VersionedVar`
/// already encodes frame identity, so counts naturally stay frame-scoped
/// without extra bookkeeping.
fn collect_producer_consumer_counts(
    stmts: &[ThreadedStmt],
    producers: &mut HashMap<VersionedVar, usize>,
    consumers: &mut HashMap<VersionedVar, usize>,
) {
    for stmt in stmts {
        match stmt {
            ThreadedStmt::Bind { target, source, .. } => {
                *producers.entry(target.clone()).or_insert(0) += 1;
                *consumers.entry(source.clone()).or_insert(0) += 1;
            }
            // ADR 0131 §2: the `MethodBody`/`BranchArm` frame nodes scope
            // their body exactly like a `Threaded` node does.
            ThreadedStmt::Threaded { body, .. }
            | ThreadedStmt::MethodBody { body, .. }
            | ThreadedStmt::BranchArm { body, .. } => {
                collect_producer_consumer_counts(body, producers, consumers);
            }
            // ADR 0118 phase 3: `condition`'s own Binds are
            // real IR now too — collected in the SAME pass as `body`'s
            // (order doesn't matter here: both just accumulate into the
            // same producer/consumer maps).
            ThreadedStmt::ConditionalLoop {
                condition, body, ..
            } => {
                collect_producer_consumer_counts(condition, producers, consumers);
                collect_producer_consumer_counts(body, producers, consumers);
            }
            ThreadedStmt::TupleAccUnpack { targets, .. } => {
                // Each target is produced exactly once by this node, straight
                // from the unversioned `AccParam` — never consumed as another
                // Bind's `source` here (that would be a later ordinary
                // `Bind`), so only `producers` is touched.
                for target in targets {
                    *producers.entry(target.clone()).or_insert(0) += 1;
                }
            }
            // ADR 0131 §2: a `LoopParam`/`MapPut` rebind is one version step,
            // counted like a `Bind`'s.
            ThreadedStmt::LocalRebind { lowering, .. } => {
                if let Some((source, target)) = lowering.version_step() {
                    *producers.entry(target.clone()).or_insert(0) += 1;
                    *consumers.entry(source.clone()).or_insert(0) += 1;
                }
            }
            ThreadedStmt::NlrCatch { .. }
            | ThreadedStmt::Return(..)
            | ThreadedStmt::Statement(..)
            | ThreadedStmt::OnDoCatch { .. }
            | ThreadedStmt::ConstructTuple { .. }
            | ThreadedStmt::DiscardLocals { .. } => {}
        }
    }
}

/// Second pass: walks `ir` tracking the active frame/mode nesting, checking
/// [`VerifyError::UnboundVersion`] and [`VerifyError::ThreadingModeUnpackMismatch`]
/// (`NonLinearVersion` is fully determined by the first pass's counts and
/// checked before this walk runs).
struct VerifyWalk<'a> {
    producers: &'a HashMap<VersionedVar, usize>,
    frame_stack: Vec<FrameId>,
    mode_stack: Vec<ThreadingMode>,
    scope: ScopeKind,
    errors: &'a mut Vec<VerifyError>,
}

impl VerifyWalk<'_> {
    fn walk(&mut self, stmts: &[ThreadedStmt]) {
        for stmt in stmts {
            self.walk_stmt(stmt);
        }
    }

    /// A version-0 var is the frame's implicit entry parameter — always
    /// bound. A version>0 var must have a producing `Bind` in a frame
    /// currently on the stack (its own frame, or an ancestor's — the
    /// frame-flow rule).
    fn check_use(&mut self, var: &VersionedVar, at: Span) {
        if var.version == 0 {
            return;
        }
        let bound = self.frame_stack.contains(&var.frame)
            && self.producers.get(var).copied().unwrap_or(0) > 0;
        if !bound {
            self.errors.push(VerifyError::UnboundVersion {
                var: var.clone(),
                at,
            });
        }
    }

    /// [`VerifyError::ActorStateInClassMethod`]: a `State`/`SelfVt` versioned
    /// var in a [`ScopeKind::ClassMethod`] scope that denotes a *family*.
    ///
    /// The `State` prefix is overloaded: inside a threading frame (a loop,
    /// fold or branch/handler arm — any `Threaded`/`ConditionalLoop` node, so
    /// `mode_stack` is non-empty) it is the `StateAcc` map's version, which
    /// carries threaded locals in a class method and is legitimate there (the
    /// class-method corpus lowers `State1`, … in such frames). Only a `State`
    /// version produced at method level (empty `mode_stack`) is the actor
    /// `State` family. `SelfVt` is never a `StateAcc` map, so it is flagged
    /// everywhere. Version 0 is a frame's entry parameter, never produced.
    fn check_class_method_family(&mut self, var: &VersionedVar, at: Span) {
        if self.scope != ScopeKind::ClassMethod || var.version == 0 {
            return;
        }
        let is_family = match var.prefix {
            VersionPrefix::State => self.mode_stack.is_empty(),
            VersionPrefix::SelfVt => true,
            VersionPrefix::Local(_) | VersionPrefix::Gensym(_) => false,
        };
        if is_family {
            self.errors.push(VerifyError::ActorStateInClassMethod {
                defect: ClassMethodDefect::FamilyVersion(var.prefix.clone()),
                at,
            });
        }
    }

    /// [`VerifyError::ActorStateInClassMethod`]: a loop/fold node whose mode is
    /// a locals-less `StateAcc` in a [`ScopeKind::ClassMethod`] scope.
    fn check_class_method_mode(&mut self, mode: &ThreadingMode, at: Span) {
        if self.scope == ScopeKind::ClassMethod
            && matches!(
                mode,
                ThreadingMode::StateAcc(StateAccFallbackReason::NoThreadedLocals)
            )
        {
            self.errors.push(VerifyError::ActorStateInClassMethod {
                defect: ClassMethodDefect::StateAccLoopWithoutLocals,
                at,
            });
        }
    }

    #[allow(clippy::too_many_lines)] // ADR 0118 phase 3 (BT-3419) split the Threaded/ConditionalLoop arm in two
    fn walk_stmt(&mut self, stmt: &ThreadedStmt) {
        match stmt {
            ThreadedStmt::Bind {
                target,
                source,
                op,
                span,
            } => {
                self.check_use(source, *span);
                self.check_class_method_family(target, *span);
                match op {
                    BindOp::Put { value, .. } | BindOp::Direct(value) => {
                        if let ValueRef::Version(v) = value {
                            self.check_use(v, *span);
                        }
                    }
                    BindOp::Unpack { .. } => {
                        if let Some(mode) = self.mode_stack.last()
                            && !matches!(mode, ThreadingMode::StateAcc(_))
                        {
                            self.errors.push(VerifyError::ThreadingModeUnpackMismatch {
                                mode: mode.clone(),
                                at: *span,
                            });
                        }
                    }
                }
            }
            ThreadedStmt::Threaded {
                mode,
                frame,
                body,
                produces,
                span,
            } => {
                self.check_class_method_mode(mode, *span);
                self.frame_stack.push(*frame);
                self.mode_stack.push(mode.clone());
                self.walk(body);
                for v in produces {
                    self.check_use(v, Span::default());
                    self.check_class_method_family(v, *span);
                }
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            ThreadedStmt::ConditionalLoop {
                mode,
                frame,
                condition,
                condition_value,
                body,
                produces,
                span,
                ..
            } => {
                self.check_class_method_mode(mode, *span);
                // ADR 0111 Addendum 2, Gap 1 / ADR 0118 phase 3:
                // `ConditionalLoop` verifies almost exactly like `Threaded`
                // — push frame/mode once, walk `condition` THEN `body` (both
                // in the SAME frame — the condition's own `Bind`s are now
                // real IR the loop's later references can see), check_use
                // `condition_value` and each `produces` entry, pop.
                // `continue_arm`/`exit_arm`/`fn_name`/`counter` are opaque
                // (or caller-supplied, non-threading) fields `verify()` does
                // not, and is not meant to, inspect — see the variant's doc
                // comment.
                self.frame_stack.push(*frame);
                self.mode_stack.push(mode.clone());
                self.walk(condition);
                if let ValueRef::Version(v) = condition_value {
                    self.check_use(v, Span::default());
                }
                self.walk(body);
                for v in produces {
                    self.check_use(v, Span::default());
                    self.check_class_method_family(v, *span);
                }
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            // Opaque by design: `NlrCatch` has no body field (`render`
            // treats the rest of the slice as its body); a `Statement` is
            // ordinary AST-directed codegen with no state-threading content
            // of its own (see the variant's doc comment).
            //
            // ADR 0131 Phase 1b adds `ConstructTuple`/`DiscardLocals` and the
            // nodes below; their own obligations (`ThreadedLocalDropped`,
            // `LocalRebindModeMismatch`, `LocalReadAfterSiblingRebind`) are
            // Phase 1c. Until then a rebind is checked exactly like the
            // `Bind` it renders as, and a frame node scopes its body like any
            // other frame.
            ThreadedStmt::NlrCatch { .. }
            | ThreadedStmt::Statement(..)
            | ThreadedStmt::ConstructTuple { .. }
            | ThreadedStmt::DiscardLocals { .. } => {}
            ThreadedStmt::LocalRebind {
                state_first,
                lowering,
                span,
                ..
            } => {
                if let Some(state) = state_first {
                    self.check_use(state, *span);
                }
                if let Some((source, target)) = lowering.version_step() {
                    self.check_use(source, *span);
                    self.check_class_method_family(target, *span);
                }
            }
            // The method's root frame: method level, so no mode is pushed (a
            // `State` version here is the actor family, exactly as for the
            // top-level slice).
            ThreadedStmt::MethodBody { frame, body, .. } => {
                self.frame_stack.push(*frame);
                self.walk(body);
                self.frame_stack.pop();
            }
            // A branch arm threads through its seeded `StateAcc` — the same
            // `StateAcc(None)` mode `verify_and_render_branch_arm`'s wrapper
            // records for an arm today.
            ThreadedStmt::BranchArm { frame, body, .. } => {
                self.frame_stack.push(*frame);
                self.mode_stack
                    .push(ThreadingMode::StateAcc(StateAccFallbackReason::None));
                self.walk(body);
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            ThreadedStmt::OnDoCatch {
                vars,
                clauses,
                span,
            } => {
                if let Some(defect) = check_catch_restore(vars, clauses) {
                    self.errors
                        .push(VerifyError::CatchWithoutClassVarRestore { defect, at: *span });
                }
                if !catch_filter_closed(clauses) {
                    self.errors
                        .push(VerifyError::CatchFilterNotClosed { at: *span });
                }
            }
            ThreadedStmt::Return(value, state, span) => {
                self.check_use(state, *span);
                self.check_class_method_family(state, *span);
                if let ValueRef::Version(v) = value {
                    self.check_use(v, *span);
                }
            }
            ThreadedStmt::TupleAccUnpack {
                gate_slots, span, ..
            } => {
                // Mirrors the `BindOp::Unpack` check above (invariant class 1:
                // legal only inside `TupleAcc` mode), plus invariant class 4:
                // the node's own `gate_slots` must match the enclosing mode's.
                if let Some(mode) = self.mode_stack.last() {
                    match mode {
                        ThreadingMode::TupleAcc(mode_gate_slots) => {
                            if *mode_gate_slots != *gate_slots {
                                self.errors.push(VerifyError::EarlyExitGateSlotMismatch {
                                    mode_gate_slots: *mode_gate_slots,
                                    node_gate_slots: *gate_slots,
                                    at: *span,
                                });
                            }
                        }
                        _ => {
                            self.errors.push(VerifyError::TupleAccUnpackModeMismatch {
                                mode: mode.clone(),
                                at: *span,
                            });
                        }
                    }
                }
                // Targets are producers-only (see `collect_producer_consumer_counts`);
                // NonLinearVersion's first pass already validated their
                // producer counts, so no further per-target `check_use` is
                // needed here — a `TupleAccUnpack` target is never itself a
                // consumer of a prior version.
            }
        }
    }
}

/// ADR 0130 §4 catch-boundary obligation of one [`ThreadedStmt::OnDoCatch`]:
/// the first defect found, or `None` when the non-NLR clause begins with the
/// restore of the node's own snapshot and both NLR pass-through shapes are
/// ordered before it.
fn check_catch_restore(
    vars: &OnDoCatchVars,
    clauses: &[CatchClause],
) -> Option<CatchRestoreDefect> {
    let Some(non_nlr_at) = clauses
        .iter()
        .position(|c| matches!(c, CatchClause::NonNlr { .. }))
    else {
        return Some(CatchRestoreDefect::NoNonNlrClause);
    };
    for shape in [NlrThrowShape::Tuple3, NlrThrowShape::Tuple4] {
        let before = clauses[..non_nlr_at]
            .iter()
            .any(|c| matches!(c, CatchClause::NlrPassThrough(s) if *s == shape));
        if !before {
            return Some(CatchRestoreDefect::NlrArmNotBeforeRestore(shape));
        }
    }
    let CatchClause::NonNlr { steps } = &clauses[non_nlr_at] else {
        return Some(CatchRestoreDefect::NoNonNlrClause);
    };
    match steps.first() {
        Some(CatchStep::ClassVarRestore { snapshot }) if *snapshot == vars.snapshot_var => None,
        Some(CatchStep::ClassVarRestore { .. }) => {
            Some(CatchRestoreDefect::RestoreReadsWrongSnapshot)
        }
        _ => Some(CatchRestoreDefect::RestoreNotFirst),
    }
}

/// Whether the non-NLR clause of a [`ThreadedStmt::OnDoCatch`] closes the class
/// filter it opens: `ClassFilter`, then `FilterHandler`, then `FilterMiss`, the
/// last three steps in that order. A node with no non-NLR clause has no filter
/// to close (the restore check reports that).
fn catch_filter_closed(clauses: &[CatchClause]) -> bool {
    clauses.iter().all(|clause| match clause {
        CatchClause::NlrPassThrough(_) => true,
        CatchClause::NonNlr { steps } => matches!(
            steps.as_slice(),
            [
                ..,
                CatchStep::ClassFilter,
                CatchStep::FilterHandler(_),
                CatchStep::FilterMiss
            ]
        ),
    })
}

impl CoreErlangGenerator {
    /// The [`ScopeKind`] the generator is currently lowering — the one place
    /// that maps generator state to the verifier's scope (BT-3725).
    pub(in crate::core_erlang) fn threaded_scope(&self) -> ScopeKind {
        if self.in_class_method() {
            ScopeKind::ClassMethod
        } else {
            ScopeKind::Instance
        }
    }

    /// [`verify_in_scope`] with the current [`Self::threaded_scope`] — what
    /// every production lowering site calls instead of bare [`verify`].
    pub(in crate::core_erlang) fn verify_threaded_ir(
        &self,
        ir: &[ThreadedStmt],
    ) -> Vec<VerifyError> {
        verify_in_scope(ir, self.threaded_scope())
    }

    /// Shared failure-reporting path for every `ThreadedIr` production
    /// invariant check (ADR 0111 §The verifier / CLAUDE.md's "never panic
    /// on user input" rule): hard-fails in debug/CI via `debug_assert!`,
    /// exactly as the deleted `debug_assert!`s this migration's checks
    /// replace did; in release builds (where `debug_assert!` is compiled
    /// out), degrades to an internal-error diagnostic on the compile result
    /// instead of silently doing nothing — the compile still succeeds with
    /// the generator's (unverified) output. Shared by every state-threading
    /// invariant check in `control_flow`, `expressions.rs`,
    /// `dispatch_codegen.rs`, and `gen_server/methods.rs` — each consolidated
    /// onto this one helper instead of an independently-added copy
    /// (CLAUDE.md's no-duplicate-implementations rule).
    ///
    /// Moved here (out of `control_flow/mod.rs`) together with
    /// [`StateAccFallbackReason`] to remove a `threaded_ir → control_flow`
    /// import cycle — this crate's `control_flow` module previously defined
    /// both and `threaded_ir.rs` imported `StateAccFallbackReason` from it;
    /// now `threaded_ir.rs` defines both and `control_flow` imports from
    /// here instead.
    pub(in crate::core_erlang) fn report_threaded_ir_verify_errors(
        &mut self,
        errors: &[VerifyError],
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
        self.add_codegen_warning(verify_errors_to_diagnostic(errors, invariant_label, span));
    }

    /// Records one synthetic verifier finding through the release-mode path
    /// (the warning, without [`Self::report_threaded_ir_verify_errors`]'s
    /// debug-build hard failure). Backs
    /// [`CodegenOptions::with_injected_verifier_violation`], the test hook
    /// that lets driver-level tests prove an `internal:` diagnostic surfaces
    /// without a release build or a real codegen bug.
    pub(in crate::core_erlang) fn inject_synthetic_verifier_violation(
        &mut self,
        enabled: bool,
        span: Span,
    ) {
        if !enabled {
            return;
        }
        let errors = [VerifyError::ThreadingModeUnpackMismatch {
            mode: ThreadingMode::DirectParams,
            at: span,
        }];
        self.add_codegen_warning(verify_errors_to_diagnostic(
            &errors,
            "injected verifier violation (test hook)",
            span,
        ));
    }
}

/// Builds the release-mode diagnostic for a `ThreadedIr` verifier finding
/// (ADR 0111 amendment, BT-3724): a **warning** (never an error, so it can
/// not fail a build that would otherwise produce usable code) whose message
/// starts with `internal:` and whose category is
/// [`DiagnosticCategory::InternalVerifier`], so drivers can forward exactly
/// these and nothing else out of `GeneratedModule::warnings`.
///
/// Independent of `cfg(debug_assertions)` so a unit test can assert it
/// without a release build; [`CoreErlangGenerator::report_threaded_ir_verify_errors`]
/// is its only production caller.
#[must_use]
pub(in crate::core_erlang) fn verify_errors_to_diagnostic(
    errors: &[VerifyError],
    invariant_label: &str,
    span: Span,
) -> Diagnostic {
    Diagnostic::warning(format!("internal: {invariant_label}: {errors:?}"), span)
        .with_category(DiagnosticCategory::InternalVerifier)
}
