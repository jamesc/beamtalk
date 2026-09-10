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

use super::super::{CoreErlangGenerator, NlrBoundary};
use super::ir::{
    BindOp, FrameId, ThreadedStmt, ThreadingMode, ValueRef, VersionPrefix, VersionedVar,
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

    /// The ADR 0110 CONTRACT check — regression-pinning, not counterfactual
    /// detection (see ADR §Verifier honesty). A class-var `Bind` at frame
    /// depth 0 (method top frame) inside a method whose body can relay a
    /// foreign NLR (a `NlrCatch` with `boundary: ClassMethod { has_class_vars:
    /// true }` present) MUST have `shadow_write: true`. Fires if a future
    /// emission path forgets the shadow write ADR 0110's fix depends on, or
    /// if a new mutation site is added without it.
    ShadowWriteMissing { mutated: VersionedVar, at: Span },

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
    /// `true` in a `ValueType` context. Regression-pinning, like
    /// `ShadowWriteMissing` (see ADR §Verifier honesty) — `select_tuple_acc`'s
    /// own early-return already makes this unreachable today.
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
    /// ADR 0118 phase 1a: constructed by [`ThreadedValue::close`], which has
    /// no production caller yet — see [`CloseContext`].
    ///
    /// The "genuine boundary such as a Tier 1 closure body" case
    /// above is exactly the class-method self-send-in-a-bare-block scenario
    /// `check_no_unsafe_class_method_self_sends` (`expressions.rs`) already
    /// diagnoses from a separate, pre-flight static predicate — replacing
    /// that diagnostic with this variant (surfaced via `close()`
    /// at `close_threaded_value_doc`, `util.rs`) is blocked on a
    /// real signal-propagation gap, not a small wiring change. See that
    /// predicate's own doc comment for the full finding; still no
    /// production caller.
    #[allow(dead_code)]
    StateEffectEscapesExpression { prefix: VersionPrefix, at: Span },
}

/// Checks `ir` against the invariants documented on each [`VerifyError`]
/// variant. Returns an empty `Vec` when `ir` is well-formed.
///
/// **Failure behavior** (per ADR 0111 §The verifier / CLAUDE.md error-recovery
/// rules): this function never panics — callers in debug/CI treat a non-empty
/// result as a hard failure; release callers must degrade a non-empty result
/// to an internal-error diagnostic, never a panic or a refusal to compile.
/// (No call site does either yet — see module docs §Status.)
pub(in crate::core_erlang) fn verify(ir: &[ThreadedStmt]) -> Vec<VerifyError> {
    let has_class_vars_nlr = contains_class_var_nlr_catch(ir);

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
        has_class_vars_nlr,
        frame_stack: vec![FrameId::ROOT],
        mode_stack: Vec::new(),
        shadow_write_eligible_stack: vec![true],
        errors: &mut errors,
    };
    walk.walk(ir);

    errors
}

/// Recursively scans `ir` for an `NlrCatch` whose boundary is
/// `ClassMethod { has_class_vars: true }` — the precondition for
/// [`VerifyError::ShadowWriteMissing`].
fn contains_class_var_nlr_catch(ir: &[ThreadedStmt]) -> bool {
    ir.iter().any(|stmt| match stmt {
        ThreadedStmt::NlrCatch { boundary, .. } => {
            matches!(
                boundary,
                NlrBoundary::ClassMethod {
                    has_class_vars: true
                }
            )
        }
        ThreadedStmt::Threaded { body, .. } => contains_class_var_nlr_catch(body),
        // ADR 0118 phase 3: `condition` scans too — a class-var
        // NLR catch nested there is exactly as relevant to `ShadowWriteMissing`
        // as one nested in `body`, even though no real lowering produces one
        // (a while condition has no NLR boundary of its own).
        ThreadedStmt::ConditionalLoop {
            condition, body, ..
        } => contains_class_var_nlr_catch(condition) || contains_class_var_nlr_catch(body),
        ThreadedStmt::Bind { .. }
        | ThreadedStmt::Return(..)
        | ThreadedStmt::TupleAccUnpack { .. }
        | ThreadedStmt::Statement(..) => false,
    })
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
            ThreadedStmt::Threaded { body, .. } => {
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
            ThreadedStmt::NlrCatch { .. }
            | ThreadedStmt::Return(..)
            | ThreadedStmt::Statement(..) => {}
        }
    }
}

/// Second pass: walks `ir` tracking the active frame/mode nesting, checking
/// [`VerifyError::UnboundVersion`], [`VerifyError::ThreadingModeUnpackMismatch`],
/// and [`VerifyError::ShadowWriteMissing`] (`NonLinearVersion` is fully
/// determined by the first pass's counts and checked before this walk runs).
struct VerifyWalk<'a> {
    producers: &'a HashMap<VersionedVar, usize>,
    has_class_vars_nlr: bool,
    frame_stack: Vec<FrameId>,
    mode_stack: Vec<ThreadingMode>,
    /// ADR 0111 Addendum 9, Question 1: parallel to `frame_stack`, seeded
    /// `[true]` (a method's own top level is always shadow-write-eligible),
    /// pushed/popped in lockstep in the `Threaded | ConditionalLoop` arm of
    /// `walk_stmt`, AND-combined with the parent's current top for
    /// defense-in-depth on hand-built fixtures (correct lowering never needs
    /// the AND — a nested node's own `block_depth`-derived flag already
    /// encodes total nesting depth). `ShadowWriteMissing`'s gate reads this
    /// stack's top instead of `target.frame == FrameId::ROOT`.
    shadow_write_eligible_stack: Vec<bool>,
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

    #[allow(clippy::too_many_lines)] // ADR 0118 phase 3 (BT-3419) split the Threaded/ConditionalLoop arm in two
    fn walk_stmt(&mut self, stmt: &ThreadedStmt) {
        match stmt {
            ThreadedStmt::Bind {
                target,
                source,
                op,
                shadow_write,
                span,
            } => {
                self.check_use(source, *span);
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
                // ADR 0118 phase 5a: `BindOp::Put` only — a
                // `BindOp::Direct` rebind (`emit_class_var_result_unwrap`'s
                // inherited-self-dispatch/loop-construct rebind,
                // `rebind_class_vars_from_doc`) is never itself a
                // shadow-write producer regardless of `shadow_write`'s
                // value (its own doc comments: the underlying mutation was
                // already shadow-written by the callee's own `Put` under
                // the identical `ClassSelf`-tagged key) — `render_bind`'s
                // `BindOp::Put` arm is the only place the ADR 0110 shadow
                // write is even constructed, so `shadow_write` is inert for
                // `Direct`. `construct_and_verify_class_var_bind`'s own
                // isolated check exempts `Direct` via a fixture-only
                // wrapping trick (never part of the returned `Bind` node —
                // see its `needs_wrap`), which a same-class self-send's
                // real `Bind`, spliced directly into a jointly-verified
                // body, does not go through — so this joint check must
                // exclude `Direct` explicitly, matching that exemption.
                if matches!(target.prefix, VersionPrefix::ClassVars)
                    && matches!(op, BindOp::Put { .. })
                    && *self.shadow_write_eligible_stack.last().unwrap()
                    && !*shadow_write
                    && self.has_class_vars_nlr
                {
                    self.errors.push(VerifyError::ShadowWriteMissing {
                        mutated: target.clone(),
                        at: *span,
                    });
                }
            }
            ThreadedStmt::Threaded {
                mode,
                frame,
                shadow_write_eligible,
                body,
                produces,
                span: _,
            } => {
                // ADR 0111 Addendum 9, Question 1: `shadow_write_eligible`
                // pushes/pops in lockstep with `frame`/`mode`, AND-combined
                // with the parent's current top.
                self.frame_stack.push(*frame);
                self.mode_stack.push(mode.clone());
                self.shadow_write_eligible_stack.push(
                    *self.shadow_write_eligible_stack.last().unwrap() && *shadow_write_eligible,
                );
                self.walk(body);
                for v in produces {
                    self.check_use(v, Span::default());
                }
                self.shadow_write_eligible_stack.pop();
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            ThreadedStmt::ConditionalLoop {
                mode,
                frame,
                shadow_write_eligible,
                condition,
                condition_value,
                body,
                produces,
                span: _,
                ..
            } => {
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
                //
                // ADR 0111 Addendum 9, Question 1: `shadow_write_eligible`
                // pushes/pops in lockstep with `frame`/`mode`, AND-combined
                // with the parent's current top.
                self.frame_stack.push(*frame);
                self.mode_stack.push(mode.clone());
                self.shadow_write_eligible_stack.push(
                    *self.shadow_write_eligible_stack.last().unwrap() && *shadow_write_eligible,
                );
                self.walk(condition);
                if let ValueRef::Version(v) = condition_value {
                    self.check_use(v, Span::default());
                }
                self.walk(body);
                for v in produces {
                    self.check_use(v, Span::default());
                }
                self.shadow_write_eligible_stack.pop();
                self.mode_stack.pop();
                self.frame_stack.pop();
            }
            // Opaque by design: `NlrCatch` has no body field (`render`
            // treats the rest of the slice as its body); a `Statement` is
            // ordinary AST-directed codegen with no state-threading content
            // of its own (see the variant's doc comment).
            ThreadedStmt::NlrCatch { .. } | ThreadedStmt::Statement(..) => {}
            ThreadedStmt::Return(value, state, span) => {
                self.check_use(state, *span);
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

impl CoreErlangGenerator {
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
        self.add_codegen_warning(
            Diagnostic::error(format!("internal: {invariant_label}: {errors:?}"), span)
                .with_category(DiagnosticCategory::Type),
        );
    }
}
