// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostics and list-op analysis predicates for state-threaded loop and
//! fold bodies.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! split out of `control_flow/mod.rs`, no logic changes.

use super::super::threaded_ir::{StateAccFallbackReason, VersionPrefix, canonical_families};
use super::super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, block_analysis};
use super::plan::ThreadingPlan;
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

// ─── ADR 0122: unified storage-family detector ─────────────────────────────
//
// ADR 0122 Decision 2's canonical slot order (`State`, then `SelfVt`) is
// `threaded_ir::FAMILY_SLOT_ORDER`, applied by `threaded_ir::canonical_families`.

/// ADR 0122 Decision 2: the storage families a construct body mutates, in
/// canonical slot order. Built by [`CoreErlangGenerator::body_threaded_families`]
/// (or, for a caller that already has its own per-family answers, by
/// [`Self::from_matches`]) — never assembled by hand, so the slot order
/// invariant can't drift per call site.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(in crate::core_erlang) struct ThreadedFamilies(Vec<VersionPrefix>);

impl ThreadedFamilies {
    /// Builds a `ThreadedFamilies` from an arbitrary-order set of matched
    /// families, re-sorting into `threaded_ir::FAMILY_SLOT_ORDER` (`State`,
    /// `SelfVt`) — the shared normalization every constructor (this
    /// one included) routes through, so canonical order is a property of the
    /// TYPE, not of caller discipline.
    pub(in crate::core_erlang) fn from_matches(matches: &[VersionPrefix]) -> Self {
        Self(canonical_families(matches))
    }

    /// Whether `prefix` is one of the families this body mutates.
    pub(in crate::core_erlang) fn contains(&self, prefix: &VersionPrefix) -> bool {
        self.0.contains(prefix)
    }

    /// The families, in canonical slot order — the raw slice
    /// [`super::family_slots::append_family_slots`]/`extract_family_slots`
    /// iterate over to build/unpack a construct's trailing tuple slots.
    /// `pub(in crate::core_erlang)` (not private) so that sibling module can
    /// walk the list without a second, hand-duplicated copy of
    /// `threaded_ir::FAMILY_SLOT_ORDER`'s ordering guarantee — [`Self::from_matches`]
    /// is still the only constructor that can PRODUCE a `ThreadedFamilies`,
    /// so this is a read-only view, never a way to build one out of order.
    ///
    /// BT-3512 (ADR 0122 Phase 3): also read directly by
    /// `value_type_codegen.rs`'s value-type/class-method Letrec loop
    /// extraction (`extract_vt_loop_family_slot`'s `.first()`,
    /// `emit_vt_loop_open_extraction`'s empty check) — that site's trailing
    /// slot is always at most one family, so it reads the slice directly
    /// rather than going through [`super::family_slots::extract_family_slots`].
    pub(in crate::core_erlang) fn as_slice(&self) -> &[VersionPrefix] {
        &self.0
    }
}

/// BT-3827: the `Foldl*` selectors whose value-type field-writing bodies thread
/// `Self` through the fold accumulator (see
/// [`CoreErlangGenerator::fold_threads_value_self`]). Any other fold selector
/// still rejects a field write in its body, in every position; widening this
/// list means opting that kind's emitter into
/// `ThreadingPlan::with_value_self_slot` and wrapping its accumulator with
/// `FoldFamilySlots`.
pub(in crate::core_erlang) const SELF_THREADING_FOLDS: &[&str] =
    &["do:", "collect:", "inject:into:"];

/// BT-3510 differential test: one [`super::plan::ThreadingPlan::new_impl`]
/// call's OLD (still-live, unchanged) `threads_value_self`
/// answer, alongside what the NEW recursive
/// [`CoreErlangGenerator::body_threaded_families`] answers for the exact same
/// body — recorded by real compiles (`generate_module` over the stdlib +
/// bootstrap-test corpus) rather than by hand-building a `CoreErlangGenerator`
/// per corpus construct, so the recorded context (`context`,
/// `in_class_method`) is always the real one the compiler itself built.
#[cfg(test)]
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct FamilyDetectorDiffRecord {
    /// `"letrec"` (`allow_direct_params`) or `"foldl"` — which of
    /// `ThreadingPlan::new_impl`'s two shapes this plan is.
    pub(in crate::core_erlang) shape: &'static str,
    /// The loop/fold body's own span — printed by the differential test so
    /// a mismatch is reviewable against source.
    pub(in crate::core_erlang) span: beamtalk_core::source_analysis::Span,
    pub(in crate::core_erlang) old_threads_value_self: bool,
    pub(in crate::core_erlang) new_families: ThreadedFamilies,
}

#[cfg(test)]
thread_local! {
    /// Cleared by the differential test before compiling each corpus file,
    /// populated by `ThreadingPlan::new_impl` (test builds only — see
    /// [`FamilyDetectorDiffRecord`]'s doc comment), read back after.
    pub(in crate::core_erlang) static FAMILY_DETECTOR_DIFF_LOG:
        std::cell::RefCell<Vec<FamilyDetectorDiffRecord>> =
        const { std::cell::RefCell::new(Vec::new()) };
}

/// Which kind of loop/fold a [`CoreErlangGenerator::nested_loop_or_fold_body`]
/// match is: only the `Letrec` shapes thread a value-type `Self` through
/// their own recursive tail call (see
/// [`CoreErlangGenerator::nested_loop_lost_value_self_mutation`]).
#[derive(Clone, Copy, PartialEq, Eq)]
enum NestedLoopShape {
    /// `whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/`to:by:do:` — the
    /// `BodyKind::Letrec` shapes.
    Letrec,
    /// `do:`/`collect:`/`select:`/... — the `BodyKind::Foldl*` shapes.
    Foldl,
}

impl CoreErlangGenerator {
    /// ADR 0122 Decision 1: the storage families [`Self::body_threaded_families`]
    /// may ask about in the CURRENT generator context, derived once here from
    /// [`CodeGenContext`] + [`Self::in_class_method`] rather than hand-listed
    /// at each call site — a hand-listed subset is exactly how BT-3489 and
    /// BT-3506 were missed (ADR 0122 §"Why the gaps keep happening").
    ///
    /// * `State` (an Actor's own scratch map) — an ACTOR INSTANCE method
    ///   only (`Actor` context, not [`Self::in_class_method`]).
    /// * `SelfVt` — a `ValueType` INSTANCE method only (`ValueType` context,
    ///   not [`Self::in_class_method`]).
    ///
    /// Class methods thread no family at all: class variables live in the
    /// class process (ADR 0130 §3).
    ///
    /// Mutually exclusive by construction: `context` alone decides `State`
    /// vs. `SelfVt`, so at most one family is ever eligible (none in `Repl`
    /// context, which is never `ValueType`/`Actor`).
    pub(in crate::core_erlang) fn eligible_families(&self) -> Vec<VersionPrefix> {
        let mut eligible = Vec::with_capacity(1);
        if matches!(self.context, CodeGenContext::Actor) && !self.in_class_method() {
            eligible.push(VersionPrefix::State);
        }
        if matches!(self.context, CodeGenContext::ValueType) && !self.in_class_method() {
            eligible.push(VersionPrefix::SelfVt);
        }
        eligible
    }

    /// ADR 0122 Decision 4: the ONE place "what counts as a mutation" is
    /// answered, per family — never re-derived at a call site:
    ///
    /// * `SelfVt` — a bare value-type field write (`self.field := ...`
    ///   outside a class method); the shape
    ///   [`Self::find_value_self_mutating_stmt`] and
    ///   `value_type_codegen::is_vt_self_field_assignment` each separately
    ///   checked.
    /// * `State` — no shape of its own: an Actor instance method's
    ///   `StateAcc` threads unconditionally, never as a function of the
    ///   body's shape (ADR 0122 "`State` already fits"), so
    ///   [`Self::eligible_families`] alone decides it and this always
    ///   answers `true` once asked (see
    ///   [`Self::body_threaded_families`]'s use of this).
    /// * `Local`/`Gensym` — not a storage family this detector answers for;
    ///   always `false`.
    ///
    /// `pub(in crate::core_erlang)` (not private) so
    /// `value_type_codegen::is_vt_self_field_assignment` — outside this
    /// module — delegates to the same one place instead of keeping its own
    /// copy of the `SelfVt` shape.
    pub(in crate::core_erlang) fn is_family_mutation(
        &self,
        prefix: &VersionPrefix,
        expr: &Expression,
    ) -> bool {
        match prefix {
            VersionPrefix::State => true,
            VersionPrefix::SelfVt => {
                !self.in_class_method()
                    && matches!(self.context, CodeGenContext::ValueType)
                    && self.is_field_assignment(expr)
            }
            VersionPrefix::Local(_) | VersionPrefix::Gensym(_) => false,
        }
    }

    /// ADR 0122 Decision 1: the unified, recursive storage-family detector —
    /// answers "does `body` mutate family `X`", for every `X` in `eligible`
    /// (see [`Self::eligible_families`]), by walking every top-level
    /// statement all the way down — including nested block bodies
    /// ([`beamtalk_core::ast_walker::walk_expression`]'s descend-into-blocks
    /// behavior) — unlike the narrower, top-level-only walks this is
    /// differential-tested against
    /// ([`Self::find_value_self_mutating_stmt`]/
    /// `value_type_codegen::is_vt_self_field_assignment`'s own callers).
    ///
    /// A site that cannot carry a mutation found below its own top level
    /// rejects it via the existing shared rejection functions
    /// ([`Self::reject_unthreadable_value_self_field_write`]) — detection and carry-capability
    /// are deliberately separate questions (ADR 0122 §Decision 1).
    ///
    /// BT-3522 wired the first live emission consumer:
    /// `exception_handling.rs`'s `exception_construct_families` (shared with
    /// `value_type_codegen.rs`'s VT-conditional and `on:do:`/`ensure:`
    /// consumer sites). The loop/`Foldl*` sites still run the older
    /// top-level-only walks, differential-tested against this one by
    /// `tests::control_flow::family_detector_differential`.
    pub(in crate::core_erlang) fn body_threaded_families(
        &self,
        body: &beamtalk_core::ast::Block,
        eligible: &[VersionPrefix],
    ) -> ThreadedFamilies {
        let matches: Vec<VersionPrefix> = eligible
            .iter()
            .filter(|prefix| {
                // `State` needs no body walk at all — see
                // `Self::is_family_mutation`'s doc comment.
                matches!(prefix, VersionPrefix::State)
                    || body.body.iter().any(|stmt| {
                        let mut hit = false;
                        beamtalk_core::ast_walker::walk_expression(&stmt.expression, &mut |e| {
                            if self.is_family_mutation(prefix, e) {
                                hit = true;
                            }
                        });
                        hit
                    })
            })
            .cloned()
            .collect();
        ThreadedFamilies::from_matches(&matches)
    }

    /// Emits a codegen diagnostic for the calling convention chosen for a loop.
    ///
    /// Reports which optimization mode was selected (direct-params, tuple-acc, hybrid,
    /// or `StateAcc` fallback with reason). Also emits a large-arity warning when >8 params
    /// are extracted. Gated by `BEAMTALK_CODEGEN_DIAGNOSTICS=1`.
    pub(in crate::core_erlang) fn emit_loop_convention_diagnostic(
        &mut self,
        plan: &ThreadingPlan,
        span: Span,
    ) {
        if !self.codegen_diagnostics_enabled {
            return;
        }
        let line_info = self
            .span_to_line(span)
            .map_or(String::new(), |l| format!(" at line {l}"));
        let n_locals = plan.threaded_locals.len();
        let n_readonly = plan.readonly_fields.len();
        let convention = plan.convention_label();

        if matches!(plan.fallback_reason, StateAccFallbackReason::None) {
            // Optimized convention chosen
            let detail = match convention {
                "direct-params" => {
                    format!("{n_locals} locals, 0 field mutations")
                }
                "tuple-acc" => {
                    format!("{n_locals} locals in tuple accumulator")
                }
                "hybrid" => {
                    format!("{n_locals} locals + {n_readonly} read-only fields as direct params")
                }
                _ => String::new(),
            };
            self.emit_codegen_diagnostic(
                format!("Loop{line_info}: using {convention} ({detail})"),
                span,
            );
        } else {
            // StateAcc fallback
            let reason = &plan.fallback_reason;
            self.emit_stateacc_fallback_diagnostic(
                format!("Loop{line_info}: StateAcc fallback — {reason}"),
                span,
            );
        }

        // Large extracted arity diagnostic (>8 direct fun params)
        let total = plan.total_extracted_params();
        if total > 8 && (plan.use_direct_params || plan.use_hybrid_params) {
            self.emit_codegen_diagnostic(
                format!("Loop{line_info}: {total} extracted params"),
                span,
            );
        }
    }

    /// whether a Letrec loop body threads a value-type `Self`
    /// mutation (`self.field := ...` in [`CodeGenContext::ValueType`])
    /// through the loop's own recursive tail call. Deliberately narrow
    /// (top-level-statement-only): an extra `letrec` fun parameter can only
    /// carry a rebind that the loop body's own STATEMENT sequence actually
    /// produces, never one scoped inside some larger sub-expression's own
    /// nested `let`.
    ///
    /// Excludes class methods: inside a class method `self.x :=` is a
    /// CLASS-var write, which lives in the class process (ADR 0130) and is
    /// never threaded.
    ///
    /// Shared by [`ThreadingPlan::new_impl`] (which turns it into
    /// `ThreadingPlan::threads_value_self`) and the value-type loop-open
    /// consumers in `value_type_codegen.rs`, so the routing decision and the
    /// tuple-shape decision can never independently drift out of sync
    /// (CLAUDE.md's no-duplicate-implementations rule).
    ///
    /// **Accepted scope limit, inherited from that same narrowing:** a field
    /// write buried inside a NESTED construct in the loop body — most
    /// notably `1 to: n do: [:i | flag ifTrue: [self.total := ...]]` — is not
    /// a top-level statement, so this returns `false` and the loop threads no
    /// `Self`; the conditional's own `Self{N}` rebind stays scoped to its own
    /// nested `let`. Widening the predicate instead produced real
    /// unbound-variable regressions, so that shape remains deliberately
    /// unsupported.
    ///
    /// What changed is only what happens to it *after* this returns `false`:
    /// it used to compile to a silently-dropped mutation (or, with no sibling
    /// local mutation to thread, an `erlc` `unbound variable 'State'` crash),
    /// and is now rejected at compile time by
    /// [`Self::reject_unthreadable_value_self_field_write`] with the same
    /// diagnostic.
    pub(in crate::core_erlang) fn loop_body_threads_value_self(
        &self,
        body: &beamtalk_core::ast::Block,
    ) -> bool {
        self.find_value_self_mutating_stmt(body).is_some()
    }

    /// BT-3827: whether `expr` is a fold whose literal body writes a value-type
    /// field and which carries that `Self` through its accumulator
    /// ([`ThreadingPlan::with_value_self_slot`], [`super::family_slots::FoldFamilySlots`]).
    ///
    /// The ONE place the set of such folds is named ([`SELF_THREADING_FOLDS`]):
    /// the fold emitters opt their plan in, and the value-type consumers
    /// ([`Self::vt_loop_threaded_families`](CoreErlangGenerator::vt_loop_threaded_families),
    /// the `do:`/fold-op predicates) read the family off the construct's tuple
    /// by this predicate, so a producer and its consumer cannot disagree about
    /// whether the result tuple has a `Self` slot. Every other fold keeps the
    /// structured rejection
    /// ([`Self::reject_unthreadable_value_self_field_write`]).
    pub(in crate::core_erlang) fn fold_threads_value_self(&self, expr: &Expression) -> bool {
        let Some((selector, body)) =
            beamtalk_core::semantic_analysis::block_facts::producer_body_block(expr)
        else {
            return false;
        };
        SELF_THREADING_FOLDS.contains(&selector.as_str()) && self.loop_body_threads_value_self(body)
    }

    /// BT-3827: runs `f` (compiling the fold `expr`) as a consumer that unpacks
    /// the fold's trailing `Self` slot, so [`Self::value_self_fold_slot`] lets
    /// the fold's emitter thread it. Every site that compiles a fold and then
    /// reads element 3 of its result (`extract_vt_loop_family_slot`) goes
    /// through here; a fold compiled anywhere else is refused.
    pub(in crate::core_erlang) fn with_fold_self_slot_consumer<T>(
        &mut self,
        expr: &Expression,
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let body = beamtalk_core::semantic_analysis::block_facts::producer_body_block(expr)
            .map(|(_, body)| body.span);
        let saved = std::mem::replace(&mut self.fold_self_slot_consumer, body);
        let result = f(self);
        self.fold_self_slot_consumer = saved;
        result
    }

    /// BT-3827: whether the fold with body `body` threads `Self` through its
    /// accumulator: it writes a field (the Letrec loops' detector,
    /// [`Self::loop_body_threads_value_self`]) AND a consumer that unpacks the
    /// result's `Self` slot is compiling it
    /// ([`Self::with_fold_self_slot_consumer`]). A fold that writes a field with no
    /// such consumer is the structured rejection, not a dropped write: its
    /// result tuple's `Self` slot would reach nobody (a stored block, an
    /// evaluated block's body).
    pub(in crate::core_erlang) fn value_self_fold_slot(
        &self,
        body: &beamtalk_core::ast::Block,
    ) -> Result<bool> {
        let Some(write) = self.find_value_self_mutating_stmt(body) else {
            return Ok(false);
        };
        if self.fold_self_slot_consumer == Some(body.span) {
            return Ok(true);
        }
        let field = self.field_assignment_name(write).unwrap_or_default();
        Err(CodeGenError::field_assignment_in_unsupported_block(
            field,
            self.location_label(write.span()),
        ))
    }

    /// Shared predicate behind [`Self::loop_body_threads_value_self`] and
    /// [`Self::nested_loop_lost_value_self_mutation`] — returns
    /// the first top-level statement of `body` that is a value-type
    /// `self.field := ...` write, or `None` if there isn't one; see
    /// [`Self::loop_body_threads_value_self`] for why it is deliberately
    /// top-level-only.
    fn find_value_self_mutating_stmt<'a>(
        &self,
        body: &'a beamtalk_core::ast::Block,
    ) -> Option<&'a Expression> {
        if self.in_class_method() || !matches!(self.context, CodeGenContext::ValueType) {
            return None;
        }
        let filtered_body = super::super::util::collect_body_exprs(&body.body);
        filtered_body
            .into_iter()
            .find(|expr| self.is_family_mutation(&VersionPrefix::SelfVt, expr))
    }

    /// Rejects a value-type `self.field := ...` write in loop-body
    /// statement `expr` that the enclosing loop **cannot** thread out, with
    /// the SAME
    /// [`CodeGenError::FieldAssignmentInUnsupportedBlock`](super::super::CodeGenError::FieldAssignmentInUnsupportedBlock)
    /// the identical class-var shape already produces.
    ///
    /// A value-type field write mints its own `Self{N}` version chain
    /// (`VersionPrefix::SelfVt`). A `Letrec` loop carries that chain
    /// through its own recursive tail call, but only for a write that is
    /// a BARE, TOP-LEVEL STATEMENT of the loop body — exactly the shape
    /// [`Self::loop_body_threads_value_self`] (and hence
    /// `ThreadingPlan::threads_value_self`, passed in here as
    /// `threads_value_self`) reports. Every other value-type write reachable
    /// from a loop body binds a `Self{N}` that dies with the nested scope it
    /// was minted in:
    ///
    /// * a write nested inside a conditional branch (or any other nested
    ///   block) of a `Letrec` body — the shape this issue is named for;
    /// * ANY write in the body of a `Foldl*` fold [`SELF_THREADING_FOLDS`] does
    ///   not name (`select:`/`reject:`/`detect:`/…), top-level included: its
    ///   accumulator has no `Self` slot, so `threads_value_self` is `false`
    ///   there. (`do:`/`collect:`/`inject:into:` carry `Self` through a
    ///   `{Acc, Self}` wrapper, BT-3827, and thread a top-level write like a
    ///   Letrec does; a write nested below their top level is still rejected.)
    ///
    /// Left un-rejected, those shapes either silently dropped the mutation or
    /// crashed `erlc` outright (`unbound variable 'State'`, from the pack
    /// prefix short-circuiting to the ambient actor `State` a value-type
    /// method does not have — confirmed empirically for the headline repro
    /// and for both `Foldl*` shapes on the parent commit). This check
    /// rejects them with a clear error through the shared
    /// [`CodeGenError::field_assignment_in_unsupported_block`](super::super::CodeGenError::field_assignment_in_unsupported_block)
    /// constructor.
    ///
    /// Ordered AFTER [`Self::nested_loop_lost_value_self_mutation`] at both
    /// call sites: a nested `Letrec` loop whose own body has a top-level write
    /// matches both, and that one's more specific
    /// `ValueSelfMutationLostAcrossNestedLoop` message wins.
    ///
    /// Walks with the shared `beamtalk_core::ast_walker::walk_expression`
    /// (descends into nested block bodies) rather than a second hand-rolled
    /// `Expression` match, so a future AST variant cannot silently hide a
    /// write from this check. The walk deliberately depends on neither the
    /// walker's traversal ORDER (the root is skipped by `std::ptr::eq`, not by
    /// "visited first") nor on its own copy of the field-write SHAPE (the name
    /// comes from `field_assignment_name`, the same helper backing
    /// `is_field_assignment`).
    ///
    /// Shaped as a `Result`-returning rejection helper (rather than a
    /// predicate each call site turns into an error itself): it is the single
    /// place that turns a positive match into the diagnostic, so the `Letrec`
    /// and `Foldl*` call sites cannot drift out of sync (CLAUDE.md's
    /// no-duplicate-implementations rule).
    pub(in crate::core_erlang) fn reject_unthreadable_value_self_field_write(
        &self,
        expr: &Expression,
        threads_value_self: bool,
    ) -> Result<()> {
        self.reject_unthreadable_value_self_field_write_outside(expr, threads_value_self, &[])
    }

    /// BT-3912 (BT-3854): [`Self::reject_unthreadable_value_self_field_write`]
    /// for a statement of a value-type conditional's arm. The arm is a frame whose
    /// loop and fold producers carry `Self` (`fold_threads_value_self`: the fold's
    /// result tuple has a `Self` slot the producer's prelude merges into the arm's
    /// own `Self` chain), so a field write inside the body of such a fold at a
    /// producer site of the statement is carried and not rejected; one anywhere
    /// else in the statement still is.
    pub(in crate::core_erlang) fn reject_unthreadable_value_self_field_write_in_arm(
        &self,
        stmt: &Expression,
    ) -> Result<()> {
        let carried: Vec<Span> =
            beamtalk_core::semantic_analysis::block_facts::loop_body_producer_sites(stmt)
                .into_iter()
                .filter(|site| self.fold_threads_value_self(site))
                .filter_map(|site| {
                    beamtalk_core::semantic_analysis::block_facts::producer_body_block(site)
                        .map(|(_, body)| body.span)
                })
                .collect();
        self.reject_unthreadable_value_self_field_write_outside(
            stmt,
            self.is_field_assignment(stmt),
            &carried,
        )
    }

    /// The walk behind both: a field write inside one of the `carried` spans
    /// (block bodies whose lowering threads `Self` itself) is not a finding.
    fn reject_unthreadable_value_self_field_write_outside(
        &self,
        expr: &Expression,
        threads_value_self: bool,
        carried: &[Span],
    ) -> Result<()> {
        if self.in_class_method() || !matches!(self.context, CodeGenContext::ValueType) {
            return Ok(());
        }
        let mut found: Option<String> = None;
        beamtalk_core::ast_walker::walk_expression(expr, &mut |e| {
            if found.is_some() {
                return;
            }
            let span = e.span();
            if carried
                .iter()
                .any(|c| c.start() <= span.start() && span.end() <= c.end())
            {
                return;
            }
            // The loop's own tail call carries a top-level statement write
            // — but nothing deeper, including one buried in this
            // very statement's own right-hand side.
            //
            // The root is identified by POINTER IDENTITY rather than by
            // "first node visited": `walk_expression` is pre-order today, but
            // that is a doc-comment promise from another crate, and a switch
            // to post-order would otherwise move this skip silently onto some
            // unrelated child — re-admitting the erlc crash this check exists
            // to prevent. `std::ptr::eq` cannot drift that way.
            if threads_value_self && std::ptr::eq(e, expr) {
                return;
            }
            // Name comes from the same helper that decides whether this IS a
            // field write, so the predicate and the diagnostic cannot disagree.
            if let Some(field) = self.field_assignment_name(e) {
                found = Some(field.to_string());
            }
        });
        let Some(field) = found else {
            return Ok(());
        };
        Err(CodeGenError::field_assignment_in_unsupported_block(
            &field,
            self.location_label(expr.span()),
        ))
    }

    /// If `expr` is itself a nested Letrec-shaped loop whose own body would thread a value-type
    /// `Self` mutation through its own recursive tail call, returns a short
    /// description of that mutation for
    /// [`CodeGenError::ValueSelfMutationLostAcrossNestedLoop`](super::super::CodeGenError::ValueSelfMutationLostAcrossNestedLoop)'s
    /// message.
    ///
    /// Same deliberate scope limit, for the same reason: nothing unpacks a
    /// nested loop's own trailing `Self` tuple slot back into the enclosing
    /// loop body's statement sequence, so the inner loop's mutation would be
    /// silently discarded. Rejecting it cleanly is the chosen behaviour;
    /// making arbitrary nesting work is explicitly out of scope.
    ///
    /// Only the `Letrec` shape is checked: a nested `Foldl*` body's
    /// value-type field write is rejected instead, via
    /// [`Self::reject_unthreadable_value_self_field_write`], which runs
    /// immediately after this check at both call sites (BT-3827: a nested
    /// fold's `Self` slot is unpacked by no enclosing body statement either).
    pub(super) fn nested_loop_lost_value_self_mutation(&self, expr: &Expression) -> Option<String> {
        let (body, shape) = Self::nested_loop_or_fold_body(expr)?;
        if !matches!(shape, NestedLoopShape::Letrec) {
            return None;
        }
        let mutating_stmt = self.find_value_self_mutating_stmt(body)?;
        let Expression::Assignment { target, .. } = mutating_stmt else {
            return None;
        };
        let Expression::FieldAccess { field, .. } = target.as_ref() else {
            return None;
        };
        Some(format!("field 'self.{}'", field.name))
    }

    /// Canonical "selector → body-block-argument position" table.
    /// Shared by every "given a keyword-selector `MessageSend`, extract its
    /// loop/fold body block" call site in this module
    /// ([`Self::nested_loop_or_fold_body`],
    /// [`Self::collect_list_op_cross_scope_mutations`],
    /// [`Self::list_op_needs_stateacc_fallback`],
    /// [`Self::expr_has_nested_counted_loop_threading`]) — before this, each
    /// independently re-matched selector strings against
    /// `arguments.first()`/`arguments.last()`/`arguments[N]`, and could
    /// silently drift out of sync with each other.
    ///
    /// This is the canonical/maximal selector set: the `BodyKind::Letrec`
    /// shapes (`whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/
    /// `to:by:do:`, see this module's `//!` doc comment) plus the
    /// `BodyKind::Foldl*` shapes (`do:`/`collect:`/`select:`/`reject:`/
    /// `anySatisfy:`/`allSatisfy:`/`inject:into:`/`detect:`/`count:`/
    /// `takeWhile:`/`dropWhile:`/`partition:`/`groupBy:`) — matching
    /// [`Self::nested_loop_or_fold_body`]'s coverage, the most
    /// complete of the four (it alone also includes the predicate-based
    /// shapes). `detect:ifNone:` is intentionally excluded: its second
    /// (`ifNone:`) block argument is a separate, not-yet-analyzed risk
    /// surface no call site here attempts to cover.
    ///
    /// [`Self::list_op_needs_stateacc_fallback`]/
    /// [`Self::collect_list_op_cross_scope_mutations`]/
    /// [`Self::expr_has_nested_counted_loop_threading`] each cover only a
    /// narrower subset of this table (their own deliberate optimization
    /// scopes, verified against each site's pre-refactor selector list
    /// rather than broadened here) — they filter the returned selector
    /// down to their own subset after calling this.
    ///
    /// Takes the already-extracted `sel` (concatenated keyword parts, e.g.
    /// `"to:by:do:"`) and `arguments` rather than the raw `Expression`, so
    /// callers that already destructured a `MessageSend` for their own
    /// purposes (e.g. reading `receiver` for `ensure:`/`on:do:` handling)
    /// don't have to re-match. Does not itself unwrap parens or an
    /// assignment RHS — callers that need that (e.g.
    /// [`Self::nested_loop_or_fold_body`]'s `unwrap_parens`,
    /// [`Self::expr_has_nested_counted_loop_threading`]'s assignment-RHS
    /// unwrap) do it before calling, matching each site's pre-existing
    /// behavior exactly.
    ///
    /// Position eligibility is delegated to
    /// `beamtalk_core::ast::is_loop_or_fold_block_arg` — the single
    /// source of truth for this NARROWER loop/fold-shape table (deliberately
    /// not the broader
    /// `beamtalk_core::state_threading_selectors::is_state_threaded_block_arg`
    /// canonical table shared by `threaded_locals_of` and
    /// `beamtalk-lint`'s `DeadAssignment` check — see that function's doc
    /// comment for why). That table also covers `ifTrue:`/`ifFalse:`/
    /// `ifTrue:ifFalse:` (threaded via dedicated codegen elsewhere, not this
    /// loop/fold table) — explicitly excluded below so a conditional's
    /// block is never misclassified as a nested loop/fold body by this
    /// function's callers (e.g. [`Self::nested_loop_or_fold_body`], which
    /// calls this with whatever keyword selector it finds, unfiltered).
    fn block_arg_for_selector<'a>(
        sel: &str,
        arguments: &'a [Expression],
    ) -> Option<&'a beamtalk_core::ast::Block> {
        if matches!(sel, "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:") {
            return None;
        }
        arguments.iter().enumerate().find_map(|(idx, arg)| {
            if beamtalk_core::ast::is_loop_or_fold_block_arg(sel, idx) {
                match arg {
                    Expression::Block(block) => Some(block),
                    _ => None,
                }
            } else {
                None
            }
        })
    }

    /// Extracts the body block of `expr`, and which family it belongs to,
    /// if it is a nested loop/fold send. Selector coverage and argument
    /// position come from [`Self::block_arg_for_selector`]; the returned
    /// [`NestedLoopShape`] tells
    /// [`Self::nested_loop_lost_value_self_mutation`] whether the construct
    /// is one that threads a value-type `Self` through its tail call.
    fn nested_loop_or_fold_body(
        expr: &Expression,
    ) -> Option<(&beamtalk_core::ast::Block, NestedLoopShape)> {
        use beamtalk_core::ast::MessageSelector;
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr.unwrap_parens()
        else {
            return None;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let block = Self::block_arg_for_selector(&sel, arguments)?;
        let shape = match sel.as_str() {
            "whileTrue:" | "whileFalse:" | "timesRepeat:" | "to:do:" | "to:by:do:" => {
                NestedLoopShape::Letrec
            }
            _ => NestedLoopShape::Foldl,
        };
        Some((block, shape))
    }

    /// Emits a diagnostic for synchronous self-send detected in a loop body.
    pub(super) fn emit_self_send_in_loop_diagnostic(&mut self, expr: &Expression, span: Span) {
        if !self.codegen_diagnostics_enabled {
            return;
        }
        // Extract selector name from the message send
        if let Expression::MessageSend { selector, .. } = expr {
            let sel_name = selector.name().to_string();
            let line_info = self
                .span_to_line(span)
                .map_or(String::new(), |l| format!(" at line {l}"));
            self.emit_codegen_diagnostic(
                format!(
                    "Self-send 'self {sel_name}' inside loop{line_info}: \
                     synchronous call to own mailbox, potential deadlock"
                ),
                span,
            );
        }
    }

    /// Returns `true` if `expr` is an inline conditional (`ifTrue:` / `ifFalse:` /
    /// `ifTrue:ifFalse:`) whose block argument writes to at least one variable in `threaded`.
    ///
    /// This catches the "pure-overwrite" pattern like `each > max ifTrue: [max := each]`
    /// where `max` is in `threaded` but the inner block's `captured_reads` is empty
    /// (no read-before-write), so `control_flow_has_mutations` returns false even
    /// though we must thread `max` through `StateAcc`.
    pub(super) fn inline_conditional_writes_threaded(
        expr: &Expression,
        threaded: &[String],
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
    ) -> bool {
        use beamtalk_core::ast::MessageSelector;
        if let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            if beamtalk_core::state_threading_selectors::is_conditional_selector(sel.as_str()) {
                for arg in arguments {
                    if let Expression::Block(block) = arg {
                        let analysis = facts
                            .block_profile(&block.span)
                            .cloned()
                            .unwrap_or_else(|| block_analysis::analyze_block(block));
                        if analysis.local_writes.iter().any(|v| threaded.contains(v)) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    /// Collects variables that are captured and mutated by nested list op blocks.
    ///
    /// Scans a body expression for list op message sends (do:, collect:, etc.) with literal
    /// blocks, and adds any variables that are captured from the outer scope and written
    /// inside the block to `out`. These variables need threading through the outer loop.
    #[allow(clippy::too_many_lines)]
    pub(in crate::core_erlang) fn collect_list_op_cross_scope_mutations(
        expr: &Expression,
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
        out: &mut std::collections::HashSet<String>,
    ) {
        use beamtalk_core::ast::MessageSelector;
        // BT-3841: an evaluated literal block (`[...] value`) is an inline arm
        // like a conditional's: a list-op's cross-scope write hides behind it
        // the same way, so recurse through its statements.
        if let Some(block) =
            beamtalk_core::semantic_analysis::block_facts::literal_block_value_arm(expr)
        {
            for stmt in &block.body {
                Self::collect_list_op_cross_scope_mutations_recursive(&stmt.expression, facts, out);
            }
            return;
        }
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();

        // ensure:/on:do:/ifNotNil: aren't list-ops/counted-loops
        // themselves, but one may be nested inside one of their blocks —
        // recurse straight through their block(s) (the receiver for
        // ensure:/on:do:, any block arguments for all three) so a list-op's
        // cross-scope mutation buried behind one of these constructs is
        // still found, instead of stopping here (the previous behavior,
        // which silently dropped such a mutation from the outer loop's own
        // threaded-locals computation).
        // ADR 0131 Phase 4 (BT-3750): and `tryDo:`, whose block is a frame of
        // its own that a list-op's cross-scope write can hide behind.
        if beamtalk_core::state_threading_selectors::is_exception_selector(&sel)
            || beamtalk_core::state_threading_selectors::is_conditional_selector(&sel)
            || beamtalk_core::semantic_analysis::block_facts::is_result_try_do(receiver, &sel)
        {
            let mut blocks: Vec<&beamtalk_core::ast::Block> = Vec::new();
            if beamtalk_core::state_threading_selectors::is_exception_selector(&sel) {
                if let Expression::Block(b) = receiver.as_ref() {
                    blocks.push(b);
                }
            }
            for arg in arguments {
                if let Expression::Block(b) = arg {
                    blocks.push(b);
                }
            }
            for block in blocks {
                // Exclude this wrapping block's own parameters (e.g.
                // `on:do:`'s exception var, `ifNotNil:`'s bound value) and
                // the locals it introduces itself before merging into `out`
                // — mirrors `construct_outer_local_writes`'s block-parameter
                // threading for the identical construct shape. Without
                // this, a nested loop reporting a write to the wrapping
                // block's own param (e.g. `x ifNotNil: [:v | nested do: [:i
                // | v := v + i]]`) or own local (`[i := 0. [i < 3]
                // whileTrue: [i := i + 1]] ensure: [...]`) would be
                // misreported as an outer-scope mutation.
                out.extend(Self::nested_cross_scope_writes_outside(block, facts));
            }
            return;
        }

        // nested counted loops (`timesRepeat:`/`to:do:`/`to:by:do:`)
        // capture and mutate outer locals just like list ops. Including them
        // here makes the *outer* loop's threaded-locals computation see
        // writes buried in an inner counted loop, so the outer loop threads
        // them via StateAcc instead of dropping them.
        // ADR 0131 Phase 2b: every loop and fold `block_arg_for_selector`
        // knows (`whileTrue:`/`whileFalse:`, `detect:`, `count:`, … as well as
        // the list ops and counted loops), not a hand-kept subset of them: a
        // nested construct the enclosing loop's threaded set
        // (`loop_threaded_locals`, the transitive `threaded_block_writes`)
        // includes must also make the enclosing construct *thread* — the two
        // answers can no longer disagree.
        let Some(body_block) = Self::block_arg_for_selector(&sel, arguments) else {
            return;
        };

        let analysis = facts
            .block_profile(&body_block.span)
            .cloned()
            .unwrap_or_else(|| block_analysis::analyze_block(body_block));

        let block_params: std::collections::HashSet<String> = body_block
            .parameters
            .iter()
            .map(|p| p.name.to_string())
            .collect();

        for v in analysis.captured_reads.intersection(&analysis.local_writes) {
            if !block_params.contains(v.as_str()) {
                out.insert(v.clone());
            }
        }

        // A `whileTrue:`/`whileFalse:` condition block is part of the loop and
        // writes outer locals like its body does.
        if matches!(sel.as_str(), "whileTrue:" | "whileFalse:") {
            if let Expression::Block(cond) = receiver.as_ref() {
                let cond_analysis = facts
                    .block_profile(&cond.span)
                    .cloned()
                    .unwrap_or_else(|| block_analysis::analyze_block(cond));
                let cond_params: std::collections::HashSet<String> =
                    cond.parameters.iter().map(|p| p.name.to_string()).collect();
                for v in cond_analysis
                    .captured_reads
                    .intersection(&cond_analysis.local_writes)
                {
                    if !cond_params.contains(v.as_str()) {
                        out.insert(v.clone());
                    }
                }
            }
        }

        // Recurse into the inner block's statements so deeper nesting
        // (a counted/list op nested two or more levels deep) is still detected.
        // `analyze_block` does not propagate writes out of nested non-conditional
        // blocks, so a write buried in a doubly-nested loop is invisible above
        // without this recursion.
        out.extend(Self::nested_cross_scope_writes_outside(body_block, facts));

        // The handler block a producer packs besides its body (`detect:ifNone:`'s
        // `ifNone:`, `do:separatedBy:`'s separator; core's
        // `producer_packed_blocks`, the one rule for what a producer packs)
        // writes outer locals like the body does: an enclosing loop must thread
        // them, or a handler-only write nested in a loop body is dropped
        // (BT-3841: `n timesRepeat: [xs do: [:x | x] separatedBy: [t := t + 1]]`
        // in an actor method answered 0).
        for packed in beamtalk_core::semantic_analysis::block_facts::producer_packed_blocks(expr) {
            if packed.span == body_block.span
                || matches!(receiver.as_ref(), Expression::Block(cond) if cond.span == packed.span)
            {
                continue;
            }
            let handler_analysis = facts
                .block_profile(&packed.span)
                .cloned()
                .unwrap_or_else(|| block_analysis::analyze_block(packed));
            let handler_params: std::collections::HashSet<String> = packed
                .parameters
                .iter()
                .map(|p| p.name.to_string())
                .collect();
            for v in handler_analysis
                .captured_reads
                .intersection(&handler_analysis.local_writes)
            {
                if !handler_params.contains(v.as_str()) {
                    out.insert(v.clone());
                }
            }
            out.extend(Self::nested_cross_scope_writes_outside(packed, facts));
        }
    }

    /// The cross-scope writes found by recursing into `block`'s statements,
    /// minus the names `block` owns: its parameters and the locals it
    /// introduces with a top-level assignment (`k := 0. [k < 3] whileTrue:
    /// [...]` — crossed by the loop nested in it but belonging to neither the
    /// enclosing constructs nor the outer scope). The one rule both the
    /// wrapping-construct and the loop/fold branches of
    /// [`Self::collect_list_op_cross_scope_mutations`] use.
    fn nested_cross_scope_writes_outside(
        block: &beamtalk_core::ast::Block,
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
    ) -> std::collections::HashSet<String> {
        let analysis = facts
            .block_profile(&block.span)
            .cloned()
            .unwrap_or_else(|| block_analysis::analyze_block(block));
        let block_params: std::collections::HashSet<&str> =
            block.parameters.iter().map(|p| p.name.as_str()).collect();
        let introduced = Self::body_introduced_locals(block, &analysis.captured_reads);
        let mut nested = std::collections::HashSet::new();
        for stmt in &block.body {
            Self::collect_list_op_cross_scope_mutations_recursive(
                &stmt.expression,
                facts,
                &mut nested,
            );
        }
        nested.retain(|v| !block_params.contains(v.as_str()) && !introduced.contains(v.as_str()));
        nested
    }

    /// The locals `body` introduces with a top-level assignment and never
    /// reads from outside (`captured_reads` is the body's own profile): decided
    /// from the body alone, as `analyze_block` decides a "new binding", not
    /// from the live scope, so classification does not depend on how far
    /// lowering has got. The one rule behind "this local is the body's own,
    /// not an outer local the construct threads".
    pub(in crate::core_erlang) fn body_introduced_locals<'a>(
        body: &'a beamtalk_core::ast::Block,
        captured_reads: &std::collections::HashSet<String>,
    ) -> std::collections::HashSet<&'a str> {
        body.body
            .iter()
            .filter_map(|stmt| match &stmt.expression {
                Expression::Assignment { target, .. } => match target.as_ref() {
                    Expression::Identifier(id) if !captured_reads.contains(id.name.as_str()) => {
                        Some(id.name.as_str())
                    }
                    _ => None,
                },
                _ => None,
            })
            .collect()
    }

    /// Returns `true` if `expr` is (or wraps, via assignment RHS or parens) a
    /// nested counted loop (`timesRepeat:`/`to:do:`/`to:by:do:`) whose body mutates one
    /// of the outer loop's `threaded_locals`.
    ///
    /// Such an inner loop returns a `{value, StateAcc}` tuple whose `element(2, …)` must
    /// be unpacked to thread the local back out; that is only possible when the outer loop
    /// uses `StateAcc` mode, so the presence of this pattern disqualifies direct-params.
    pub(super) fn expr_has_nested_counted_loop_threading(
        &self,
        expr: &Expression,
        threaded_locals: &[String],
    ) -> bool {
        use beamtalk_core::ast::MessageSelector;

        let inner = match expr.unwrap_parens() {
            Expression::Assignment { value, .. } => value.unwrap_parens(),
            other => other,
        };
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = inner
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        let body_block = match Self::block_arg_for_selector(&sel, arguments) {
            Some(block) if matches!(sel.as_str(), "timesRepeat:" | "to:do:" | "to:by:do:") => block,
            _ => return false,
        };

        // The inner counted loop threads back exactly the outer locals its own body
        // mutates (read+write or write-only). If any of those overlap the threaded set
        // the outer loop must thread, the inner tuple must be unpacked into StateAcc.
        let inner_threaded = self.loop_threaded_locals(body_block, None);
        inner_threaded.iter().any(|v| threaded_locals.contains(v))
    }

    /// Returns `true` if `expr` is a list op (do:, collect:, select:, reject:,
    /// anySatisfy:, allSatisfy:, inject:into:) whose block captures and mutates outer-scope locals but whose inner
    /// block is NOT eligible for tuple-acc optimization.
    ///
    /// When this returns `true`, the list op would fall back to map-accumulator mode which
    /// references `StateAcc` — incompatible with direct-params loops. The outer loop must
    /// fall back to `StateAcc` mode.
    fn list_op_needs_stateacc_fallback(
        expr: &Expression,
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
    ) -> bool {
        use beamtalk_core::ast::MessageSelector;
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();

        // Identify list ops and their body block argument
        let body_block = match Self::block_arg_for_selector(&sel, arguments) {
            Some(block)
                if matches!(
                    sel.as_str(),
                    "do:"
                        | "collect:"
                        | "select:"
                        | "reject:"
                        | "anySatisfy:"
                        | "allSatisfy:"
                        | "inject:into:"
                ) =>
            {
                block
            }
            _ => return false,
        };

        let analysis = facts
            .block_profile(&body_block.span)
            .cloned()
            .unwrap_or_else(|| block_analysis::analyze_block(body_block));

        // Check if inner block captures and mutates outer-scope locals
        let block_params: std::collections::HashSet<String> = body_block
            .parameters
            .iter()
            .map(|p| p.name.to_string())
            .collect();
        let has_cross_scope_mutations = analysis
            .captured_reads
            .intersection(&analysis.local_writes)
            .any(|v| !block_params.contains(v.as_str()));

        if !has_cross_scope_mutations {
            return false;
        }

        // Inner block has cross-scope mutations. Check if tuple-acc would be blocked.
        // These mirror the guards in ThreadingPlan::new_impl for use_tuple_acc.
        if analysis.has_state_effects() {
            // Field mutations — outer direct-params is already blocked by has_state_effects
            // propagation through analyze_block's nested Block handling. But be safe.
            return true;
        }

        // Check for conditional writes to threaded locals within the inner block body
        let inner_threaded: Vec<String> = analysis
            .captured_reads
            .intersection(&analysis.local_writes)
            .filter(|v| !block_params.contains(v.as_str()))
            .cloned()
            .collect();
        for stmt in &body_block.body {
            if Self::inline_conditional_writes_threaded(&stmt.expression, &inner_threaded, facts) {
                return true;
            }
        }

        // Check for destructure as last expression
        if body_block
            .body
            .last()
            .is_some_and(|s| matches!(s.expression, Expression::DestructureAssignment { .. }))
        {
            return true;
        }

        false
    }

    /// ADR 0131 Phase 2b/2c: whether `expr` (through an assignment's value and
    /// parentheses) is a list op over a literal block that captures and
    /// mutates an outer local, is not a local-threading producer
    /// (`producer_body_arg_index` does not list it), and lowers in
    /// map-accumulator mode, which references `StateAcc` — so, like
    /// [`Self::list_op_needs_stateacc_fallback`] for the shapes it covers, it
    /// disqualifies the enclosing loop from direct-params.
    ///
    /// Answers `false` outside the `ValueType` codegen context: that is the
    /// only context with no tuple-accumulator mode (`select_tuple_acc`); an
    /// actor or class-method body does not take this fallback. Today only
    /// `anySatisfy:`/`allSatisfy:` qualify: `do:` joined the producer table in
    /// BT-3796, and the other list ops (`collect:`, `select:`, `reject:`,
    /// `inject:into:`, …) were producers already — in a direct-params body
    /// their tuple is rebound through the loop's argument chain.
    pub(in crate::core_erlang) fn nested_non_producer_map_acc_list_op(
        &self,
        expr: &Expression,
    ) -> bool {
        use beamtalk_core::ast::MessageSelector;
        if !matches!(self.context, super::super::CodeGenContext::ValueType) {
            return false;
        }
        let inner = match expr.unwrap_parens() {
            Expression::Assignment { value, .. } => value.unwrap_parens(),
            other => other,
        };
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = inner
        else {
            return false;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
        if !matches!(sel.as_str(), "anySatisfy:" | "allSatisfy:")
            || beamtalk_core::state_threading_selectors::producer_body_arg_index(
                &sel,
                arguments.len(),
            )
            .is_some()
        {
            return false;
        }
        let Some(body_block) = Self::block_arg_for_selector(&sel, arguments) else {
            return false;
        };
        !self.loop_threaded_locals(body_block, None).is_empty()
    }

    /// Recursive wrapper for `list_op_needs_stateacc_fallback` that also
    /// looks inside Assignment values. Without this, `result := items collect: [...]`
    /// inside a counted loop body would not be detected by the top-level scan.
    pub(super) fn list_op_needs_stateacc_fallback_recursive(
        expr: &Expression,
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
    ) -> bool {
        match expr {
            Expression::Assignment { value, .. } => {
                Self::list_op_needs_stateacc_fallback_recursive(value, facts)
            }
            Expression::MessageSend { .. } => Self::list_op_needs_stateacc_fallback(expr, facts),
            _ => false,
        }
    }
}
