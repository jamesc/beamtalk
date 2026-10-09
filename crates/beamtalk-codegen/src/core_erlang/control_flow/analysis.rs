// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostics and list-op analysis predicates for state-threaded loop and
//! fold bodies.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! split out of `control_flow/mod.rs`, no logic changes.

use super::super::threaded_ir::{StateAccFallbackReason, VersionPrefix};
use super::super::{CodeGenContext, CodeGenError, CoreErlangGenerator, Result, block_analysis};
use super::plan::ThreadingPlan;
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

// ─── ADR 0122: unified storage-family detector ─────────────────────────────

/// ADR 0122 Decision 2: canonical slot order for [`ThreadedFamilies`] — the
/// scratch map (`State`) first, then `SelfVt` — regardless of the order
/// callers pass an `eligible`/match set in.
const FAMILY_CANONICAL_ORDER: [VersionPrefix; 2] = [VersionPrefix::State, VersionPrefix::SelfVt];

/// ADR 0122 Decision 2: the storage families a construct body mutates, in
/// canonical slot order. Built by [`CoreErlangGenerator::body_threaded_families`]
/// (or, for a caller that already has its own per-family answers, by
/// [`Self::from_matches`]) — never assembled by hand, so the slot order
/// invariant can't drift per call site.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub(in crate::core_erlang) struct ThreadedFamilies(Vec<VersionPrefix>);

impl ThreadedFamilies {
    /// Builds a `ThreadedFamilies` from an arbitrary-order set of matched
    /// families, re-sorting into [`FAMILY_CANONICAL_ORDER`] — the shared
    /// normalization every constructor (this one included) routes through,
    /// so canonical order is a property of the TYPE, not of caller
    /// discipline.
    pub(in crate::core_erlang) fn from_matches(matches: &[VersionPrefix]) -> Self {
        Self(
            FAMILY_CANONICAL_ORDER
                .iter()
                .filter(|prefix| matches.contains(prefix))
                .cloned()
                .collect(),
        )
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
    /// [`FAMILY_CANONICAL_ORDER`]'s ordering guarantee — [`Self::from_matches`]
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
    /// * ANY write in a `Foldl*` (`do:`/`collect:`/…) body, top-level
    ///   included: a fold accumulator has no trailing `Self` slot, so
    ///   `threads_value_self` is never set for a `Foldl*` plan (see
    ///   `ThreadingPlan::new_impl`) and this predicate's `threads_value_self`
    ///   argument is correspondingly always `false` there.
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
        if self.in_class_method() || !matches!(self.context, CodeGenContext::ValueType) {
            return Ok(());
        }
        let mut found: Option<String> = None;
        beamtalk_core::ast_walker::walk_expression(expr, &mut |e| {
            if found.is_some() {
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
    /// Only the `Letrec` shape is checked: a `Foldl*` (`do:`/`collect:`/…)
    /// body's value-type field write has no `Self` threading of its own to
    /// lose here — `generate_field_assignment_open` never threads one
    /// through a fold accumulator. That shape is rejected instead, via
    /// [`Self::reject_unthreadable_value_self_field_write`], which runs
    /// immediately after this check at both call sites.
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
        if beamtalk_core::state_threading_selectors::is_exception_selector(&sel)
            || beamtalk_core::state_threading_selectors::is_conditional_selector(&sel)
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
                // Exclude this wrapping block's own
                // parameters (e.g. `on:do:`'s exception var, `ifNotNil:`'s bound
                // value) before merging into `out` — mirrors
                // `construct_outer_local_writes`'s block-parameter
                // threading for the identical construct shape. Without this, a
                // nested loop reporting a write to the wrapping block's own
                // param (e.g. `x ifNotNil: [:v | nested do: [:i | v := v + i]]`)
                // would be misreported as an outer-scope mutation.
                let block_params: std::collections::HashSet<String> = block
                    .parameters
                    .iter()
                    .map(|p| p.name.to_string())
                    .collect();
                let mut nested = std::collections::HashSet::new();
                for stmt in &block.body {
                    Self::collect_list_op_cross_scope_mutations_recursive(
                        &stmt.expression,
                        facts,
                        &mut nested,
                    );
                }
                for v in nested {
                    if !block_params.contains(v.as_str()) {
                        out.insert(v);
                    }
                }
            }
            return;
        }

        // nested counted loops (`timesRepeat:`/`to:do:`/`to:by:do:`)
        // capture and mutate outer locals just like list ops. Including them
        // here makes the *outer* loop's threaded-locals computation see
        // writes buried in an inner counted loop, so the outer loop threads
        // them via StateAcc instead of dropping them.
        let body_block = match Self::block_arg_for_selector(&sel, arguments) {
            Some(block)
                if beamtalk_core::state_threading_selectors::is_nested_threaded_loop_selector(
                    &sel,
                ) =>
            {
                block
            }
            _ => return,
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

        // Recurse into the inner block's statements so deeper nesting
        // (a counted/list op nested two or more levels deep) is still detected.
        // `analyze_block` does not propagate writes out of nested non-conditional
        // blocks, so a write buried in a doubly-nested loop is invisible above
        // without this recursion. Block parameters of the inner block are not
        // outer locals, so drop any cross-scope name shadowed by a block param.
        let mut nested = std::collections::HashSet::new();
        for stmt in &body_block.body {
            Self::collect_list_op_cross_scope_mutations_recursive(
                &stmt.expression,
                facts,
                &mut nested,
            );
        }
        for v in nested {
            if !block_params.contains(v.as_str()) {
                out.insert(v);
            }
        }
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
