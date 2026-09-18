// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostics and class-var/list-op analysis predicates for state-threaded
//! loop and fold bodies.
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
/// scratch map (`State`) first, then `ClassVars`, then `SelfVt` — regardless
/// of the order callers pass an `eligible`/match set in.
const FAMILY_CANONICAL_ORDER: [VersionPrefix; 3] = [
    VersionPrefix::State,
    VersionPrefix::ClassVars,
    VersionPrefix::SelfVt,
];

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
/// call's OLD (still-live, unchanged) `threads_class_vars`/`threads_value_self`
/// answers, alongside what the NEW recursive
/// [`CoreErlangGenerator::body_threaded_families`] answers for the exact same
/// body — recorded by real compiles (`generate_module` over the stdlib +
/// bootstrap-test corpus) rather than by hand-building a `CoreErlangGenerator`
/// per corpus construct, so the recorded context (`class_var_names`,
/// `class_method_selectors`, `context`, `in_class_method`) is always the real
/// one the compiler itself built.
#[cfg(test)]
#[derive(Debug, Clone)]
pub(in crate::core_erlang) struct FamilyDetectorDiffRecord {
    /// `"letrec"` (`allow_direct_params`) or `"foldl"` — which of
    /// `ThreadingPlan::new_impl`'s two shapes this plan is, since the two
    /// have genuinely different OLD formulas (see
    /// `ThreadingPlan::threads_class_vars`'s doc comment).
    pub(in crate::core_erlang) shape: &'static str,
    /// The loop/fold body's own span — printed by the differential test so
    /// a mismatch is reviewable against source.
    pub(in crate::core_erlang) span: beamtalk_core::source_analysis::Span,
    pub(in crate::core_erlang) old_threads_class_vars: bool,
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

/// BT-3522: the two shapes [`CoreErlangGenerator::is_family_mutation`]
/// recognises for [`VersionPrefix::ClassVars`], reduced to just what
/// [`CoreErlangGenerator::reject_unthreadable_class_var_mutation`]'s
/// diagnostic needs.
///
/// Exists because `ast_walker::walk_expression`'s visitor is a
/// higher-ranked `FnMut(&Expression)` — the matched node cannot outlive the
/// walk, so the facts are copied out at the point of match instead of the
/// reference being carried back to the caller.
#[derive(Debug)]
enum ClassVarMutationSite {
    /// A bare `self.classVar := ...` write.
    FieldWrite { field: String, span: Span },
    /// A same-class self-send whose target may mutate a class variable.
    SelfSend { selector: String, span: Span },
}

impl ClassVarMutationSite {
    /// Classifies an expression [`CoreErlangGenerator::is_family_mutation`]
    /// has ALREADY matched for `ClassVars` — the caller's guard, not a
    /// second copy of the shape rule (ADR 0122 Decision 4).
    fn new(expr: &Expression) -> Self {
        if let Some(field) = CoreErlangGenerator::field_assignment_name(expr) {
            return Self::FieldWrite {
                field: field.to_string(),
                span: expr.span(),
            };
        }
        let Expression::MessageSend { selector, .. } = expr.unwrap_parens() else {
            // `is_family_mutation(&ClassVars, _)` matches exactly two
            // shapes: the field write handled above, and a self-send —
            // which `is_class_method_self_send` only ever reports for a
            // `MessageSend`.
            unreachable!("a non-field-write ClassVars mutation is always a MessageSend");
        };
        Self::SelfSend {
            selector: selector.name().to_string(),
            span: expr.span(),
        }
    }

    fn span(&self) -> Span {
        match self {
            Self::FieldWrite { span, .. } | Self::SelfSend { span, .. } => *span,
        }
    }

    /// Reuses the existing diagnostic that already describes each shape —
    /// no third "nested class-var mutation" variant is needed (CLAUDE.md's
    /// no-duplicate-implementations rule).
    fn into_error(self, location: String) -> CodeGenError {
        match self {
            Self::FieldWrite { field, .. } => {
                CodeGenError::ClassVarAssignmentInThreadedBody { field, location }
            }
            Self::SelfSend { selector, .. } => {
                CodeGenError::ClassMethodSelfSendInUnthreadedBlock { selector, location }
            }
        }
    }
}

/// which family a [`Self::nested_loop_or_fold_body`] match belongs
/// to — `ThreadingPlan::threads_class_vars` uses a genuinely different
/// formula for each (see that field's doc comment), so
/// [`Self::nested_loop_lost_class_var_mutation`] must apply the matching
/// one rather than a single one-size-fits-all check.
#[derive(Clone, Copy, PartialEq, Eq)]
enum NestedLoopShape {
    /// `whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/`to:by:do:` — the
    /// `BodyKind::Letrec` shapes, gated by the narrow, top-level-only
    /// `loop_body_threads_class_vars`.
    Letrec,
    /// `do:`/`collect:`/`select:`/... — the `BodyKind::Foldl*` shapes,
    /// gated by the recursive, Actor-excluded `has_self_sends` formula.
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
    ///   only (`Actor` context, not [`Self::in_class_method`]); a class
    ///   method's own `gen_server` state is `ClassVars`, never this family.
    /// * `ClassVars` — whenever [`Self::in_class_method`], regardless of the
    ///   class's own instance `context` (an Actor's and a `ValueType`'s
    ///   class methods compile identically).
    /// * `SelfVt` — a `ValueType` INSTANCE method only (`ValueType` context,
    ///   not [`Self::in_class_method`]).
    ///
    /// Mutually exclusive by construction: [`Self::in_class_method`] alone
    /// decides `State` vs. `ClassVars`, and `context` alone decides
    /// `ClassVars` vs. `SelfVt` once [`Self::in_class_method`] is fixed — so
    /// at most one family is ever eligible in `Repl` context too (none, in
    /// fact, since `Repl` never sets `in_class_method` and is never
    /// `ValueType`/`Actor`).
    pub(in crate::core_erlang) fn eligible_families(&self) -> Vec<VersionPrefix> {
        let mut eligible = Vec::with_capacity(1);
        if matches!(self.context, CodeGenContext::Actor) && !self.in_class_method() {
            eligible.push(VersionPrefix::State);
        }
        if self.in_class_method() {
            eligible.push(VersionPrefix::ClassVars);
        }
        if matches!(self.context, CodeGenContext::ValueType) && !self.in_class_method() {
            eligible.push(VersionPrefix::SelfVt);
        }
        eligible
    }

    /// ADR 0122 Decision 4: the ONE place "what counts as a mutation" is
    /// answered, per family — never re-derived at a call site:
    ///
    /// * `ClassVars` — a bare class-var field write (`self.classVar := ...`)
    ///   or a same-class-method self-send, the same OR
    ///   [`Self::find_class_var_mutating_stmt`] (Letrec shape) and the
    ///   `Foldl*`-shape `threads_class_vars` branch
    ///   ([`super::plan::ThreadingPlan::new_impl`]) each separately checked
    ///   before this existed.
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
            VersionPrefix::ClassVars => {
                (Self::is_field_assignment(expr) && self.is_class_var_assignment(expr))
                    || self.is_class_method_self_send(expr)
            }
            VersionPrefix::SelfVt => {
                !self.in_class_method()
                    && matches!(self.context, CodeGenContext::ValueType)
                    && Self::is_field_assignment(expr)
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
    /// ([`Self::find_class_var_mutating_stmt`]/
    /// [`Self::find_value_self_mutating_stmt`]/
    /// `value_type_codegen::is_vt_self_field_assignment`'s own callers).
    ///
    /// A site that cannot carry a mutation found below its own top level
    /// rejects it via the existing shared rejection functions
    /// ([`Self::reject_unthreadable_value_self_field_write`],
    /// [`Self::reject_unthreadable_class_var_mutation`],
    /// `reject_class_var_field_assignment`) — detection and carry-capability
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

    /// ADR 0111 Addendum 9, Questions 3/4: whether a Letrec loop
    /// body threads a `ClassVars` mutation through the loop's own recursive
    /// tail call. True exactly when the body is compiled inside a class
    /// method AND has a direct class-var field write or a same-class
    /// self-send — per Question 4 Part A, either shape already
    /// unconditionally forces `StateAcc` mode for the loop's own
    /// local-variable threading (`has_state_effects()`/`has_self_sends`
    /// exclude `DirectParams`/`TupleAcc`/`Hybrid`), so `ClassVars`
    /// composition only needs to be designed against that one shape.
    ///
    /// Shared by [`ThreadingPlan::new_impl`] (Letrec-only, via
    /// `allow_direct_params`) and the value-type/class-method loop-open
    /// consumers (`value_type_codegen.rs`) so the routing decision and the
    /// tuple-shape decision can never independently drift out of sync
    /// (CLAUDE.md's no-duplicate-implementations rule).
    pub(in crate::core_erlang) fn loop_body_threads_class_vars(
        &self,
        body: &beamtalk_core::ast::Block,
    ) -> bool {
        self.find_class_var_mutating_stmt(body).is_some()
    }

    /// whether a Letrec loop body threads a value-type `Self`
    /// mutation (`self.field := ...` in [`CodeGenContext::ValueType`])
    /// through the loop's own recursive tail call — the `SelfVt` mirror of
    /// [`Self::loop_body_threads_class_vars`], deliberately built the same
    /// (narrow, top-level-statement-only) way and for the same reason: an
    /// extra `letrec` fun parameter can only carry a rebind that the loop
    /// body's own STATEMENT sequence actually produces, never one scoped
    /// inside some larger sub-expression's own nested `let` (see
    /// [`Self::find_class_var_mutating_stmt`]'s doc comment for the
    /// empirically-confirmed unbound-variable regression that narrowing
    /// prevents).
    ///
    /// Mutually exclusive with [`Self::loop_body_threads_class_vars`] by
    /// construction — that one requires `in_class_method()`, this one
    /// excludes it (inside a class method `self.x :=` is a CLASS-var write,
    /// [`FieldWriteSite::ClassVar`](super::FieldWriteSite::ClassVar), which
    /// already has its own threading) — so the two never both claim the
    /// loop's single extra trailing tuple slot.
    ///
    /// Shared by [`ThreadingPlan::new_impl`] (which turns it into
    /// `ThreadingPlan::threads_value_self`) and the value-type loop-open
    /// consumers in `value_type_codegen.rs`, so the routing decision and the
    /// tuple-shape decision can never independently drift out of sync
    /// (CLAUDE.md's no-duplicate-implementations rule) — exactly the
    /// arrangement `loop_body_threads_class_vars` already has.
    ///
    /// **Accepted scope limit, inherited from that same narrowing:** a field
    /// write buried inside a NESTED construct in the loop body — most
    /// notably `1 to: n do: [:i | flag ifTrue: [self.total := ...]]` — is not
    /// a top-level statement, so this returns `false` and the loop threads no
    /// `Self`; the conditional's own `Self{N}` rebind stays scoped to its own
    /// nested `let`. Widening the predicate instead produced real
    /// unbound-variable regressions — see
    /// [`Self::nested_loop_lost_class_var_mutation`]'s doc comment — so that
    /// shape remains deliberately unsupported.
    ///
    /// What changed is only what happens to it *after* this returns `false`:
    /// it used to compile to a silently-dropped mutation (or, with no sibling
    /// local mutation to thread, an `erlc` `unbound variable 'State'` crash),
    /// and is now rejected at compile time by
    /// [`Self::reject_unthreadable_value_self_field_write`] with the same
    /// diagnostic the identical class-var shape already produced.
    pub(in crate::core_erlang) fn loop_body_threads_value_self(
        &self,
        body: &beamtalk_core::ast::Block,
    ) -> bool {
        self.find_value_self_mutating_stmt(body).is_some()
    }

    /// Shared predicate behind [`Self::loop_body_threads_value_self`] and
    /// [`Self::nested_loop_lost_value_self_mutation`] — returns
    /// the first top-level statement of `body` that is a value-type
    /// `self.field := ...` write, or `None` if there isn't one. The `SelfVt`
    /// mirror of [`Self::find_class_var_mutating_stmt`]; see
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
    /// and for both `Foldl*` shapes on the parent commit). The identical
    /// CLASS-VAR shape has always been rejected cleanly —
    /// `needs_mutation_threading`'s `in_class_method()` arm does not count
    /// field writes, so such a branch block never reaches the inline
    /// mutation-threading path at all and falls through to `generate_block`'s
    /// [`CoreErlangGenerator::validate_stored_closure`] diagnostic. This
    /// check closes that value-type gap by producing the same error through
    /// the shared
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
    /// predicate each call site turns into an error itself) for the same
    /// reason as [`CoreErlangGenerator::reject_class_var_field_assignment`]:
    /// it is the single place that turns a positive match into the
    /// diagnostic, so the `Letrec` and `Foldl*` call sites cannot drift out of
    /// sync (CLAUDE.md's no-duplicate-implementations rule).
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
            if let Some(field) = Self::field_assignment_name(e) {
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

    /// BT-3522 (ADR 0122): the `ClassVars` counterpart of
    /// [`Self::reject_unthreadable_value_self_field_write`] — rejects a
    /// class-var mutation that `expr` (ONE top-level statement of an
    /// `on:do:`/`ensure:` arm) hides inside a NESTED BLOCK, which the
    /// enclosing construct's trailing `ClassVars` slot cannot carry.
    ///
    /// # Why a nested-block walk, not a whole-subtree walk
    ///
    /// The `SelfVt` sibling rejects any write below the statement's root,
    /// its own right-hand side included. `ClassVars` is genuinely different,
    /// and the difference was confirmed empirically rather than assumed:
    ///
    /// * A class-var mutation in this statement's own SUB-EXPRESSION
    ///   (`t := 1 + (self bump)`) **is** carried. ADR 0118 phase 5b taught
    ///   `subexpr_needs_prelude`/`thread_ahead` to recognize a class-var
    ///   producer, so `generate_exception_body_with_threading_inner`'s E6/E7
    ///   arms lower it into a real `ThreadedStmt::Bind` in the arm's OWN
    ///   frame, advancing the ambient class-var version that
    ///   `ExceptionArm::family_mutated_version` then reads back out into the
    ///   construct's trailing slot. (Before BT-3522 widened
    ///   `exception_construct_families` to the recursive
    ///   [`Self::body_threaded_families`], that `Bind` had no slot to land
    ///   in and the shape tripped `verify()`'s `UnboundVersion` — an `erlc`
    ///   unbound-variable crash in a release build. Rejecting it now would
    ///   swap one regression for another.)
    /// * A class-var mutation inside a nested BLOCK (`flag ifTrue: [self
    ///   bump]`, a nested `on:do:`/`ensure:` arm, a nested loop body) is
    ///   **not** carried: that block compiles to its own closure or its own
    ///   branch-merge tuple, whose `ClassVars` rebind is scoped strictly
    ///   inside it, and nothing in this construct's E1..E7 dispatch unpacks
    ///   it back out. Confirmed empirically: the mutation is simply
    ///   discarded on normal return (the BT-3522 headline repro returned
    ///   `0` instead of `1`), with the emitted arm tuple silently naming a
    ///   same-spelled outer version instead.
    ///
    /// So the walk descends the statement looking for nested blocks OR
    /// `match:` arms, then searches each one's own body (and a `match:`
    /// arm's own guard, if it has one) — at any depth, since
    /// [`beamtalk_core::ast_walker::walk_expression`] descends into blocks
    /// and `match:` arms itself — for a match. "What counts as a mutation"
    /// is [`Self::is_family_mutation`] (ADR 0122 Decision 4), so the bare
    /// class-var field write and the same-class self-send shapes are covered
    /// by the one shared rule rather than re-derived here.
    ///
    /// # Why `match:` arms need the same treatment as a nested block
    ///
    /// BT-3522 adversarial review: [`beamtalk_core::ast::MatchArm::body`] is
    /// a bare `Expression`, not a [`beamtalk_core::ast::Block`], so it is
    /// invisible to a walk that only special-cases
    /// [`Expression::Block`](beamtalk_core::ast::Expression::Block). ADR
    /// 0122 Phase 9 (BT-3517) made `match:` declare `[State, ClassVars]` as
    /// data it may thread — but only for a `match:` that is ITSELF the
    /// construct doing the threading (i.e. reached the same way a top-level
    /// bare mutation is). Confirmed empirically that this does NOT extend to
    /// a `match:` sitting inside `on:do:`/`ensure:`: a class-method self-send
    /// inside a `1 -> self bump` arm, itself inside an `ensure:`'s try body,
    /// compiled cleanly and silently returned the pre-mutation value — the
    /// exact silent-drop shape this issue exists to close, just reached
    /// through a `match:` arm instead of an `ifTrue:` block. So a `match:`
    /// arm's body (and its guard, which could in principle hide the same
    /// shape) gets the identical "can't carry, so reject" treatment as a
    /// nested block, rather than being assumed safe because it isn't
    /// syntactically one.
    ///
    /// # Why the self-send shape is narrowed once more before rejecting
    ///
    /// [`Self::is_family_mutation`] answers the DETECTION question, and for
    /// `ClassVars` it counts every same-class self-send, mutating or not —
    /// correct there, because `generate_class_method_self_send` rebinds
    /// `ClassVars` from the callee's `{'class_var_result', …}` reply
    /// unconditionally, so the construct needs a slot either way. It is the
    /// wrong question for REJECTING: when the callee provably never writes a
    /// class variable, the rebind it returns is the caller's own map
    /// unchanged, so losing it inside a nested block loses nothing, and
    /// erroring would break code that compiles and behaves correctly today
    /// (confirmed empirically on a `class helper => 42` self-send inside an
    /// arm's `ifTrue:`). `class_var_mutating_selectors()` — a whole-class
    /// fixed point that already assumes the worst for anything it cannot
    /// resolve (`compute_class_var_mutating_selectors`) — is the same
    /// narrowing `check_no_unsafe_class_method_self_sends` (`blocks.rs`)
    /// applies for the identical "a block cannot thread this back" reason,
    /// reused here rather than re-derived.
    ///
    /// Reuses the two existing diagnostics rather than adding a third:
    /// [`CodeGenError::ClassVarAssignmentInThreadedBody`](super::super::CodeGenError::ClassVarAssignmentInThreadedBody)
    /// for a bare write and
    /// [`CodeGenError::ClassMethodSelfSendInUnthreadedBlock`](super::super::CodeGenError::ClassMethodSelfSendInUnthreadedBlock)
    /// for a self-send — whose wording ("this block has no way to thread
    /// such a mutation back to the class method that owns it") already
    /// describes exactly this shape.
    pub(super) fn reject_unthreadable_class_var_mutation(&self, expr: &Expression) -> Result<()> {
        if !self.in_class_method() {
            return Ok(());
        }
        // The visitor is a `FnMut(&Expression)` with a higher-ranked
        // lifetime, so the matched node itself cannot escape the walk —
        // the diagnostic's own inputs are extracted in place instead.
        let mut found: Option<ClassVarMutationSite> = None;
        beamtalk_core::ast_walker::walk_expression(expr, &mut |e| {
            if found.is_some() {
                return;
            }
            match e {
                Expression::Block(block) => {
                    for stmt in &block.body {
                        if let Some(site) = self.find_class_var_mutation_in_scope(&stmt.expression)
                        {
                            found = Some(site);
                            return;
                        }
                    }
                }
                Expression::Match { arms, .. } => {
                    for arm in arms {
                        if let Some(guard) = &arm.guard {
                            if let Some(site) = self.find_class_var_mutation_in_scope(guard) {
                                found = Some(site);
                                return;
                            }
                        }
                        if let Some(site) = self.find_class_var_mutation_in_scope(&arm.body) {
                            found = Some(site);
                            return;
                        }
                    }
                }
                _ => {}
            }
        });
        let Some(site) = found else {
            return Ok(());
        };
        let location = self.location_label(site.span());
        Err(site.into_error(location))
    }

    /// Searches one nested scope (a block's own statement, or a `match:`
    /// arm's body/guard) for the first [`Self::is_family_mutation`] match
    /// for `ClassVars`, applying the same self-send purity narrowing
    /// [`Self::reject_unthreadable_class_var_mutation`]'s own doc comment
    /// explains ("Why the self-send shape is narrowed once more"). Factored
    /// out so [`Self::reject_unthreadable_class_var_mutation`]'s two scope
    /// kinds (block statements, `match:` arms) share one search rather than
    /// two copies of this narrowing.
    fn find_class_var_mutation_in_scope(
        &self,
        scope_expr: &Expression,
    ) -> Option<ClassVarMutationSite> {
        let mut found: Option<ClassVarMutationSite> = None;
        beamtalk_core::ast_walker::walk_expression(scope_expr, &mut |inner| {
            if found.is_some() || !self.is_family_mutation(&VersionPrefix::ClassVars, inner) {
                return;
            }
            let site = ClassVarMutationSite::new(inner);
            if let ClassVarMutationSite::SelfSend { selector, .. } = &site {
                if !self
                    .class_var_mutating_selectors()
                    .contains(selector.as_str())
                {
                    return;
                }
            }
            found = Some(site);
        });
        found
    }

    /// the `SelfVt` mirror of
    /// [`Self::nested_loop_lost_class_var_mutation`] — if `expr` is itself a
    /// nested Letrec-shaped loop whose own body would thread a value-type
    /// `Self` mutation through its own recursive tail call, returns a short
    /// description of that mutation for
    /// [`CodeGenError::ValueSelfMutationLostAcrossNestedLoop`](super::super::CodeGenError::ValueSelfMutationLostAcrossNestedLoop)'s
    /// message.
    ///
    /// Same deliberate scope limit, for the same reason: nothing unpacks a
    /// nested loop's own trailing `Self` tuple slot back into the enclosing
    /// loop body's statement sequence, so the inner loop's mutation would be
    /// silently discarded. Rejecting it cleanly is consistent with the class-var
    /// precedent; making arbitrary nesting work is explicitly out of scope.
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

    /// Shared predicate behind [`Self::loop_body_threads_class_vars`] and
    /// [`Self::nested_loop_lost_class_var_mutation`] — returns the
    /// first top-level statement of `body` that is a bare class-var
    /// assignment or class-method self-send, or `None` if there isn't one.
    ///
    /// Deliberately narrower than `block_analysis::analyze_block`'s own
    /// (recursive) `field_writes`/`has_self_sends` — those also count a
    /// class-var write or self-send NESTED inside a conditional, a binary
    /// op, or any other sub-expression position, which is exactly right
    /// for THEIR job (deciding whether the body needs `StateAcc` fallback
    /// at all) but wrong for this one: `lower_letrec_body`/`lower_foldl_body`
    /// only ever thread `ClassVars` through the loop's tail call for a
    /// BARE, top-level class-var-assignment or class-method-self-send
    /// STATEMENT (the two shapes it has real Bind-construction branches
    /// for) — never for one buried inside a larger expression, whose own
    /// `ClassVarsN` rebind is scoped to that expression's own nested
    /// `let`, not threaded out to the loop body's own statement sequence.
    /// Confirmed by a real regression while validating this issue: a
    /// pre-existing, previously-compiling fixture
    /// (`class_var_sub_expr.bt`'s `tickInLoopConditional`, a self-send
    /// nested inside a `to:do:` body's `ifTrue:` *condition*) started
    /// emitting `unbound variable 'ClassVars1'` once this predicate used
    /// the recursive analysis — the self-send's own internally-minted
    /// rebind was correctly scoped to its own conditional's nested `let`,
    /// but this predicate's resulting extra loop-level `ClassVars` fun
    /// parameter/tail-call argument then referenced that same
    /// already-out-of-scope name.
    fn find_class_var_mutating_stmt<'a>(
        &self,
        body: &'a beamtalk_core::ast::Block,
    ) -> Option<&'a Expression> {
        if !self.in_class_method() {
            return None;
        }
        let filtered_body = super::super::util::collect_body_exprs(&body.body);
        filtered_body
            .into_iter()
            .find(|expr| self.is_family_mutation(&VersionPrefix::ClassVars, expr))
    }

    /// if `expr` is itself a nested `Letrec`- or `Foldl*`-shaped
    /// loop (per [`Self::nested_loop_or_fold_body`]) whose own body would
    /// thread a `ClassVars` mutation through its own recursive tail call or
    /// fold accumulator, returns a short description of that mutation for
    /// use in [`CodeGenError::ClassVarMutationLostAcrossNestedLoop`]'s
    /// message. Returns `None` for anything else, including a nested
    /// loop/fold whose own body has no class-var mutation to lose in the
    /// first place.
    ///
    /// Two independent triggers, matching each shape's own real threading
    /// gate:
    /// * [`Self::loop_body_threads_class_vars`] — a BARE, top-level
    ///   class-var field write or class-method self-send (the `Letrec`
    ///   gate, `ThreadingPlan::threads_class_vars`'s `allow_direct_params`
    ///   branch).
    /// * `block_analysis::analyze_block(body).has_self_sends` — ANY
    ///   same-class self-send anywhere in the body, however deeply nested
    ///   in a conditional or another block (the `Foldl*` gate, that same
    ///   field's `else` branch) — deliberately recursive here, unlike the
    ///   first trigger, because that IS how `Foldl*`'s own
    ///   `ThreadingPlan::new_impl` decides `threads_class_vars`. A bare
    ///   class-var field write inside a `Foldl*` body needs no matching
    ///   trigger here: `generate_field_assignment_open` never threads one
    ///   regardless of nesting (`loop_mode.threading_families` stays scoped
    ///   to `BodyKind::Letrec`), so it is already unconditionally rejected by
    ///   `reject_class_var_field_assignment` at any depth.
    ///
    /// BT-3530: the `Foldl*` gate's own diagnostic-message fallback below
    /// must mirror that same recursive reach for a same-class send spelled
    /// `ClassName foo` (as opposed to `self foo`) — `self_send_selectors`
    /// only ever records a bare `self`-receiver send, so a `ClassName`-spelled
    /// mutation reached this predicate's `Some(body)` match but fell through
    /// to `None` unrejected. Closed via `block_analysis::same_class_reference_send_selectors`,
    /// the same BT-3529/BT-3522 helper, rather than a second copy of the
    /// same-class-send detection.
    ///
    /// This is a detection-only predicate, deliberately separate from
    /// `ThreadingPlan::threads_class_vars`, which stays scoped to the OUTER
    /// body's own top-level statements (see that field's doc comment)
    /// rather than being extended to also thread the inner construct's
    /// `ClassVars` value through — no code path currently unpacks a nested
    /// loop/fold's `ClassVars` tuple element back into an enclosing body
    /// (confirmed empirically for the `Foldl*`-in-`Foldl*` shape: the
    /// nested fold's own `next_class_var()` mint permanently advances the
    /// generator's single, unscoped class-var-name counter even though the
    /// resulting name is never surfaced to the enclosing body, producing an
    /// `erlc` "unbound variable" compile crash rather than a clean
    /// diagnostic) — so this predicate exists purely to reject the shape,
    /// not to make it work.
    pub(super) fn nested_loop_lost_class_var_mutation(&self, expr: &Expression) -> Option<String> {
        let (body, shape) = Self::nested_loop_or_fold_body(expr)?;
        if let Some(mutating_stmt) = self.find_class_var_mutating_stmt(body) {
            if Self::is_field_assignment(mutating_stmt)
                && self.is_class_var_assignment(mutating_stmt)
            {
                if let Expression::Assignment { target, .. } = mutating_stmt {
                    if let Expression::FieldAccess { field, .. } = target.as_ref() {
                        return Some(format!("class variable '{}'", field.name));
                    }
                }
            } else if let Expression::MessageSend { selector, .. } = mutating_stmt {
                return Some(format!("'self {}'", selector.name()));
            }
        }
        // The recursive self-send fallback must match
        // `ThreadingPlan::new_impl`'s OWN per-shape gate exactly, not apply
        // uniformly to both shapes. `Letrec`'s real gate
        // (`loop_body_threads_class_vars`, already checked above) is
        // deliberately top-level-only — recursing into a conditional
        // buried inside a `Letrec` body is EXACTLY the shape that predicate
        // was narrowed to exclude (the `class_var_sub_expr.bt`
        // `tickInLoopConditional` regression), and it's also the shape
        // `class_var_sub_expr_test.bt`'s `testTickInLoopConditionalCompilesAndRuns`
        // pins as already-accepted, out-of-scope, silently-non-threading
        // behavior at a single loop level — rejecting only the
        // nested-loop variant of that exact same shape would be an
        // inconsistent, surprising new restriction this predicate has no
        // business introducing. Only `Foldl*`'s own real gate
        // (`!Actor && in_class_method() && body_analysis.has_self_sends`)
        // is genuinely recursive, so the fallback below applies only when
        // `shape` is `Foldl` — matching `context` too.
        if matches!(shape, NestedLoopShape::Foldl)
            && !matches!(self.context, CodeGenContext::Actor)
            && self.in_class_method()
        {
            let analysis = block_analysis::analyze_block(body);
            // `self_send_selectors` is a `HashSet` (default `RandomState`) —
            // pick the lexicographically-smallest selector so the
            // diagnostic text is reproducible across runs for identical
            // source, rather than depending on hash-iteration order. Which
            // selector is named doesn't affect the accept/reject decision,
            // only the message.
            if let Some(selector) = analysis.self_send_selectors.iter().min() {
                return Some(format!("'self {selector}'"));
            }
            // BT-3530: `self_send_selectors` only ever records a bare
            // `self`-receiver send (`beamtalk_core::semantic_analysis::block_facts::analyze_expression`),
            // so a same-class send spelled `ClassName foo` — the exact same
            // same-class, same-activation call `is_class_method_self_send`
            // treats identically to `self foo` — was invisible to the check
            // above, silently skipping this rejection instead of catching
            // the lost mutation at compile time. Same blind spot BT-3529
            // already fixed at `check_no_unsafe_class_method_self_sends`'s
            // call sites; reuse the same helper rather than a second copy.
            let class_name = self.class_name();
            let class_reference_sends =
                block_analysis::same_class_reference_send_selectors(&body.body, &class_name);
            if let Some(selector) = class_reference_sends.iter().min() {
                return Some(format!("'{class_name} {selector}'"));
            }
        }
        None
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
    /// canonical table shared by `get_control_flow_threaded_vars` and
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
    /// [`NestedLoopShape`] tells [`Self::nested_loop_lost_class_var_mutation`]
    /// which of `ThreadingPlan`'s two `threads_class_vars` gates applies.
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
                // `collect_nested_loop_outer_local_writes`'s `all_excluded`
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
                if matches!(
                    sel.as_str(),
                    "do:"
                        | "collect:"
                        | "select:"
                        | "reject:"
                        | "anySatisfy:"
                        | "allSatisfy:"
                        | "timesRepeat:"
                        | "inject:into:"
                        | "to:do:"
                        | "to:by:do:"
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
        let inner_threaded = self.compute_threaded_locals_for_loop(body_block, None);
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
