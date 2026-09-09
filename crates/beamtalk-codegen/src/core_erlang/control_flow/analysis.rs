// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Diagnostics and class-var/list-op analysis predicates for state-threaded
//! loop and fold bodies.
//!
//! **DDD Context:** Compilation — Code Generation
//!
//! BT-3459: split out of `control_flow/mod.rs`, no logic changes.

use super::super::threaded_ir::StateAccFallbackReason;
use super::super::{CodeGenContext, CoreErlangGenerator, block_analysis};
use super::plan::ThreadingPlan;
use beamtalk_core::ast::Expression;
use beamtalk_core::source_analysis::Span;

/// BT-3172: which family a [`Self::nested_loop_or_fold_body`] match belongs
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
    /// BT-1343: Emits a codegen diagnostic for the calling convention chosen for a loop.
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

        // BT-1343: Large extracted arity diagnostic (>8 direct fun params)
        let total = plan.total_extracted_params();
        if total > 8 && (plan.use_direct_params || plan.use_hybrid_params) {
            self.emit_codegen_diagnostic(
                format!("Loop{line_info}: {total} extracted params"),
                span,
            );
        }
    }

    /// ADR 0111 Addendum 9 (BT-3168), Questions 3/4: whether a Letrec loop
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

    /// BT-3484: whether a Letrec loop body threads a value-type `Self`
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
    /// nested `let` and the mutation is silently lost. That is exactly the
    /// class-var precedent's own accepted, separately-pinned behavior for the
    /// identical shape (`class_var_sub_expr_test.bt`'s
    /// `testTickInLoopConditionalCompilesAndRuns`, BT-2308 — see
    /// [`Self::nested_loop_lost_class_var_mutation`]'s doc comment on why
    /// widening the predicate instead produced real unbound-variable
    /// regressions), and is deliberately out of BT-3484's scope.
    pub(in crate::core_erlang) fn loop_body_threads_value_self(
        &self,
        body: &beamtalk_core::ast::Block,
    ) -> bool {
        self.find_value_self_mutating_stmt(body).is_some()
    }

    /// Shared predicate behind [`Self::loop_body_threads_value_self`] and
    /// [`Self::nested_loop_lost_value_self_mutation`] (BT-3484) — returns
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
            .find(|expr| Self::is_field_assignment(expr))
    }

    /// BT-3484: the `SelfVt` mirror of
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
    /// silently discarded (exactly the BT-3484 bug class this issue fixes at
    /// one level). Rejecting it cleanly is consistent with the class-var
    /// precedent; making arbitrary nesting work is explicitly out of scope.
    ///
    /// Only the `Letrec` shape is checked: a `Foldl*` (`do:`/`collect:`/…)
    /// body's value-type field write has no `Self` threading of its own to
    /// lose here — `generate_field_assignment_open` never threads one
    /// through a fold accumulator — so it is handled (and rejected, where
    /// unsupported) by the pre-existing paths, unchanged by this issue.
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
    /// [`Self::nested_loop_lost_class_var_mutation`] (BT-3172) — returns the
    /// first top-level statement of `body` that is a bare class-var
    /// assignment or class-method self-send, or `None` if there isn't one.
    ///
    /// Deliberately narrower than `block_analysis::analyze_block`'s own
    /// (recursive) `field_writes`/`has_self_sends` — those also count a
    /// class-var write or self-send NESTED inside a conditional, a binary
    /// op, or any other sub-expression position, which is exactly right
    /// for THEIR job (deciding whether the body needs `StateAcc` fallback
    /// at all) but wrong for this one: `generate_threaded_loop_body_inner`
    /// only ever threads `ClassVars` through the loop's tail call for a
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
        filtered_body.into_iter().find(|expr| {
            (Self::is_field_assignment(expr) && self.is_class_var_assignment(expr))
                || self.is_class_method_self_send(expr)
        })
    }

    /// BT-3172: if `expr` is itself a nested `Letrec`- or `Foldl*`-shaped
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
    ///   regardless of nesting (`loop_threads_class_vars` stays scoped to
    ///   `BodyKind::Letrec`), so it is already unconditionally rejected by
    ///   `reject_class_var_field_assignment` at any depth.
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
        // BT-3172 review: the recursive self-send fallback must match
        // `ThreadingPlan::new_impl`'s OWN per-shape gate exactly, not apply
        // uniformly to both shapes. `Letrec`'s real gate
        // (`loop_body_threads_class_vars`, already checked above) is
        // deliberately top-level-only — recursing into a conditional
        // buried inside a `Letrec` body is EXACTLY the shape that predicate
        // was narrowed to exclude (the `class_var_sub_expr.bt`
        // `tickInLoopConditional` regression), and it's also the shape
        // `class_var_sub_expr_test.bt`'s `testTickInLoopConditionalCompilesAndRuns`
        // pins as already-accepted, out-of-scope, silently-non-threading
        // behavior (BT-2308) at a single loop level — rejecting only the
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
        }
        None
    }

    /// BT-3175: Canonical "selector → body-block-argument position" table.
    /// Shared by every "given a keyword-selector `MessageSend`, extract its
    /// loop/fold body block" call site in this module
    /// ([`Self::nested_loop_or_fold_body`],
    /// [`Self::collect_list_op_cross_scope_mutations`],
    /// [`Self::list_op_needs_stateacc_fallback`],
    /// [`Self::expr_has_nested_counted_loop_threading`]) — before this, each
    /// independently re-matched selector strings against
    /// `arguments.first()`/`arguments.last()`/`arguments[N]`, and could
    /// silently drift out of sync (see BT-3175).
    ///
    /// This is the canonical/maximal selector set: the `BodyKind::Letrec`
    /// shapes (`whileTrue:`/`whileFalse:`/`timesRepeat:`/`to:do:`/
    /// `to:by:do:`, see this module's `//!` doc comment) plus the
    /// `BodyKind::Foldl*` shapes (`do:`/`collect:`/`select:`/`reject:`/
    /// `anySatisfy:`/`allSatisfy:`/`inject:into:`/`detect:`/`count:`/
    /// `takeWhile:`/`dropWhile:`/`partition:`/`groupBy:`) — matching
    /// [`Self::nested_loop_or_fold_body`]'s pre-BT-3175 coverage, the most
    /// complete of the four (BT-3172 added the predicate-based shapes
    /// there only). `detect:ifNone:` is intentionally excluded: its second
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
    /// source of truth for this NARROWER loop/fold-shape table (BT-3423:
    /// deliberately not the broader
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

    /// BT-1343: Emits a diagnostic for synchronous self-send detected in a loop body.
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

    /// BT-1329: Collects variables that are captured and mutated by nested list op blocks.
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

        // BT-3173: ensure:/on:do:/ifNotNil: aren't list-ops/counted-loops
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
                // BT-3173 review follow-up: exclude this wrapping block's own
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

        // BT-2363: nested counted loops (`timesRepeat:`/`to:do:`/`to:by:do:`)
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

        // BT-2363: Recurse into the inner block's statements so deeper nesting
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

    /// BT-2363: Returns `true` if `expr` is (or wraps, via assignment RHS or parens) a
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

    /// BT-1329: Returns `true` if `expr` is a list op (do:, collect:, select:, reject:,
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

    /// BT-1329: Recursive wrapper for `list_op_needs_stateacc_fallback` that also
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
