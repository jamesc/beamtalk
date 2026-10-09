// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Control-flow threaded-variable analysis: predicates and helpers that
//! decide *whether* an expression, block, or loop body needs state/class-var
//! threading, and *which* variables it threads — consumed across
//! `control_flow/`, `expressions.rs`, `value_type_codegen.rs`, and
//! `gen_server/methods.rs`.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::control_flow::analysis::ThreadedFamilies;
use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::{CodeGenContext, CodeGenError, Result, block_analysis};
use beamtalk_core::ast::{Block, Expression};
use beamtalk_core::semantic_analysis::block_facts::{
    ConstructPosition, LocalThreadingFamily, OuterLocalWrite, TodayLowering,
    local_threading_construct, threaded_block_writes, threaded_today_block_writes,
    threaded_today_blocks,
};

/// Which construct a [`ThreadedLocals`] set belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum ThreadedConstruct {
    /// A construct `beamtalk_core`'s [`local_threading_construct`]
    /// recognizes, by family.
    Inline(LocalThreadingFamily),
    /// A `value`-family send to a Tier 2 block-valued local.
    Tier2Value,
    /// An ADR 0128 opaque-callable fold over a Tier 2 block-valued local,
    /// in actor instance context.
    OpaqueFold,
}

/// ADR 0131 §1: the threaded outer locals of one local-threading construct,
/// built by [`CoreErlangGenerator::threaded_locals_of`].
///
/// Production codegen reads only [`Self::lowered`], through
/// [`CoreErlangGenerator::lowered_threaded_locals_of`]. [`Self::construct`],
/// [`Self::names`] and the [`ThreadedConstruct::Tier2Value`] /
/// [`ThreadedConstruct::OpaqueFold`] shapes are ADR 0131 scaffolding: today
/// only tests read them, and the phase 2–4 producers (BT-3749 and later)
/// consume them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(in crate::core_erlang) struct ThreadedLocals {
    /// The construct.
    pub(in crate::core_erlang) construct: ThreadedConstruct,
    /// The construct's threaded set, sorted and never empty: every outer
    /// local its blocks write, closed over nested producers. In the REPL
    /// it includes the workspace bindings the construct writes.
    pub(in crate::core_erlang) names: Vec<String>,
    /// The subset of [`Self::names`] today's lowering packs into the
    /// construct's `{Value, StateAcc}` result under `__local__` keys, sorted
    /// and possibly empty. ADR 0131 phases 2–4 grow it until it is
    /// [`Self::names`] for every construct.
    pub(in crate::core_erlang) lowered: Vec<String>,
}

impl ThreadedLocals {
    /// [`Self::lowered`], or `None` when today's lowering packs nothing for
    /// this construct.
    pub(in crate::core_erlang) fn into_lowered(self) -> Option<Vec<String>> {
        if self.lowered.is_empty() {
            None
        } else {
            Some(self.lowered)
        }
    }
}

impl CoreErlangGenerator {
    /// Check if mutation threading should be used for a block.
    /// In REPL mode, local variable mutations trigger threading.
    /// In actor module mode, field writes, self-sends, OR local variable
    /// mutations trigger threading. Local vars are threaded through the state accumulator
    /// map alongside fields.
    /// In value type module mode, only field writes trigger threading (no state map).
    /// Class methods have no State variable — field/self-send threading is disabled.
    /// Captured local variable mutations in class method blocks are threaded
    /// via a fresh local map (same as value types).
    pub(in crate::core_erlang) fn needs_mutation_threading(
        &self,
        analysis: &block_analysis::BlockMutationAnalysis,
    ) -> bool {
        if self.is_repl_mode() {
            // REPL: both local vars and fields need threading
            analysis.has_mutations()
        } else if self.in_class_method() {
            // Class methods have no actor State variable — field writes and
            // self-sends must NOT trigger state threading.
            // However, captured local variable mutations (outer vars both read
            // and written in the block) DO need threading via a fresh local map, same as
            // value types. Without this, `do:` blocks silently lose local mutations.
            analysis
                .captured_reads
                .iter()
                .any(|v| analysis.local_writes.contains(v))
        } else if self.context == CodeGenContext::Actor {
            // Actor methods: field writes, self-sends,
            // OR local variable mutations all need threading.
            //
            // ADR 0122/BT-3514: the "family half" of this OR (field
            // writes/self-sends) is gated behind `ThreadedFamilies`
            // non-emptiness — `eligible_families()` reports exactly
            // `[State]` for a non-class-method Actor instance method,
            // never empty, matching ADR 0122 §"State already fits": "the
            // predicate is always true" once `State` is eligible. ANDed
            // with the real, data-dependent `has_state_effects()` check
            // (a block with no field write/self-send still doesn't need
            // threading on this account), this is behavior-preserving —
            // the "outer-local half" (`!analysis.local_writes.is_empty()`)
            // is untouched.
            let family_half = !ThreadedFamilies::from_matches(&self.eligible_families())
                .as_slice()
                .is_empty()
                && analysis.has_state_effects();
            family_half || !analysis.local_writes.is_empty()
        } else {
            // Value types have no State variable, so self-sends should
            // NOT trigger state threading. Only field writes need threading.
            // Captured local variable mutations (outer vars both read and
            // written in the block) also need threading via a fresh local map.
            !analysis.field_writes.is_empty()
                || analysis
                    .captured_reads
                    .iter()
                    .any(|v| analysis.local_writes.contains(v))
        }
    }

    /// Returns `true` if the block body contains list op message sends
    /// (do:, collect:, select:, reject:, inject:into:) whose blocks capture and
    /// mutate variables from the outer scope. These cross-scope mutations are
    /// invisible to `analyze_block` (which doesn't propagate `local_writes` from
    /// nested non-conditional blocks), so they require a separate scan.
    pub(in crate::core_erlang) fn body_has_list_op_cross_scope_mutations(
        &self,
        body: &beamtalk_core::ast::Block,
    ) -> bool {
        let mut cross_scope_writes = std::collections::HashSet::new();
        for stmt in &body.body {
            Self::collect_list_op_cross_scope_mutations_recursive(
                &stmt.expression,
                &self.semantic_facts,
                &mut cross_scope_writes,
            );
        }
        !cross_scope_writes.is_empty()
    }

    /// The precomputed [`block_analysis::BlockMutationAnalysis`]
    /// profile for `block` when one exists (populated by `compute_semantic_facts`
    /// alongside every other fact), else computed on the fly. This exact
    /// `block_profile(…).cloned().unwrap_or_else(|| analyze_block(…))` pattern
    /// was independently inlined at every call site that needed a block's
    /// analysis; extracted so it (and, via [`Self::block_arg_needs_threading`],
    /// the "does this literal block need mutation threading" check built on
    /// top of it) has one home.
    pub(in crate::core_erlang) fn block_profile_or_analyze(
        &self,
        block: &Block,
    ) -> block_analysis::BlockMutationAnalysis {
        self.semantic_facts
            .block_profile(&block.span)
            .cloned()
            .unwrap_or_else(|| block_analysis::analyze_block(block))
    }

    /// ADR 0118 phase 6: the single "does this literal block's body
    /// need mutation threading" check — block-local/field mutations
    /// ([`Self::needs_mutation_threading`], reading [`Self::block_profile_or_analyze`])
    /// OR a cross-scope mutation buried in a nested list-op/counted loop the
    /// block-level analysis alone can't see
    /// ([`Self::body_has_list_op_cross_scope_mutations`]). Replaces three
    /// identical copies of this exact two-part check that were inlined in
    /// `control_flow_has_mutations` (`gen_server/methods.rs`: the exception-
    /// selector receiver, each conditional branch argument, and the default
    /// per-argument loop) plus a fourth copy in `enumeration_block_needs_threading`
    /// (`control_flow/list_ops/enumeration_ops.rs`) — the "must stay in sync"
    /// duplication ADR 0118 §Context calls out.
    /// [`Self::conditional_needs_mutation_threading`] (`intrinsics.rs`) also
    /// calls this for its per-block check, extending it with an
    /// in-loop-body-local-write disjunct that the other four call sites don't
    /// need.
    pub(in crate::core_erlang) fn block_arg_needs_threading(&self, block: &Block) -> bool {
        let analysis = self.block_profile_or_analyze(block);
        self.needs_mutation_threading(&analysis)
            || self.body_has_list_op_cross_scope_mutations(block)
    }

    /// Recursively scans an expression for list op message sends with
    /// cross-scope mutations. Unlike `collect_list_op_cross_scope_mutations`,
    /// this also looks inside Assignment values.
    pub(in crate::core_erlang) fn collect_list_op_cross_scope_mutations_recursive(
        expr: &Expression,
        facts: &beamtalk_core::semantic_analysis::SemanticFacts,
        out: &mut std::collections::HashSet<String>,
    ) {
        match expr.unwrap_parens() {
            Expression::Assignment { value, .. } => {
                Self::collect_list_op_cross_scope_mutations_recursive(value, facts, out);
            }
            send @ Expression::MessageSend { .. } => {
                Self::collect_list_op_cross_scope_mutations(send, facts, out);
            }
            _ => {}
        }
    }

    /// ADR 0131 §1 "One recognizer, one set" (BT-3746): the threaded outer
    /// locals of `expr`, or `None` when `expr` is not a local-threading
    /// construct or threads no outer local.
    ///
    /// This is the only recognizer of a local-threading construct and the
    /// only source of its threaded set. Every tuple builder packs from it,
    /// through [`ThreadedLocals::lowered`] or the block-level kernel
    /// [`Self::threaded_locals_of_blocks`] it is defined over, and every
    /// unpacking site reads it back. A gate that says "this threads" can
    /// therefore never pair with a smaller write-back set.
    ///
    /// It covers:
    ///
    /// - every construct `beamtalk_core`'s
    ///   [`local_threading_construct`] recognizes: loops, folds and `do:`
    ///   (including `eachWithIndex:`/`do:separatedBy:`), the conditional
    ///   family, `on:do:`/`ensure:`, `value` sent to a block literal, the
    ///   block-taking lookup selectors and `Result tryDo:`;
    /// - a `value`-family send to a Tier 2 block-valued local
    ///   (`tier2_local_vars`);
    /// - an ADR 0128 opaque-callable fold over such a local, in actor
    ///   instance context only.
    ///
    /// `match:` has no construct of its own. Its arms are not closures, so
    /// their writes belong to whichever construct block contains the
    /// `match:`, and the walk descends into them.
    ///
    /// [`ThreadedLocals::names`] is the transitive closure over nested
    /// producers (ADR 0131 §1 "Transitive closure"): core's
    /// [`threaded_block_writes`] descends into the blocks of every nested
    /// construct. In the REPL it is the bindings the construct writes: every
    /// name assigned that is not bound inside the construct.
    /// [`ThreadedLocals::lowered`] is exactly what today's lowering packs:
    /// core's [`threaded_today_blocks`] (one rule, for the top-level construct
    /// and, through [`threaded_today_block_writes`], every nested one) picks
    /// the blocks.
    ///
    /// Facts read: core's `block_facts` ([`local_threading_construct`],
    /// [`threaded_block_writes`], [`threaded_today_blocks`],
    /// [`threaded_today_block_writes`], which read only the AST and the
    /// `bound_outside` scope predicate) and `state_threading_selectors`
    /// (through them). The scope predicate is
    /// [`Self::lookup_var`]. The same two core functions drive the Phase 0
    /// allow-set check in `beamtalk-core`'s
    /// `semantic_analysis/validators/local_threading.rs`, with that pass's
    /// own scope.
    pub(in crate::core_erlang) fn threaded_locals_of(
        &self,
        expr: &Expression,
    ) -> Option<ThreadedLocals> {
        let expr = expr.unwrap_parens();
        let set = self.tier2_threaded_locals(expr).or_else(|| {
            let construct = local_threading_construct(expr)?;
            let lowered_blocks = threaded_today_blocks(
                &construct,
                expr,
                ConstructPosition::Top(self.today_lowering()),
            );
            let set = self.threaded_locals_of_blocks(
                ThreadedConstruct::Inline(construct.family),
                &construct.blocks,
                &lowered_blocks,
            );
            #[cfg(test)]
            recorded_sets::record_lowered(
                recorded_sets::Side::Unpack,
                &lowered_blocks,
                set.as_ref().map_or(&[][..], |s| &s.lowered),
            );
            set
        });
        #[cfg(test)]
        recorded_sets::record(expr, set.as_ref());
        set
    }

    /// The set today's lowering packs for `expr` ([`ThreadedLocals::lowered`]
    /// of [`Self::threaded_locals_of`]), or `None` when it packs nothing.
    /// This is what every result-unpacking site reads.
    pub(in crate::core_erlang) fn lowered_threaded_locals_of(
        &self,
        expr: &Expression,
    ) -> Option<Vec<String>> {
        self.threaded_locals_of(expr)
            .and_then(ThreadedLocals::into_lowered)
    }

    /// The codegen context facts core's [`threaded_today_blocks`] needs for
    /// a top-level construct.
    fn today_lowering(&self) -> TodayLowering {
        TodayLowering {
            actor_fold: self.enumeration_threads_actor_state(),
            actor_context: self.context == CodeGenContext::Actor,
            class_method: self.in_class_method(),
        }
    }

    /// The block-level kernel of [`Self::threaded_locals_of`]: the threaded
    /// set of a construct of kind `construct` whose blocks are `blocks`,
    /// with `lowered_blocks` (a subset of `blocks`, selected by core's
    /// [`threaded_today_blocks`]) the ones today's lowering packs. `None`
    /// when the set is empty.
    ///
    /// The loop, fold, conditional and exception generators call it
    /// directly with the blocks they lower, because they are handed the
    /// blocks rather than the send. They pass the same blocks
    /// [`threaded_today_blocks`] selects from the send, so packing and
    /// unpacking agree; the agreement corpus test
    /// (`tests/threaded_locals_agreement.rs`) checks it.
    pub(in crate::core_erlang) fn threaded_locals_of_blocks(
        &self,
        construct: ThreadedConstruct,
        blocks: &[&Block],
        lowered_blocks: &[&Block],
    ) -> Option<ThreadedLocals> {
        let repl = self.is_repl_mode();
        // The REPL threads its loops and folds through the bindings map
        // (`KeyStyle::ReplPlain`), not through `__local__` keys, so today's
        // lowering packs none of their names. Its conditionals and
        // exception handlers pack the locals bound in the generated code.
        let repl_map_threaded = repl
            && matches!(
                construct,
                ThreadedConstruct::Inline(LocalThreadingFamily::Loop | LocalThreadingFamily::Fold)
            );
        let lowered = if repl_map_threaded || lowered_blocks.is_empty() {
            Vec::new()
        } else {
            Self::write_names(threaded_today_block_writes(lowered_blocks, &|name| {
                self.lookup_var(name).is_some()
            }))
        };
        let names = Self::write_names(threaded_block_writes(blocks, &|name| {
            repl || self.lookup_var(name).is_some()
        }));
        if names.is_empty() {
            return None;
        }
        Some(ThreadedLocals {
            construct,
            names,
            lowered,
        })
    }

    /// The lowered set of a loop or fold generator's `body` block (and,
    /// for `whileTrue:`/`whileFalse:`, its `condition`, when
    /// [`TodayLowering::threads_loop_condition`] says it is packed): the
    /// [`Self::threaded_locals_of_blocks`] kernel over the same blocks
    /// [`threaded_today_blocks`] selects from the send. Empty when it
    /// threads nothing.
    pub(in crate::core_erlang) fn loop_threaded_locals(
        &self,
        body: &Block,
        condition: Option<&Expression>,
    ) -> Vec<String> {
        let mut blocks: Vec<&Block> = Vec::with_capacity(2);
        if let Some(Expression::Block(cond)) = condition {
            if self.today_lowering().threads_loop_condition(cond) {
                blocks.push(cond);
            }
        }
        blocks.push(body);
        // `Loop` and `Fold` lower alike here (they differ only in which
        // blocks the send contributes, which the caller has already picked).
        let lowered = self
            .threaded_locals_of_blocks(
                ThreadedConstruct::Inline(LocalThreadingFamily::Loop),
                &blocks,
                &blocks,
            )
            .map(|t| t.lowered)
            .unwrap_or_default();
        #[cfg(test)]
        recorded_sets::record_lowered(recorded_sets::Side::Pack, &blocks, &lowered);
        lowered
    }

    /// The lowered set of a conditional's branch blocks, or of an
    /// `on:do:`/`ensure:`'s protected body and handler blocks: the
    /// [`Self::threaded_locals_of_blocks`] kernel over the same blocks
    /// [`threaded_today_blocks`] selects from the send. The same set drives
    /// the seeding emitted by `generate_*_with_mutations` and the
    /// extraction emitted by the method-body sequencer, so a branch that
    /// does not run never leaves a `__local__` key missing.
    pub(in crate::core_erlang) fn branch_threaded_locals(&self, blocks: &[&Block]) -> Vec<String> {
        let lowered = self.branch_lowered_locals(blocks);
        #[cfg(test)]
        recorded_sets::record_lowered(recorded_sets::Side::Pack, blocks, &lowered);
        lowered
    }

    /// Whether any of `blocks` writes a local today's conditional and
    /// exception lowering threads ([`Self::branch_threaded_locals`]'s set is
    /// not empty). A gate on one block of a construct, not a packing site.
    pub(in crate::core_erlang) fn blocks_write_threaded_local(&self, blocks: &[&Block]) -> bool {
        !self.branch_lowered_locals(blocks).is_empty()
    }

    fn branch_lowered_locals(&self, blocks: &[&Block]) -> Vec<String> {
        self.threaded_locals_of_blocks(
            ThreadedConstruct::Inline(LocalThreadingFamily::Conditional),
            blocks,
            blocks,
        )
        .map(|t| t.lowered)
        .unwrap_or_default()
    }

    /// The Tier 2 shapes of [`Self::threaded_locals_of`]: a `value`-family
    /// send to a Tier 2 block-valued local, and (actor instance context
    /// only, ADR 0128 §"Explicitly narrowed") an opaque-callable fold over
    /// one. Their set is the block's captured mutations, recorded by
    /// `prescan_tier2_local_vars`. Today's tuple builders pack neither (the
    /// Tier 2 protocol threads them), so [`ThreadedLocals::lowered`] is
    /// empty.
    fn tier2_threaded_locals(&self, expr: &Expression) -> Option<ThreadedLocals> {
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        else {
            return None;
        };
        let (construct, local) = if selector.is_block_invocation() {
            let Expression::Identifier(id) = receiver.as_ref() else {
                return None;
            };
            (ThreadedConstruct::Tier2Value, id.name.as_str())
        } else if self.context == CodeGenContext::Actor
            && !self.in_class_method()
            && !self.is_repl_mode()
            && beamtalk_core::state_threading_selectors::is_opaque_callable_hom_send(expr)
        {
            let callable = beamtalk_core::state_threading_selectors::opaque_fold_callable_arg(
                &selector.name(),
                arguments,
            );
            let Some(Expression::Identifier(id)) = callable.map(Expression::unwrap_parens) else {
                return None;
            };
            (ThreadedConstruct::OpaqueFold, id.name.as_str())
        } else {
            return None;
        };
        if !self.tier2_local_vars.contains(local) {
            return None;
        }
        let mut names = self
            .tier2_local_var_captured_mutations
            .get(local)
            .cloned()
            .unwrap_or_default();
        if names.is_empty() {
            return None;
        }
        names.sort();
        names.dedup();
        Some(ThreadedLocals {
            construct,
            names,
            lowered: Vec::new(),
        })
    }

    /// The sorted, deduplicated names of `writes`.
    fn write_names(writes: Vec<OuterLocalWrite>) -> Vec<String> {
        let mut names: Vec<String> = writes.into_iter().map(|w| w.name.to_string()).collect();
        names.sort();
        names.dedup();
        names
    }

    /// Validates a block's mutation analysis for shapes that can't correctly thread state:
    /// field assignments, and (separately) captured-local mutations. Returns an error for
    /// either; the local-mutation error is phrased as a warning in its message.
    ///
    /// **Precondition for production callers:** only call this when
    /// `analysis.field_writes` is non-empty. A valid Tier 2 block (captured-local
    /// mutations, no field writes) passed here would incorrectly hit the
    /// local-mutation branch and produce a spurious `LocalMutationInStoredClosure`
    /// — that branch exists for this function's own unit tests, which construct
    /// analyses field-write-empty on purpose to test it, not for callers on a path
    /// where a genuine Tier 2 block could reach this function.
    ///
    /// Blocks with mutations are supported via the Tier 2 stateful block protocol
    /// (ADR 0041), but only for *captured local* mutations
    /// (`captured_mutations_for_block` in `expressions.rs`, which promotes to
    /// `generate_block_stateful`) — Tier 2 promotion never triggers on `self.field :=`
    /// writes. A block with field writes that reaches the generic "pure fun" fallback
    /// in `generate_block` would silently emit Core Erlang `erlc` rejects with
    /// "unbound variable" (the block's own `fun` bumps the shared state-version
    /// counter, but that binding is scoped inside the `fun` and never reaches the
    /// caller).
    ///
    /// Called from `generate_block` (with an already-computed analysis, so callers
    /// that need more than this check don't re-walk the block's AST) to turn that into
    /// a clear compile-time diagnostic instead. `generate_block` only calls this when
    /// `field_writes` is non-empty (checked *before* the Tier 2 captured-local-mutation
    /// promotion, so a block with both a field write and a local mutation errors here
    /// instead of silently reaching Tier 2 for the local mutation alone), so the field
    /// branch below always fires from that call site and the local-mutation branch is
    /// unreachable from it — it's kept live (and directly unit-tested) since this
    /// function checks a block's mutation shape in general, not just the field-write
    /// case `generate_block` currently cares about. Lifting the
    /// field-write restriction for stored/opaque blocks by generalizing Tier 2 the same
    /// way is tracked separately; once that lands this function's field-write branch
    /// should shrink to whatever shapes remain genuinely unsupported.
    ///
    /// `location` is a lazy thunk rather than a pre-formatted `String`, so formatting
    /// only happens when an error is actually produced. From `generate_block`'s call
    /// site this always errors (it's only called when `field_writes` is non-empty), but
    /// `generate_block` still runs `analyze_block` and checks `field_writes` on every
    /// block it compiles — the thunk keeps this function itself free of `span_to_line`/
    /// `format!` cost for callers (present or future) that reach it on a path where an
    /// `Ok(())` result is actually possible, e.g. this function's own unit tests.
    pub(in crate::core_erlang) fn validate_stored_closure(
        analysis: &block_analysis::BlockMutationAnalysis,
        location: impl FnOnce() -> String,
    ) -> Result<()> {
        // ERROR: Field assignments that can't thread state back are not allowed.
        // Sort before picking one so the reported field is deterministic across
        // builds/runs when a block writes more than one field.
        if !analysis.field_writes.is_empty() {
            let mut fields: Vec<&String> = analysis.field_writes.iter().collect();
            fields.sort_unstable();
            let field = fields[0];
            return Err(CodeGenError::field_assignment_in_unsupported_block(
                field,
                location(),
            ));
        }

        // WARNING: Local mutations in stored closures won't work as expected
        // Note: For now we're treating this as an error too, but the error type
        // is labeled as a warning in the message.
        // Only flag mutations of captured variables, not new local definitions.
        // A "captured mutation" is a write to a variable that was read before being
        // locally defined (i.e., it captures from outer scope).
        if let Some(variable) = {
            let mut vars: Vec<&String> = analysis
                .local_writes
                .intersection(&analysis.captured_reads)
                .collect();
            vars.sort_unstable();
            vars.into_iter().next()
        } {
            return Err(CodeGenError::LocalMutationInStoredClosure {
                variable: variable.clone(),
                location: location(),
            });
        }

        Ok(())
    }
}

/// Test-only record of every [`CoreErlangGenerator::threaded_locals_of`]
/// answer and every lowered set the packing and unpacking sides compute, so
/// the agreement corpus test (`tests/threaded_locals_agreement.rs`) can
/// check them during real codegen: the sets against the `beamtalk-core`
/// diagnostic pass's recognizer, and, per construct, the packing side's
/// block selection and lowered set against the unpacking side's.
#[cfg(test)]
pub(in crate::core_erlang) mod recorded_sets {
    use super::ThreadedLocals;
    use beamtalk_core::ast::{Block, Expression};
    use beamtalk_core::source_analysis::Span;
    use std::cell::RefCell;

    /// Which side of a construct computed a lowered set.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    pub(in crate::core_erlang) enum Side {
        /// A generator building the construct's result tuple
        /// (`loop_threaded_locals`, `branch_threaded_locals`).
        Pack,
        /// A sequencer reading it back (`threaded_locals_of`).
        Unpack,
    }

    /// One lowered set: which side, the spans of the blocks it was
    /// computed from (sorted), and the set.
    pub(in crate::core_erlang) type Lowered = (Side, Vec<Span>, Vec<String>);

    /// What one recording captured.
    #[derive(Debug, Default)]
    pub(in crate::core_erlang) struct Records {
        /// Each `threaded_locals_of` answer: the construct's span and its
        /// `names` (`None`: not a construct, or it threads nothing).
        pub(in crate::core_erlang) names: Vec<(Span, Option<Vec<String>>)>,
        /// Each lowered set computed.
        pub(in crate::core_erlang) lowered: Vec<Lowered>,
    }

    thread_local! {
        static RECORDS: RefCell<Option<Records>> = const { RefCell::new(None) };
    }

    pub(super) fn record(expr: &Expression, set: Option<&ThreadedLocals>) {
        RECORDS.with(|r| {
            if let Some(records) = r.borrow_mut().as_mut() {
                records
                    .names
                    .push((expr.span(), set.map(|s| s.names.clone())));
            }
        });
    }

    pub(super) fn record_lowered(side: Side, blocks: &[&Block], lowered: &[String]) {
        RECORDS.with(|r| {
            if let Some(records) = r.borrow_mut().as_mut() {
                let mut spans: Vec<Span> = blocks.iter().map(|b| b.span).collect();
                spans.sort_by_key(|s| (s.start(), s.end()));
                records.lowered.push((side, spans, lowered.to_vec()));
            }
        });
    }

    /// Runs `f` and returns what it recorded.
    pub(in crate::core_erlang) fn recording<T>(f: impl FnOnce() -> T) -> (T, Records) {
        RECORDS.with(|r| *r.borrow_mut() = Some(Records::default()));
        let out = f();
        let records = RECORDS.with(|r| r.borrow_mut().take()).unwrap_or_default();
        (out, records)
    }
}
