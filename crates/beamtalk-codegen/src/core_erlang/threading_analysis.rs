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
use beamtalk_core::ast::{Block, Expression, MessageSelector};
use beamtalk_core::semantic_analysis::block_facts::{
    LocalThreadingConstruct, LocalThreadingFamily, OuterLocalWrite, RootMethodKind,
    expression_threaded_writes, local_threading_construct, lookup_arm_blocks, producer_body_block,
    producer_packed_blocks, threaded_block_writes, threaded_today_block_writes,
};
use beamtalk_core::state_threading_selectors::state_threaded_block_arg_indices;

/// Which construct a [`ThreadedLocals`] set belongs to.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::core_erlang) enum ThreadedConstruct {
    /// A construct `beamtalk_core`'s [`local_threading_construct`]
    /// recognizes, by family.
    Inline(LocalThreadingFamily),
    /// A `value`-family send to a Tier 2 block-valued local.
    Tier2Value,
}

/// ADR 0131 §1: the threaded outer locals of one local-threading construct,
/// built by [`CoreErlangGenerator::threaded_locals_of`].
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
        // The per-context rule lives in `beamtalk-core`
        // (`BlockMutationAnalysis::needs_mutation_threading`), shared with the
        // ADR 0133 `groupBy:` residual check (BT-3909).
        let root = if self.is_repl_mode() {
            None
        } else if self.threads_no_actor_state() {
            Some(RootMethodKind::Class)
        } else if self.context == CodeGenContext::Actor {
            Some(RootMethodKind::Actor)
        } else {
            Some(RootMethodKind::ValueType)
        };
        // ADR 0122/BT-3514: the actor "family half" (field writes/self-sends)
        // is gated behind `ThreadedFamilies` non-emptiness —
        // `eligible_families()` reports exactly `[State]` for a non-class-method
        // Actor instance method, never empty (ADR 0122 §"State already fits").
        let actor_state_eligible = !ThreadedFamilies::from_matches(&self.eligible_families())
            .as_slice()
            .is_empty();
        // ADR 0137: a construct whose blocks run in place here and touch the
        // actor's state threads `State` rather than run its blocks as closures
        // against the cell (a bracketed call, or a plain field read, which must
        // see the version the method holds).
        let synced = self.synced_calls_active() && self.analysis_touches_actor_state(analysis);
        analysis.needs_mutation_threading_with(root, actor_state_eligible, synced)
    }

    /// Whether an evaluated literal block (`[...] value`) is lowered in place with
    /// `State` threaded. Holding only calls that may run user code does not
    /// make it so (ADR 0137): such a block is a closure the call as a whole
    /// brackets, as for a loop.
    pub(in crate::core_erlang) fn value_block_needs_threading(
        &self,
        analysis: &block_analysis::BlockMutationAnalysis,
    ) -> bool {
        self.needs_mutation_threading_with_synced(analysis, false)
    }

    /// Whether the evaluated literal block `block` is inlined with its writes
    /// threaded (`generate_block_value_inline_with_mutations`, or the actor
    /// producer [`Self::actor_evaluated_block_producer_parts`]) rather than
    /// applied as a plain fun: it writes a captured local, it writes a field or
    /// sends to `self` where `State` is threaded
    /// ([`Self::value_block_needs_threading`]), or, in the REPL, it writes a
    /// binding anywhere (BT-3823). The one predicate the `value`-family
    /// intrinsics, the statement classifier (`is_tier2_value_call`) and the
    /// actor producer gate read.
    pub(in crate::core_erlang) fn evaluated_block_inlines_with_mutations(
        &self,
        block: &Block,
    ) -> bool {
        !Self::captured_mutations_for_block(block).is_empty()
            || self.value_block_needs_threading(&block_analysis::analyze_block(block))
            || self.repl_blocks_write_bindings(&[block])
    }

    /// BT-3982: whether the frame being lowered threads the actor `State` an
    /// evaluated literal block continues: an actor instance method outside the
    /// REPL, an actor closure (ADR 0137 §3) and a frame with a private scratch
    /// map (`conditional_has_private_scratch`). The scope half of
    /// [`Self::actor_evaluated_block_producer_parts`], and where the legacy
    /// inline lowering's raw `{Value, State}` tuple reports
    /// `VerifyError::UnthreadedEvaluatedBlock` unless consumed.
    pub(in crate::core_erlang) fn threads_actor_evaluated_block_state(&self) -> bool {
        !self.is_repl_mode()
            && self.in_actor_instance_context()
            && !self.conditional_has_private_scratch(self.current_frame())
    }

    /// BT-3982: the block and arguments of `expr` (paren-unwrapped) when it is
    /// an evaluated literal block of the positional `value` family
    /// (`beamtalk_core`'s `literal_block_value_send`) that an actor instance
    /// method inlines with `State` threaded
    /// ([`Self::evaluated_block_inlines_with_mutations`]), or `None`. Such a
    /// send is a producer wherever it sits: one `ConstructTuple` in the
    /// enclosing frame and its `State` `Bind`, answering `element(1, Carrier)`
    /// (`generate_evaluated_block_producer`), so its `{Value, State}` carrier
    /// never crosses an expression boundary. The one predicate behind the
    /// producer gate ([`Self::inline_control_flow_needs_threading`], read by
    /// the prelude probe) and its lowering (`lower_inline_control_flow`).
    ///
    /// Not in the REPL (the bindings map is unpacked by `repl_loop_mutated`),
    /// a value-type method (`vt_inline_block_value_producer` splices `Self`),
    /// a class method or an actor closure (ADR 0137 §3: no `State` there;
    /// fields are written in place, so only the block's locals thread), nor
    /// in a frame with a private scratch map (a direct-params or tuple-acc
    /// loop or fold body, `conditional_has_private_scratch`), which has no
    /// `State` to continue: a block there writes only locals of its own
    /// (one that writes a field or sends to `self` makes the loop thread
    /// `State`), the `InlineBlockLocalInLoop` residual's shape (BT-3948).
    pub(in crate::core_erlang) fn actor_evaluated_block_producer_parts<'a>(
        &self,
        expr: &'a Expression,
    ) -> Option<(&'a Block, &'a [Expression])> {
        if !self.threads_actor_evaluated_block_state() {
            return None;
        }
        #[cfg(test)]
        if super::threaded_expr::fault::active()
            == Some(super::threaded_expr::fault::LocalRebindFault::EvaluatedBlockNotAProducer)
        {
            return None;
        }
        beamtalk_core::semantic_analysis::block_facts::literal_block_value_send(expr)
            .filter(|(block, _)| self.evaluated_block_inlines_with_mutations(block))
    }

    /// [`Self::block_touches_actor_state`] over an already-computed analysis.
    pub(in crate::core_erlang) fn analysis_touches_actor_state(
        &self,
        analysis: &block_analysis::BlockMutationAnalysis,
    ) -> bool {
        !analysis.field_reads.is_empty()
            || !analysis.field_writes.is_empty()
            || analysis.has_self_sends
            || analysis.has_user_code_sends
            || analysis.captured_reads.iter().any(|name| {
                !matches!(name.as_str(), "self" | "super" | "true" | "false" | "nil")
                    && self.lookup_var(name).is_none()
            })
    }

    /// [`Self::needs_mutation_threading`] with the bracket's contribution
    /// decided by the caller ([`Self::block_arg_needs_threading_with`]).
    fn needs_mutation_threading_with_synced(
        &self,
        analysis: &block_analysis::BlockMutationAnalysis,
        synced: bool,
    ) -> bool {
        if synced {
            return self.needs_mutation_threading(analysis);
        }
        let root = if self.is_repl_mode() {
            None
        } else if self.threads_no_actor_state() {
            Some(RootMethodKind::Class)
        } else if self.context == CodeGenContext::Actor {
            Some(RootMethodKind::Actor)
        } else {
            Some(RootMethodKind::ValueType)
        };
        let actor_state_eligible = !ThreadedFamilies::from_matches(&self.eligible_families())
            .as_slice()
            .is_empty();
        analysis.needs_mutation_threading_with(root, actor_state_eligible, false)
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
        // ADR 0131 Phase 2b: a local the body itself introduces with a
        // top-level assignment and never reads from outside
        // (`w := 0. [w < 2] whileTrue: [w := w + 1]`) is crossed by the nested
        // loop but is not this construct's to thread: counting it makes the
        // enclosing construct thread an empty set. This filter is decided from
        // the body alone (as `analyze_block` decides a "new binding"); the
        // catch-boundary scan below also reads the live scope, which is why
        // method-body classification runs per statement, interleaved with
        // lowering (BT-3840).
        let analysis = self.block_profile_or_analyze(body);
        let body_defined = Self::body_introduced_locals(body, &analysis.captured_reads);
        cross_scope_writes
            .iter()
            .any(|name| !body_defined.contains(name.as_str()))
            || self.body_has_operand_loop_or_fold_writes(body)
    }

    /// ADR 0131 Phase 2b: whether a loop or fold at a sequenced operand site
    /// of one of `body`'s statements (`(xs collect: [:x | t := t + x]) size`,
    /// `r := (xs inject: …) + 1`) writes an outer local. The statement-level
    /// scan above sees only a nested construct that *is* the statement (or an
    /// assignment's value); this is the same recognizer the loop-body
    /// producer fires on (`sequenced_operand_sites`, `threaded_locals_of`), so
    /// the enclosing construct threads exactly when its body contains a
    /// producer that writes.
    fn body_has_operand_loop_or_fold_writes(&self, body: &Block) -> bool {
        let analysis = self.block_profile_or_analyze(body);
        let body_defined = Self::body_introduced_locals(body, &analysis.captured_reads);
        body.body.iter().any(|stmt| {
            let loop_or_fold =
                beamtalk_core::semantic_analysis::block_facts::loop_body_operand_sites(
                    &stmt.expression,
                )
                .into_iter()
                .any(|site| {
                    local_threading_construct(site).is_some_and(|c| {
                        matches!(
                            c.family,
                            LocalThreadingFamily::Loop | LocalThreadingFamily::Fold
                        )
                    }) && self.threaded_locals_of(site).is_some_and(|set| {
                        // BT-3823: a REPL loop or fold threads the bindings
                        // map, so its `lowered` set is empty; it writes a
                        // binding when `names` is not.
                        !set.lowered.is_empty() || (self.is_repl_mode() && !set.names.is_empty())
                    })
                });
            loop_or_fold || self.statement_catch_boundary_writes(stmt, &body_defined)
        })
    }

    /// ADR 0131 Phase 4 (BT-3750): whether a `Result tryDo:` or `on:do:`/
    /// `ensure:` that is the statement of a body, or sits at one of its operand
    /// sites (`r := Result tryDo: [...]`), threads an outer local the
    /// block-level analysis ([`analyze_block`](block_analysis::analyze_block))
    /// does not propagate out: a write behind a nested construct it does not see
    /// through (a `Dictionary` lookup in the block). A name the body introduces
    /// itself is not the enclosing construct's to thread (`body_defined`, the
    /// same filter as the loop-and-fold scan above).
    fn statement_catch_boundary_writes(
        &self,
        stmt: &beamtalk_core::ast::ExpressionStatement,
        body_defined: &std::collections::HashSet<&str>,
    ) -> bool {
        use beamtalk_core::semantic_analysis::block_facts::loop_body_operand_sites;
        loop_body_operand_sites(&stmt.expression)
            .into_iter()
            .chain(std::iter::once(stmt.expression.unwrap_parens()))
            .any(|site| {
                local_threading_construct(site).is_some_and(|c| {
                    matches!(
                        c.family,
                        LocalThreadingFamily::TryDo | LocalThreadingFamily::Exception
                    )
                }) && self.threaded_locals_of(site).is_some_and(|set| {
                    // An outer local is a name bound in the enclosing scope
                    // (`threaded_locals_of`'s scope predicate), not one the
                    // body introduces itself.
                    set.names
                        .iter()
                        .any(|name| !body_defined.contains(name.as_str()))
                })
            })
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
            .block_profile(block.id)
            .cloned()
            .unwrap_or_else(|| block_analysis::analyze_block(block))
    }

    /// ADR 0118 phase 6: the "does this literal block's body need mutation
    /// threading" heuristic — block-local/field mutations
    /// ([`Self::needs_mutation_threading`], reading [`Self::block_profile_or_analyze`])
    /// OR a cross-scope mutation buried in a nested list-op/counted loop the
    /// block-level analysis alone can't see
    /// ([`Self::body_has_list_op_cross_scope_mutations`]).
    ///
    /// Read directly only for a conditional arm and an `on:do:`/`ensure:`
    /// block: `control_flow_has_mutations` (`gen_server/methods.rs`: the
    /// exception-selector receiver and handler blocks, each conditional
    /// branch argument) and [`Self::conditional_needs_mutation_threading`]
    /// (`intrinsics.rs`, which extends it with an in-loop-body-local-write
    /// disjunct). Loops and folds read it through
    /// [`Self::loop_body_has_carried_mutations`] (BT-3834).
    ///
    /// BT-3823: in the REPL it is also true for a block that writes a binding
    /// anywhere, a nested construct included ([`Self::repl_blocks_write_bindings`]),
    /// so a REPL arm whose write sits in a fold inside a nested conditional is
    /// inlined over the bindings map. Through
    /// [`Self::loop_body_has_carried_mutations`] this widens what a REPL loop or
    /// fold treats as carried the same way: such a body threads the bindings map
    /// (the REPL always has that ambient carrier).
    pub(in crate::core_erlang) fn block_arg_needs_threading(&self, block: &Block) -> bool {
        self.block_arg_needs_threading_with(block, true)
    }

    /// [`Self::block_arg_needs_threading`] with the ADR 0137 bracket decided by
    /// the caller: a conditional arm or protected body that holds a bracketed
    /// call threads `State` through it (`synced`), a loop or fold body does
    /// not. A body with no state effect of its own is a plain closure that
    /// runs in cell mode ([`Self::in_cell_closure`]); the loop call as a whole
    /// is the bracket.
    fn block_arg_needs_threading_with(&self, block: &Block, synced: bool) -> bool {
        let analysis = self.block_profile_or_analyze(block);
        self.needs_mutation_threading_with_synced(&analysis, synced)
            || self.body_has_list_op_cross_scope_mutations(block)
            // BT-3823: in the REPL, a block that writes a binding anywhere (a
            // conditional arm whose write sits in a fold inside a nested
            // conditional) threads the bindings map, so it is a REPL arm.
            || self.repl_blocks_write_bindings(&[block])
    }

    /// BT-3834: whether a loop or fold over `body` has something to carry, so
    /// it is lowered with mutation threading (a `ThreadingPlan`) rather than as
    /// a plain loop.
    ///
    /// The loop's threaded set is [`Self::loop_threaded_locals`] (ADR 0131's
    /// one recognizer): when it is non-empty the loop threads, whatever the
    /// mutation heuristic ([`Self::block_arg_needs_threading`]) says. That
    /// heuristic misses a write-only local, and a write buried in a construct
    /// the block-level analysis does not see through (BT-3839: a `Dictionary`
    /// lookup's handler in a loop nested in the body, which left an actor
    /// `do:` around it lowered by its statement as a plain loop while the
    /// nested loop threaded the write, so the write was dropped). When the
    /// heuristic says "threads" but the set is empty, a `StateAcc` lowering
    /// carries a map only if the scope has one to seed it with: the actor
    /// `State` of an actor instance method, the REPL bindings, or a family the
    /// body threads (`SelfVt` in a value-type method that writes a field).
    /// Anywhere else (a class method, a value-type method writing no field)
    /// the loop threads nothing, and a locals-less `StateAcc` loop would be
    /// seeded from an unbound `State`
    /// (`ClassMethodDefect::StateAccLoopWithoutLocals`), so it lowers as a
    /// plain loop instead.
    ///
    /// BT-3834: the gate of every counted loop and fold lowering — whether
    /// `body` is lowered with mutation threading (a `ThreadingPlan`) rather
    /// than as a plain loop. Every loop and fold gate reads this directly
    /// (the counted loops, the list ops, `eachWithIndex:`/`do:separatedBy:`,
    /// `while_loop_threads`) and so does the statement classifier
    /// (`control_flow_has_mutations`) for a loop or fold's block arguments,
    /// so the lowering and the caller that unpacks it cannot disagree.
    pub(in crate::core_erlang) fn loop_body_has_carried_mutations(&self, body: &Block) -> bool {
        !self.loop_threaded_locals(body, None).is_empty()
            || (self.block_arg_needs_threading_with(body, false)
                && self.loop_has_ambient_carrier(body))
    }

    /// BT-3999: whether a `whileTrue:`/`whileFalse:` `condition` has a state
    /// effect the loop carries, so the condition is compiled in the loop's own
    /// frame and hands back `{Bool, StateAcc}`.
    ///
    /// The effect itself is core's one rule
    /// ([`while_condition_has_state_effects`](beamtalk_core::semantic_analysis::block_facts::while_condition_has_state_effects):
    /// a field write, a self-send, a stored-block call or an opaque-callable HOM
    /// send; none in a class method). It is the condition-side twin of
    /// [`Self::loop_body_has_carried_mutations`] (BT-3834): an effect counts only
    /// when the loop has something to carry it in. That is a threaded outer
    /// local (the loop's set over `condition` and `body`, ADR 0131's one
    /// recognizer) or the scope's ambient carrier for the condition or the body
    /// ([`Self::loop_has_ambient_carrier`]: the actor `State`, the REPL bindings,
    /// a `SelfVt` family). Anywhere else (a value-type or `TestCase` method that
    /// writes no field) a self-send in the condition rebinds nothing, and
    /// answering `true` there opened a locals-less `StateAcc` loop seeded from
    /// an unbound `State` (`[self done] whileTrue: [nil]`, caught by
    /// `ClassMethodDefect::StateAccLoopWithoutLocals` in the `ValueType` scope).
    ///
    /// `body` is the loop's literal body block when it has one; without one
    /// only the condition's own locals and carrier are consulted.
    ///
    /// BT-4015: a condition that writes an outer local (in the REPL, a
    /// binding; [`Self::loop_condition_written_locals`]) is carried too: a loop
    /// carries the union of what its condition and its body produce, and a
    /// written local is a threaded local of the loop (or, in the REPL, a key of
    /// the bindings map the loop always carries). In a compiled method the
    /// Phase 0 check rejects such a write unless the condition also has a
    /// state effect; it reaches codegen from the REPL and a legacy workspace
    /// block body. Before, such a condition was a plain closure answering only
    /// its boolean: the write was lost (the loop re-read the pre-loop value
    /// forever), or a body that read it read the closure's own variable
    /// (`erlc`: unbound variable). Like a state effect, a write counts only
    /// when the loop has something to carry it in (BT-3999).
    pub(in crate::core_erlang) fn while_condition_has_carried_effects(
        &self,
        condition: &Expression,
        body: Option<&Block>,
    ) -> bool {
        let Expression::Block(cond_block) = condition else {
            return false;
        };
        // ADR 0137: in an actor instance method a condition holding a bracketed
        // call threads `State` like a state effect does, once the loop threads
        // at all. A loop whose body carries nothing is a plain loop whose
        // condition is a cell closure, bracketed with the loop as one call.
        let synced = self.synced_calls_active()
            && body.is_some_and(|body| self.loop_body_has_carried_mutations(body))
            && beamtalk_core::semantic_analysis::block_facts::while_condition_has_synced_user_code_sends(
                condition,
            );
        if !synced
            && !self.condition_write_is_carried(condition)
            && !beamtalk_core::semantic_analysis::block_facts::while_condition_has_state_effects(
                condition,
                self.in_class_method(),
            )
        {
            return false;
        }
        let threaded_locals = match body {
            Some(body) => self.loop_threaded_locals(body, Some(condition)),
            None => self.loop_threaded_locals(cond_block, None),
        };
        !threaded_locals.is_empty()
            || self.loop_has_ambient_carrier(cond_block)
            || body.is_some_and(|body| self.loop_has_ambient_carrier(body))
    }

    /// BT-4015: the outer locals (in the REPL, the bindings) a
    /// `whileTrue:`/`whileFalse:` `condition` block writes, a nested
    /// construct's write included: ADR 0131's one recognizer
    /// ([`Self::threaded_locals_of_blocks`]) over the condition block, its
    /// `names` (every write the condition must hand back, whether or not
    /// today's tuple builders pack it). Read before the condition is lowered,
    /// so a name the condition binds is not mistaken for an outer one. Empty
    /// for a condition that is not a literal block.
    ///
    /// The one rule both the loop gate
    /// ([`Self::while_condition_has_carried_effects`]: such a condition is
    /// carried) and the loop node's `condition_families` (what the verifier
    /// requires carried) read.
    pub(in crate::core_erlang) fn loop_condition_written_locals(
        &self,
        condition: &Expression,
    ) -> Vec<String> {
        let Expression::Block(cond_block) = condition else {
            return Vec::new();
        };
        self.threaded_locals_of_blocks(
            ThreadedConstruct::Inline(LocalThreadingFamily::Loop),
            &[cond_block],
            &[],
        )
        .map(|t| t.names)
        .unwrap_or_default()
    }

    /// BT-4015: whether `condition` writes an outer local the loop must carry
    /// ([`Self::loop_condition_written_locals`]).
    fn condition_write_is_carried(&self, condition: &Expression) -> bool {
        // Test-only fault injection: the pre-BT-4015 answer, so the verifier's
        // `ConditionFamilyNotCarried` over a `Local` family is shown to fire on
        // a real compile (`tests/while_condition_binding_corpus.rs`).
        #[cfg(test)]
        if super::threaded_expr::fault::active()
            == Some(super::threaded_expr::fault::LocalRebindFault::ConditionWritesNotCarried)
        {
            return false;
        }
        !self.loop_condition_written_locals(condition).is_empty()
    }

    /// Whether a loop over `body` that threads no local still has a map to
    /// seed a `StateAcc` with: the REPL bindings, or a family the body
    /// threads (the actor `State`, `SelfVt` in a value-type method that
    /// writes a field). See [`Self::loop_body_has_carried_mutations`].
    fn loop_has_ambient_carrier(&self, body: &Block) -> bool {
        self.is_repl_mode()
            || !self
                .body_threaded_families(body, &self.eligible_families())
                .as_slice()
                .is_empty()
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
    ///   (`tier2_local_vars`).
    ///
    /// `match:` has no construct of its own. Its arms are not closures, so
    /// their writes belong to whichever construct block contains the
    /// `match:`, and the walk descends into them.
    ///
    /// The set is the transitive closure over nested producers (ADR 0131 §1
    /// "Transitive closure"): core's [`threaded_block_writes`] descends into
    /// the blocks of every nested construct. [`ThreadedLocals::lowered`] uses
    /// [`threaded_today_block_writes`], which descends only into those whose
    /// construct [`LocalThreadingConstruct::is_threaded_today`]. In the REPL the
    /// set is the bindings the construct writes: every name assigned that is
    /// not bound inside the construct.
    ///
    /// Facts read: core's `block_facts` ([`local_threading_construct`],
    /// [`threaded_block_writes`], [`threaded_today_block_writes`], which read
    /// only the AST and the
    /// `bound_outside` scope predicate) and `state_threading_selectors`
    /// (through them, plus `state_threaded_block_arg_indices` for
    /// [`ThreadedLocals::lowered`]). The scope predicate is
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
            let lowered_blocks = self.lowered_construct_blocks(&construct, expr);
            self.threaded_locals_of_blocks(
                ThreadedConstruct::Inline(construct.family),
                &construct.blocks,
                &lowered_blocks,
            )
        });
        #[cfg(test)]
        recorded_sets::record(expr, set.as_ref());
        set
    }

    /// ADR 0131 §1a: the outer locals threaded by any local-threading
    /// construct reachable from `expr` without crossing a closure boundary —
    /// `expr` itself, or a construct nested anywhere in its operands
    /// (`self id: (c ifTrue: [t := …] …)`, `(c ifTrue: [t := …] …) + 0`),
    /// closed over the constructs nested in those. The union of
    /// [`Self::threaded_locals_of`]'s set for `expr` (which also covers its
    /// Tier 2 shapes) and core's [`expression_threaded_writes`], under the
    /// same scope predicate. Sorted, deduplicated.
    ///
    /// `also_bound` widens the scope with names the caller knows are bound
    /// outside the constructs although the generator's scope does not hold them
    /// yet (BT-3939: a local the same statement assigns before the construct
    /// runs); the narrow-scope callers pass `&[]`.
    pub(in crate::core_erlang) fn reachable_threaded_locals(
        &self,
        expr: &Expression,
        also_bound: &[String],
    ) -> Vec<String> {
        let mut names = Self::write_names(expression_threaded_writes(expr, &|name| {
            self.binds_threadable_local(name) || also_bound.iter().any(|a| a == name)
        }));
        if let Some(set) = self.threaded_locals_of(expr) {
            names.extend(set.names);
        }
        // BT-3910: and the local a value-position assignment inside it rebinds
        // (`#[t, (t := 7), t]`: the first `t` is read before the rebind).
        names.extend(self.contained_local_assignment_targets(expr));
        names.sort();
        names.dedup();
        names
    }

    /// BT-3910: the locals the value-position local assignments inside `expr`
    /// (itself included, outside block literals) rebind: the producers
    /// [`Self::lower_local_assignment_producer`] lowers.
    pub(in crate::core_erlang) fn contained_local_assignment_targets(
        &self,
        expr: &Expression,
    ) -> Vec<String> {
        if self.value_assign_sites.is_empty() {
            return Vec::new();
        }
        beamtalk_core::semantic_analysis::block_facts::assignments_outside_blocks(expr)
            .into_iter()
            .filter(|(_, span)| self.value_assign_sites.contains(span))
            .map(|(name, _)| name.to_string())
            .collect()
    }

    /// The scope predicate every threaded-set walk uses: `name` is bound
    /// outside the construct (any name in the REPL, whose set is the
    /// bindings the construct writes).
    pub(in crate::core_erlang) fn binds_threadable_local(&self, name: &str) -> bool {
        self.is_repl_mode() || self.lookup_var(name).is_some()
    }

    /// The block-level kernel of [`Self::threaded_locals_of`]: the threaded
    /// set of a construct of kind `construct` whose blocks are `blocks`,
    /// with `lowered_blocks` (a subset of `blocks`) the ones today's
    /// lowering packs. `None` when the set is empty.
    ///
    /// The loop, fold, conditional and exception generators call it
    /// directly with the blocks they lower, because they are handed the
    /// blocks rather than the send. They pass the same blocks
    /// [`Self::threaded_locals_of`] derives from the send, so packing and
    /// unpacking agree by construction.
    pub(in crate::core_erlang) fn threaded_locals_of_blocks(
        &self,
        construct: ThreadedConstruct,
        blocks: &[&Block],
        lowered_blocks: &[&Block],
    ) -> Option<ThreadedLocals> {
        let names = Self::write_names(threaded_block_writes(blocks, &|name| {
            self.binds_threadable_local(name)
        }));
        let repl = self.is_repl_mode();
        if names.is_empty() {
            return None;
        }
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
        Some(ThreadedLocals {
            construct,
            names,
            lowered,
        })
    }

    /// BT-3841: the threaded set of an evaluated literal block
    /// (`[...] value`, `literal_block_value_arm`) whose `arms` is its one block:
    /// the [`Self::threaded_locals_of_blocks`] kernel over the arm, with the arm
    /// packed (`lowered`) like a conditional's. [`Self::threaded_locals_of`]
    /// leaves `lowered` empty for the same send (the legacy inline-block path
    /// unpacks its own tuple), so the producer reads this one; the producer rule
    /// (`conditional_producer_locals`) is the only caller.
    pub(in crate::core_erlang) fn block_value_threaded_locals(
        &self,
        arms: &[&Block],
    ) -> Option<ThreadedLocals> {
        self.threaded_locals_of_blocks(
            ThreadedConstruct::Inline(LocalThreadingFamily::BlockValue),
            arms,
            arms,
        )
    }

    /// The lowered set of a loop or fold generator's `body` block (and,
    /// for `whileTrue:`/`whileFalse:`, its `condition`): the
    /// [`Self::threaded_locals_of_blocks`] kernel over the same blocks
    /// [`Self::threaded_locals_of`] reads from the send. Empty when it
    /// threads nothing.
    pub(in crate::core_erlang) fn loop_threaded_locals(
        &self,
        body: &Block,
        condition: Option<&Expression>,
    ) -> Vec<String> {
        self.loop_threaded_locals_with_handler(body, condition, None)
    }

    /// [`Self::loop_threaded_locals`] for a fold that also packs a handler
    /// block (ADR 0131 Phase 2c: `detect:ifNone:`'s `ifNone:` block,
    /// `producer_handler_arg_index`).
    pub(in crate::core_erlang) fn loop_threaded_locals_with_handler(
        &self,
        body: &Block,
        condition: Option<&Expression>,
        handler: Option<&Block>,
    ) -> Vec<String> {
        let mut blocks: Vec<&Block> = Vec::with_capacity(3);
        if let Some(Expression::Block(cond)) = condition {
            blocks.push(cond);
        }
        blocks.push(body);
        blocks.extend(handler);
        // `Loop` and `Fold` lower alike here (they differ only in which
        // blocks the send contributes, which the caller has already picked).
        self.threaded_locals_of_blocks(
            ThreadedConstruct::Inline(LocalThreadingFamily::Loop),
            &blocks,
            &blocks,
        )
        .map(|t| t.lowered)
        .unwrap_or_default()
    }

    /// The lowered set of the loop or fold producer send `expr`
    /// (paren-unwrapped): the writes of every block core's
    /// `producer_packed_blocks` names — the body and, for `detect:ifNone:`,
    /// the handler. Empty when `expr` is not a producer send or packs
    /// nothing. The fold plans, the value-type/class tuple unwrapping and the
    /// producer rule all read this set.
    pub(in crate::core_erlang) fn producer_threaded_locals(
        &self,
        expr: &Expression,
    ) -> Vec<String> {
        let packed = producer_packed_blocks(expr);
        if packed.is_empty() {
            return Vec::new();
        }
        self.threaded_locals_of_blocks(
            ThreadedConstruct::Inline(LocalThreadingFamily::Fold),
            &packed,
            &packed,
        )
        .map(|t| t.lowered)
        .unwrap_or_default()
    }

    /// The lowered set of a conditional's branch blocks, or of an
    /// `on:do:`/`ensure:`'s protected body and handler blocks: the
    /// [`Self::threaded_locals_of_blocks`] kernel over the same blocks
    /// [`Self::threaded_locals_of`] reads from the send. The same set drives
    /// the seeding emitted by `generate_*_with_mutations` and the
    /// extraction emitted by the method-body sequencer, so a branch that
    /// does not run never leaves a `__local__` key missing.
    pub(in crate::core_erlang) fn branch_threaded_locals(&self, blocks: &[&Block]) -> Vec<String> {
        self.threaded_locals_of_blocks(
            ThreadedConstruct::Inline(LocalThreadingFamily::Conditional),
            blocks,
            blocks,
        )
        .map(|t| t.lowered)
        .unwrap_or_default()
    }

    /// BT-3823: whether `blocks` (a conditional's arms or an
    /// `on:do:`/`ensure:`'s blocks) write a REPL binding anywhere, a nested
    /// loop or fold included: the [`Self::threaded_locals_of_blocks`] kernel's
    /// `names`. The REPL threads its bindings map, not `__local__` keys, so
    /// its `lowered` set ([`Self::branch_threaded_locals`]) is empty for a
    /// binding and cannot answer this.
    pub(in crate::core_erlang) fn repl_blocks_write_bindings(&self, blocks: &[&Block]) -> bool {
        self.is_repl_mode()
            && self
                .threaded_locals_of_blocks(
                    ThreadedConstruct::Inline(LocalThreadingFamily::Conditional),
                    blocks,
                    blocks,
                )
                .is_some()
    }

    /// The blocks of `construct` (recognized from `expr`) whose writes
    /// today's tuple builders pack into `__local__` keys. Empty for a
    /// construct whose result is not unpacked that way yet: the unary loops,
    /// `value` sent to a block literal (threaded by the inline-block path,
    /// `get_inline_block_captured_mutations`), the lookup selectors that are
    /// not `Dictionary`-inline (BT-3798), and `eachWithIndex:`/
    /// `do:separatedBy:` outside an actor's own fold
    /// ([`Self::enumeration_threads_actor_state`]).
    fn lowered_construct_blocks<'a>(
        &self,
        construct: &LocalThreadingConstruct<'a>,
        expr: &'a Expression,
    ) -> Vec<&'a Block> {
        let Expression::MessageSend {
            selector: MessageSelector::Keyword(_),
            arguments,
            ..
        } = expr
        else {
            return Vec::new();
        };
        let literal = |e: Option<&'a Expression>| match e {
            Some(Expression::Block(b)) => Some(b),
            _ => None,
        };
        let sel = construct.selector.as_str();
        match construct.family {
            LocalThreadingFamily::Conditional
            | LocalThreadingFamily::Exception
            | LocalThreadingFamily::TryDo => construct.blocks.clone(),
            LocalThreadingFamily::Loop | LocalThreadingFamily::Fold => match sel {
                // A loop or fold producer packs the blocks core's one rule
                // names (`producer_packed_blocks`): the body, `whileTrue:`'s
                // condition and `detect:ifNone:`'s handler. The body is
                // packed whether or not the condition is a literal block
                // (`aBlock whileTrue: [...]`), exactly as the while
                // generators' `loop_threaded_locals(body, condition)`.
                _ if producer_body_block(expr).is_some() => producer_packed_blocks(expr),
                "whileTrue:" | "whileFalse:" => Vec::new(),
                "eachWithIndex:" | "do:separatedBy:" => {
                    let arity = if sel == "eachWithIndex:" { 1 } else { 2 };
                    if arguments.len() == arity && self.enumeration_threads_actor_state() {
                        construct.blocks.clone()
                    } else {
                        Vec::new()
                    }
                }
                _ => state_threaded_block_arg_indices(sel)
                    .iter()
                    .filter_map(|&i| literal(arguments.get(i)))
                    .collect(),
            },
            // ADR 0131 Phase 2d (BT-3798): a `Dictionary`-inline lookup packs
            // its handler, the one block core's `lookup_arm_blocks` names.
            LocalThreadingFamily::Lookup => lookup_arm_blocks(expr).unwrap_or_default(),
            LocalThreadingFamily::BlockValue => Vec::new(),
        }
    }

    /// The Tier 2 shape of [`Self::threaded_locals_of`]: a `value`-family
    /// send to a Tier 2 block-valued local. Its set is the block's captured
    /// mutations, recorded by `prescan_tier2_local_vars`. Today's tuple
    /// builders pack nothing for it (the Tier 2 protocol threads it), so
    /// [`ThreadedLocals::lowered`] is empty.
    ///
    /// An ADR 0128 opaque-callable fold over such a local (`#(1) do: blk`) is
    /// not a construct here: `prescan_tier2_local_vars` promotes a stored
    /// block only when every later use is a `value`-family send, so passing
    /// it to a collection HOM keeps it a plain closure, and ADR 0131 §6
    /// rejects that site if the block writes an outer local (BT-3798).
    fn tier2_threaded_locals(&self, expr: &Expression) -> Option<ThreadedLocals> {
        let Expression::MessageSend {
            receiver, selector, ..
        } = expr
        else {
            return None;
        };
        if !selector.is_block_invocation() {
            return None;
        }
        let Expression::Identifier(id) = receiver.as_ref() else {
            return None;
        };
        let local = id.name.as_str();
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
            construct: ThreadedConstruct::Tier2Value,
            names,
            lowered: Vec::new(),
        })
    }

    /// The sorted, deduplicated names of `writes`.
    pub(in crate::core_erlang) fn write_names(writes: Vec<OuterLocalWrite>) -> Vec<String> {
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
/// answer, so the agreement corpus test (`tests/threaded_locals_agreement.rs`)
/// can compare what codegen computed, in codegen's own scope, against the
/// `beamtalk-core` diagnostic pass's recognizer.
#[cfg(test)]
pub(in crate::core_erlang) mod recorded_sets {
    use super::ThreadedLocals;
    use beamtalk_core::ast::Expression;
    use beamtalk_core::source_analysis::Span;
    use std::cell::RefCell;

    /// One answer: the construct's span and its set (`None`: not a
    /// construct, or it threads nothing).
    pub(in crate::core_erlang) type Record = (Span, Option<Vec<String>>);

    thread_local! {
        static RECORDS: RefCell<Option<Vec<Record>>> = const { RefCell::new(None) };
    }

    pub(super) fn record(expr: &Expression, set: Option<&ThreadedLocals>) {
        RECORDS.with(|r| {
            if let Some(records) = r.borrow_mut().as_mut() {
                records.push((expr.span(), set.map(|s| s.names.clone())));
            }
        });
    }

    /// Runs `f` and returns what it recorded.
    pub(in crate::core_erlang) fn recording<T>(f: impl FnOnce() -> T) -> (T, Vec<Record>) {
        RECORDS.with(|r| *r.borrow_mut() = Some(Vec::new()));
        let out = f();
        let records = RECORDS.with(|r| r.borrow_mut().take()).unwrap_or_default();
        (out, records)
    }
}
