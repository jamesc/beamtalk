// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Control-flow threaded-variable analysis: predicates and helpers that
//! decide *whether* an expression, block, or loop body needs state/class-var
//! threading, and *which* variables it threads — consumed across
//! `control_flow/`, `expressions.rs`, `value_type_codegen.rs`, and
//! `gen_server/methods.rs`.
//!
//! **DDD Context:** Compilation — Code Generation

use crate::core_erlang::generator::CoreErlangGenerator;
use crate::core_erlang::{CodeGenContext, CodeGenError, Result, block_analysis};
use beamtalk_core::ast::{Block, Expression, MessageSelector, WellKnownSelector};
use std::collections::HashSet;

impl CoreErlangGenerator {
    /// BT-153/BT-245/BT-598: Check if mutation threading should be used for a block.
    /// In REPL mode, local variable mutations trigger threading.
    /// In actor module mode, field writes, self-sends, OR local variable
    /// mutations trigger threading. Local vars are threaded through the state accumulator
    /// map alongside fields.
    /// In value type module mode, only field writes trigger threading (no state map).
    /// BT-1346: Class methods have no State variable — field/self-send threading is disabled.
    /// BT-1414: Captured local variable mutations in class method blocks are threaded
    /// via a fresh local map (same as value types).
    pub(in crate::core_erlang) fn needs_mutation_threading(
        &self,
        analysis: &block_analysis::BlockMutationAnalysis,
    ) -> bool {
        if self.is_repl_mode() {
            // REPL: both local vars and fields need threading
            analysis.has_mutations()
        } else if self.in_class_method() {
            // BT-1346: Class methods have no actor State variable — field writes and
            // self-sends must NOT trigger state threading.
            // BT-1414: However, captured local variable mutations (outer vars both read
            // and written in the block) DO need threading via a fresh local map, same as
            // value types. Without this, `do:` blocks silently lose local mutations.
            analysis
                .captured_reads
                .iter()
                .any(|v| analysis.local_writes.contains(v))
        } else if self.context == CodeGenContext::Actor {
            // BT-598: Actor methods: field writes, self-sends,
            // OR local variable mutations all need threading
            analysis.has_state_effects() || !analysis.local_writes.is_empty()
        } else {
            // BT-892: Value types have no State variable, so self-sends should
            // NOT trigger state threading. Only field writes need threading.
            // BT-1053: Captured local variable mutations (outer vars both read and
            // written in the block) also need threading via a fresh local map.
            !analysis.field_writes.is_empty()
                || analysis
                    .captured_reads
                    .iter()
                    .any(|v| analysis.local_writes.contains(v))
        }
    }

    /// BT-1329: Returns `true` if the block body contains list op message sends
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

    /// BT-3423: the precomputed [`block_analysis::BlockMutationAnalysis`]
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

    /// BT-3423 (ADR 0118 phase 6): the single "does this literal block's body
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

    /// BT-1329: Recursively scans an expression for list op message sends with
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

    /// BT-2363: Collects outer-scope locals that are *written* inside a nested
    /// counted loop (`timesRepeat:`/`to:do:`/`to:by:do:`) or list op, including
    /// write-only mutations that `collect_list_op_cross_scope_mutations` (read+write
    /// only) misses.
    ///
    /// A name is collected when it is in the nested block's `local_writes`, is not a
    /// parameter of any enclosing block (`excluded_params` or the nested block's own
    /// params), and resolves to an existing outer-scope binding (`lookup_var`). The
    /// `lookup_var` guard is why this needs `&self` rather than being a free function:
    /// it distinguishes a genuine outer local from a block-internal temporary.
    pub(in crate::core_erlang) fn collect_nested_loop_outer_local_writes(
        &self,
        expr: &Expression,
        excluded_params: &HashSet<String>,
        out: &mut HashSet<String>,
    ) {
        use crate::core_erlang::block_analysis::analyze_block;
        use beamtalk_core::ast::MessageSelector;

        // Peel parens then an assignment RHS (which may itself be parenthesized) so
        // forms like `_r := (1 to: 5 do: [...])` are still inspected — mirrors
        // `expr_has_nested_counted_loop_threading`.
        let inner = match expr.unwrap_parens() {
            Expression::Assignment { value, .. } => value.unwrap_parens(),
            other => other,
        };
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = inner
        else {
            return;
        };
        let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();

        // BT-3173: ensure:/on:do:/ifNotNil: aren't loops themselves, but a
        // loop may be nested inside one of their blocks — recurse straight
        // through (the receiver for ensure:/on:do:, any block arguments for
        // all three) so a nested loop's outer-local write buried behind one
        // of these constructs is still found. Mirrors the identical
        // extension in `control_flow::collect_list_op_cross_scope_mutations`.
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
                let mut all_excluded: HashSet<String> = excluded_params.clone();
                all_excluded.extend(Self::block_param_names(block));
                for stmt in &block.body {
                    self.collect_nested_loop_outer_local_writes(
                        &stmt.expression,
                        &all_excluded,
                        out,
                    );
                }
            }
            return;
        }

        let body_block = match sel.as_str() {
            "do:" | "collect:" | "select:" | "reject:" | "anySatisfy:" | "allSatisfy:"
            | "timesRepeat:" => match arguments.last() {
                Some(Expression::Block(block)) => block,
                _ => return,
            },
            "inject:into:" | "to:do:" if arguments.len() == 2 => match &arguments[1] {
                Expression::Block(block) => block,
                _ => return,
            },
            "to:by:do:" if arguments.len() == 3 => match &arguments[2] {
                Expression::Block(block) => block,
                _ => return,
            },
            _ => return,
        };

        let analysis = self
            .semantic_facts
            .block_profile(&body_block.span)
            .cloned()
            .unwrap_or_else(|| analyze_block(body_block));
        // Accumulate enclosing params with this block's own params so a loop variable
        // bound at any enclosing level is never mistaken for a threadable outer local.
        let mut all_excluded: HashSet<String> = excluded_params.clone();
        all_excluded.extend(Self::block_param_names(body_block));

        for v in &analysis.local_writes {
            if !all_excluded.contains(v.as_str()) && self.lookup_var(v).is_some() {
                out.insert(v.clone());
            }
        }

        // Recurse so deeper nesting (loops two or more levels deep) is detected.
        for stmt in &body_block.body {
            self.collect_nested_loop_outer_local_writes(&stmt.expression, &all_excluded, out);
        }
    }

    /// Generates code for field access (e.g., self.value).
    /// Generates a method body with the reply tuple embedded.
    ///
    /// This is used for actor method dispatch to ensure state threading works correctly.
    /// The generated code looks like:
    /// ```erlang
    /// let _Val1 = <value1> in let State1 = ... in
    /// let _Val2 = <value2> in let State2 = ... in
    ///
    /// Check if an expression is a control flow construct (whileTrue:, whileFalse:, timesRepeat:, etc.)
    /// with literal blocks that has threaded mutations. Returns the threaded variable names if so.
    ///
    /// BT-2374: the loop / foldl-list-op extraction set is no longer re-derived by a
    /// parallel `threaded_vars_*` family — it delegates to the single packing-side
    /// authority [`Self::compute_threaded_locals_for_loop`] (which already branches per
    /// context). The extraction side reading back exactly the set the packing side wrote
    /// is the invariant that keeps `maps:get/2` from hitting a missing `__local__` key;
    /// sharing one function makes that symmetry structural rather than a hand-maintained
    /// mirror. Conditionals retain [`Self::conditional_threaded_locals`], which is already
    /// the shared seed/extract authority for the inline-`case` path.
    pub(in crate::core_erlang) fn get_control_flow_threaded_vars(
        &self,
        expr: &Expression,
    ) -> Option<Vec<String>> {
        // BT-2355: `_r := (loop)` wraps the construct in parentheses; peel them so
        // the threaded locals are still discovered when the construct is an
        // assignment RHS or sub-expression.
        let expr = expr.unwrap_parens();
        let Expression::MessageSend {
            receiver,
            selector,
            arguments,
            ..
        } = expr
        else {
            return None;
        };

        // BT-2073: `whileTrue:` / `whileFalse:` are well-known; dispatch via the enum.
        // The condition block (receiver) and body block (first argument) reads/writes are
        // unioned by `compute_threaded_locals_for_loop(body, Some(condition))`.
        if matches!(
            selector.well_known(),
            Some(WellKnownSelector::WhileTrue | WellKnownSelector::WhileFalse)
        ) {
            let (Expression::Block(_), Some(Expression::Block(body_block))) =
                (receiver.as_ref(), arguments.first())
            else {
                return None;
            };
            return Self::non_empty(
                self.compute_threaded_locals_for_loop(body_block, Some(receiver.as_ref())),
            );
        }

        let MessageSelector::Keyword(parts) = selector else {
            return None;
        };
        let selector_name: String = parts.iter().map(|kw| kw.keyword.as_str()).collect();

        // BT-2703: `eachWithIndex:`/`do:separatedBy:` desugar to an `inject:into:`
        // fold (see `enumeration_ops`), packing the block's outer-local mutations
        // into the same `__local__` StateAcc keys. The element block is the first
        // argument; `do:separatedBy:`'s separator (the second block) runs in the
        // fold too, so its outer-local writes are unioned in as well. Gated on
        // `enumeration_threads_actor_state`: only the actor fold packs those keys
        // into a `{Acc, State}` reply tuple, so outside it (value types, REPL, a
        // direct-params loop) there is no `__local__` StateAcc to extract from —
        // context-dependent threading is exactly why BT-3423's shared
        // `state_threaded_block_arg_indices` table excludes these two selectors
        // (see its doc comment), so they're handled here instead of falling
        // through to it.
        match selector_name.as_str() {
            "eachWithIndex:" if arguments.len() == 1 && self.enumeration_threads_actor_state() => {
                return self.threaded_locals_of_loop_body(arguments.first());
            }
            "do:separatedBy:" if arguments.len() == 2 && self.enumeration_threads_actor_state() => {
                return Self::non_empty(
                    self.conditional_threaded_locals(&Self::block_args(arguments)),
                );
            }
            _ => {}
        }

        // BT-2355: conditionals thread outer-local mutations through the StateAcc
        // map under `__local__` keys (see generate_*_with_mutations, which also seed
        // those keys so extraction is safe even when the taken branch did not write
        // them). `is_conditional_selector` names exactly the selectors with a
        // `generate_*_with_mutations` inline-case generator — others (`ifFalse:ifTrue:`,
        // …) are not routed through that path, so treating them here would be
        // unreachable.
        if beamtalk_core::state_threading_selectors::is_conditional_selector(&selector_name) {
            return Self::non_empty(self.conditional_threaded_locals(&Self::block_args(arguments)));
        }

        // BT-3160: on:do:/ensure: thread outer-local mutations the same way a
        // conditional's branches do — the try (receiver) block and any
        // handler/cleanup block(s) are mutually-exclusive-or-sequential
        // alternatives that are all compiled, only some of which run at a given
        // call, so the union of their local writes is the threaded set. The
        // seeding counterpart (`generate_on_do_with_mutations`/
        // `generate_ensure_with_mutations`, via `seed_conditional_locals`)
        // guarantees every `__local__` key extracted here is present even on a
        // path that didn't itself write it.
        if beamtalk_core::state_threading_selectors::is_exception_selector(&selector_name) {
            let mut blocks: Vec<&Block> = Vec::new();
            if let Expression::Block(b) = receiver.as_ref() {
                blocks.push(b);
            }
            blocks.extend(Self::block_args(arguments));
            return Self::non_empty(self.conditional_threaded_locals(&blocks));
        }

        // BT-3423 (ADR 0118 §7): everything else is the loop/list-op family —
        // `to:do:`/`to:by:do:`/`inject:into:` (block at a non-zero index) and
        // the `timesRepeat:`/`do:`/`collect:`/… foldl family (block at index
        // 0), all of which pack updated locals into the StateAcc map returned
        // as `element(2, …)` of the result tuple (BT-1276/BT-2355/BT-2356) via
        // `compute_threaded_locals_for_loop`, the single packing-side
        // authority. The shared `state_threaded_block_arg_indices` table
        // (single source with `is_state_threading_keyword_selector`) says
        // which argument index(es) hold the threaded block(s); every entry
        // reaching this fallback today has exactly one, but the union path
        // below stays generic rather than assuming that.
        match beamtalk_core::state_threading_selectors::state_threaded_block_arg_indices(
            &selector_name,
        ) {
            [] => None,
            [i] => self.threaded_locals_of_loop_body(arguments.get(*i)),
            indices => {
                let blocks: Vec<&Block> = indices
                    .iter()
                    .filter_map(|&i| match arguments.get(i) {
                        Some(Expression::Block(b)) => Some(b),
                        _ => None,
                    })
                    .collect();
                Self::non_empty(self.conditional_threaded_locals(&blocks))
            }
        }
    }

    /// BT-2374: Computes the threaded outer-locals for a counted loop (`timesRepeat:`,
    /// `to:do:`, `to:by:do:`) or foldl list/dict op body block via the single packing-side
    /// authority [`Self::compute_threaded_locals_for_loop`], returning `None` when the set
    /// is empty (so the caller's `if let Some(..)` short-circuits) or when `body_arg` is
    /// not a literal block.
    pub(in crate::core_erlang) fn threaded_locals_of_loop_body(
        &self,
        body_arg: Option<&Expression>,
    ) -> Option<Vec<String>> {
        let Some(Expression::Block(body_block)) = body_arg else {
            return None;
        };
        Self::non_empty(self.compute_threaded_locals_for_loop(body_block, None))
    }

    /// BT-2374: `Some(v)` when `v` is non-empty, else `None`. Lets the threaded-locals
    /// extraction collapse a `Vec<String>` packing-side set into the `Option<Vec<String>>`
    /// the Actor method-body sequencer consumes, where empty and absent are equivalent.
    pub(in crate::core_erlang) fn non_empty(v: Vec<String>) -> Option<Vec<String>> {
        if v.is_empty() { None } else { Some(v) }
    }

    /// BT-2355: Collects the `Block` arguments of a message send (e.g. the branch
    /// blocks of a conditional), preserving order.
    pub(in crate::core_erlang) fn block_args(arguments: &[Expression]) -> Vec<&Block> {
        arguments
            .iter()
            .filter_map(|a| {
                if let Expression::Block(b) = a {
                    Some(b)
                } else {
                    None
                }
            })
            .collect()
    }

    /// BT-2355: Computes the outer-local variables that a conditional's branch
    /// blocks mutate and that must be threaded back through the `StateAcc` map.
    ///
    /// A variable is threaded when it is written in some branch, is bound in the
    /// enclosing (outer) scope, and is not a block parameter. This covers both
    /// write-only (`flag ifTrue: [m := 9]`) and read+write
    /// (`flag ifTrue: [sum := sum + 7]`) mutations, while excluding block-local
    /// temporaries (which are not bound in the outer scope).
    ///
    /// The same set drives both the seeding emitted by `generate_*_with_mutations`
    /// and the extraction emitted by the method-body sequencer, keeping them in
    /// sync so a non-taken branch never leaves a `__local__` key missing.
    pub(in crate::core_erlang) fn conditional_threaded_locals(
        &self,
        blocks: &[&Block],
    ) -> Vec<String> {
        use crate::core_erlang::block_analysis::analyze_block;

        let mut set = HashSet::new();
        for block in blocks {
            let analysis = self
                .semantic_facts
                .block_profile(&block.span)
                .cloned()
                .unwrap_or_else(|| analyze_block(block));
            let params = Self::block_param_names(block);
            // BT-2356: `analyze_block` does not propagate `local_writes` out of nested
            // (non-conditional) blocks, so an outer local mutated by a nested list op in a
            // branch — e.g. `flag ifTrue: [ items do: [:x | sum := sum + x] ]` — is invisible
            // to `analysis.local_writes`. Collect those cross-scope mutations too so the var is
            // both seeded (by `seed_conditional_locals`) and extracted by the method-body
            // sequencer. The branch body re-threads the nested op's mutation into the branch's
            // returned StateAcc (the nested op is itself classified as state-threading), so the
            // seeded key is overwritten with the live value rather than left stale.
            let mut cross_scope = HashSet::new();
            for stmt in &block.body {
                Self::collect_list_op_cross_scope_mutations_recursive(
                    &stmt.expression,
                    &self.semantic_facts,
                    &mut cross_scope,
                );
            }
            for v in analysis.local_writes.iter().chain(cross_scope.iter()) {
                if params.contains(v) {
                    continue;
                }
                if self.lookup_var(v).is_some() {
                    set.insert(v.clone());
                }
            }
        }
        let mut out: Vec<String> = set.into_iter().collect();
        // Deterministic order for stable codegen output.
        out.sort();
        out
    }

    /// Returns the set of block parameter names for exclusion from threaded vars.
    pub(in crate::core_erlang) fn block_param_names(block: &Block) -> HashSet<String> {
        block
            .parameters
            .iter()
            .map(|p| p.name.to_string())
            .collect()
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
    /// BT-852 claimed production call sites could be removed because blocks with
    /// mutations are supported via the Tier 2 stateful block protocol (ADR 0041).
    /// BT-2792 found that's only true for *captured local* mutations
    /// (`captured_mutations_for_block` in `expressions.rs`, which promotes to
    /// `generate_block_stateful`) — Tier 2 promotion never triggers on `self.field :=`
    /// writes. A block with field writes that reaches the generic "pure fun" fallback
    /// in `generate_block` silently emits Core Erlang `erlc` rejects with "unbound
    /// variable" (the block's own `fun` bumps the shared state-version counter, but
    /// that binding is scoped inside the `fun` and never reaches the caller).
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
    /// case `generate_block` currently cares about. BT-2797 tracks lifting the
    /// field-write restriction for stored/opaque blocks by generalizing Tier 2 the same
    /// way; once that lands this function's field-write branch should shrink to
    /// whatever shapes remain genuinely unsupported.
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
        // BT-665: Only flag mutations of captured variables, not new local definitions.
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
