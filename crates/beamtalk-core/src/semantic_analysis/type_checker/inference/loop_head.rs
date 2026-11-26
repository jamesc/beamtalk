// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-5298: the loop-head join for block literals that may run more than
//! once.
//!
//! A block body is inferred once, starting from the environment at the point
//! the block is written. That is exact for a block that runs at most once per
//! evaluation of its send (the arms of `ifTrue:ifFalse:`, `and:`, `ifNil:`,
//! a block receiving `value`, ...). It is wrong for one that may run again —
//! a `do:` body, a `whileTrue:` condition or body, a block handed to any
//! method that may call it repeatedly — because a later run starts from the
//! values an earlier run left in the enclosing method's locals:
//!
//! ```text
//! refused := nil
//! items do: [:each |
//!   (refused notNil and: [...]) ifTrue: [^Result error: refused]   "String | Nil here"
//!   refused := each]
//! ```
//!
//! So such a block's entry type for every outer local it writes is the join
//! of the type before the block and the type the body leaves it with, taken
//! to a fixpoint ([`TypeChecker::loop_head_widenings`]). The body is inferred
//! speculatively to find that type ([`TypeChecker::speculate`]), every side
//! effect of the speculative pass is rolled back, and then the body is
//! inferred for real from the widened entry.
//!
//! **DDD Context:** Semantic Analysis

use ecow::EcoString;

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::narrowing::visitors::block_has_return;
use crate::semantic_analysis::type_checker::{
    DynamicReason, EnvKey, InferredType, TypeChecker, TypeEnv,
};

/// How many speculative passes [`TypeChecker::loop_head_widenings`] runs
/// before it stops widening. One pass reaches the fixpoint unless a written
/// local's new type depends on another written local's, which is rare; the
/// cap keeps a pathological body from being inferred many times.
const MAX_LOOP_HEAD_PASSES: usize = 3;

/// One outer local's widened entry type for a repeatable block body.
pub(in crate::semantic_analysis::type_checker) type LoopHeadWidening = (EcoString, InferredType);

/// The side-effect state of a [`TypeChecker`] that inferring a block body may
/// change, captured by [`TypeChecker::speculate`] and restored after it.
struct SpeculationSnapshot {
    diagnostics: usize,
    collected_returns: usize,
    deferred_block_widenings: usize,
    defer_block_widening: bool,
    self_send_epoch: u64,
    known_remote_spans: std::collections::HashMap<
        crate::source_analysis::Span,
        crate::semantic_analysis::type_checker::known_remote::KnownRemote,
    >,
    block_param_pushdown:
        std::collections::HashMap<crate::source_analysis::Span, Vec<InferredType>>,
    unresolved_selector_sends: std::collections::HashSet<crate::source_analysis::Span>,
}

/// Whether every block literal argument of a send of `selector` runs at most
/// once per evaluation of the send. Anything not named here is assumed to be
/// able to call its block arguments repeatedly, which only ever widens a
/// type (the type before the block always stays a member of the join).
fn args_run_at_most_once(selector: &MessageSelector) -> bool {
    matches!(
        selector.name().as_str(),
        "ifTrue:"
            | "ifFalse:"
            | "ifTrue:ifFalse:"
            | "ifFalse:ifTrue:"
            | "ifNil:"
            | "ifNotNil:"
            | "ifNil:ifNotNil:"
            | "ifNotNil:ifNil:"
            | "and:"
            | "or:"
            | "ifOk:"
            | "ifError:"
            | "ifOk:ifError:"
            | "then:"
            | "ensure:"
            | "ifCurtailed:"
            | "on:do:"
            | "at:ifAbsent:"
            | "at:ifAbsentPut:"
            | "ifEmpty:"
            | "ifNotEmpty:"
            | "ifEmpty:ifNotEmpty:"
            | "ifNotEmpty:ifEmpty:"
    )
}

/// Whether a block literal *receiving* `selector` runs at most once per
/// evaluation of the send: invoking it (`value`, `value:`, ...,
/// `valueWithArguments:`) or guarding it (`ensure:`, `ifCurtailed:`,
/// `on:do:`). `whileTrue:` and the other loops run their receiver again.
fn receiver_runs_at_most_once(selector: &MessageSelector) -> bool {
    selector.is_block_invocation()
        || matches!(
            selector.name().as_str(),
            "ensure:" | "ifCurtailed:" | "on:do:"
        )
}

impl TypeChecker {
    /// BT-5298: record which of a send's block literals — `receiver` (when
    /// given) and `arguments` — run at most once per evaluation of the send,
    /// so their bodies are inferred from the plain entry environment. Every
    /// other block literal gets the loop-head join.
    pub(in crate::semantic_analysis::type_checker) fn mark_run_once_blocks(
        &mut self,
        receiver: Option<&Expression>,
        selector: &MessageSelector,
        arguments: &[Expression],
    ) {
        let mut repeatable: Vec<&crate::ast::Block> = Vec::new();
        if let Some(Expression::Block(block)) = receiver {
            if receiver_runs_at_most_once(selector) {
                self.run_once_blocks.insert(block.span);
            } else {
                repeatable.push(block);
            }
        }
        let args_once = args_run_at_most_once(selector);
        for arg in arguments {
            if let Expression::Block(block) = arg {
                if args_once {
                    self.run_once_blocks.insert(block.span);
                } else {
                    repeatable.push(block);
                }
            }
        }
        // Two repeatable blocks of one send may run interleaved
        // (`[...] whileTrue: [...]`, `do:separatedBy:`): each one's later
        // runs also see the other's writes.
        if repeatable.len() >= 2 {
            for block in &repeatable {
                let siblings: Vec<crate::ast::Block> = repeatable
                    .iter()
                    .filter(|other| other.span != block.span)
                    .map(|other| (*other).clone())
                    .collect();
                self.loop_siblings.insert(block.span, siblings);
            }
        }
    }

    /// BT-5298: the entry types of the outer locals `block` and its
    /// interleaved sibling blocks ([`Self::mark_run_once_blocks`]) write, for
    /// a block that may run more than once: each local's type before the
    /// block joined with every type a run of the body — or of a sibling's
    /// body — leaves it with, iterated to a fixpoint (at most
    /// [`MAX_LOOP_HEAD_PASSES`] speculative passes).
    ///
    /// `build_env` makes the block's entry environment from a set of
    /// widenings (block parameters, narrowings and all), exactly as the real
    /// pass will; it is called once per speculative pass. A sibling is
    /// inferred from a plain child of `env` with the same widenings and its
    /// parameters `Dynamic`, which can only make the join wider.
    ///
    /// Answers no widenings for a block that runs at most once, one ending in
    /// a top-level `^` (it never returns to run again), one whose family
    /// writes no outer local, and while already speculating: a nested block
    /// inside a speculative pass is inferred once, so the cost stays linear
    /// in the nesting depth.
    pub(in crate::semantic_analysis::type_checker) fn loop_head_widenings(
        &mut self,
        block: &crate::ast::Block,
        env: &TypeEnv,
        build_env: &dyn Fn(&[LoopHeadWidening]) -> TypeEnv,
        hierarchy: &ClassHierarchy,
        in_abstract_method: bool,
    ) -> Vec<LoopHeadWidening> {
        if self.speculating || self.run_once_blocks.contains(&block.span) || block_has_return(block)
        {
            return Vec::new();
        }
        let siblings = self
            .loop_siblings
            .get(&block.span)
            .cloned()
            .unwrap_or_default();
        let mut written: Vec<(EcoString, InferredType)> = Vec::new();
        for member in std::iter::once(block).chain(&siblings) {
            for write in
                crate::semantic_analysis::block_facts::outer_local_writes(member, &|name| {
                    env.get_local(name).is_some()
                })
            {
                if !matches!(
                    write.kind,
                    crate::semantic_analysis::block_facts::WriteKind::Local
                ) || written.iter().any(|(name, _)| *name == write.name)
                {
                    continue;
                }
                if let Some(before) = env.get_local(&write.name) {
                    written.push((write.name, before));
                }
            }
        }
        if written.is_empty() {
            return Vec::new();
        }

        // `widenings` holds only the locals whose entry type differs from
        // their type before the block.
        let mut widenings: Vec<LoopHeadWidening> = Vec::new();
        for _ in 0..MAX_LOOP_HEAD_PASSES {
            let mut exits = vec![build_env(&widenings)];
            for sibling in &siblings {
                let mut sibling_env = Self::block_child_env(sibling, env);
                Self::apply_loop_head_widenings(&widenings, &mut sibling_env);
                for param in &sibling.parameters {
                    sibling_env.set_local(
                        param.name.clone(),
                        InferredType::Dynamic(DynamicReason::UnannotatedParam),
                    );
                }
                exits.push(sibling_env);
            }
            let bodies = std::iter::once(block).chain(&siblings);
            self.speculate(|checker| {
                for (member, exit) in bodies.zip(exits.iter_mut()) {
                    // A body ending in a top-level `^` never hands its
                    // writes to a later run.
                    if !block_has_return(member) {
                        checker.infer_stmts(&member.body, hierarchy, exit, in_abstract_method);
                    }
                }
            });
            let mut next: Vec<LoopHeadWidening> = Vec::new();
            for (name, before) in &written {
                let entry = widenings
                    .iter()
                    .find(|(n, _)| n == name)
                    .map_or(before, |(_, ty)| ty);
                let mut arms = vec![entry.clone()];
                for exit in &exits {
                    if let Some(after) = exit.get_local(name)
                        && !arms.contains(&after)
                    {
                        arms.push(after);
                    }
                }
                let joined = if arms.len() == 1 {
                    entry.clone()
                } else {
                    Self::join_branch_arms(&arms)
                };
                if joined != *before {
                    next.push((name.clone(), joined));
                }
            }
            let converged = next == widenings;
            widenings = next;
            if converged {
                break;
            }
        }
        widenings
    }

    /// Apply loop-head `widenings` to a block's freshly made entry
    /// environment. The flow facts the old binding carried (raw-Binary,
    /// known-remote, `Result` error variant) described the value before the
    /// block, not the join, so they are dropped with it — the same rule as
    /// [`Self::apply_deferred_block_widenings`].
    pub(in crate::semantic_analysis::type_checker) fn apply_loop_head_widenings(
        widenings: &[LoopHeadWidening],
        block_env: &mut TypeEnv,
    ) {
        for (name, ty) in widenings {
            block_env.remove(&EnvKey::local(name.clone()));
            block_env.set_local(name.clone(), ty.clone());
        }
    }

    /// Run `f` and then undo every side effect it had on the checker:
    /// diagnostics, collected `^` types, the LSP type map and the other
    /// span-keyed tables, and the send-frame bookkeeping. Only its effect on
    /// the environment `f` itself was handed survives, which is what
    /// [`Self::loop_head_widenings`] reads.
    fn speculate(&mut self, f: impl FnOnce(&mut Self)) {
        let snapshot = SpeculationSnapshot {
            diagnostics: self.diagnostics.len(),
            collected_returns: self.collected_returns.len(),
            deferred_block_widenings: self.deferred_block_widenings.len(),
            defer_block_widening: self.defer_block_widening,
            self_send_epoch: self.self_send_epoch,
            known_remote_spans: self.known_remote_spans.clone(),
            block_param_pushdown: self.block_param_pushdown.clone(),
            unresolved_selector_sends: self.unresolved_selector_sends.clone(),
        };
        let outer_speculating = std::mem::replace(&mut self.speculating, true);
        self.type_map.begin_journal();
        f(self);
        self.type_map.rollback_journal();
        self.speculating = outer_speculating;
        self.diagnostics.truncate(snapshot.diagnostics);
        self.collected_returns.truncate(snapshot.collected_returns);
        self.deferred_block_widenings
            .truncate(snapshot.deferred_block_widenings);
        self.defer_block_widening = snapshot.defer_block_widening;
        self.self_send_epoch = snapshot.self_send_epoch;
        self.known_remote_spans = snapshot.known_remote_spans;
        self.block_param_pushdown = snapshot.block_param_pushdown;
        self.unresolved_selector_sends = snapshot.unresolved_selector_sends;
    }
}
