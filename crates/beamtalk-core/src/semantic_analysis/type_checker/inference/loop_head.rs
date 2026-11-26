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

use std::collections::{HashMap, HashSet};

use ecow::EcoString;

use crate::ast::{Expression, MessageSelector};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::known_remote::KnownRemote;
use crate::semantic_analysis::type_checker::narrowing::visitors::block_has_return;
use crate::semantic_analysis::type_checker::{
    DynamicReason, EnvKey, InferredType, TypeChecker, TypeEnv,
};
use crate::source_analysis::Span;
use crate::state_threading_selectors;

/// How many speculative passes [`TypeChecker::loop_head_widenings`] runs
/// before it gives up on a fixpoint. One pass is enough unless a written
/// local's new type depends on another written local's; a local still
/// changing after the last pass enters the block as `Dynamic`.
const MAX_LOOP_HEAD_PASSES: usize = 3;

/// One outer local's widened entry type for a repeatable block body, and its
/// known-remote fact (ADR 0126 §6) when the value before the block and every
/// value a run leaves it with are known-remote.
#[derive(Debug, Clone, PartialEq)]
pub(in crate::semantic_analysis::type_checker) struct LoopHeadWidening {
    name: EcoString,
    ty: InferredType,
    remote: Option<KnownRemote>,
}

/// The side-effect state of a [`TypeChecker`] that inferring a block body may
/// change, captured by [`TypeChecker::speculate`] and restored after it. The
/// span-keyed tables are cloned: they hold only known-remote identifier uses,
/// unresolved selectors and not-yet-consumed block pushdowns, so they stay
/// small, unlike the type map, which is journaled instead.
struct SpeculationSnapshot {
    diagnostics: usize,
    collected_returns: usize,
    deferred_block_widenings: Vec<super::block_outer_writes::BlockWriteWidening>,
    defer_block_widening: bool,
    self_send_epoch: u64,
    known_remote_spans: HashMap<Span, KnownRemote>,
    block_param_pushdown: HashMap<Span, Vec<InferredType>>,
    unresolved_selector_sends: HashSet<Span>,
}

/// Whether every block literal argument of a send of `selector` runs at most
/// once per evaluation of the send: the conditionals and `and:`/`or:`, the
/// exception constructs, the lookups and `tryDo:` (the shared
/// `state_threading_selectors` tables), and the `Result` branches. Anything
/// else is assumed to be able to call its block arguments repeatedly, which
/// only ever widens a type (the type before the block stays a member of the
/// join).
fn args_run_at_most_once(selector: &MessageSelector) -> bool {
    let name = selector.name();
    let name = name.as_str();
    state_threading_selectors::is_conditional_selector(name)
        || state_threading_selectors::is_exception_selector(name)
        || state_threading_selectors::lookup_shape(name).is_some()
        || state_threading_selectors::is_try_do_selector(name)
        || name == super::blocks::THEN_SELECTOR
        || matches!(name, "ifOk:" | "ifError:" | "ifOk:ifError:")
}

/// Whether a block literal *receiving* `selector` runs at most once per
/// evaluation of the send: invoking it (`value`, ..., `valueWithArguments:`)
/// or guarding it (`on:do:`, `ensure:`). `whileTrue:` and the other loops
/// run their receiver again.
fn receiver_runs_at_most_once(selector: &MessageSelector) -> bool {
    selector.is_block_invocation()
        || state_threading_selectors::is_state_threaded_block_receiver(&selector.name())
}

impl TypeChecker {
    /// BT-5298: forget the per-body block tables when a new method body (or
    /// the module's top-level statements) starts; block spans never repeat
    /// across bodies.
    pub(in crate::semantic_analysis::type_checker) fn clear_loop_head_tables(&mut self) {
        self.run_once_blocks.clear();
        self.loop_siblings.clear();
        self.resolved_block_params.clear();
    }

    /// BT-5298: record which of a send's block literals — `receiver` (when
    /// given) and `arguments` — run at most once per evaluation of the send,
    /// so their bodies are inferred from the plain entry environment. Every
    /// other block literal gets the loop-head join, and when a send has two
    /// or more of them they run interleaved (`[...] whileTrue: [...]`,
    /// `do:separatedBy:`), so each one's loop head also joins the others'
    /// writes.
    ///
    /// A plain send calls this twice, before its receiver (with it) and
    /// before its arguments (without it); the first sibling set recorded for
    /// a block, the one that includes a block receiver, is kept.
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
        if repeatable.len() < 2 {
            return;
        }
        for block in &repeatable {
            if self.loop_siblings.contains_key(&block.span) {
                continue;
            }
            let siblings: Vec<crate::ast::Block> = repeatable
                .iter()
                .filter(|other| other.span != block.span)
                .map(|other| (*other).clone())
                .collect();
            self.loop_siblings.insert(block.span, siblings);
        }
    }

    /// BT-5298: the entry types of the outer locals `block` and its
    /// interleaved sibling blocks ([`Self::mark_run_once_blocks`]) write, for
    /// a block that may run more than once: each local's type before the
    /// block joined with every type a run of the body — or of a sibling's
    /// body — leaves it with, iterated to a fixpoint. A local still changing
    /// after [`MAX_LOOP_HEAD_PASSES`] speculative passes is widened to
    /// `Dynamic`, never left narrower than a later run may see.
    ///
    /// `build_env` makes the block's entry environment from a set of
    /// widenings (block parameters, narrowings and all), exactly as the real
    /// pass will; it is called once per speculative pass. A sibling is
    /// inferred from a child of `env` with the same widenings and the
    /// parameter types its own real pass resolved, when it has run already
    /// (an earlier argument), else `Dynamic`.
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
        // Taken out for the duration and put back: the speculative passes
        // need it only by reference.
        let siblings = self.loop_siblings.remove(&block.span).unwrap_or_default();
        let widenings = self.loop_head_fixpoint(
            block,
            &siblings,
            env,
            build_env,
            hierarchy,
            in_abstract_method,
        );
        if !siblings.is_empty() {
            self.loop_siblings.insert(block.span, siblings);
        }
        widenings
    }

    fn loop_head_fixpoint(
        &mut self,
        block: &crate::ast::Block,
        siblings: &[crate::ast::Block],
        env: &TypeEnv,
        build_env: &dyn Fn(&[LoopHeadWidening]) -> TypeEnv,
        hierarchy: &ClassHierarchy,
        in_abstract_method: bool,
    ) -> Vec<LoopHeadWidening> {
        let written = Self::loop_written_locals(block, siblings, env);
        if written.is_empty() {
            return Vec::new();
        }

        // `widenings` holds only the locals whose entry type differs from
        // their type before the block.
        let mut widenings: Vec<LoopHeadWidening> = Vec::new();
        for pass in 1..=MAX_LOOP_HEAD_PASSES {
            let exits = self.speculate_loop_exits(
                block,
                siblings,
                env,
                build_env(&widenings),
                &widenings,
                hierarchy,
                in_abstract_method,
            );
            let mut next: Vec<LoopHeadWidening> = Vec::new();
            for (name, before) in &written {
                let key = EnvKey::local(name.clone());
                let current = widenings.iter().find(|w| w.name == *name);
                let (entry, entry_remote) = match current {
                    Some(w) => (&w.ty, w.remote.clone()),
                    None => (before, env.known_remote(&key)),
                };
                let mut arms = vec![entry.clone()];
                let mut exit_remotes = Vec::with_capacity(exits.len());
                for exit in &exits {
                    exit_remotes.push(exit.known_remote(&key));
                    if let Some(after) = exit.get_local(name)
                        && !arms.contains(&after)
                    {
                        arms.push(after);
                    }
                }
                let ty = if arms.len() == 1 {
                    entry.clone()
                } else {
                    Self::join_branch_arms(&arms)
                };
                if ty != *before {
                    next.push(LoopHeadWidening {
                        name: name.clone(),
                        ty,
                        remote: KnownRemote::join(
                            entry_remote,
                            exit_remotes.iter().map(Option::as_ref),
                        ),
                    });
                }
            }
            if next == widenings {
                return widenings;
            }
            if pass == MAX_LOOP_HEAD_PASSES {
                // No fixpoint: a local whose entry type changed in this pass,
                // or that has not been widened yet, may still grow on a later
                // run (a longer dependency chain), so it enters the block as
                // `Dynamic`. Only a local this pass left unchanged keeps its
                // join.
                return written
                    .iter()
                    .map(|(name, _)| {
                        let settled = next
                            .iter()
                            .find(|w| w.name == *name)
                            .filter(|w| widenings.contains(w));
                        settled.cloned().unwrap_or_else(|| LoopHeadWidening {
                            name: name.clone(),
                            ty: InferredType::Dynamic(DynamicReason::DynamicBranch),
                            remote: None,
                        })
                    })
                    .collect();
            }
            widenings = next;
        }
        widenings
    }

    /// The outer locals `block` or any of its `siblings` write, each with
    /// its type before the block.
    fn loop_written_locals(
        block: &crate::ast::Block,
        siblings: &[crate::ast::Block],
        env: &TypeEnv,
    ) -> Vec<(EcoString, InferredType)> {
        let mut written: Vec<(EcoString, InferredType)> = Vec::new();
        for member in std::iter::once(block).chain(siblings) {
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
        written
    }

    /// One speculative pass of the loop head: infer `block`'s body from
    /// `entry` and each sibling's from `env` with `widenings` applied, and
    /// answer the environments they finish with (`block`'s first).
    #[allow(clippy::too_many_arguments)]
    fn speculate_loop_exits(
        &mut self,
        block: &crate::ast::Block,
        siblings: &[crate::ast::Block],
        env: &TypeEnv,
        entry: TypeEnv,
        widenings: &[LoopHeadWidening],
        hierarchy: &ClassHierarchy,
        in_abstract_method: bool,
    ) -> Vec<TypeEnv> {
        let mut exits = vec![entry];
        for sibling in siblings {
            let mut sibling_env = Self::block_child_env(sibling, env);
            Self::apply_loop_head_widenings(widenings, &mut sibling_env);
            let params = self.resolved_block_params.get(&sibling.span);
            for (i, param) in sibling.parameters.iter().enumerate() {
                let ty = params
                    .and_then(|p| p.get(i))
                    .cloned()
                    .unwrap_or(InferredType::Dynamic(DynamicReason::UnannotatedParam));
                sibling_env.set_local(param.name.clone(), ty);
            }
            exits.push(sibling_env);
        }
        let bodies = std::iter::once(block).chain(siblings);
        self.speculate(|checker| {
            for (member, exit) in bodies.zip(exits.iter_mut()) {
                // A body ending in a top-level `^` never hands its
                // writes to a later run.
                if !block_has_return(member) {
                    checker.infer_stmts(&member.body, hierarchy, exit, in_abstract_method);
                }
            }
        });
        exits
    }

    /// Apply loop-head `widenings` to a block's freshly made entry
    /// environment. The flow facts the old binding carried (origin note,
    /// raw-Binary, `Result` error variant) described the value before the
    /// block, not the join, so they are dropped with it; the known-remote
    /// fact is kept only as [`KnownRemote::join`] allows, the same rule as
    /// [`Self::apply_deferred_block_widenings`].
    pub(in crate::semantic_analysis::type_checker) fn apply_loop_head_widenings(
        widenings: &[LoopHeadWidening],
        block_env: &mut TypeEnv,
    ) {
        for w in widenings {
            let key = EnvKey::local(w.name.clone());
            block_env.remove(&key);
            block_env.set_local(w.name.clone(), w.ty.clone());
            if let Some(remote) = &w.remote {
                block_env.mark_known_remote(key, remote.clone());
            }
        }
    }

    /// Run `f` and then undo every side effect it had on the checker:
    /// diagnostics, collected `^` types, the LSP type map and the other
    /// span-keyed tables, and the send-frame bookkeeping. Only its effect on
    /// the environments `f` itself was handed survives, which is what
    /// [`Self::loop_head_widenings`] reads.
    ///
    /// `f` starts a fresh send frame, as a real block body does (the block
    /// inference functions take `defer_block_widening`), so a nested block's
    /// writes are joined into `f`'s environments rather than queued for the
    /// enclosing send.
    fn speculate(&mut self, f: impl FnOnce(&mut Self)) {
        let snapshot = SpeculationSnapshot {
            diagnostics: self.diagnostics.len(),
            collected_returns: self.collected_returns.len(),
            deferred_block_widenings: std::mem::take(&mut self.deferred_block_widenings),
            defer_block_widening: std::mem::take(&mut self.defer_block_widening),
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
        self.deferred_block_widenings = snapshot.deferred_block_widenings;
        self.defer_block_widening = snapshot.defer_block_widening;
        self.self_send_epoch = snapshot.self_send_epoch;
        self.known_remote_spans = snapshot.known_remote_spans;
        self.block_param_pushdown = snapshot.block_param_pushdown;
        self.unresolved_selector_sends = snapshot.unresolved_selector_sends;
    }
}
