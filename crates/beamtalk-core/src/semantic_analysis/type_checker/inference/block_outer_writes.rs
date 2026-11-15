// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Narrowing bookkeeping for blocks that diverge or write outer locals
//! (split from `blocks.rs`).
//!
//! **DDD Context:** Semantic Analysis

use crate::ast::{Expression, WellKnownSelector};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::narrowing::visitors::block_has_return;
use crate::semantic_analysis::type_checker::type_resolver;
use crate::semantic_analysis::type_checker::{
    DynamicReason, EnvKey, InferredType, TypeChecker, TypeEnv,
};

/// BT-4636: one outer local a block writes — the type the block leaves it
/// with and its known-remote fact (ADR 0126 §6) — queued until every arm of
/// the enclosing send is inferred.
#[derive(Debug, Clone)]
pub(in crate::semantic_analysis::type_checker) struct BlockWriteWidening {
    pub(in crate::semantic_analysis::type_checker) name: ecow::EcoString,
    pub(in crate::semantic_analysis::type_checker) ty: InferredType,
    pub(in crate::semantic_analysis::type_checker) remote:
        Option<crate::semantic_analysis::type_checker::known_remote::KnownRemote>,
}

impl TypeChecker {
    /// BT-4715: a block that diverges (`^` or a `-> Never` tail) never hands
    /// control back to the code after its enclosing send, so a self-send in
    /// it (`[self log. ^0]`) cannot have reassigned a field that code reads.
    /// Rewind `self_send_epoch` so the enclosing expression keeps its other
    /// fields' narrowings.
    pub(super) fn forget_diverging_block_self_sends(
        &mut self,
        epoch_before: u64,
        block: &crate::ast::Block,
        body_ty: &InferredType,
    ) {
        if block_has_return(block) || matches!(body_ty, InferredType::Never) {
            self.self_send_epoch = epoch_before;
        }
    }

    /// BT-4636: a block's writes to an outer local are threaded back to the
    /// enclosing scope at runtime, but the block body is inferred in a child
    /// environment that is then dropped, so `found := nil. xs do: [:e | found
    /// := e]` left `found` typed `UndefinedObject` afterwards. Widen each outer
    /// local the block writes to the join of its type before the block and the
    /// type the block leaves it with (`String | Nil`), using the same
    /// [`Self::join_branch_arms`] the arms' result types use. The block may run
    /// zero times, so the prior type always stays a member. Nested blocks apply
    /// the same rule to their own parent, so a write under `ifFalse: [...]`
    /// inside a `do:` block reaches the method scope one level at a time.
    ///
    /// While `deferring` (the block is an argument of a send) the pair is
    /// queued instead, and [`Self::apply_deferred_block_widenings`] joins all
    /// of the send's arms at once. A block stored in a local is widened where
    /// it is defined, which is approximate: it does not track a later
    /// reassignment before the block runs.
    pub(super) fn widen_outer_locals_written_by_block(
        &mut self,
        block: &crate::ast::Block,
        block_env: &TypeEnv,
        env: &mut TypeEnv,
        deferring: bool,
    ) {
        // A block ending in a top-level `^` never returns to the code after
        // the send, so its writes cannot be seen there (the same rule that
        // gives it a `Never` body type).
        if block_has_return(block) {
            return;
        }
        let writes = crate::semantic_analysis::block_facts::outer_local_writes(block, &|name| {
            env.get_local(name).is_some()
        });
        for write in writes {
            if !matches!(
                write.kind,
                crate::semantic_analysis::block_facts::WriteKind::Local
            ) {
                continue;
            }
            let Some(after) = block_env.get_local(&write.name) else {
                continue;
            };
            let widening = BlockWriteWidening {
                remote: block_env.known_remote(&EnvKey::local(write.name.clone())),
                name: write.name,
                ty: after,
            };
            if deferring {
                self.deferred_block_widenings.push(widening);
            } else {
                Self::apply_deferred_block_widenings(&[widening], env);
            }
        }
    }

    /// Join each deferred block write into `env`: the local's current type
    /// joined with every type the send's arms left it with.
    pub(in crate::semantic_analysis::type_checker) fn apply_deferred_block_widenings(
        deferred: &[BlockWriteWidening],
        env: &mut TypeEnv,
    ) {
        let mut names: Vec<&ecow::EcoString> = Vec::new();
        for w in deferred {
            if !names.contains(&&w.name) {
                names.push(&w.name);
            }
        }
        for name in names {
            let Some(before) = env.get_local(name) else {
                continue;
            };
            let key = EnvKey::local(name.clone());
            let writes: Vec<&BlockWriteWidening> =
                deferred.iter().filter(|w| w.name == *name).collect();
            // ADR 0126 §6: the known-remote fact survives only when the
            // binding and every arm are known-remote; otherwise a stale fact
            // would describe a value the local may no longer hold.
            // `node_hint` names the node the local was spawned on, so it is
            // kept only when the binding and every arm agree on it; otherwise
            // the fact stays with no hint and the diagnostic says "there".
            let remote = env.known_remote(&key).and_then(|binding| {
                let mut node_hint = binding.node_hint;
                for w in &writes {
                    let arm = w.remote.as_ref()?;
                    if arm.node_hint != node_hint {
                        node_hint = None;
                    }
                }
                Some(
                    crate::semantic_analysis::type_checker::known_remote::KnownRemote { node_hint },
                )
            });
            let mut arms = vec![before.clone()];
            arms.extend(writes.iter().map(|w| w.ty.clone()));
            if arms.iter().skip(1).all(|ty| *ty == before) {
                match remote {
                    Some(remote) => env.mark_known_remote(key, remote),
                    None => env.clear_known_remote(&key),
                }
                continue;
            }
            let joined = Self::join_branch_arms(&arms);
            // The old origin note and flow facts (raw-Binary) described the
            // earlier binding, not this union: drop them.
            env.remove(&key);
            env.set_local(name.clone(), joined);
            if let Some(remote) = remote {
                env.mark_known_remote(key, remote);
            }
        }
    }

    /// Resolve each parameter's type for a block literal (BT-4102 Decision A):
    /// an explicit `:: T` annotation wins over the `pushed` type (from a
    /// callee signature, declared return type or annotated local), which wins
    /// over `Dynamic(UnannotatedParam)`.
    ///
    /// Annotations are resolved exactly like method parameters in
    /// [`Self::set_param_types`]: alias and protocol registries, and the
    /// enclosing method's substitution, which inside a method body is the
    /// identity (BT-4210). An annotation naming a method-local or class-level
    /// type variable (`[:x :: E | ...]`) therefore resolves to the same rigid
    /// `Known("E")` placeholder as a method parameter or `-> E` return typed
    /// `E`. It is never `Dynamic`, and it is never the *callee's* binding for
    /// a same-named variable: the pushed types come from the callee's
    /// substitution, and the callee's `E` is a different variable.
    ///
    /// When an annotation and a non-Dynamic pushed type are unrelated, a
    /// mismatch warning is emitted at the parameter. A type-variable
    /// placeholder on either side is compatible with anything (see
    /// [`Self::detect_assignment_type_mismatch`]).
    pub(in crate::semantic_analysis::type_checker) fn resolve_block_param_types(
        &mut self,
        block: &crate::ast::Block,
        pushed: &[InferredType],
        hierarchy: &ClassHierarchy,
    ) -> Vec<InferredType> {
        let subst = type_resolver::SubstitutionMap::new();
        let mut out = Vec::with_capacity(block.parameters.len());
        for (i, param) in block.parameters.iter().enumerate() {
            let pushed_ty = pushed
                .get(i)
                .cloned()
                .unwrap_or(InferredType::Dynamic(DynamicReason::UnannotatedParam));
            let ty = match &param.type_annotation {
                Some(ann) => {
                    // Inline (not `resolution_context()`) so the disjoint
                    // `protocol_registry` borrow composes.
                    let declared = type_resolver::ResolutionContext::new(
                        self.alias_registry.as_deref(),
                        &mut self.referenced_aliases,
                    )
                    .resolve_type_annotation(
                        ann,
                        &subst,
                        self.protocol_registry.as_deref(),
                    );
                    if let Some(mismatch) = Self::detect_assignment_type_mismatch(
                        &pushed_ty,
                        &declared,
                        hierarchy,
                        self.protocol_registry.as_deref(),
                    ) {
                        self.emit_assignment_type_mismatch(&mismatch, param.span);
                    }
                    declared
                }
                None => pushed_ty,
            };
            out.push(ty);
        }
        out
    }

    /// BT-4444: a `value`-family send (`value:`, `value:value:`, …) directly
    /// to a block literal checks each argument against the matching
    /// parameter's `:: T` annotation, by the same rule as an annotated local
    /// ([`Self::detect_assignment_type_mismatch`]) — so `C class` / `P class`
    /// metatype parameters judge a class-object argument by conformance.
    ///
    /// Unannotated parameters are not checked (their type is the pushed or
    /// `Dynamic` type, never a declaration), and an arity mismatch is left
    /// to the runtime, as elsewhere for block arity.
    pub(in crate::semantic_analysis::type_checker) fn check_block_literal_value_args(
        &mut self,
        receiver: &Expression,
        receiver_ty: &InferredType,
        selector: &crate::ast::MessageSelector,
        arguments: &[Expression],
        arg_types: &[InferredType],
        hierarchy: &ClassHierarchy,
    ) {
        // `ast::normalize` (BT-4059) already peeled parens off a send receiver.
        let Expression::Block(block) = receiver else {
            return;
        };
        if arguments.is_empty()
            || arguments.len() != block.parameters.len()
            || !selector
                .well_known()
                .is_some_and(WellKnownSelector::is_block_value)
        {
            return;
        }
        // The literal's own inferred `Block(P…, R)` carries each annotation
        // already resolved (aliases, protocols, metatypes).
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = receiver_ty
        else {
            return;
        };
        if class_name != "Block" {
            return;
        }
        for (i, param) in block.parameters.iter().enumerate() {
            if param.type_annotation.is_none() {
                continue;
            }
            let (Some(declared), Some(arg_ty), Some(arg)) =
                (type_args.get(i), arg_types.get(i), arguments.get(i))
            else {
                continue;
            };
            if let Some(mismatch) = Self::detect_assignment_type_mismatch(
                arg_ty,
                declared,
                hierarchy,
                self.protocol_registry.as_deref(),
            ) {
                self.emit_assignment_type_mismatch(&mismatch, arg.span());
            }
        }
    }
}
