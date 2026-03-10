// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pre-codegen semantic facts computed in a single AST pass.
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! [`SemanticFacts`] is a side-car produced before code generation begins.
//! Codegen consumes it via hash-map lookup instead of re-deriving facts
//! inline at every call site (BT-1288).

use crate::ast::{Expression, MessageSelector, Module};
use crate::semantic_analysis::block_facts::{analyze_block, BlockMutationAnalysis};
use crate::source_analysis::Span;
use std::collections::{HashMap, HashSet};

/// Re-export `BlockMutationAnalysis` as `BlockProfile` for use in this module.
pub type BlockProfile = BlockMutationAnalysis;

/// Dispatch classification for a [`Expression::MessageSend`] node.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DispatchKind {
    /// Synchronous call to `self` (`self method`).
    SelfSend,
    /// Fire-and-forget cast to `self` (`self method!`).
    ActorCast,
    /// State-threading control flow selector (`whileTrue:`, `do:`, `collect:`, etc.).
    ControlFlow,
    /// Classification could not be determined without class hierarchy.
    Unknown,
}

/// Set of variables known to be non-future (synchronous) at a program point.
#[derive(Debug, Clone, Default)]
pub struct SyncEnv {
    /// Variable names that are provably non-future at this point.
    pub sync_vars: HashSet<String>,
}

/// Pre-computed semantic facts for a module, produced before code generation.
///
/// Built by [`compute_semantic_facts`] and consumed by [`CoreErlangGenerator`] via
/// lookup instead of inline re-derivation.
#[derive(Debug, Clone, Default)]
pub struct SemanticFacts {
    /// Block mutation/capture profile for every [`Block`] node, keyed by block span.
    pub block_profiles: HashMap<Span, BlockProfile>,
    /// Dispatch classification for every [`Expression::MessageSend`] node, keyed by message span.
    pub dispatch_kinds: HashMap<Span, DispatchKind>,
    /// Sync variable environment for every method body, keyed by method definition span.
    /// Contains the set of method parameters that are provably non-future.
    pub sync_envs: HashMap<Span, SyncEnv>,
}

impl SemanticFacts {
    /// Returns the block profile for the given span, or `None` if not found.
    pub fn block_profile(&self, span: &Span) -> Option<&BlockProfile> {
        self.block_profiles.get(span)
    }

    /// Returns the dispatch kind for the given span, or `DispatchKind::Unknown`.
    pub fn dispatch_kind(&self, span: &Span) -> DispatchKind {
        self.dispatch_kinds
            .get(span)
            .copied()
            .unwrap_or(DispatchKind::Unknown)
    }
}

/// Compute semantic facts for all nodes in a module.
///
/// Performs a single top-level pass over the AST:
/// - Populates `block_profiles` by calling [`analyze_block`] for every [`Block`] node.
/// - Populates `dispatch_kinds` for every [`Expression::MessageSend`] node.
/// - Populates `sync_envs` for every method definition (method parameters are always sync).
pub fn compute_semantic_facts(module: &Module) -> SemanticFacts {
    let mut facts = SemanticFacts::default();

    // Process module-level expressions
    for stmt in &module.expressions {
        collect_expression_facts(&stmt.expression, &mut facts);
    }

    // Process class methods
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            // Populate sync_envs for this method's parameters
            let sync_vars: HashSet<String> = method
                .parameters
                .iter()
                .map(|p| p.name.name.to_string())
                .collect();
            if !sync_vars.is_empty() {
                facts.sync_envs.insert(method.span, SyncEnv { sync_vars });
            }

            for stmt in &method.body {
                collect_expression_facts(&stmt.expression, &mut facts);
            }
        }
    }

    // Process standalone method definitions
    for standalone in &module.method_definitions {
        let method = &standalone.method;
        let sync_vars: HashSet<String> = method
            .parameters
            .iter()
            .map(|p| p.name.name.to_string())
            .collect();
        if !sync_vars.is_empty() {
            facts.sync_envs.insert(method.span, SyncEnv { sync_vars });
        }
        for stmt in &method.body {
            collect_expression_facts(&stmt.expression, &mut facts);
        }
    }

    facts
}

/// Recursively collect facts from an expression tree.
fn collect_expression_facts(expr: &Expression, facts: &mut SemanticFacts) {
    match expr {
        Expression::Block(block) => {
            // Compute block profile for this block node
            let profile = analyze_block(block);
            facts.block_profiles.insert(block.span, profile);
            // Recurse into block body
            for stmt in &block.body {
                collect_expression_facts(&stmt.expression, facts);
            }
        }

        Expression::MessageSend {
            receiver,
            selector,
            arguments,
            is_cast,
            span,
            ..
        } => {
            // Classify this message send
            let kind = classify_dispatch(receiver, selector, *is_cast);
            facts.dispatch_kinds.insert(*span, kind);
            // Recurse
            collect_expression_facts(receiver, facts);
            for arg in arguments {
                collect_expression_facts(arg, facts);
            }
        }

        Expression::Assignment { target, value, .. } => {
            collect_expression_facts(target, facts);
            collect_expression_facts(value, facts);
        }

        Expression::Return { value, .. } | Expression::DestructureAssignment { value, .. } => {
            collect_expression_facts(value, facts);
        }

        Expression::Cascade {
            receiver, messages, ..
        } => {
            collect_expression_facts(receiver, facts);
            for msg in messages {
                for arg in &msg.arguments {
                    collect_expression_facts(arg, facts);
                }
            }
        }

        Expression::Parenthesized { expression, .. } => {
            collect_expression_facts(expression, facts);
        }

        Expression::Match { value, arms, .. } => {
            collect_expression_facts(value, facts);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_expression_facts(guard, facts);
                }
                collect_expression_facts(&arm.body, facts);
            }
        }

        Expression::FieldAccess { receiver, .. } => {
            collect_expression_facts(receiver, facts);
        }

        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                collect_expression_facts(&pair.key, facts);
                collect_expression_facts(&pair.value, facts);
            }
        }

        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                collect_expression_facts(elem, facts);
            }
            if let Some(t) = tail {
                collect_expression_facts(t, facts);
            }
        }

        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                collect_expression_facts(elem, facts);
            }
        }

        Expression::StringInterpolation { segments, .. } => {
            for seg in segments {
                if let crate::ast::StringSegment::Interpolation(e) = seg {
                    collect_expression_facts(e, facts);
                }
            }
        }

        // Leaf nodes: no sub-expressions to recurse into
        Expression::Literal(..)
        | Expression::Identifier(_)
        | Expression::ClassReference { .. }
        | Expression::Primitive { .. }
        | Expression::Super(_)
        | Expression::Error { .. }
        | Expression::ExpectDirective { .. } => {}
    }
}

/// Returns `true` if `sel` is a state-threading keyword selector.
///
/// Mirrors `codegen::core_erlang::state_threading_selectors` without creating
/// a cross-layer dependency from `semantic_analysis` into codegen.
fn is_state_threading_keyword_selector(sel: &str) -> bool {
    matches!(
        sel,
        "whileTrue:"
            | "whileFalse:"
            | "timesRepeat:"
            | "to:do:"
            | "to:by:do:"
            | "do:"
            | "collect:"
            | "select:"
            | "reject:"
            | "inject:into:"
            | "on:do:"
            | "ensure:"
            | "ifTrue:"
            | "ifFalse:"
            | "ifTrue:ifFalse:"
            | "ifNotNil:"
    )
}

/// Returns `true` if `sel` is a state-threading unary selector.
fn is_state_threading_unary_selector(sel: &str) -> bool {
    matches!(sel, "whileTrue" | "whileFalse" | "timesRepeat")
}

/// Classify a message send based on static AST information.
fn classify_dispatch(
    receiver: &Expression,
    selector: &MessageSelector,
    is_cast: bool,
) -> DispatchKind {
    // Self-send classification
    if let Expression::Identifier(id) = receiver {
        if id.name == "self" {
            return if is_cast {
                DispatchKind::ActorCast
            } else {
                DispatchKind::SelfSend
            };
        }
    }

    // Control flow classification
    let is_control_flow = match selector {
        MessageSelector::Keyword(parts) => {
            let sel: String = parts.iter().map(|p| p.keyword.as_str()).collect();
            is_state_threading_keyword_selector(sel.as_str())
        }
        MessageSelector::Unary(name) => is_state_threading_unary_selector(name.as_str()),
        MessageSelector::Binary(_) => false,
    };

    if is_control_flow {
        return DispatchKind::ControlFlow;
    }

    DispatchKind::Unknown
}
