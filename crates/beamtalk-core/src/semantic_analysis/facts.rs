// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pre-codegen semantic facts computed in a single AST pass.
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! [`SemanticFacts`] is a side-car produced before code generation begins.
//! Codegen consumes it via hash-map lookup instead of re-deriving facts
//! inline at every call site (BT-1288).

use crate::ast::{Expression, MessageSelector, MethodKind, Module};
use crate::semantic_analysis::block_facts::{BlockMutationAnalysis, analyze_block};
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

/// Pre-computed facts for a single class definition.
///
/// Populated by [`compute_semantic_facts`] and stored in [`SemanticFacts::class_facts`].
/// Turns O(n) method scans in codegen into O(1) hash-map lookups.
#[derive(Debug, Clone, Default)]
pub struct ClassFacts {
    /// Whether the class has any primary instance method.
    pub has_primary_method: bool,
    /// Whether the class has any selector-based `@primitive` instance method.
    ///
    /// A method is "primitive" when its body is a single quoted `@primitive` expression
    /// (e.g. `@primitive "show:"`).  Unquoted structural intrinsics are excluded.
    pub has_primitive_instance_methods: bool,
    /// Instance methods indexed by selector name → index into `class.methods`.
    pub instance_methods_by_selector: HashMap<String, usize>,
    /// Class methods indexed by selector name → index into `class.class_methods`.
    pub class_methods_by_selector: HashMap<String, usize>,
}

impl ClassFacts {
    /// Returns `true` if the class has an instance method with the given selector.
    pub fn has_instance_method(&self, selector: &str) -> bool {
        self.instance_methods_by_selector.contains_key(selector)
    }

    /// Returns `true` if the class has a class method with the given selector.
    pub fn has_class_method(&self, selector: &str) -> bool {
        self.class_methods_by_selector.contains_key(selector)
    }

    /// Returns the index of the instance method with the given selector into `class.methods`.
    pub fn instance_method_index(&self, selector: &str) -> Option<usize> {
        self.instance_methods_by_selector.get(selector).copied()
    }

    /// Returns the index of the class method with the given selector into `class.class_methods`.
    pub fn class_method_index(&self, selector: &str) -> Option<usize> {
        self.class_methods_by_selector.get(selector).copied()
    }
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
    /// Per-class method index, keyed by class name.
    pub class_facts: HashMap<String, ClassFacts>,
}

impl SemanticFacts {
    /// Returns the block profile for the given span, or `None` if not found.
    pub fn block_profile(&self, span: &Span) -> Option<&BlockProfile> {
        self.block_profiles.get(span)
    }

    /// Returns the pre-computed facts for the class with the given name, or `None`.
    pub fn class_facts(&self, class_name: &str) -> Option<&ClassFacts> {
        self.class_facts.get(class_name)
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

    // Compute per-class method index facts
    for class in &module.classes {
        let mut cf = ClassFacts::default();
        for (i, method) in class.methods.iter().enumerate() {
            cf.instance_methods_by_selector
                .insert(method.selector.name().to_string(), i);
            if method.kind == MethodKind::Primary {
                cf.has_primary_method = true;
            }
            if method.body.len() == 1
                && matches!(
                    &method.body[0].expression,
                    Expression::Primitive {
                        is_quoted: true,
                        ..
                    }
                )
            {
                cf.has_primitive_instance_methods = true;
            }
        }
        for (i, method) in class.class_methods.iter().enumerate() {
            cf.class_methods_by_selector
                .insert(method.selector.name().to_string(), i);
        }
        facts.class_facts.insert(class.name.name.to_string(), cf);
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
            // Classify this message send.
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
