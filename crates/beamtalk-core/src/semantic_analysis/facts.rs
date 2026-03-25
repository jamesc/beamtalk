// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Pre-codegen semantic facts computed in a single AST pass.
//!
//! **DDD Context:** Compilation — Semantic Analysis
//!
//! [`SemanticFacts`] is a side-car produced before code generation begins.
//! Codegen consumes it via hash-map lookup instead of re-deriving facts
//! inline at every call site (BT-1288).

use crate::ast::{
    Block, CascadeMessage, Expression, ExpressionStatement, MessageSelector, MethodKind, Module,
    StringSegment,
};
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
    /// Per-class method index, keyed by class name.
    pub class_facts: HashMap<String, ClassFacts>,
    /// Set of method (or legacy block) spans whose body contains a `^` inside a block.
    ///
    /// Populated by [`compute_semantic_facts`]. Codegen uses this for O(1) NLR detection
    /// instead of re-walking the AST at every call site.
    pub methods_with_block_nlr: HashSet<Span>,
    /// `true` once [`compute_semantic_facts`] has finished populating this struct.
    ///
    /// When `false` (e.g. `SemanticFacts::default()` in unit tests that construct
    /// [`CoreErlangGenerator`] directly), lookup methods fall back to local AST analysis
    /// rather than silently returning incorrect defaults.
    is_populated: bool,
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

    /// Returns `true` if the method or block identified by `span` contains a `^` inside a block.
    pub fn has_block_nlr(&self, span: &Span) -> bool {
        self.methods_with_block_nlr.contains(span)
    }

    /// Returns `true` if the method contains `^` inside a block, with a fallback for when
    /// semantic facts have not been populated.
    ///
    /// When [`is_populated`] is `false` (e.g. in unit tests that construct
    /// [`CoreErlangGenerator`] directly via `new`), the pre-computed set is empty and
    /// `has_block_nlr` would silently return `false` for all methods, causing the NLR wrapper
    /// to be skipped. This method mirrors the `control_flow_has_mutations` pattern: if facts
    /// are not yet computed, fall back to an inline AST walk over `body_stmts`.
    pub fn has_block_nlr_or_walk(&self, span: &Span, body_stmts: &[ExpressionStatement]) -> bool {
        if self.is_populated {
            self.methods_with_block_nlr.contains(span)
        } else {
            body_stmts
                .iter()
                .any(|stmt| expr_has_block_nlr(&stmt.expression, false))
        }
    }
}

/// Returns `true` if `expr` (or any sub-expression) contains a `^` (Return) inside a block.
///
/// `inside_block` is `true` when the caller is already within a block body. A `^` at method
/// level is a normal early return handled elsewhere; only `^` *inside* a block is an NLR.
fn expr_has_block_nlr(expr: &Expression, inside_block: bool) -> bool {
    match expr {
        Expression::Return { value, .. } => inside_block || expr_has_block_nlr(value, inside_block),
        Expression::Block(block) => block_has_nlr(block),
        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            expr_has_block_nlr(receiver, inside_block)
                || arguments
                    .iter()
                    .any(|a| expr_has_block_nlr(a, inside_block))
        }
        Expression::Assignment { target, value, .. } => {
            expr_has_block_nlr(target, inside_block) || expr_has_block_nlr(value, inside_block)
        }
        Expression::Parenthesized { expression, .. } => {
            expr_has_block_nlr(expression, inside_block)
        }
        Expression::Cascade {
            receiver, messages, ..
        } => {
            expr_has_block_nlr(receiver, inside_block)
                || messages.iter().any(|m: &CascadeMessage| {
                    m.arguments
                        .iter()
                        .any(|a| expr_has_block_nlr(a, inside_block))
                })
        }
        Expression::FieldAccess { receiver, .. } => expr_has_block_nlr(receiver, inside_block),
        Expression::Match { value, arms, .. } => {
            expr_has_block_nlr(value, inside_block)
                || arms.iter().any(|arm| {
                    arm.guard
                        .as_ref()
                        .is_some_and(|g| expr_has_block_nlr(g, inside_block))
                        || expr_has_block_nlr(&arm.body, true)
                })
        }
        Expression::MapLiteral { pairs, .. } => pairs.iter().any(|pair| {
            expr_has_block_nlr(&pair.key, inside_block)
                || expr_has_block_nlr(&pair.value, inside_block)
        }),
        Expression::ListLiteral { elements, tail, .. } => {
            elements.iter().any(|e| expr_has_block_nlr(e, inside_block))
                || tail
                    .as_ref()
                    .is_some_and(|t| expr_has_block_nlr(t, inside_block))
        }
        Expression::ArrayLiteral { elements, .. } => {
            elements.iter().any(|e| expr_has_block_nlr(e, inside_block))
        }
        Expression::StringInterpolation { segments, .. } => segments.iter().any(|s| match s {
            StringSegment::Literal(_) => false,
            StringSegment::Interpolation(e) => expr_has_block_nlr(e, inside_block),
        }),
        Expression::DestructureAssignment { value, .. } => expr_has_block_nlr(value, inside_block),
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::Error { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. } => false,
    }
}

/// Returns `true` if the block body contains a `^` (Return) expression anywhere inside it.
fn block_has_nlr(block: &Block) -> bool {
    block
        .body
        .iter()
        .any(|s| expr_has_block_nlr(&s.expression, true))
}

/// Compute semantic facts for all nodes in a module.
///
/// Performs a single top-level pass over the AST:
/// - Populates `block_profiles` by calling [`analyze_block`] for every [`Block`] node.
/// - Populates `dispatch_kinds` for every [`Expression::MessageSend`] node.
/// - Populates `methods_with_block_nlr` for methods whose body contains `^` inside a block.
pub fn compute_semantic_facts(module: &Module) -> SemanticFacts {
    let mut facts = SemanticFacts::default();

    // Process module-level expressions
    for stmt in &module.expressions {
        // Detect legacy expression-based method blocks (used by gen_server dispatch legacy path).
        // These are top-level assignments of the form `methodName := [:param | body]`.
        // The block body is treated as method-level (inside_block=false): a ^ directly in the
        // block is a normal early return, not an NLR — only ^ inside a *nested* block is NLR.
        if let Expression::Assignment { value, .. } = &stmt.expression {
            if let Expression::Block(block) = value.as_ref() {
                if block
                    .body
                    .iter()
                    .any(|s| expr_has_block_nlr(&s.expression, false))
                {
                    facts.methods_with_block_nlr.insert(block.span);
                }
            }
        }
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
            // Detect block NLR: ^ inside a block requires the NLR throw/catch mechanism.
            if method
                .body
                .iter()
                .any(|stmt| expr_has_block_nlr(&stmt.expression, false))
            {
                facts.methods_with_block_nlr.insert(method.span);
            }

            for stmt in &method.body {
                collect_expression_facts(&stmt.expression, &mut facts);
            }
        }
    }

    // Process standalone method definitions
    for standalone in &module.method_definitions {
        let method = &standalone.method;

        // Detect block NLR for standalone methods.
        if method
            .body
            .iter()
            .any(|stmt| expr_has_block_nlr(&stmt.expression, false))
        {
            facts.methods_with_block_nlr.insert(method.span);
        }

        for stmt in &method.body {
            collect_expression_facts(&stmt.expression, &mut facts);
        }
    }

    facts.is_populated = true;
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
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. } => {}
    }
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
            crate::state_threading_selectors::is_state_threading_keyword_selector(sel.as_str())
        }
        MessageSelector::Unary(name) => {
            crate::state_threading_selectors::is_state_threading_unary_selector(name.as_str())
        }
        MessageSelector::Binary(_) => false,
    };

    if is_control_flow {
        return DispatchKind::ControlFlow;
    }

    DispatchKind::Unknown
}
