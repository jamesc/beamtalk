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
                        // Force `inside_block = true`: match arm bodies compile
                        // to Core Erlang `case` arms, which (like block closures)
                        // cannot return directly — `^` needs the NLR throw/catch.
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Block, ClassDefinition, Expression, ExpressionStatement, Identifier, KeywordPart, Literal,
        MethodDefinition, Module, StandaloneMethodDefinition,
    };
    use crate::source_analysis::Span;

    fn ts() -> Span {
        Span::new(0, 0)
    }

    fn lit(n: i64) -> Expression {
        Expression::Literal(Literal::Integer(n), ts())
    }

    fn ident(name: &str) -> Expression {
        Expression::Identifier(Identifier::new(name, ts()))
    }

    fn ret(val: Expression) -> Expression {
        Expression::Return {
            value: Box::new(val),
            span: ts(),
        }
    }

    fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    fn empty_block(span: Span) -> Block {
        Block::new(vec![], vec![], span)
    }

    fn block_with(body: Vec<ExpressionStatement>, span: Span) -> Block {
        Block::new(vec![], body, span)
    }

    // ---------------------------------------------------------------
    // classify_dispatch — private function
    // ---------------------------------------------------------------

    #[test]
    fn classify_self_send() {
        let sel = MessageSelector::Unary("increment".into());
        assert_eq!(
            classify_dispatch(&ident("self"), &sel, false),
            DispatchKind::SelfSend
        );
    }

    #[test]
    fn classify_actor_cast() {
        let sel = MessageSelector::Unary("increment".into());
        assert_eq!(
            classify_dispatch(&ident("self"), &sel, true),
            DispatchKind::ActorCast
        );
    }

    #[test]
    fn classify_control_flow_keyword() {
        let sel = MessageSelector::Keyword(vec![KeywordPart::new("timesRepeat:", ts())]);
        assert_eq!(
            classify_dispatch(&lit(5), &sel, false),
            DispatchKind::ControlFlow
        );
    }

    #[test]
    fn classify_control_flow_unary() {
        let sel = MessageSelector::Unary("whileTrue".into());
        let block_expr = Expression::Block(empty_block(ts()));
        assert_eq!(
            classify_dispatch(&block_expr, &sel, false),
            DispatchKind::ControlFlow
        );
    }

    #[test]
    fn classify_unknown_non_self_receiver() {
        let sel = MessageSelector::Unary("size".into());
        assert_eq!(
            classify_dispatch(&ident("other"), &sel, false),
            DispatchKind::Unknown
        );
    }

    #[test]
    fn classify_binary_selector_is_unknown() {
        let sel = MessageSelector::Binary("+".into());
        assert_eq!(
            classify_dispatch(&lit(1), &sel, false),
            DispatchKind::Unknown
        );
    }

    // ---------------------------------------------------------------
    // expr_has_block_nlr — private recursive function
    // ---------------------------------------------------------------

    #[test]
    fn nlr_literal_leaf_is_false() {
        assert!(!expr_has_block_nlr(&lit(42), false));
        assert!(!expr_has_block_nlr(&lit(42), true));
    }

    #[test]
    fn nlr_return_at_method_level_is_false() {
        // ^ at top level (inside_block=false) is a normal early return, not NLR
        assert!(!expr_has_block_nlr(&ret(lit(1)), false));
    }

    #[test]
    fn nlr_return_inside_block_is_true() {
        let block_expr = Expression::Block(block_with(vec![bare(ret(lit(1)))], ts()));
        assert!(expr_has_block_nlr(&block_expr, false));
    }

    #[test]
    fn nlr_block_without_return_is_false() {
        let block_expr = Expression::Block(block_with(vec![bare(lit(42))], ts()));
        assert!(!expr_has_block_nlr(&block_expr, false));
    }

    #[test]
    fn nlr_return_in_message_arg_block() {
        // receiver with: [^1]  → NLR in block arg
        let arg_block = Expression::Block(block_with(vec![bare(ret(lit(1)))], ts()));
        let send = Expression::MessageSend {
            receiver: Box::new(ident("x")),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("with:", ts())]),
            arguments: vec![arg_block],
            is_cast: false,
            span: ts(),
        };
        assert!(expr_has_block_nlr(&send, false));
    }

    #[test]
    fn nlr_message_send_without_return_is_false() {
        let send = Expression::MessageSend {
            receiver: Box::new(ident("x")),
            selector: MessageSelector::Unary("size".into()),
            arguments: vec![],
            is_cast: false,
            span: ts(),
        };
        assert!(!expr_has_block_nlr(&send, false));
    }

    #[test]
    fn nlr_nested_block_with_return_is_true() {
        // Outer block body: [inner block containing ^1]
        let inner = Expression::Block(block_with(vec![bare(ret(lit(1)))], ts()));
        let outer = Expression::Block(block_with(vec![bare(inner)], ts()));
        assert!(expr_has_block_nlr(&outer, false));
    }

    // ---------------------------------------------------------------
    // block_has_nlr — private function
    // ---------------------------------------------------------------

    #[test]
    fn block_has_nlr_true_when_body_has_return() {
        let b = block_with(vec![bare(ret(lit(0)))], ts());
        assert!(block_has_nlr(&b));
    }

    #[test]
    fn block_has_nlr_false_when_no_return() {
        let b = block_with(vec![bare(lit(0))], ts());
        assert!(!block_has_nlr(&b));
    }

    // ---------------------------------------------------------------
    // ClassFacts helper methods
    // ---------------------------------------------------------------

    fn make_class_facts() -> ClassFacts {
        let mut cf = ClassFacts::default();
        cf.instance_methods_by_selector
            .insert("getValue".to_string(), 0);
        cf.instance_methods_by_selector
            .insert("setValue:".to_string(), 1);
        cf.class_methods_by_selector.insert("new".to_string(), 0);
        cf
    }

    #[test]
    fn class_facts_has_instance_method_found() {
        let cf = make_class_facts();
        assert!(cf.has_instance_method("getValue"));
    }

    #[test]
    fn class_facts_has_instance_method_not_found() {
        let cf = make_class_facts();
        assert!(!cf.has_instance_method("missing"));
    }

    #[test]
    fn class_facts_has_class_method_found() {
        let cf = make_class_facts();
        assert!(cf.has_class_method("new"));
    }

    #[test]
    fn class_facts_has_class_method_not_found() {
        let cf = make_class_facts();
        assert!(!cf.has_class_method("missing"));
    }

    #[test]
    fn class_facts_instance_method_index_found() {
        let cf = make_class_facts();
        assert_eq!(cf.instance_method_index("getValue"), Some(0));
        assert_eq!(cf.instance_method_index("setValue:"), Some(1));
    }

    #[test]
    fn class_facts_instance_method_index_not_found() {
        let cf = make_class_facts();
        assert_eq!(cf.instance_method_index("missing"), None);
    }

    #[test]
    fn class_facts_class_method_index_found() {
        let cf = make_class_facts();
        assert_eq!(cf.class_method_index("new"), Some(0));
    }

    #[test]
    fn class_facts_class_method_index_not_found() {
        let cf = make_class_facts();
        assert_eq!(cf.class_method_index("missing"), None);
    }

    // ---------------------------------------------------------------
    // SemanticFacts default/accessor methods
    // ---------------------------------------------------------------

    #[test]
    fn semantic_facts_default_dispatch_kind_is_unknown() {
        let facts = SemanticFacts::default();
        assert_eq!(
            facts.dispatch_kind(&Span::new(0, 10)),
            DispatchKind::Unknown
        );
    }

    #[test]
    fn semantic_facts_default_block_profile_is_none() {
        let facts = SemanticFacts::default();
        assert!(facts.block_profile(&Span::new(0, 10)).is_none());
    }

    #[test]
    fn semantic_facts_default_has_block_nlr_is_false() {
        let facts = SemanticFacts::default();
        assert!(!facts.has_block_nlr(&Span::new(0, 10)));
    }

    #[test]
    fn semantic_facts_default_class_facts_is_none() {
        let facts = SemanticFacts::default();
        assert!(facts.class_facts("MissingClass").is_none());
    }

    // ---------------------------------------------------------------
    // SemanticFacts::has_block_nlr_or_walk — is_populated fallback
    // ---------------------------------------------------------------

    #[test]
    fn has_block_nlr_or_walk_unpopulated_detects_nlr_via_ast_walk() {
        // is_populated=false (default): falls back to expr_has_block_nlr walk
        let facts = SemanticFacts::default();
        let body = vec![bare(Expression::Block(block_with(
            vec![bare(ret(lit(1)))],
            ts(),
        )))];
        assert!(facts.has_block_nlr_or_walk(&Span::new(0, 100), &body));
    }

    #[test]
    fn has_block_nlr_or_walk_unpopulated_no_nlr_returns_false() {
        let facts = SemanticFacts::default();
        let body = vec![bare(lit(42))];
        assert!(!facts.has_block_nlr_or_walk(&Span::new(0, 100), &body));
    }

    #[test]
    fn has_block_nlr_or_walk_populated_uses_set_not_walk() {
        // Populate facts via compute_semantic_facts with a method that has block NLR
        let method_span = Span::new(100, 200);
        let method = MethodDefinition::new(
            MessageSelector::Unary("test".into()),
            vec![],
            vec![bare(Expression::Block(block_with(
                vec![bare(ret(lit(42)))],
                ts(),
            )))],
            method_span,
        );
        let class = ClassDefinition::new(
            Identifier::new("MyClass", ts()),
            Identifier::new("Object", ts()),
            vec![],
            vec![method],
            ts(),
        );
        let facts = compute_semantic_facts(&Module::with_classes(vec![class], ts()));

        // Populated: method_span is in methods_with_block_nlr → true
        assert!(facts.has_block_nlr_or_walk(&method_span, &[]));
        // Populated: other span is NOT in set → false, even if body_stmts contain NLR
        let nlr_body = vec![bare(Expression::Block(block_with(
            vec![bare(ret(lit(1)))],
            ts(),
        )))];
        assert!(!facts.has_block_nlr_or_walk(&Span::new(999, 999), &nlr_body));
    }

    // ---------------------------------------------------------------
    // compute_semantic_facts — integration
    // ---------------------------------------------------------------

    #[test]
    fn compute_facts_empty_module_produces_empty_facts() {
        let facts = compute_semantic_facts(&Module::new(vec![], ts()));
        assert!(facts.block_profiles.is_empty());
        assert!(facts.dispatch_kinds.is_empty());
        assert!(facts.class_facts.is_empty());
        assert!(facts.methods_with_block_nlr.is_empty());
    }

    #[test]
    fn compute_facts_block_in_module_expr_populates_block_profiles() {
        let block_span = Span::new(5, 20);
        let b = block_with(vec![bare(lit(1))], block_span);
        let module = Module::new(vec![bare(Expression::Block(b))], ts());
        let facts = compute_semantic_facts(&module);
        assert!(facts.block_profiles.contains_key(&block_span));
    }

    #[test]
    fn compute_facts_self_send_classified_as_self_send() {
        let msg_span = Span::new(10, 20);
        let send = Expression::MessageSend {
            receiver: Box::new(ident("self")),
            selector: MessageSelector::Unary("increment".into()),
            arguments: vec![],
            is_cast: false,
            span: msg_span,
        };
        let facts = compute_semantic_facts(&Module::new(vec![bare(send)], ts()));
        assert_eq!(facts.dispatch_kind(&msg_span), DispatchKind::SelfSend);
    }

    #[test]
    fn compute_facts_control_flow_keyword_classified_as_control_flow() {
        let msg_span = Span::new(10, 30);
        let send = Expression::MessageSend {
            receiver: Box::new(lit(3)),
            selector: MessageSelector::Keyword(vec![KeywordPart::new("timesRepeat:", ts())]),
            arguments: vec![Expression::Block(empty_block(ts()))],
            is_cast: false,
            span: msg_span,
        };
        let facts = compute_semantic_facts(&Module::new(vec![bare(send)], ts()));
        assert_eq!(facts.dispatch_kind(&msg_span), DispatchKind::ControlFlow);
    }

    #[test]
    fn compute_facts_instance_methods_indexed_in_class_facts() {
        let method = MethodDefinition::new(
            MessageSelector::Unary("getValue".into()),
            vec![],
            vec![bare(lit(0))],
            ts(),
        );
        let class = ClassDefinition::new(
            Identifier::new("Counter", ts()),
            Identifier::new("Object", ts()),
            vec![],
            vec![method],
            ts(),
        );
        let facts = compute_semantic_facts(&Module::with_classes(vec![class], ts()));
        let cf = facts
            .class_facts("Counter")
            .expect("Counter should have class facts");
        assert!(cf.has_instance_method("getValue"));
        assert_eq!(cf.instance_method_index("getValue"), Some(0));
    }

    #[test]
    fn compute_facts_class_side_methods_indexed_in_class_facts() {
        // class_methods (class-side) indexing is a separate branch in compute_semantic_facts
        let class_method = MethodDefinition::new(
            MessageSelector::Unary("withDefault".into()),
            vec![],
            vec![bare(lit(0))],
            ts(),
        );
        let mut class = ClassDefinition::new(
            Identifier::new("Counter", ts()),
            Identifier::new("Object", ts()),
            vec![],
            vec![],
            ts(),
        );
        class.class_methods.push(class_method);
        let facts = compute_semantic_facts(&Module::with_classes(vec![class], ts()));
        let cf = facts
            .class_facts("Counter")
            .expect("Counter should have class facts");
        assert!(cf.has_class_method("withDefault"));
        assert_eq!(cf.class_method_index("withDefault"), Some(0));
        assert!(!cf.has_instance_method("withDefault"));
    }

    #[test]
    fn compute_facts_method_with_block_nlr_populates_nlr_set() {
        let method_span = Span::new(50, 150);
        let method = MethodDefinition::new(
            MessageSelector::Unary("withNlr".into()),
            vec![],
            vec![bare(Expression::Block(block_with(
                vec![bare(ret(lit(42)))],
                ts(),
            )))],
            method_span,
        );
        let class = ClassDefinition::new(
            Identifier::new("MyClass", ts()),
            Identifier::new("Object", ts()),
            vec![],
            vec![method],
            ts(),
        );
        let facts = compute_semantic_facts(&Module::with_classes(vec![class], ts()));
        assert!(
            facts.methods_with_block_nlr.contains(&method_span),
            "method with ^ inside block should be in methods_with_block_nlr"
        );
    }

    #[test]
    fn compute_facts_method_without_block_nlr_not_in_nlr_set() {
        let method_span = Span::new(50, 150);
        let method = MethodDefinition::new(
            MessageSelector::Unary("plain".into()),
            vec![],
            vec![bare(lit(0))],
            method_span,
        );
        let class = ClassDefinition::new(
            Identifier::new("MyClass", ts()),
            Identifier::new("Object", ts()),
            vec![],
            vec![method],
            ts(),
        );
        let facts = compute_semantic_facts(&Module::with_classes(vec![class], ts()));
        assert!(!facts.methods_with_block_nlr.contains(&method_span));
    }

    #[test]
    fn compute_facts_module_level_assignment_block_nlr() {
        // Legacy gen_server dispatch: myMethod := [:p | [^42]]
        // The outer block is treated as method-level (inside_block=false).
        // A ^ nested inside an inner block IS NLR — its span enters methods_with_block_nlr.
        let block_span = Span::new(5, 50);
        let inner = Expression::Block(block_with(vec![bare(ret(lit(42)))], ts()));
        let outer_block = block_with(vec![bare(inner)], block_span);
        let assignment = Expression::Assignment {
            target: Box::new(ident("myMethod")),
            value: Box::new(Expression::Block(outer_block)),
            type_annotation: None,
            span: ts(),
        };
        let module = Module::new(vec![bare(assignment)], ts());
        let facts = compute_semantic_facts(&module);
        assert!(facts.methods_with_block_nlr.contains(&block_span));
    }

    #[test]
    fn compute_facts_standalone_method_nlr() {
        // Standalone method: Counter >> withNlr => [^1]
        let method_span = Span::new(10, 100);
        let method = MethodDefinition::new(
            MessageSelector::Unary("withNlr".into()),
            vec![],
            vec![bare(Expression::Block(block_with(
                vec![bare(ret(lit(1)))],
                ts(),
            )))],
            method_span,
        );
        let standalone = StandaloneMethodDefinition {
            class_name: Identifier::new("Counter", ts()),
            package: None,
            is_class_method: false,
            method,
            span: ts(),
        };
        let mut module = Module::new(vec![], ts());
        module.method_definitions.push(standalone);
        let facts = compute_semantic_facts(&module);
        assert!(facts.methods_with_block_nlr.contains(&method_span));
    }
}
