// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type checking and inference for semantic analysis (ADR 0025 Phase 1).
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module implements zero-syntax type inference. It walks the AST,
//! infers types from literals, assignments, and message sends, then
//! validates message sends against `ClassHierarchy` method tables.
//!
//! **Key design decisions:**
//! - Warnings only, never errors (avoid false positives)
//! - `Dynamic` type = no checking (fallback for unknowns)
//! - Classes with `doesNotUnderstand:args:` override suppress warnings
//! - Cascade receiver type unchanged
//!
//! **References:**
//! - `docs/ADR/0025-gradual-typing-and-protocols.md` — Phase 1

use crate::ast::{Expression, Literal, MessageSelector, Module};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::source_analysis::{Diagnostic, Span};
use ecow::EcoString;
use std::collections::HashMap;

/// Inferred type for an expression or variable.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InferredType {
    /// A known concrete class type (e.g., "Integer", "Counter").
    Known(EcoString),
    /// Type cannot be determined — skip all checking.
    Dynamic,
}

impl InferredType {
    /// Returns the class name if this is a known type.
    #[must_use]
    pub fn as_known(&self) -> Option<&EcoString> {
        match self {
            Self::Known(name) => Some(name),
            Self::Dynamic => None,
        }
    }
}

/// Map of expression start offsets to their inferred types.
///
/// Used by LSP providers (hover, completions) to query types at cursor positions.
#[derive(Debug, Clone, Default)]
pub struct TypeMap {
    types: HashMap<u32, InferredType>,
}

impl TypeMap {
    /// Creates an empty type map.
    #[must_use]
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    /// Looks up the inferred type at a byte offset.
    ///
    /// Returns `None` if no type is recorded at that offset.
    #[must_use]
    pub fn get(&self, offset: u32) -> Option<&InferredType> {
        self.types.get(&offset)
    }

    /// Records an inferred type at a byte offset (expression start position).
    fn insert(&mut self, offset: u32, ty: InferredType) {
        self.types.insert(offset, ty);
    }
}

/// Runs type inference on a module and returns the type map.
///
/// This is the main entry point for LSP providers that need type information
/// at specific positions (hover, completions).
#[must_use]
pub fn infer_types(module: &Module, hierarchy: &ClassHierarchy) -> TypeMap {
    let mut checker = TypeChecker::new();
    checker.check_module(module, hierarchy);
    checker.take_type_map()
}

/// Type checking domain service.
///
/// **DDD Context:** Semantic Analysis - Domain Service
///
/// Performs zero-syntax type inference (ADR 0025 Phase 1):
/// - Infers types from literals and class references
/// - Tracks variable types through assignments
/// - Validates message sends against class method tables
/// - Emits warnings for unknown selectors with hints
#[derive(Debug)]
pub struct TypeChecker {
    diagnostics: Vec<Diagnostic>,
    type_map: TypeMap,
}

impl TypeChecker {
    /// Creates a new type checker.
    #[must_use]
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
            type_map: TypeMap::new(),
        }
    }

    /// Checks types in a module using the class hierarchy for method resolution.
    pub fn check_module(&mut self, module: &Module, hierarchy: &ClassHierarchy) {
        let mut env = TypeEnv::new();

        // Check top-level expressions
        for expr in &module.expressions {
            self.infer_expr(expr, hierarchy, &mut env, false);
        }

        // Check method bodies inside class definitions
        for class in &module.classes {
            let is_abstract = class.is_abstract || hierarchy.is_abstract(&class.name.name);
            for method in &class.methods {
                let mut method_env = TypeEnv::new();
                // `self` has the type of the class
                method_env.set("self", InferredType::Known(class.name.name.clone()));
                for expr in &method.body {
                    self.infer_expr(expr, hierarchy, &mut method_env, is_abstract);
                }
            }
            for method in &class.class_methods {
                let mut method_env = TypeEnv::new();
                // `self` in class methods refers to the class itself
                method_env.set("self", InferredType::Known(class.name.name.clone()));
                for expr in &method.body {
                    self.infer_expr(expr, hierarchy, &mut method_env, is_abstract);
                }
            }
        }

        // Check standalone method definitions (Tonel-style: `Counter >> increment => ...`)
        for standalone in &module.method_definitions {
            let class_name = &standalone.class_name.name;
            let is_abstract = hierarchy.is_abstract(class_name);
            let mut method_env = TypeEnv::new();
            // `self` is the class for class methods, an instance for instance methods
            method_env.set("self", InferredType::Known(class_name.clone()));
            for expr in &standalone.method.body {
                self.infer_expr(expr, hierarchy, &mut method_env, is_abstract);
            }
        }
    }

    /// Infer the type of an expression, emitting diagnostics for invalid sends.
    ///
    /// `in_abstract_method` suppresses warnings for `self` sends in abstract classes,
    /// since subclasses may provide the missing methods.
    #[allow(clippy::too_many_lines)] // one arm per AST variant — irreducible
    fn infer_expr(
        &mut self,
        expr: &Expression,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let ty = match expr {
            // Literals have known types
            Expression::Literal(lit, _span) => Self::infer_literal(lit),

            // Identifiers look up the environment
            Expression::Identifier(ident) => {
                let name = ident.name.as_str();
                match name {
                    "true" | "false" => InferredType::Known("Boolean".into()),
                    "nil" => InferredType::Known("UndefinedObject".into()),
                    "self" => env.get("self").unwrap_or(InferredType::Dynamic),
                    _ => env.get(name).unwrap_or(InferredType::Dynamic),
                }
            }

            // Class references are the class itself (class-side receiver)
            Expression::ClassReference { name, .. } => InferredType::Known(name.name.clone()),

            // Field access — we don't know field types
            Expression::FieldAccess { .. } => InferredType::Dynamic,

            // Message sends — the core of type checking
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                span,
            } => self.infer_message_send(
                receiver,
                selector,
                arguments,
                *span,
                hierarchy,
                env,
                in_abstract_method,
            ),

            // Assignments track the type of the value
            Expression::Assignment { target, value, .. } => {
                let ty = self.infer_expr(value, hierarchy, env, in_abstract_method);
                if let Expression::Identifier(ident) = target.as_ref() {
                    env.set(ident.name.as_str(), ty.clone());
                }
                ty
            }

            // Returns propagate the value type
            Expression::Return { value, .. } => {
                self.infer_expr(value, hierarchy, env, in_abstract_method)
            }

            // Cascades: receiver type unchanged, check each message
            Expression::Cascade {
                receiver, messages, ..
            } => {
                let receiver_ty = self.infer_expr(receiver, hierarchy, env, in_abstract_method);
                let is_class_ref = matches!(receiver.as_ref(), Expression::ClassReference { .. });
                for msg in messages {
                    let selector_name = msg.selector.name();
                    for arg in &msg.arguments {
                        self.infer_expr(arg, hierarchy, env, in_abstract_method);
                    }
                    if is_class_ref {
                        if let Expression::ClassReference { name, .. } = receiver.as_ref() {
                            self.check_class_side_send(
                                &name.name,
                                &selector_name,
                                msg.span,
                                hierarchy,
                            );
                        }
                    } else if let InferredType::Known(ref class_name) = receiver_ty {
                        if !in_abstract_method || !Self::is_self_receiver(receiver) {
                            self.check_instance_selector(
                                class_name,
                                &selector_name,
                                msg.span,
                                hierarchy,
                            );
                        }
                    }
                }
                receiver_ty
            }

            // Parenthesized — unwrap
            Expression::Parenthesized { expression, .. } => {
                self.infer_expr(expression, hierarchy, env, in_abstract_method)
            }

            // Blocks — infer body but return Block type
            Expression::Block(block) => {
                let mut block_env = env.child();
                for param in &block.parameters {
                    block_env.set(param.name.as_str(), InferredType::Dynamic);
                }
                for body_expr in &block.body {
                    self.infer_expr(body_expr, hierarchy, &mut block_env, in_abstract_method);
                }
                InferredType::Known("Block".into())
            }

            // Pipe — infer both sides, result is target's return
            Expression::Pipe { value, target, .. } => {
                self.infer_expr(value, hierarchy, env, in_abstract_method);
                self.infer_expr(target, hierarchy, env, in_abstract_method)
            }

            // Match — result is Dynamic (branches may differ)
            Expression::Match { value, arms, .. } => {
                self.infer_expr(value, hierarchy, env, in_abstract_method);
                for arm in arms {
                    let mut arm_env = env.child();
                    if let Some(guard) = &arm.guard {
                        self.infer_expr(guard, hierarchy, &mut arm_env, in_abstract_method);
                    }
                    self.infer_expr(&arm.body, hierarchy, &mut arm_env, in_abstract_method);
                }
                InferredType::Dynamic
            }

            // Map literal → Dictionary
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    self.infer_expr(&pair.key, hierarchy, env, in_abstract_method);
                    self.infer_expr(&pair.value, hierarchy, env, in_abstract_method);
                }
                InferredType::Known("Dictionary".into())
            }

            // List literal → List
            Expression::ListLiteral { elements, tail, .. } => {
                for elem in elements {
                    self.infer_expr(elem, hierarchy, env, in_abstract_method);
                }
                if let Some(t) = tail {
                    self.infer_expr(t, hierarchy, env, in_abstract_method);
                }
                InferredType::Known("List".into())
            }

            // String interpolation → String
            Expression::StringInterpolation { segments, .. } => {
                for seg in segments {
                    if let crate::ast::StringSegment::Interpolation(inner_expr) = seg {
                        self.infer_expr(inner_expr, hierarchy, env, in_abstract_method);
                    }
                }
                InferredType::Known("String".into())
            }

            // Super, Primitive, Error — Dynamic
            Expression::Super(_) | Expression::Primitive { .. } | Expression::Error { .. } => {
                InferredType::Dynamic
            }
        };

        // Record inferred type at expression start position for LSP queries
        self.type_map.insert(expr.span().start(), ty.clone());
        ty
    }

    /// Infer the type of a message send and validate the selector.
    #[allow(clippy::too_many_arguments)] // hierarchy + env + flag needed for recursive checking
    fn infer_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let receiver_ty = self.infer_expr(receiver, hierarchy, env, in_abstract_method);
        let selector_name = selector.name();

        // Infer argument types (for side effects / variable tracking)
        for arg in arguments {
            self.infer_expr(arg, hierarchy, env, in_abstract_method);
        }

        // If receiver is a class reference, check class-side methods
        if let Expression::ClassReference { name, .. } = receiver {
            let class_name = &name.name;
            return self.check_class_side_send(class_name, &selector_name, span, hierarchy);
        }

        // For instance-side sends on known types
        if let InferredType::Known(ref class_name) = receiver_ty {
            // Skip checking self sends in abstract class method bodies —
            // subclasses may provide the missing methods (template method pattern)
            let skip = in_abstract_method && Self::is_self_receiver(receiver);
            if !skip {
                self.check_instance_selector(class_name, &selector_name, span, hierarchy);
            }

            // Infer return type from method info
            if let Some(method) = hierarchy.find_method(class_name, &selector_name) {
                if let Some(ref ret_ty) = method.return_type {
                    return InferredType::Known(ret_ty.clone());
                }
            }
        }

        InferredType::Dynamic
    }

    /// Returns true if the expression is `self` (direct identifier reference).
    fn is_self_receiver(expr: &Expression) -> bool {
        matches!(expr, Expression::Identifier(ident) if ident.name == "self")
    }

    /// Check a class-side message send (e.g., `Counter spawn`, `Object new`).
    fn check_class_side_send(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        if !hierarchy.has_class(class_name) {
            return InferredType::Dynamic;
        }

        // Check if class-side method exists
        let has_class_method = hierarchy.find_class_method(class_name, selector).is_some();
        if !has_class_method {
            // Also check instance-side (some methods are both)
            if !hierarchy.resolves_selector(class_name, selector) {
                self.emit_unknown_selector_warning(class_name, selector, span, hierarchy, true);
            }
        }

        // Infer return type for known factory methods
        match selector {
            "spawn" | "spawnWith:" | "new" | "new:" => InferredType::Known(class_name.clone()),
            _ => {
                if let Some(method) = hierarchy.find_class_method(class_name, selector) {
                    if let Some(ref ret_ty) = method.return_type {
                        return InferredType::Known(ret_ty.clone());
                    }
                }
                InferredType::Dynamic
            }
        }
    }

    /// Check if an instance-side selector is valid for a known class.
    fn check_instance_selector(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        if !hierarchy.has_class(class_name) {
            return;
        }

        // Classes with doesNotUnderstand: override accept any message
        if hierarchy.has_dnu_override(class_name) {
            return;
        }

        if !hierarchy.resolves_selector(class_name, selector) {
            self.emit_unknown_selector_warning(class_name, selector, span, hierarchy, false);
        }
    }

    /// Emit a warning diagnostic for an unknown selector.
    fn emit_unknown_selector_warning(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
    ) {
        let side = if is_class_side { " class" } else { "" };
        let message: EcoString =
            format!("{class_name}{side} does not understand '{selector}'").into();

        let mut diag = Diagnostic::warning(message, span);

        // Try to suggest similar selectors
        if let Some(suggestion) =
            Self::find_similar_selector(class_name, selector, hierarchy, is_class_side)
        {
            diag.hint = Some(format!("Did you mean '{suggestion}'?").into());
        }

        self.diagnostics.push(diag);
    }

    /// Find a similar selector for "did you mean" hints.
    fn find_similar_selector(
        class_name: &EcoString,
        selector: &str,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
    ) -> Option<EcoString> {
        let methods = if is_class_side {
            hierarchy.all_class_methods(class_name)
        } else {
            hierarchy.all_methods(class_name)
        };

        let mut best: Option<(EcoString, usize)> = None;
        for method in &methods {
            let dist = edit_distance(selector, method.selector.as_str());
            // Only suggest if distance ≤ 3 and less than half the selector length
            if dist <= 3
                && dist < selector.len() / 2 + 1
                && best.as_ref().is_none_or(|(_, d)| dist < *d)
            {
                best = Some((method.selector.clone(), dist));
            }
        }

        best.map(|(sel, _)| sel)
    }

    /// Infer the type of a literal value.
    fn infer_literal(lit: &Literal) -> InferredType {
        match lit {
            Literal::Integer(_) => InferredType::Known("Integer".into()),
            Literal::Float(_) => InferredType::Known("Float".into()),
            Literal::String(_) => InferredType::Known("String".into()),
            Literal::Symbol(_) => InferredType::Known("Symbol".into()),
            Literal::Character(_) => InferredType::Known("Character".into()),
            Literal::List(_) => InferredType::Known("List".into()),
        }
    }

    /// Returns all diagnostics collected during type checking.
    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Takes ownership of diagnostics, leaving an empty vec.
    pub fn take_diagnostics(&mut self) -> Vec<Diagnostic> {
        std::mem::take(&mut self.diagnostics)
    }

    /// Returns a reference to the type map built during checking.
    #[must_use]
    pub fn type_map(&self) -> &TypeMap {
        &self.type_map
    }

    /// Takes ownership of the type map, leaving an empty map.
    pub fn take_type_map(&mut self) -> TypeMap {
        std::mem::take(&mut self.type_map)
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

/// Type environment for tracking variable → type mappings.
///
/// Supports nested scopes via `child()` which clones the parent env.
#[derive(Debug, Clone)]
struct TypeEnv {
    bindings: HashMap<EcoString, InferredType>,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    fn get(&self, name: &str) -> Option<InferredType> {
        self.bindings.get(name).cloned()
    }

    fn set(&mut self, name: &str, ty: InferredType) {
        self.bindings.insert(name.into(), ty);
    }

    /// Create a child scope that inherits parent bindings.
    fn child(&self) -> Self {
        self.clone()
    }
}

/// Simple edit distance (Levenshtein) for "did you mean" suggestions.
fn edit_distance(a: &str, b: &str) -> usize {
    let a_chars: Vec<char> = a.chars().collect();
    let b_chars: Vec<char> = b.chars().collect();
    let m = a_chars.len();
    let n = b_chars.len();

    let mut dp = vec![vec![0usize; n + 1]; m + 1];
    for (i, row) in dp.iter_mut().enumerate().take(m + 1) {
        row[0] = i;
    }
    for (j, val) in dp[0].iter_mut().enumerate().take(n + 1) {
        *val = j;
    }

    for i in 1..=m {
        for j in 1..=n {
            let cost = usize::from(a_chars[i - 1] != b_chars[j - 1]);
            dp[i][j] = (dp[i - 1][j] + 1)
                .min(dp[i][j - 1] + 1)
                .min(dp[i - 1][j - 1] + cost);
        }
    }

    dp[m][n]
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Block, CascadeMessage, ClassDefinition, Identifier, KeywordPart, MethodDefinition,
        MethodKind, Module,
    };
    use crate::source_analysis::Span;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: span(),
        }
    }

    fn make_module(expressions: Vec<Expression>) -> Module {
        Module::new(expressions, span())
    }

    fn make_module_with_classes(
        expressions: Vec<Expression>,
        classes: Vec<ClassDefinition>,
    ) -> Module {
        let mut module = Module::new(expressions, span());
        module.classes = classes;
        module
    }

    fn msg_send(
        receiver: Expression,
        selector: MessageSelector,
        args: Vec<Expression>,
    ) -> Expression {
        Expression::MessageSend {
            receiver: Box::new(receiver),
            selector,
            arguments: args,
            span: span(),
        }
    }

    fn int_lit(n: i64) -> Expression {
        Expression::Literal(Literal::Integer(n), span())
    }

    fn str_lit(s: &str) -> Expression {
        Expression::Literal(Literal::String(s.into()), span())
    }

    fn var(name: &str) -> Expression {
        Expression::Identifier(ident(name))
    }

    fn class_ref(name: &str) -> Expression {
        Expression::ClassReference {
            name: ident(name),
            span: span(),
        }
    }

    fn assign(name: &str, value: Expression) -> Expression {
        Expression::Assignment {
            target: Box::new(var(name)),
            value: Box::new(value),
            span: span(),
        }
    }

    // ---- Tests ----

    #[test]
    fn test_literal_type_inference() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Integer(42)),
            InferredType::Known("Integer".into())
        );
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Float(1.5)),
            InferredType::Known("Float".into())
        );
        assert_eq!(
            TypeChecker::infer_literal(&Literal::String("hello".into())),
            InferredType::Known("String".into())
        );
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Symbol("sym".into())),
            InferredType::Known("Symbol".into())
        );
    }

    #[test]
    fn test_variable_tracking_through_assignment() {
        // x := 42
        // x + 1   ← should know x is Integer
        let module = make_module(vec![
            assign("x", int_lit(42)),
            msg_send(
                var("x"),
                MessageSelector::Binary("+".into()),
                vec![int_lit(1)],
            ),
        ]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(checker.diagnostics().is_empty(), "Integer responds to +");
    }

    #[test]
    fn test_unknown_selector_warning() {
        // 42 foo  ← Integer does not understand 'foo'
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Unary("foo".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert_eq!(checker.diagnostics().len(), 1);
        let diag = &checker.diagnostics()[0];
        assert!(
            diag.message.contains("Integer"),
            "should mention Integer: {}",
            diag.message
        );
        assert!(
            diag.message.contains("foo"),
            "should mention foo: {}",
            diag.message
        );
    }

    #[test]
    fn test_valid_selector_no_warning() {
        // 42 + 1  ← Integer responds to +
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("+".into()),
            vec![int_lit(1)],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(checker.diagnostics().is_empty());
    }

    #[test]
    fn test_class_side_spawn_returns_class_type() {
        // x := Counter spawn
        // x increment  ← should know x is Counter
        let module = make_module(vec![
            assign(
                "x",
                msg_send(
                    class_ref("Counter"),
                    MessageSelector::Unary("spawn".into()),
                    vec![],
                ),
            ),
            msg_send(
                var("x"),
                MessageSelector::Unary("nonExistentMethod".into()),
                vec![],
            ),
        ]);

        // Build a hierarchy with Counter that has 'increment' but not 'nonExistentMethod'
        let counter_module = make_module_with_classes(
            vec![],
            vec![ClassDefinition {
                name: ident("Counter"),
                superclass: Some(ident("Actor")),
                is_abstract: false,
                is_sealed: false,
                state: vec![],
                methods: vec![MethodDefinition {
                    selector: MessageSelector::Unary("increment".into()),
                    parameters: vec![],
                    body: vec![],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    doc_comment: None,
                    span: span(),
                }],
                class_methods: vec![],
                class_variables: vec![],
                doc_comment: None,
                span: span(),
            }],
        );
        let (hierarchy, _) = ClassHierarchy::build(&counter_module);

        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert_eq!(
            checker.diagnostics().len(),
            1,
            "should warn about nonExistentMethod"
        );
        assert!(checker.diagnostics()[0].message.contains("Counter"));
    }

    #[test]
    fn test_dynamic_type_no_warnings() {
        // x := someUnknownThing
        // x foo  ← x is Dynamic, no warning
        let module = make_module(vec![
            assign("x", var("someUnknownThing")),
            msg_send(var("x"), MessageSelector::Unary("foo".into()), vec![]),
        ]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "Dynamic type should not produce warnings"
        );
    }

    #[test]
    fn test_cascade_receiver_type_unchanged() {
        // 42 negated; abs  ← should be fine, Integer responds to both
        let module = make_module(vec![Expression::Cascade {
            receiver: Box::new(int_lit(42)),
            messages: vec![
                CascadeMessage::new(MessageSelector::Unary("negated".into()), vec![], span()),
                CascadeMessage::new(MessageSelector::Unary("abs".into()), vec![], span()),
            ],
            span: span(),
        }]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(checker.diagnostics().is_empty());
    }

    #[test]
    fn test_cascade_invalid_selector() {
        // 42 negated; bogus  ← Integer doesn't understand 'bogus'
        let module = make_module(vec![Expression::Cascade {
            receiver: Box::new(int_lit(42)),
            messages: vec![
                CascadeMessage::new(MessageSelector::Unary("negated".into()), vec![], span()),
                CascadeMessage::new(MessageSelector::Unary("bogus".into()), vec![], span()),
            ],
            span: span(),
        }]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert_eq!(checker.diagnostics().len(), 1);
        assert!(checker.diagnostics()[0].message.contains("bogus"));
    }

    #[test]
    fn test_string_methods() {
        // 'hello' length  ← String responds to length
        let module = make_module(vec![msg_send(
            str_lit("hello"),
            MessageSelector::Unary("length".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(checker.diagnostics().is_empty());
    }

    #[test]
    fn test_block_infers_block_type() {
        let mut checker = TypeChecker::new();
        let hierarchy = ClassHierarchy::with_builtins();
        let mut env = TypeEnv::new();

        let block_expr = Expression::Block(Block {
            parameters: vec![],
            body: vec![int_lit(42)],
            span: span(),
        });

        let ty = checker.infer_expr(&block_expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Known("Block".into()));
    }

    #[test]
    fn test_map_literal_infers_dictionary() {
        let mut checker = TypeChecker::new();
        let hierarchy = ClassHierarchy::with_builtins();
        let mut env = TypeEnv::new();

        let map_expr = Expression::MapLiteral {
            pairs: vec![],
            span: span(),
        };

        let ty = checker.infer_expr(&map_expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Known("Dictionary".into()));
    }

    #[test]
    fn test_list_literal_infers_list() {
        let mut checker = TypeChecker::new();
        let hierarchy = ClassHierarchy::with_builtins();
        let mut env = TypeEnv::new();

        let list_expr = Expression::ListLiteral {
            elements: vec![int_lit(1), int_lit(2)],
            tail: None,
            span: span(),
        };

        let ty = checker.infer_expr(&list_expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Known("List".into()));
    }

    #[test]
    fn test_unknown_variable_is_dynamic() {
        let mut checker = TypeChecker::new();
        let hierarchy = ClassHierarchy::with_builtins();
        let mut env = TypeEnv::new();

        let ty = checker.infer_expr(&var("unknownVar"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Dynamic);
    }

    #[test]
    fn test_true_false_nil_types() {
        let mut checker = TypeChecker::new();
        let hierarchy = ClassHierarchy::with_builtins();
        let mut env = TypeEnv::new();

        assert_eq!(
            checker.infer_expr(&var("true"), &hierarchy, &mut env, false),
            InferredType::Known("Boolean".into())
        );
        assert_eq!(
            checker.infer_expr(&var("false"), &hierarchy, &mut env, false),
            InferredType::Known("Boolean".into())
        );
        assert_eq!(
            checker.infer_expr(&var("nil"), &hierarchy, &mut env, false),
            InferredType::Known("UndefinedObject".into())
        );
    }

    #[test]
    fn test_did_you_mean_hint() {
        // 'hello' lenght  ← should suggest 'length'
        let module = make_module(vec![msg_send(
            str_lit("hello"),
            MessageSelector::Unary("lenght".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert_eq!(checker.diagnostics().len(), 1);
        let diag = &checker.diagnostics()[0];
        assert!(diag.hint.is_some(), "should have a 'did you mean' hint");
        assert!(
            diag.hint.as_ref().unwrap().contains("length"),
            "hint should suggest 'length': {:?}",
            diag.hint
        );
    }

    #[test]
    fn test_edit_distance() {
        assert_eq!(edit_distance("abc", "abc"), 0);
        assert_eq!(edit_distance("abc", "abd"), 1);
        assert_eq!(edit_distance("abc", "abcd"), 1);
        assert_eq!(edit_distance("abc", "xyz"), 3);
        assert_eq!(edit_distance("lenght", "length"), 2);
    }

    #[test]
    fn test_type_env_child_scope() {
        let mut parent = TypeEnv::new();
        parent.set("x", InferredType::Known("Integer".into()));

        let mut child = parent.child();
        assert_eq!(child.get("x"), Some(InferredType::Known("Integer".into())));

        child.set("y", InferredType::Known("String".into()));
        assert!(parent.get("y").is_none(), "parent should not see child's y");
    }

    #[test]
    fn test_match_returns_dynamic() {
        let mut checker = TypeChecker::new();
        let hierarchy = ClassHierarchy::with_builtins();
        let mut env = TypeEnv::new();

        let match_expr = Expression::Match {
            value: Box::new(int_lit(42)),
            arms: vec![],
            span: span(),
        };

        let ty = checker.infer_expr(&match_expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Dynamic);
    }

    #[test]
    fn test_class_method_body_gets_self_type() {
        // Define a class with a method that sends 'unknownMsg' to self
        let module = make_module_with_classes(
            vec![],
            vec![ClassDefinition {
                name: ident("Greeter"),
                superclass: Some(ident("Object")),
                is_abstract: false,
                is_sealed: false,
                state: vec![],
                methods: vec![MethodDefinition {
                    selector: MessageSelector::Unary("greet".into()),
                    parameters: vec![],
                    body: vec![msg_send(
                        var("self"),
                        MessageSelector::Unary("nonExistent".into()),
                        vec![],
                    )],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    doc_comment: None,
                    span: span(),
                }],
                class_methods: vec![],
                class_variables: vec![],
                doc_comment: None,
                span: span(),
            }],
        );
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        // Should warn about 'nonExistent' on Greeter
        assert_eq!(checker.diagnostics().len(), 1);
        assert!(checker.diagnostics()[0].message.contains("Greeter"));
    }

    #[test]
    fn test_warnings_only_never_errors() {
        // All diagnostics should be warnings, never errors
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Unary("bogus".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        for diag in checker.diagnostics() {
            assert_eq!(
                diag.severity,
                crate::source_analysis::Severity::Warning,
                "type checker should only emit warnings"
            );
        }
    }

    #[test]
    fn test_keyword_message_validation() {
        // 'hello' at: 1  ← String responds to at:
        let module = make_module(vec![msg_send(
            str_lit("hello"),
            MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
            vec![int_lit(1)],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(checker.diagnostics().is_empty());
    }

    #[test]
    fn test_inherited_method_no_warning() {
        // 42 describe  ← Integer inherits describe from Object
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Unary("describe".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "inherited methods should not produce warnings"
        );
    }

    #[test]
    fn test_stub_returns_no_diagnostics() {
        let module = Module::new(vec![], Span::default());
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(checker.diagnostics().is_empty());
    }

    #[test]
    fn type_map_records_literal_types() {
        let module = make_module(vec![int_lit(42)]);
        let hierarchy = ClassHierarchy::with_builtins();
        let type_map = infer_types(&module, &hierarchy);

        // The integer literal at offset 0 should be recorded as Integer
        let ty = type_map.get(0);
        assert!(ty.is_some(), "TypeMap should record literal type");
        assert_eq!(
            ty.unwrap(),
            &InferredType::Known("Integer".into()),
            "Integer literal should infer as Integer"
        );
    }

    #[test]
    fn type_map_records_variable_types_after_assignment() {
        // x := 42 → x should be Integer
        let module = make_module(vec![assign("x", int_lit(42)), var("x")]);
        let hierarchy = ClassHierarchy::with_builtins();
        let type_map = infer_types(&module, &hierarchy);

        // The second expression (var "x") should be recorded as Integer
        let x_expr = &module.expressions[1];
        let ty = type_map.get(x_expr.span().start());
        assert!(
            ty.is_some(),
            "TypeMap should record variable type after assignment"
        );
        assert_eq!(
            ty.unwrap(),
            &InferredType::Known("Integer".into()),
            "Variable assigned integer should infer as Integer"
        );
    }

    #[test]
    fn infer_types_convenience_function() {
        let module = make_module(vec![str_lit("hello")]);
        let hierarchy = ClassHierarchy::with_builtins();
        let type_map = infer_types(&module, &hierarchy);
        let ty = type_map.get(0);
        assert_eq!(
            ty,
            Some(&InferredType::Known("String".into())),
            "infer_types should return correct type map"
        );
    }
}
