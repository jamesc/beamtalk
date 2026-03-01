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

use crate::ast::{
    Expression, ExpressionStatement, Literal, MessageSelector, Module, TypeAnnotation,
};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::string_utils::edit_distance;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
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

/// Map of expression spans to their inferred types.
///
/// Used by LSP providers (hover, completions) to query types at cursor positions.
/// Keyed by full span (start + end) to avoid collisions between nested expressions
/// that share the same start offset (e.g., a message send and its receiver).
#[derive(Debug, Clone, Default)]
pub struct TypeMap {
    types: HashMap<Span, InferredType>,
}

impl TypeMap {
    /// Creates an empty type map.
    #[must_use]
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    /// Looks up the inferred type for an expression span.
    ///
    /// Returns `None` if no type is recorded at that span.
    #[must_use]
    pub fn get(&self, span: Span) -> Option<&InferredType> {
        self.types.get(&span)
    }

    /// Records an inferred type for an expression span.
    fn insert(&mut self, span: Span, ty: InferredType) {
        self.types.insert(span, ty);
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
        self.infer_stmts(&module.expressions, hierarchy, &mut env, false);

        // Check method bodies inside class definitions
        for class in &module.classes {
            let is_abstract = class.is_abstract || hierarchy.is_abstract(&class.name.name);

            // Determine if this class requires type annotations (typed modifier or inherited)
            let is_typed = hierarchy.is_typed(&class.name.name);

            for method in &class.methods {
                let mut method_env = TypeEnv::new();
                method_env.set("self", InferredType::Known(class.name.name.clone()));
                Self::set_param_types(&mut method_env, &method.parameters);
                let body_type =
                    self.infer_stmts(&method.body, hierarchy, &mut method_env, is_abstract);
                self.check_return_type(method, &body_type, &class.name.name, hierarchy);
                self.check_override_param_compatibility(method, &class.name.name, hierarchy);
                if is_typed {
                    self.check_typed_method_annotations(method, &class.name.name);
                }
            }
            for method in &class.class_methods {
                let mut method_env = TypeEnv::new();
                method_env.in_class_method = true;
                method_env.set("self", InferredType::Known(class.name.name.clone()));
                Self::set_param_types(&mut method_env, &method.parameters);
                let body_type =
                    self.infer_stmts(&method.body, hierarchy, &mut method_env, is_abstract);
                self.check_return_type(method, &body_type, &class.name.name, hierarchy);
                if is_typed {
                    self.check_typed_method_annotations(method, &class.name.name);
                }
            }

            // Check state default values match declared types
            self.check_state_defaults(class, hierarchy);
        }

        // Check standalone method definitions (Tonel-style: `Counter >> increment => ...`)
        for standalone in &module.method_definitions {
            let class_name = &standalone.class_name.name;
            let is_abstract = hierarchy.is_abstract(class_name);
            let mut method_env = TypeEnv::new();
            method_env.in_class_method = standalone.is_class_method;
            method_env.set("self", InferredType::Known(class_name.clone()));
            Self::set_param_types(&mut method_env, &standalone.method.parameters);
            let body_type = self.infer_stmts(
                &standalone.method.body,
                hierarchy,
                &mut method_env,
                is_abstract,
            );
            self.check_return_type(&standalone.method, &body_type, class_name, hierarchy);
        }
    }

    /// Sets parameter types in the type environment from annotations.
    ///
    /// Only `Simple` type annotations are wired in. Union types and other complex
    /// annotations are left as `Dynamic` — full union type support is Phase 3.
    fn set_param_types(env: &mut TypeEnv, parameters: &[crate::ast::ParameterDefinition]) {
        for param in parameters {
            if let Some(TypeAnnotation::Simple(type_id)) = &param.type_annotation {
                env.set(&param.name.name, InferredType::Known(type_id.name.clone()));
            }
        }
    }

    /// Infer the type of an expression, emitting diagnostics for invalid sends.
    ///
    /// `in_abstract_method` suppresses warnings for `self` class-side sends in
    /// abstract classes, since subclasses may provide class-side methods.
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

            // Field access — infer type from declared state type for self.field
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                let mut result = InferredType::Dynamic;
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    if recv_id.name == "self" {
                        if let Some(InferredType::Known(class_name)) = env.get("self") {
                            if let Some(field_type) =
                                hierarchy.state_field_type(&class_name, &field.name)
                            {
                                result = InferredType::Known(field_type);
                            }
                        }
                    }
                }
                result
            }

            // Primitives and errors — no type info available
            Expression::Primitive { .. }
            | Expression::Error { .. }
            | Expression::ExpectDirective { .. }
            | Expression::MessageSend { is_cast: true, .. } => InferredType::Dynamic,

            // Message sends — the core of type checking
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                is_cast: false,
                span,
                ..
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
            Expression::Assignment {
                target,
                value,
                span,
            } => {
                let ty = self.infer_expr(value, hierarchy, env, in_abstract_method);
                match target.as_ref() {
                    Expression::Identifier(ident) => {
                        env.set(ident.name.as_str(), ty.clone());
                    }
                    Expression::FieldAccess {
                        receiver, field, ..
                    } => {
                        let is_self_receiver = matches!(
                            receiver.as_ref(),
                            Expression::Identifier(recv_id) if recv_id.name == "self"
                        );
                        if is_self_receiver {
                            // `self.field := value` — validate against declared state type
                            self.check_field_assignment(field, &ty, *span, hierarchy, env);
                        } else {
                            // `other.field := value` or `(expr).field := value` —
                            // objects cannot mutate another object's state.
                            // Value types are immutable; actors can only mutate their
                            // own state via `self.x :=`.
                            // Suggest the functional `withField:` pattern.
                            let with_sel = {
                                let mut chars = field.name.chars();
                                match chars.next() {
                                    None => "with:".to_string(),
                                    Some(first) => {
                                        let cap: String = first.to_uppercase().collect();
                                        format!("with{}{}:", cap, chars.as_str())
                                    }
                                }
                            };
                            let recv_name = match receiver.as_ref() {
                                Expression::Identifier(recv_id) => recv_id.name.as_str(),
                                _ => "receiver",
                            };
                            let field_name = field.name.as_str();
                            let mut diag = Diagnostic::warning(
                                format!(
                                    "Cannot assign to `{recv_name}.{field_name}` — objects cannot mutate another object's state"
                                ),
                                *span,
                            );
                            diag.hint = Some(
                                format!(
                                    "Use `{recv_name} := {recv_name} {with_sel} newValue` to get an updated copy"
                                )
                                .into(),
                            );
                            self.diagnostics.push(diag);
                        }
                    }
                    _ => {}
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
                        if env.in_class_method && Self::is_self_receiver(receiver) {
                            if !in_abstract_method {
                                self.check_class_side_send(
                                    class_name,
                                    &selector_name,
                                    msg.span,
                                    hierarchy,
                                );
                            }
                        } else {
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
                self.infer_stmts(&block.body, hierarchy, &mut block_env, in_abstract_method);
                InferredType::Known("Block".into())
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

            // Array literal → Array
            Expression::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    self.infer_expr(elem, hierarchy, env, in_abstract_method);
                }
                InferredType::Known("Array".into())
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

            // Super — resolve to parent class type for method validation
            Expression::Super(_) => {
                // Look up current class from 'self' type, then find parent
                if let Some(InferredType::Known(class_name)) = env.get("self") {
                    if let Some(class_info) = hierarchy.get_class(&class_name) {
                        if let Some(ref parent) = class_info.superclass {
                            InferredType::Known(parent.clone())
                        } else {
                            InferredType::Dynamic
                        }
                    } else {
                        InferredType::Dynamic
                    }
                } else {
                    InferredType::Dynamic
                }
            }
        };

        // Record inferred type for the expression's full span for LSP queries
        self.type_map.insert(expr.span(), ty.clone());
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
        let arg_types: Vec<InferredType> = arguments
            .iter()
            .map(|arg| self.infer_expr(arg, hierarchy, env, in_abstract_method))
            .collect();

        // Handle asType: compile-time type assertion (ADR 0025 Phase 2b)
        // `expr asType: SomeClass` asserts expr is SomeClass, returns Known(SomeClass)
        if selector_name == "asType:" {
            if let Some(Expression::ClassReference { name, .. }) = arguments.first() {
                return InferredType::Known(name.name.clone());
            }
            return receiver_ty;
        }

        // Validate binary operand types when both sides are known
        // Only check if the receiver type actually defines the operator (avoids
        // duplicate warnings when the selector is already unknown).
        if let MessageSelector::Binary(op) = selector {
            if let (InferredType::Known(recv_ty), Some(InferredType::Known(arg_ty))) =
                (&receiver_ty, arg_types.first())
            {
                if hierarchy.resolves_selector(recv_ty, &selector_name) {
                    self.check_binary_operand_types(recv_ty, op, arg_ty, span, hierarchy);
                }
            }
        }

        // If receiver is a class reference, check class-side methods
        if let Expression::ClassReference { name, .. } = receiver {
            let class_name = &name.name;
            self.check_argument_types(
                class_name,
                &selector_name,
                &arg_types,
                span,
                hierarchy,
                true,
            );
            return self.check_class_side_send(class_name, &selector_name, span, hierarchy);
        }

        // For instance-side sends on known types
        if let InferredType::Known(ref class_name) = receiver_ty {
            // In class methods, self sends should check class-side methods
            if env.in_class_method && Self::is_self_receiver(receiver) {
                if !in_abstract_method {
                    self.check_argument_types(
                        class_name,
                        &selector_name,
                        &arg_types,
                        span,
                        hierarchy,
                        true,
                    );
                    return self.check_class_side_send(class_name, &selector_name, span, hierarchy);
                }
                return InferredType::Dynamic;
            }

            self.check_instance_selector(class_name, &selector_name, span, hierarchy);
            // Skip argument type check for binary messages — check_binary_operand_types
            // already provides more specific warnings for arithmetic/comparison/concat.
            if !matches!(selector, MessageSelector::Binary(_)) {
                self.check_argument_types(
                    class_name,
                    &selector_name,
                    &arg_types,
                    span,
                    hierarchy,
                    false,
                );
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

        // Check if class-side method exists (skip warning for DNU override classes)
        let has_class_method = hierarchy.find_class_method(class_name, selector).is_some();

        if !has_class_method && !hierarchy.has_class_dnu_override(class_name) {
            // Fall back to the Class→Behaviour→Object→ProtoObject instance-method chain.
            // At runtime, class objects dispatch through this chain (ADR 0032 Phase 0
            // fallthrough), so the type checker must model the same path.
            //
            // Factory selectors (spawn, new, etc.) are defined as instance methods on
            // Actor/Object but routed class-side by beamtalk_class_dispatch.erl.
            // Only these specific selectors bypass the Class chain check.
            let is_factory_selector = matches!(selector, "spawn" | "spawnWith:" | "new" | "new:");
            let has_class_chain_method = hierarchy.resolves_selector("Class", selector)
                || (is_factory_selector && hierarchy.resolves_selector(class_name, selector));
            if !has_class_chain_method {
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

        // Classes with instance-side doesNotUnderstand: override accept any message
        if hierarchy.has_instance_dnu_override(class_name) {
            return;
        }

        if !hierarchy.resolves_selector(class_name, selector) {
            self.emit_unknown_selector_warning(class_name, selector, span, hierarchy, false);
        }
    }

    /// Check that methods in typed classes have proper type annotations.
    fn check_typed_method_annotations(
        &mut self,
        method: &crate::ast::MethodDefinition,
        class_name: &EcoString,
    ) {
        // Skip primitive/intrinsic methods — their types are runtime-defined
        if method
            .body
            .iter()
            .any(|s| matches!(s.expression, Expression::Primitive { .. }))
        {
            return;
        }

        let selector = method.selector.name();

        // Check each parameter for missing type annotation
        for param in &method.parameters {
            if param.type_annotation.is_none() {
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Missing type annotation for parameter `{}` in typed class `{class_name}` (method `{selector}`)",
                            param.name.name
                        ),
                        param.name.span,
                    )
                    .with_category(DiagnosticCategory::Type),
                );
            }
        }

        // Check for missing return type
        if method.return_type.is_none() {
            self.diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Missing return type annotation in typed class `{class_name}` (method `{selector}`)"
                    ),
                    method.span,
                )
                .with_category(DiagnosticCategory::Type),
            );
        }
    }

    /// Check if `actual` type is compatible with `expected` type.
    ///
    /// Compatibility rules:
    /// - Same type → compatible
    /// - `expected` appears in `actual`'s superclass chain → compatible (e.g., Integer for Number)
    /// - Either type is unknown to the hierarchy → compatible (conservative)
    fn is_type_compatible(
        actual: &EcoString,
        expected: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        if actual == expected {
            return true;
        }
        // If either type isn't known to the hierarchy, don't warn (conservative)
        if !hierarchy.has_class(actual) || !hierarchy.has_class(expected) {
            return true;
        }
        // Walk superclass chain: if expected is an ancestor of actual, it's compatible
        let chain = hierarchy.superclass_chain(actual);
        chain.iter().any(|ancestor| ancestor == expected)
    }

    /// Check argument types against declared parameter types for a message send.
    fn check_argument_types(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
    ) {
        let method = if is_class_side {
            hierarchy.find_class_method(class_name, selector)
        } else {
            hierarchy.find_method(class_name, selector)
        };
        let Some(method) = method else { return };
        if method.param_types.is_empty() {
            return;
        }

        for (i, (arg_ty, expected)) in arg_types.iter().zip(method.param_types.iter()).enumerate() {
            let Some(expected_ty) = expected else {
                continue;
            };
            let InferredType::Known(actual_ty) = arg_ty else {
                continue; // Dynamic arguments never produce warnings
            };
            if !Self::is_type_compatible(actual_ty, expected_ty, hierarchy) {
                let param_pos = i + 1;
                let mut diag = Diagnostic::warning(
                    format!(
                        "Argument {param_pos} of '{selector}' on {class_name} expects {expected_ty}, got {actual_ty}"
                    ),
                    span,
                )
                .with_category(DiagnosticCategory::Type);
                diag.hint =
                    Some(format!("Expected {expected_ty} (or a subclass), got {actual_ty}").into());
                self.diagnostics.push(diag);
            }
        }
    }

    /// Check that a method body's inferred return type matches its declared return type.
    fn check_return_type(
        &mut self,
        method: &crate::ast::MethodDefinition,
        body_type: &InferredType,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
    ) {
        // Skip primitive methods
        if method
            .body
            .iter()
            .any(|s| matches!(s.expression, Expression::Primitive { .. }))
        {
            return;
        }

        let Some(ref declared) = method.return_type else {
            return;
        };
        // Only check Simple type annotations (Phase 3 handles unions/generics)
        let TypeAnnotation::Simple(type_id) = declared else {
            return;
        };
        let InferredType::Known(actual_ty) = body_type else {
            return; // Dynamic body — can't check
        };

        let expected_ty = &type_id.name;
        if !Self::is_type_compatible(actual_ty, expected_ty, hierarchy) {
            let selector = method.selector.name();
            let mut diag = Diagnostic::warning(
                format!(
                    "Method '{selector}' in {class_name} declares return type {expected_ty}, but body returns {actual_ty}"
                ),
                method.span,
            )
            .with_category(DiagnosticCategory::Type);
            diag.hint = Some(
                format!("Declared -> {expected_ty}, inferred body type is {actual_ty}").into(),
            );
            self.diagnostics.push(diag);
        }
    }

    /// Check that a child method's parameter types are compatible with its parent's.
    fn check_override_param_compatibility(
        &mut self,
        method: &crate::ast::MethodDefinition,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
    ) {
        let selector = method.selector.name();
        let Some(class_info) = hierarchy.get_class(class_name) else {
            return;
        };
        let Some(ref superclass) = class_info.superclass else {
            return;
        };
        let Some(parent_method) = hierarchy.find_method(superclass, &selector) else {
            return;
        };
        if parent_method.param_types.is_empty() {
            return;
        }

        // Get child param types from MethodInfo
        let Some(child_method) = hierarchy.find_method(class_name, &selector) else {
            return;
        };

        for (i, (child_ty, parent_ty)) in child_method
            .param_types
            .iter()
            .zip(parent_method.param_types.iter())
            .enumerate()
        {
            let (Some(child_t), Some(parent_t)) = (child_ty, parent_ty) else {
                continue;
            };
            if !Self::is_type_compatible(child_t, parent_t, hierarchy) {
                let param_pos = i + 1;
                let mut diag = Diagnostic::warning(
                    format!(
                        "Parameter {param_pos} of '{selector}' in {class_name} has type {child_t}, incompatible with parent's {parent_t}"
                    ),
                    method.span,
                )
                .with_category(DiagnosticCategory::Type);
                diag.hint = Some(
                    format!("Parent class {superclass} declares parameter type {parent_t}").into(),
                );
                self.diagnostics.push(diag);
            }
        }
    }

    /// Validate binary message operand types for arithmetic and string concatenation.
    ///
    /// When both receiver and argument types are known, checks that the argument type
    /// is compatible with the operator. Only emits warnings (not errors) to allow
    /// for dynamic dispatch.
    fn check_binary_operand_types(
        &mut self,
        receiver_ty: &EcoString,
        operator: &str,
        arg_ty: &EcoString,
        span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        let is_numeric = |ty: &str| hierarchy.is_numeric_type(ty);
        let is_arithmetic = matches!(operator, "+" | "-" | "*" | "/");
        let is_comparison = matches!(operator, "<" | ">" | "<=" | ">=");

        // Arithmetic operators on numeric types require numeric arguments
        if is_arithmetic && is_numeric(receiver_ty) && !is_numeric(arg_ty) {
            let mut diag = Diagnostic::warning(
                format!("`{operator}` on {receiver_ty} expects a numeric argument, got {arg_ty}"),
                span,
            )
            .with_category(DiagnosticCategory::Type);
            diag.hint = Some("Arithmetic operators require Integer or Float operands".into());
            self.diagnostics.push(diag);
            return;
        }

        // String concatenation with ++ expects a String argument
        if operator == "++"
            && receiver_ty.as_str() == "String"
            && arg_ty.as_str() != "String"
            && arg_ty.as_str() != "Symbol"
        {
            let mut diag = Diagnostic::warning(
                format!("`++` on String expects a String argument, got {arg_ty}"),
                span,
            )
            .with_category(DiagnosticCategory::Type);
            diag.hint = Some("Convert the argument to String first".into());
            self.diagnostics.push(diag);
            return;
        }

        // Comparison operators on numeric types require numeric arguments
        if is_comparison && is_numeric(receiver_ty) && !is_numeric(arg_ty) {
            let mut diag = Diagnostic::warning(
                format!("`{operator}` on {receiver_ty} expects a numeric argument, got {arg_ty}"),
                span,
            )
            .with_category(DiagnosticCategory::Type);
            diag.hint = Some("Comparison operators require compatible types".into());
            self.diagnostics.push(diag);
        }
    }

    /// Check a field assignment `self.field := value` against the declared state type.
    ///
    /// If the field has a type annotation in the class hierarchy and the inferred
    /// value type is known and incompatible, emits a warning.
    fn check_field_assignment(
        &mut self,
        field: &crate::ast::Identifier,
        value_ty: &InferredType,
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &TypeEnv,
    ) {
        let InferredType::Known(value_type) = value_ty else {
            return; // Dynamic expressions never produce warnings
        };
        let Some(InferredType::Known(class_name)) = env.get("self") else {
            return;
        };
        let Some(declared_type) = hierarchy.state_field_type(&class_name, &field.name) else {
            return; // No type annotation on this field
        };
        if !Self::is_assignable_to(value_type, &declared_type, hierarchy) {
            let mut diag = Diagnostic::warning(
                format!(
                    "Type mismatch: field `{}` declared as {declared_type}, got {value_type}",
                    field.name
                ),
                span,
            )
            .with_category(DiagnosticCategory::Type);
            diag.hint = Some(format!("Expected {declared_type} but assigning {value_type}").into());
            self.diagnostics.push(diag);
        }
    }

    /// Check state default values match declared types at class definition time.
    fn check_state_defaults(
        &mut self,
        class: &crate::ast::ClassDefinition,
        hierarchy: &ClassHierarchy,
    ) {
        for decl in &class.state {
            let Some(ref type_annotation) = decl.type_annotation else {
                continue;
            };
            let Some(ref default_value) = decl.default_value else {
                continue;
            };
            let declared_type = type_annotation.type_name();
            let mut env = TypeEnv::new();
            env.set("self", InferredType::Known(class.name.name.clone()));
            let inferred = self.infer_expr(default_value, hierarchy, &mut env, false);
            let InferredType::Known(value_type) = &inferred else {
                continue; // Dynamic defaults are fine
            };
            if !Self::is_assignable_to(value_type, &declared_type, hierarchy) {
                let mut diag = Diagnostic::warning(
                    format!(
                        "Type mismatch: state `{}` declared as {declared_type}, default is {value_type}",
                        decl.name.name
                    ),
                    decl.span,
                )
                .with_category(DiagnosticCategory::Type);
                diag.hint = Some(
                    format!(
                        "Default value type {value_type} is not compatible with {declared_type}"
                    )
                    .into(),
                );
                self.diagnostics.push(diag);
            }
        }
    }

    /// Returns true if `value_type` is assignable to `declared_type`.
    ///
    /// A type is assignable if it is the same type or a subclass of the declared type.
    /// For example, Integer is assignable to Number because Integer's superclass
    /// chain includes Number.
    ///
    /// Returns true (permissive) for complex type annotations (unions, generics)
    /// that are not yet supported — full union type support is Phase 3.
    fn is_assignable_to(
        value_type: &EcoString,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        if value_type == declared_type {
            return true;
        }
        // Skip checking for complex type annotations (unions, generics, singletons)
        // that contain non-class-name characters. These need Phase 3 union support.
        if declared_type.contains('|')
            || declared_type.contains('<')
            || declared_type.starts_with('#')
        {
            return true;
        }
        // Check if value_type is a subclass of declared_type
        hierarchy
            .superclass_chain(value_type)
            .iter()
            .any(|ancestor| ancestor == declared_type)
    }

    /// Infer types for a sequence of expression statements.
    ///
    /// Skips `@expect` directive nodes so they don't reset the inferred body type
    /// to `Dynamic` and interfere with return-type checking.  Suppression of matching
    /// diagnostics is handled separately by `apply_expect_directives` in
    /// `diagnostic_provider` after all diagnostics have been collected.
    ///
    /// Returns the inferred type of the last non-directive expression, or `Dynamic`
    /// for an empty list.
    fn infer_stmts(
        &mut self,
        stmts: &[ExpressionStatement],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let mut body_type = InferredType::Dynamic;

        for stmt in stmts {
            let expr = &stmt.expression;

            // @expect directives are compile-time only; skip them so they don't
            // clobber body_type and affect return-type inference.
            if matches!(expr, Expression::ExpectDirective { .. }) {
                continue;
            }

            body_type = self.infer_expr(expr, hierarchy, env, in_abstract_method);

            if matches!(expr, Expression::Return { .. }) {
                break;
            }
        }

        body_type
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

        let mut diag = Diagnostic::hint(message, span).with_category(DiagnosticCategory::Dnu);

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
    /// Whether we're inside a class method body (self refers to class-side).
    in_class_method: bool,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            in_class_method: false,
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

#[cfg(test)]
mod tests {
    //! Tests for zero-syntax type inference across all expression forms.
    use super::*;
    use crate::ast::{
        Block, CascadeMessage, ClassDefinition, ClassKind, CommentAttachment, ExpressionStatement,
        Identifier, KeywordPart, MethodDefinition, MethodKind, Module, ParameterDefinition,
        StateDeclaration, TypeAnnotation,
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

    fn bare(expr: Expression) -> ExpressionStatement {
        ExpressionStatement::bare(expr)
    }

    fn make_module(expressions: Vec<Expression>) -> Module {
        Module::new(
            expressions
                .into_iter()
                .map(ExpressionStatement::bare)
                .collect(),
            span(),
        )
    }

    fn make_module_with_classes(
        expressions: Vec<Expression>,
        classes: Vec<ClassDefinition>,
    ) -> Module {
        let mut module = Module::new(
            expressions
                .into_iter()
                .map(ExpressionStatement::bare)
                .collect(),
            span(),
        );
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
            is_cast: false,
            span: span(),
        }
    }

    fn int_lit(n: i64) -> Expression {
        Expression::Literal(Literal::Integer(n), span())
    }

    fn str_lit(s: &str) -> Expression {
        Expression::Literal(Literal::String(s.into()), span())
    }

    fn char_lit(c: char) -> Expression {
        Expression::Literal(Literal::Character(c), span())
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
    fn test_class_side_dnu_suppresses_warning() {
        // Erlang erlang  ← Erlang has class-side doesNotUnderstand:args:
        // Should NOT produce "does not understand" warning
        let module = make_module(vec![msg_send(
            class_ref("Erlang"),
            MessageSelector::Unary("erlang".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert!(
            dnu_warnings.is_empty(),
            "Erlang class-side DNU should suppress warnings, got: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_character_literal_integer_method_no_warning() {
        // BT-778: $A + 1 — Character inherits Integer's +, no DNU warning
        let module = make_module(vec![msg_send(
            char_lit('A'),
            MessageSelector::Binary("+".into()),
            vec![int_lit(1)],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert!(
            dnu_warnings.is_empty(),
            "Character should understand '+' via Integer inheritance: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_character_literal_own_method_no_warning() {
        // BT-778: $A isLetter — Character's own method, no DNU warning
        let module = make_module(vec![msg_send(
            char_lit('A'),
            MessageSelector::Unary("isLetter".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "Character should understand 'isLetter': {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_character_literal_bogus_method_warning() {
        // BT-778: $A bogusMethod — should still produce DNU warning
        let module = make_module(vec![msg_send(
            char_lit('A'),
            MessageSelector::Unary("bogusMethod".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert_eq!(
            dnu_warnings.len(),
            1,
            "Character should NOT understand 'bogusMethod': {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_character_literal_non_numeric_operand_warns() {
        // BT-778: $A + 'hello' — Character inherits Integer's +, but 'hello' is not numeric.
        // The type checker should warn that + expects a numeric argument, not a String.
        let module = make_module(vec![msg_send(
            char_lit('A'),
            MessageSelector::Binary("+".into()),
            vec![str_lit("hello")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let type_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("expects a numeric argument"))
            .collect();
        assert_eq!(
            type_warnings.len(),
            1,
            "Character + String should warn about non-numeric operand: {:?}",
            checker.diagnostics()
        );
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
                class_kind: ClassKind::Actor,
                is_abstract: false,
                is_sealed: false,
                is_typed: false,
                state: vec![],
                methods: vec![MethodDefinition {
                    selector: MessageSelector::Unary("increment".into()),
                    parameters: vec![],
                    body: vec![],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: span(),
                }],
                class_methods: vec![],
                class_variables: vec![],
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
        );
        let hierarchy = ClassHierarchy::build(&counter_module).0.unwrap();

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
            body: vec![bare(int_lit(42))],
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
                class_kind: ClassKind::Object,
                is_abstract: false,
                is_sealed: false,
                is_typed: false,
                state: vec![],
                methods: vec![MethodDefinition {
                    selector: MessageSelector::Unary("greet".into()),
                    parameters: vec![],
                    body: vec![bare(msg_send(
                        var("self"),
                        MessageSelector::Unary("nonExistent".into()),
                        vec![],
                    ))],
                    return_type: None,
                    is_sealed: false,
                    kind: MethodKind::Primary,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: span(),
                }],
                class_methods: vec![],
                class_variables: vec![],
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
        );
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        // Should warn about 'nonExistent' on Greeter
        assert_eq!(checker.diagnostics().len(), 1);
        assert!(checker.diagnostics()[0].message.contains("Greeter"));
    }

    #[test]
    fn test_warnings_only_never_errors() {
        // All diagnostics should be warnings or hints, never errors
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Unary("bogus".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        for diag in checker.diagnostics() {
            assert_ne!(
                diag.severity,
                crate::source_analysis::Severity::Error,
                "type checker should only emit warnings or hints, not errors: {}",
                diag.message
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
        // 42 printString  ← Integer inherits printString from Object
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Unary("printString".into()),
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

        // The integer literal should be recorded as Integer
        let ty = type_map.get(module.expressions[0].expression.span());
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
        let ty = type_map.get(x_expr.expression.span());
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
        let ty = type_map.get(module.expressions[0].expression.span());
        assert_eq!(
            ty,
            Some(&InferredType::Known("String".into())),
            "infer_types should return correct type map"
        );
    }

    // BT-614: Class method self-send tests

    fn make_class_with_class_methods(
        name: &str,
        instance_methods: Vec<MethodDefinition>,
        class_methods: Vec<MethodDefinition>,
    ) -> ClassDefinition {
        ClassDefinition {
            name: ident(name),
            superclass: Some(ident("Actor")),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: instance_methods,
            class_methods,
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }
    }

    fn make_method(selector: &str, body: Vec<Expression>) -> MethodDefinition {
        MethodDefinition {
            selector: MessageSelector::Unary(selector.into()),
            parameters: vec![],
            body: body.into_iter().map(ExpressionStatement::bare).collect(),
            return_type: None,
            is_sealed: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }
    }

    #[test]
    fn test_class_method_self_send_no_false_warning() {
        // class create => self.instanceCount := ...
        // class createAndReport => self create  ← should NOT warn
        let class_def = make_class_with_class_methods(
            "ClassVarCounter",
            vec![],
            vec![
                make_method("create", vec![int_lit(1)]),
                make_method(
                    "createAndReport",
                    vec![msg_send(
                        var("self"),
                        MessageSelector::Unary("create".into()),
                        vec![],
                    )],
                ),
            ],
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "self send to existing class method should not warn, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_class_method_self_send_warns_on_unknown() {
        // class doStuff => self bogusMethod  ← should warn
        let class_def = make_class_with_class_methods(
            "MyClass",
            vec![],
            vec![make_method(
                "doStuff",
                vec![msg_send(
                    var("self"),
                    MessageSelector::Unary("bogusMethod".into()),
                    vec![],
                )],
            )],
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert_eq!(
            checker.diagnostics().len(),
            1,
            "should warn about unknown class method"
        );
        assert!(
            checker.diagnostics()[0].message.contains("bogusMethod"),
            "warning should mention 'bogusMethod': {}",
            checker.diagnostics()[0].message
        );
    }

    #[test]
    fn test_instance_method_self_send_no_regression() {
        // instance method: greet => self increment  ← should NOT warn if increment exists
        let class_def = make_class_with_class_methods(
            "Counter",
            vec![
                make_method("increment", vec![int_lit(1)]),
                make_method(
                    "greet",
                    vec![msg_send(
                        var("self"),
                        MessageSelector::Unary("increment".into()),
                        vec![],
                    )],
                ),
            ],
            vec![],
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "instance method self send should not regress, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_cascade_in_class_method_checks_class_side() {
        // class doStuff => self create; getCount  ← both class methods, no warning
        let class_def = make_class_with_class_methods(
            "ClassVarCounter",
            vec![],
            vec![
                make_method("create", vec![int_lit(1)]),
                make_method("getCount", vec![int_lit(0)]),
                make_method(
                    "doStuff",
                    vec![Expression::Cascade {
                        receiver: Box::new(var("self")),
                        messages: vec![
                            CascadeMessage::new(
                                MessageSelector::Unary("create".into()),
                                vec![],
                                span(),
                            ),
                            CascadeMessage::new(
                                MessageSelector::Unary("getCount".into()),
                                vec![],
                                span(),
                            ),
                        ],
                        span: span(),
                    }],
                ),
            ],
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "cascade self sends to class methods should not warn, got: {:?}",
            checker.diagnostics()
        );
    }

    // --- Super type inference tests (BT-596) ---

    #[test]
    fn test_super_infers_parent_class_type() {
        // class Child < Parent; method: reset => super reset
        // super should infer as Parent type, not Dynamic
        let parent = ClassDefinition {
            name: ident("Parent"),
            superclass: Some(ident("Object")),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("reset".into()),
                parameters: vec![],
                body: vec![bare(int_lit(0))],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        };

        let super_span = Span::new(100, 105);
        let msg_span = Span::new(100, 115);
        let child = ClassDefinition {
            name: ident("Child"),
            superclass: Some(ident("Parent")),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("reset".into()),
                parameters: vec![],
                body: vec![bare(Expression::MessageSend {
                    receiver: Box::new(Expression::Super(super_span)),
                    selector: MessageSelector::Unary("reset".into()),
                    arguments: vec![],
                    is_cast: false,
                    span: msg_span,
                })],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        };

        let module = make_module_with_classes(vec![], vec![parent, child]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let type_map = infer_types(&module, &hierarchy);

        // super should be inferred as Parent (not Dynamic)
        let ty = type_map.get(super_span);
        assert_eq!(
            ty,
            Some(&InferredType::Known("Parent".into())),
            "super should infer as parent class type"
        );
    }

    #[test]
    fn test_super_unknown_selector_warns() {
        // class Child < Parent; method: test => super nonExistent
        // Should warn because Parent doesn't have nonExistent
        let parent = ClassDefinition {
            name: ident("Parent"),
            superclass: Some(ident("Object")),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("reset".into()),
                parameters: vec![],
                body: vec![bare(int_lit(0))],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        };

        let child = ClassDefinition {
            name: ident("Child"),
            superclass: Some(ident("Parent")),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![],
            methods: vec![MethodDefinition {
                selector: MessageSelector::Unary("test".into()),
                parameters: vec![],
                body: vec![bare(msg_send(
                    Expression::Super(span()),
                    MessageSelector::Unary("nonExistent".into()),
                    vec![],
                ))],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        };

        let module = make_module_with_classes(vec![], vec![parent, child]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);

        let warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert_eq!(
            warnings.len(),
            1,
            "should warn about unknown super selector"
        );
        assert!(
            warnings[0].message.contains("Parent"),
            "warning should reference parent class"
        );
    }

    // --- Binary operand type validation tests (BT-596) ---

    #[test]
    fn test_integer_plus_string_warns() {
        // 42 + "hello" — type mismatch
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("+".into()),
            vec![str_lit("hello")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);

        let type_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("expects a numeric argument"))
            .collect();
        assert_eq!(type_warnings.len(), 1);
        assert!(type_warnings[0].message.contains("Integer"));
        assert!(type_warnings[0].message.contains("String"));
    }

    #[test]
    fn test_integer_plus_integer_no_warning() {
        // 42 + 1 — valid
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("+".into()),
            vec![int_lit(1)],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);

        let type_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("expects a numeric"))
            .collect();
        assert!(type_warnings.is_empty());
    }

    #[test]
    fn test_string_plus_integer_warns_unknown_selector() {
        // "hello" + 42 — String doesn't define +, so we get unknown selector (not operand type)
        let module = make_module(vec![msg_send(
            str_lit("hello"),
            MessageSelector::Binary("+".into()),
            vec![int_lit(42)],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);

        // String doesn't define +, so we get "does not understand" (not operand type warning)
        let warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].message.contains("String"));
    }

    #[test]
    fn test_string_concat_no_operand_warning() {
        // "hello" ++ " world" — valid String concatenation
        let module = make_module(vec![msg_send(
            str_lit("hello"),
            MessageSelector::Binary("++".into()),
            vec![str_lit(" world")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);

        let type_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("expects a"))
            .collect();
        assert!(type_warnings.is_empty());
    }

    #[test]
    fn test_string_concat_integer_warns() {
        // "hello" ++ 42 — wrong type for string concat
        let module = make_module(vec![msg_send(
            str_lit("hello"),
            MessageSelector::Binary("++".into()),
            vec![int_lit(42)],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);

        let type_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("expects a String argument"))
            .collect();
        assert_eq!(type_warnings.len(), 1);
    }

    #[test]
    fn test_dynamic_type_skips_operand_check() {
        // x + 42 — x is dynamic, no warning
        let module = make_module(vec![msg_send(
            var("x"),
            MessageSelector::Binary("+".into()),
            vec![int_lit(42)],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);

        let type_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("expects a"))
            .collect();
        assert!(type_warnings.is_empty());
    }

    #[test]
    fn test_comparison_integer_vs_string_warns() {
        // 42 < "hello" — type mismatch for comparison
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("<".into()),
            vec![str_lit("hello")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);

        let type_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("expects a numeric argument"))
            .collect();
        assert_eq!(type_warnings.len(), 1);
    }

    #[test]
    fn test_binary_operand_warnings_are_warnings_not_errors() {
        // All operand type diagnostics should be warnings
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("+".into()),
            vec![str_lit("hello")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);

        for diag in checker.diagnostics() {
            if diag.message.contains("expects a") {
                assert_eq!(
                    diag.severity,
                    crate::source_analysis::Severity::Warning,
                    "operand type diagnostics should be warnings"
                );
            }
        }
    }

    // ---- Typed class tests (BT-587) ----

    #[test]
    fn test_typed_class_warns_on_missing_param_annotation() {
        // typed class with untyped parameter
        let mut class_def = ClassDefinition::with_modifiers(
            ident("StrictCounter"),
            Some(ident("Actor")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![ParameterDefinition::new(ident("amount"))], // no type annotation
                vec![bare(int_lit(0))],
                span(),
            )],
            span(),
        );
        class_def.is_typed = true;
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("Missing type annotation for parameter"))
            .collect();
        assert_eq!(
            warnings.len(),
            1,
            "should warn about untyped param `amount`"
        );
        assert!(warnings[0].message.contains("amount"));
    }

    #[test]
    fn test_typed_class_warns_on_missing_return_type() {
        // typed class with method missing return type
        let mut class_def = ClassDefinition::with_modifiers(
            ident("StrictCounter"),
            Some(ident("Actor")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Unary("increment".into()),
                vec![],
                vec![bare(int_lit(1))],
                span(),
            )],
            span(),
        );
        class_def.is_typed = true;
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("Missing return type"))
            .collect();
        assert_eq!(warnings.len(), 1, "should warn about missing return type");
    }

    #[test]
    fn test_typed_class_no_warning_when_fully_annotated() {
        // typed class with fully annotated method
        let mut class_def = ClassDefinition::with_modifiers(
            ident("StrictCounter"),
            Some(ident("Actor")),
            false,
            false,
            vec![],
            vec![MethodDefinition::with_return_type(
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![ParameterDefinition::with_type(
                    ident("amount"),
                    TypeAnnotation::Simple(ident("Integer")),
                )],
                vec![bare(int_lit(0))],
                TypeAnnotation::Simple(ident("Integer")),
                span(),
            )],
            span(),
        );
        class_def.is_typed = true;
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let typed_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("Missing"))
            .collect();
        assert!(
            typed_warnings.is_empty(),
            "fully annotated method should not warn, got: {typed_warnings:?}"
        );
    }

    #[test]
    fn test_non_typed_class_no_warnings() {
        // non-typed class with untyped method — no warnings expected
        let class_def = ClassDefinition::with_modifiers(
            ident("Counter"),
            Some(ident("Actor")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![ParameterDefinition::new(ident("amount"))],
                vec![bare(int_lit(0))],
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let typed_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("Missing"))
            .collect();
        assert!(
            typed_warnings.is_empty(),
            "non-typed class should not warn about annotations"
        );
    }

    #[test]
    fn test_typed_class_skips_primitive_methods() {
        // typed class with @primitive method — no warnings
        let mut class_def = ClassDefinition::with_modifiers(
            ident("StrictInteger"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Binary("+".into()),
                vec![ParameterDefinition::new(ident("other"))],
                vec![bare(Expression::Primitive {
                    name: "+".into(),
                    is_quoted: true,
                    is_intrinsic: false,
                    span: span(),
                })],
                span(),
            )],
            span(),
        );
        class_def.is_typed = true;
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let typed_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("Missing"))
            .collect();
        assert!(
            typed_warnings.is_empty(),
            "primitive methods should be skipped in typed classes"
        );
    }

    #[test]
    fn test_as_type_assertion_infers_correct_type() {
        // x asType: Integer  should infer x as Integer
        let module = make_module(vec![msg_send(
            var("x"),
            MessageSelector::Keyword(vec![KeywordPart::new("asType:", span())]),
            vec![class_ref("Integer")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        // The send itself shouldn't warn (asType: is special)
        assert!(
            checker.diagnostics().is_empty(),
            "asType: should not produce warnings, got: {:?}",
            checker.diagnostics()
        );
    }

    // ---- BT-671: Argument and return type checking tests ----

    #[test]
    fn test_integer_plus_string_warns_operand_type() {
        // 42 + "hello" — Integer + expects numeric arg, String is wrong type
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("+".into()),
            vec![str_lit("hello")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            !checker.diagnostics().is_empty(),
            "42 + 'hello' should produce a type warning"
        );
    }

    #[test]
    fn test_integer_plus_integer_no_arg_type_warning() {
        // 3 + 4 — valid, no warnings (acceptance criteria)
        let module = make_module(vec![msg_send(
            int_lit(3),
            MessageSelector::Binary("+".into()),
            vec![int_lit(4)],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "3 + 4 should not produce warnings, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_integer_subtype_of_number_no_warning() {
        // Integer is a subtype of Number via superclass chain
        // Integer + Integer should work since param declares Number
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("+".into()),
            vec![int_lit(1)],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "Integer should be valid for Number param"
        );
    }

    #[test]
    fn test_dynamic_args_never_warn() {
        // unknownVar + 42 — receiver is Dynamic, no warning
        let module = make_module(vec![msg_send(
            var("unknownVar"),
            MessageSelector::Binary("+".into()),
            vec![var("alsoUnknown")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "Dynamic arguments should never produce warnings"
        );
    }

    #[test]
    fn test_return_type_mismatch_warns() {
        // getBalance -> Integer => 'oops' warns (acceptance criteria)
        let class_def = ClassDefinition::with_modifiers(
            ident("Account"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::with_return_type(
                MessageSelector::Unary("getBalance".into()),
                vec![],
                vec![bare(str_lit("oops"))], // Returns String, declared Integer
                TypeAnnotation::Simple(ident("Integer")),
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let return_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("declares return type"))
            .collect();
        assert_eq!(
            return_warnings.len(),
            1,
            "should warn about return type mismatch"
        );
        assert!(return_warnings[0].message.contains("Integer"));
        assert!(return_warnings[0].message.contains("String"));
    }

    #[test]
    fn test_return_type_match_no_warning() {
        // getBalance -> Integer => 42  — correct return type
        let class_def = ClassDefinition::with_modifiers(
            ident("Account"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::with_return_type(
                MessageSelector::Unary("getBalance".into()),
                vec![],
                vec![bare(int_lit(42))],
                TypeAnnotation::Simple(ident("Integer")),
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let return_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("declares return type"))
            .collect();
        assert!(
            return_warnings.is_empty(),
            "matching return type should not warn"
        );
    }

    #[test]
    fn test_return_type_skip_primitive_methods() {
        // @primitive method — should not check return type
        let class_def = ClassDefinition::with_modifiers(
            ident("MyInt"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::with_return_type(
                MessageSelector::Unary("value".into()),
                vec![],
                vec![bare(Expression::Primitive {
                    name: "value".into(),
                    is_quoted: true,
                    is_intrinsic: false,
                    span: span(),
                })],
                TypeAnnotation::Simple(ident("Integer")),
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let return_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("declares return type"))
            .collect();
        assert!(
            return_warnings.is_empty(),
            "primitive methods should skip return type check"
        );
    }

    #[test]
    fn test_return_type_no_annotation_no_warning() {
        // Method without return type annotation — no warning
        let class_def = ClassDefinition::with_modifiers(
            ident("Counter"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Unary("count".into()),
                vec![],
                vec![bare(str_lit("oops"))],
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let return_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("declares return type"))
            .collect();
        assert!(
            return_warnings.is_empty(),
            "method without return type should not warn about return type"
        );
    }

    // Helper: lex + parse a source string into a Module.
    fn parse_source(source: &str) -> Module {
        use crate::source_analysis::{lex_with_eof, parse};
        let tokens = lex_with_eof(source);
        let (module, _) = parse(tokens);
        module
    }

    // Helper: run the full diagnostic pipeline (type check + @expect suppression).
    // Suppression is owned by `apply_expect_directives` in diagnostic_provider —
    // the type checker itself does NOT suppress.
    fn run_with_expect(module: &Module, hierarchy: &ClassHierarchy) -> Vec<Diagnostic> {
        let mut checker = TypeChecker::new();
        checker.check_module(module, hierarchy);
        let mut diags = checker.take_diagnostics();
        crate::queries::diagnostic_provider::apply_expect_directives(module, &mut diags);
        diags
    }

    #[test]
    fn test_expect_dnu_suppresses_dnu_warning() {
        // @expect dnu before a DNU-producing send suppresses the hint.
        // Real-world use: `self species withAll: result` in Collection.bt.
        // Spans must be real for apply_expect_directives to match by span.
        let hierarchy = ClassHierarchy::with_builtins();

        // Without @expect: should warn
        let module_bare = parse_source("42 unknownSelector");
        let diags_without = run_with_expect(&module_bare, &hierarchy);
        assert!(
            !diags_without.is_empty(),
            "42 unknownSelector should produce a DNU hint without @expect"
        );

        // With @expect dnu: should be suppressed
        let module_with = parse_source("@expect dnu\n42 unknownSelector");
        let diags_with = run_with_expect(&module_with, &hierarchy);
        assert!(
            diags_with.is_empty(),
            "@expect dnu should suppress DNU hint, got: {diags_with:?}"
        );
    }

    #[test]
    fn test_expect_type_suppresses_type_warning() {
        // @expect type before a type-mismatch expression suppresses the warning.
        let hierarchy = ClassHierarchy::with_builtins();

        // Without @expect: should warn
        let module_bare = parse_source("1 + \"hello\"");
        let diags_without = run_with_expect(&module_bare, &hierarchy);
        assert!(
            !diags_without.is_empty(),
            "1 + \"hello\" should produce a type warning without @expect"
        );

        // With @expect type: should be suppressed
        let module_with = parse_source("@expect type\n1 + \"hello\"");
        let diags_with = run_with_expect(&module_with, &hierarchy);
        assert!(
            diags_with.is_empty(),
            "@expect type should suppress type warning, got: {diags_with:?}"
        );
    }

    #[test]
    fn test_expect_does_not_suppress_next_next_expression() {
        // @expect dnu only suppresses the immediately following expression.
        // Here @expect applies to `42` (no DNU) → stale error is emitted,
        // and `42 unknownSelector` still produces its own DNU hint.
        let module = parse_source("@expect dnu\n42\n42 unknownSelector");
        let hierarchy = ClassHierarchy::with_builtins();
        let diags = run_with_expect(&module, &hierarchy);
        assert!(
            !diags.is_empty(),
            "@expect dnu on `42` must not suppress DNU on the following expression"
        );
    }

    #[test]
    fn test_as_type_suppresses_subsequent_warnings() {
        // (x asType: Integer) + "hello" — x is asserted Integer, should warn about String arg
        let module = make_module(vec![msg_send(
            msg_send(
                var("x"),
                MessageSelector::Keyword(vec![KeywordPart::new("asType:", span())]),
                vec![class_ref("Integer")],
            ),
            MessageSelector::Binary("+".into()),
            vec![str_lit("hello")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        // After asType:, x is Known(Integer), so + "hello" should warn
        assert!(
            !checker.diagnostics().is_empty(),
            "asType: Integer + String should produce a type warning"
        );
    }

    #[test]
    fn test_keyword_arg_type_mismatch_warns() {
        // Counter with typed parameter: deposit: amount: Integer
        // Sending deposit: "hello" should warn
        let class_def = ClassDefinition::with_modifiers(
            ident("Counter"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![ParameterDefinition::with_type(
                    ident("amount"),
                    TypeAnnotation::Simple(ident("Integer")),
                )],
                vec![bare(int_lit(0))],
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(
            vec![
                // c := Counter new
                assign(
                    "c",
                    msg_send(
                        class_ref("Counter"),
                        MessageSelector::Unary("new".into()),
                        vec![],
                    ),
                ),
                // c deposit: "hello"
                msg_send(
                    var("c"),
                    MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                    vec![str_lit("hello")],
                ),
            ],
            vec![class_def],
        );
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let arg_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("expects Integer"))
            .collect();
        assert_eq!(
            arg_warnings.len(),
            1,
            "should warn about String argument where Integer expected, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_keyword_arg_type_match_no_warning() {
        // Counter deposit: 42 — correct type
        let class_def = ClassDefinition::with_modifiers(
            ident("Counter"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![ParameterDefinition::with_type(
                    ident("amount"),
                    TypeAnnotation::Simple(ident("Integer")),
                )],
                vec![bare(int_lit(0))],
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(
            vec![
                assign(
                    "c",
                    msg_send(
                        class_ref("Counter"),
                        MessageSelector::Unary("new".into()),
                        vec![],
                    ),
                ),
                msg_send(
                    var("c"),
                    MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                    vec![int_lit(42)],
                ),
            ],
            vec![class_def],
        );
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let arg_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("expects"))
            .collect();
        assert!(
            arg_warnings.is_empty(),
            "correct arg type should not warn, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_override_incompatible_param_type_warns() {
        // Parent: deposit: amount: Number
        // Child: deposit: amount: String (incompatible)
        let parent = ClassDefinition::with_modifiers(
            ident("Base"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![ParameterDefinition::with_type(
                    ident("amount"),
                    TypeAnnotation::Simple(ident("Number")),
                )],
                vec![bare(int_lit(0))],
                span(),
            )],
            span(),
        );
        let child = ClassDefinition::with_modifiers(
            ident("Derived"),
            Some(ident("Base")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![ParameterDefinition::with_type(
                    ident("amount"),
                    TypeAnnotation::Simple(ident("String")),
                )],
                vec![bare(str_lit("ok"))],
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![parent, child]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let override_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("incompatible with parent"))
            .collect();
        assert_eq!(
            override_warnings.len(),
            1,
            "should warn about incompatible override param type"
        );
    }

    #[test]
    fn test_override_compatible_param_type_no_warning() {
        // Parent: deposit: amount: Number
        // Child: deposit: amount: Integer (compatible — Integer is subclass of Number)
        let parent = ClassDefinition::with_modifiers(
            ident("Base"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![ParameterDefinition::with_type(
                    ident("amount"),
                    TypeAnnotation::Simple(ident("Number")),
                )],
                vec![bare(int_lit(0))],
                span(),
            )],
            span(),
        );
        let child = ClassDefinition::with_modifiers(
            ident("Derived"),
            Some(ident("Base")),
            false,
            false,
            vec![],
            vec![MethodDefinition::new(
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![ParameterDefinition::with_type(
                    ident("amount"),
                    TypeAnnotation::Simple(ident("Integer")),
                )],
                vec![bare(int_lit(0))],
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![parent, child]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let override_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("incompatible with parent"))
            .collect();
        assert!(
            override_warnings.is_empty(),
            "compatible override should not warn"
        );
    }

    #[test]
    fn test_all_type_warnings_are_severity_warning() {
        // Verify all diagnostics from type checking are Severity::Warning
        let class_def = ClassDefinition::with_modifiers(
            ident("Account"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::with_return_type(
                MessageSelector::Unary("getBalance".into()),
                vec![],
                vec![bare(str_lit("oops"))],
                TypeAnnotation::Simple(ident("Integer")),
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(
            vec![msg_send(
                int_lit(42),
                MessageSelector::Binary("+".into()),
                vec![str_lit("hello")],
            )],
            vec![class_def],
        );
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        for diag in checker.diagnostics() {
            assert_eq!(
                diag.severity,
                crate::source_analysis::Severity::Warning,
                "all type checking diagnostics should be warnings, not errors: {}",
                diag.message
            );
        }
    }

    // --- State field assignment type checking tests (BT-672) ---

    fn field_access(field_name: &str) -> Expression {
        Expression::FieldAccess {
            receiver: Box::new(var("self")),
            field: ident(field_name),
            span: span(),
        }
    }

    fn field_assign(field_name: &str, value: Expression) -> Expression {
        Expression::Assignment {
            target: Box::new(field_access(field_name)),
            value: Box::new(value),
            span: span(),
        }
    }

    fn counter_class_with_typed_state(
        methods: Vec<MethodDefinition>,
        state: Vec<StateDeclaration>,
    ) -> ClassDefinition {
        ClassDefinition {
            name: ident("Counter"),
            superclass: Some(ident("Object")),
            class_kind: ClassKind::Object,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state,
            methods,
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }
    }

    #[test]
    fn test_empty_body_with_return_type_no_crash() {
        // Empty method body with return type annotation — should not crash
        let class_def = ClassDefinition::with_modifiers(
            ident("Counter"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::with_return_type(
                MessageSelector::Unary("count".into()),
                vec![],
                vec![], // empty body
                TypeAnnotation::Simple(ident("Integer")),
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        // Dynamic body type — no return type warning (can't infer from empty body)
        let return_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("declares return type"))
            .collect();
        assert!(
            return_warnings.is_empty(),
            "empty body should not produce return type mismatch"
        );
    }

    #[test]
    fn test_integer_plus_string_exactly_one_warning() {
        // 42 + "hello" — should produce exactly 1 warning (binary operand check),
        // not 2 (no duplicate from check_argument_types)
        let module = make_module(vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("+".into()),
            vec![str_lit("hello")],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert_eq!(
            checker.diagnostics().len(),
            1,
            "should produce exactly 1 warning (no duplicate), got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_field_assign_typed_mismatch_warns() {
        // state: count: Integer = 0; self.count := "bad" → warning
        let state = vec![StateDeclaration::with_type_and_default(
            ident("count"),
            TypeAnnotation::simple("Integer", span()),
            int_lit(0),
            span(),
        )];
        let method = make_method("badMethod", vec![field_assign("count", str_lit("bad"))]);
        let class = counter_class_with_typed_state(vec![method], state);
        let module = make_module_with_classes(vec![], vec![class]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("Type mismatch"))
            .collect();
        assert_eq!(
            warnings.len(),
            1,
            "Expected 1 type mismatch warning, got: {:?}",
            checker.diagnostics()
        );
        assert!(warnings[0].message.contains("count"));
        assert!(warnings[0].message.contains("Integer"));
        assert!(warnings[0].message.contains("String"));
    }

    #[test]
    fn test_field_assign_typed_match_no_warn() {
        // state: count: Integer = 0; self.count := 42 → no warning
        let state = vec![StateDeclaration::with_type_and_default(
            ident("count"),
            TypeAnnotation::simple("Integer", span()),
            int_lit(0),
            span(),
        )];
        let method = make_method("goodMethod", vec![field_assign("count", int_lit(42))]);
        let class = counter_class_with_typed_state(vec![method], state);
        let module = make_module_with_classes(vec![], vec![class]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "No warnings expected, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_return_type_subtype_compatible() {
        // Method declares -> Number, body returns Integer (subtype) — no warning
        let class_def = ClassDefinition::with_modifiers(
            ident("Calculator"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::with_return_type(
                MessageSelector::Unary("compute".into()),
                vec![],
                vec![bare(int_lit(42))], // Integer is subtype of Number
                TypeAnnotation::Simple(ident("Number")),
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let return_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("declares return type"))
            .collect();
        assert!(
            return_warnings.is_empty(),
            "Integer return should be compatible with Number declaration"
        );
    }

    #[test]
    fn test_field_assign_untyped_no_warn() {
        // state: value = 0; self.value := "anything" → no warning
        let state = vec![StateDeclaration::with_default(
            ident("value"),
            int_lit(0),
            span(),
        )];
        let method = make_method(
            "setAnything",
            vec![field_assign("value", str_lit("anything"))],
        );
        let class = counter_class_with_typed_state(vec![method], state);
        let module = make_module_with_classes(vec![], vec![class]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "Untyped fields should not produce warnings, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_early_return_uses_return_type_not_trailing() {
        // Method with early return: ^ 42 followed by unreachable "oops"
        // Return type checking should use the return expression type (Integer),
        // not the unreachable trailing expression (String)
        let class_def = ClassDefinition::with_modifiers(
            ident("Calculator"),
            Some(ident("Object")),
            false,
            false,
            vec![],
            vec![MethodDefinition::with_return_type(
                MessageSelector::Unary("compute".into()),
                vec![],
                vec![
                    bare(Expression::Return {
                        value: Box::new(int_lit(42)),
                        span: span(),
                    }),
                    bare(str_lit("unreachable")), // would be String, bare(but never reached)
                ],
                TypeAnnotation::Simple(ident("Integer")),
                span(),
            )],
            span(),
        );
        let module = make_module_with_classes(vec![], vec![class_def]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let return_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("declares return type"))
            .collect();
        assert!(
            return_warnings.is_empty(),
            "early return of Integer should match declared Integer, not unreachable String"
        );
    }

    #[test]
    fn test_field_assign_subtype_no_warn() {
        // Integer is assignable to Number (Integer's superclass chain includes Number)
        let state = vec![StateDeclaration::with_type(
            ident("value"),
            TypeAnnotation::simple("Number", span()),
            span(),
        )];
        let method = make_method("setNumber", vec![field_assign("value", int_lit(42))]);
        let class = counter_class_with_typed_state(vec![method], state);
        let module = make_module_with_classes(vec![], vec![class]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "Integer should be assignable to Number, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_state_default_value_mismatch_warns() {
        // state: count: Integer = "bad" → warning at class definition time
        let state = vec![StateDeclaration::with_type_and_default(
            ident("count"),
            TypeAnnotation::simple("Integer", span()),
            str_lit("bad"),
            span(),
        )];
        let class = counter_class_with_typed_state(vec![], state);
        let module = make_module_with_classes(vec![], vec![class]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("Type mismatch"))
            .collect();
        assert_eq!(
            warnings.len(),
            1,
            "Expected 1 type mismatch for default value, got: {:?}",
            checker.diagnostics()
        );
        assert!(warnings[0].message.contains("count"));
    }

    #[test]
    fn test_state_default_value_match_no_warn() {
        // state: count: Integer = 0 → no warning
        let state = vec![StateDeclaration::with_type_and_default(
            ident("count"),
            TypeAnnotation::simple("Integer", span()),
            int_lit(0),
            span(),
        )];
        let class = counter_class_with_typed_state(vec![], state);
        let module = make_module_with_classes(vec![], vec![class]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "Default value matching declared type should not warn, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_dynamic_expression_no_warn() {
        // Assigning a dynamic expression (unknown variable) to a typed field → no warning
        let state = vec![StateDeclaration::with_type(
            ident("count"),
            TypeAnnotation::simple("Integer", span()),
            span(),
        )];
        let method = make_method("setUnknown", vec![field_assign("count", var("unknownVar"))]);
        let class = counter_class_with_typed_state(vec![method], state);
        let module = make_module_with_classes(vec![], vec![class]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "Dynamic expressions should never produce warnings, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn test_union_type_annotation_no_false_positive() {
        // state: value: Integer | String = 42 → no warning (union types skip check)
        let state = vec![StateDeclaration::with_type_and_default(
            ident("value"),
            TypeAnnotation::union(
                vec![
                    TypeAnnotation::simple("Integer", span()),
                    TypeAnnotation::simple("String", span()),
                ],
                span(),
            ),
            int_lit(42),
            span(),
        )];
        let method = make_method("setValue", vec![field_assign("value", str_lit("hello"))]);
        let class = counter_class_with_typed_state(vec![method], state);
        let module = make_module_with_classes(vec![], vec![class]);
        let hierarchy = ClassHierarchy::build(&module).0.unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        assert!(
            checker.diagnostics().is_empty(),
            "Union type annotations should not produce false positives, got: {:?}",
            checker.diagnostics()
        );
    }

    // --- Behaviour protocol / Class hierarchy fallback tests (BT-777) ---

    #[test]
    fn test_behaviour_protocol_superclass_no_warning() {
        // Integer superclass — should NOT warn (superclass is a Behaviour instance method
        // resolved via the Class→Behaviour chain fallback)
        let module = make_module(vec![msg_send(
            class_ref("Integer"),
            MessageSelector::Unary("superclass".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert!(
            dnu_warnings.is_empty(),
            "Integer superclass should produce no warning, got: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_behaviour_protocol_methods_no_warning() {
        // Integer methods — should NOT warn
        let module = make_module(vec![msg_send(
            class_ref("Integer"),
            MessageSelector::Unary("methods".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert!(
            dnu_warnings.is_empty(),
            "Integer methods should produce no warning, got: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_behaviour_protocol_subclasses_no_warning() {
        // Integer subclasses — should NOT warn
        let module = make_module(vec![msg_send(
            class_ref("Integer"),
            MessageSelector::Unary("subclasses".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert!(
            dnu_warnings.is_empty(),
            "Integer subclasses should produce no warning, got: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_behaviour_protocol_all_superclasses_no_warning() {
        // String allSuperclasses — should NOT warn
        let module = make_module(vec![msg_send(
            class_ref("String"),
            MessageSelector::Unary("allSuperclasses".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert!(
            dnu_warnings.is_empty(),
            "String allSuperclasses should produce no warning, got: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_class_protocol_name_no_warning() {
        // Integer name — should NOT warn (name is a Class instance method)
        let module = make_module(vec![msg_send(
            class_ref("Integer"),
            MessageSelector::Unary("name".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert!(
            dnu_warnings.is_empty(),
            "Integer name should produce no warning, got: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_class_protocol_is_class_no_warning() {
        // Integer isClass — should NOT warn
        let module = make_module(vec![msg_send(
            class_ref("Integer"),
            MessageSelector::Unary("isClass".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert!(
            dnu_warnings.is_empty(),
            "Integer isClass should produce no warning, got: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_unknown_class_side_message_still_warns() {
        // Integer bogusClassMethod — SHOULD warn (not in Class chain or instance methods)
        let module = make_module(vec![msg_send(
            class_ref("Integer"),
            MessageSelector::Unary("bogusClassMethod".into()),
            vec![],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert_eq!(
            dnu_warnings.len(),
            1,
            "Integer bogusClassMethod should produce exactly one warning, got: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_behaviour_protocol_can_understand_no_warning() {
        // Integer canUnderstand: #+ — should NOT warn (canUnderstand: is a Behaviour method)
        let module = make_module(vec![msg_send(
            class_ref("Integer"),
            MessageSelector::Keyword(vec![crate::ast::KeywordPart {
                keyword: "canUnderstand:".into(),
                span: span(),
            }]),
            vec![Expression::Literal(Literal::Symbol("+".into()), span())],
        )]);
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let dnu_warnings: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("does not understand"))
            .collect();
        assert!(
            dnu_warnings.is_empty(),
            "Integer canUnderstand: should produce no warning, got: {dnu_warnings:?}"
        );
    }

    #[test]
    fn test_non_self_field_assignment_produces_warning_not_error() {
        // `other.x := 1` should produce a Severity::Warning, not Severity::Error,
        // consistent with the module's "Warnings only, never errors" design principle.
        use crate::source_analysis::Severity;
        let source =
            "Value subclass: Point\n  state: x\n  state: y\n  bad: other =>\n    other.x := 1";
        let tokens = crate::source_analysis::lex_with_eof(source);
        let (module, parse_diags) = crate::source_analysis::parse(tokens);
        assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
        let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
            .0
            .unwrap();
        let mut checker = TypeChecker::new();
        checker.check_module(&module, &hierarchy);
        let field_diags: Vec<_> = checker
            .diagnostics()
            .iter()
            .filter(|d| d.message.contains("Cannot assign"))
            .collect();
        assert_eq!(
            field_diags.len(),
            1,
            "Expected exactly one non-self field assignment diagnostic, got: {field_diags:?}"
        );
        assert_eq!(
            field_diags[0].severity,
            Severity::Warning,
            "Non-self field assignment should be a warning, not an error"
        );
    }
}
