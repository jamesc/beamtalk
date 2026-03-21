// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type inference — walking the AST to determine expression types.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module contains the core inference methods of [`TypeChecker`]:
//! - Module-level type checking orchestration
//! - Expression type inference
//! - Message send type resolution
//! - Literal type mapping

use std::collections::{HashMap, HashSet};

use crate::ast::{
    Expression, ExpressionStatement, Literal, MessageSelector, Module, Pattern, TypeAnnotation,
};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::source_analysis::{Diagnostic, Span};
use ecow::EcoString;

use super::{InferredType, TypeChecker, TypeEnv};

/// Describes a control-flow narrowing detected from a type-test expression.
///
/// When a Boolean-producing expression like `x class = Foo` or `x isNil` is used
/// as the receiver of `ifTrue:`/`ifFalse:`, the type checker narrows the tested
/// variable inside the block scope (ADR 0068 Phase 1g).
///
/// The `respondsTo:` variant (ADR 0068 Phase 2e) narrows to `Dynamic` in the
/// true block, suppressing DNU warnings for the tested selector. If a protocol
/// in the registry requires exactly that selector, protocol conformance can be
/// inferred.
#[derive(Debug, Clone)]
pub(super) struct NarrowingInfo {
    /// The variable name being narrowed.
    pub(super) variable: EcoString,
    /// The type the variable is narrowed to in the *true* branch.
    pub(super) true_type: InferredType,
    /// Whether this is a nil-check (`isNil`). If so, the *false* branch
    /// narrows to non-nil and early-return narrowing applies.
    pub(super) is_nil_check: bool,
    /// The selector tested in a `respondsTo:` narrowing (ADR 0068 Phase 2e).
    ///
    /// When set, the narrowing was detected from `x respondsTo: #selector`
    /// and the variable is narrowed to `Dynamic` in the true block.
    /// Read by tests and reserved for future protocol inference integration
    /// (e.g., matching `responded_selector` to protocol required methods).
    #[allow(dead_code)]
    pub(super) responded_selector: Option<EcoString>,
}

impl TypeChecker {
    /// Checks types in a module using the class hierarchy for method resolution.
    ///
    /// Method bodies are processed first so that inferred return types are
    /// available when type-checking top-level expressions (BT-1047). This
    /// enables single-pass chain resolution: the `TypeChecker` consults its own
    /// `method_return_types` map when the hierarchy has no explicit annotation.
    #[allow(clippy::too_many_lines)] // struct patterns expanded by BT-1569 refactor
    pub fn check_module(&mut self, module: &Module, hierarchy: &ClassHierarchy) {
        let mut env = TypeEnv::new();

        // Check method bodies inside class definitions first, so that inferred
        // return types are available for chain resolution in top-level code.
        for class in &module.classes {
            let is_abstract = class.is_abstract || hierarchy.is_abstract(&class.name.name);

            // Determine if this class requires type annotations (typed modifier or inherited)
            let is_typed = hierarchy.is_typed(&class.name.name);

            for method in &class.methods {
                let mut method_env = TypeEnv::new();
                method_env.set("self", InferredType::known(class.name.name.clone()));
                Self::set_param_types(&mut method_env, &method.parameters);
                let body_type =
                    self.infer_stmts(&method.body, hierarchy, &mut method_env, is_abstract);
                self.check_return_type(method, &body_type, &class.name.name, hierarchy);
                self.check_override_param_compatibility(method, &class.name.name, hierarchy);
                self.check_no_self_in_params(method, &class.name.name);
                if is_typed {
                    self.check_typed_method_annotations(method, &class.name.name);
                }
                if method.return_type.is_none()
                    && !method
                        .body
                        .iter()
                        .any(|s| matches!(s.expression, Expression::Primitive { .. }))
                {
                    if let InferredType::Known {
                        class_name: ref inferred,
                        ..
                    } = body_type
                    {
                        self.method_return_types.insert(
                            (class.name.name.clone(), method.selector.name(), false),
                            inferred.clone(),
                        );
                    }
                }
            }
            for method in &class.class_methods {
                let mut method_env = TypeEnv::new();
                method_env.in_class_method = true;
                method_env.set("self", InferredType::known(class.name.name.clone()));
                Self::set_param_types(&mut method_env, &method.parameters);
                let body_type =
                    self.infer_stmts(&method.body, hierarchy, &mut method_env, is_abstract);
                self.check_return_type(method, &body_type, &class.name.name, hierarchy);
                self.check_no_self_in_params(method, &class.name.name);
                if is_typed {
                    self.check_typed_method_annotations(method, &class.name.name);
                }
                if method.return_type.is_none()
                    && !method
                        .body
                        .iter()
                        .any(|s| matches!(s.expression, Expression::Primitive { .. }))
                {
                    if let InferredType::Known {
                        class_name: ref inferred,
                        ..
                    } = body_type
                    {
                        self.method_return_types.insert(
                            (class.name.name.clone(), method.selector.name(), true),
                            inferred.clone(),
                        );
                    }
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
            method_env.set("self", InferredType::known(class_name.clone()));
            Self::set_param_types(&mut method_env, &standalone.method.parameters);
            let body_type = self.infer_stmts(
                &standalone.method.body,
                hierarchy,
                &mut method_env,
                is_abstract,
            );
            self.check_return_type(&standalone.method, &body_type, class_name, hierarchy);
            self.check_no_self_in_params(&standalone.method, class_name);
            if standalone.method.return_type.is_none()
                && !standalone
                    .method
                    .body
                    .iter()
                    .any(|s| matches!(s.expression, Expression::Primitive { .. }))
            {
                if let InferredType::Known {
                    class_name: ref inferred,
                    ..
                } = body_type
                {
                    self.method_return_types.insert(
                        (
                            class_name.clone(),
                            standalone.method.selector.name(),
                            standalone.is_class_method,
                        ),
                        inferred.clone(),
                    );
                }
            }
        }

        // Check top-level expressions last — method return types are now available.
        self.infer_stmts(&module.expressions, hierarchy, &mut env, false);
    }

    /// Sets parameter types in the type environment from annotations.
    ///
    /// All parameters are always registered. Typed parameters are resolved
    /// via [`resolve_type_annotation`]; untyped parameters are registered as
    /// `Dynamic`. Generic annotations (e.g., `:: Result(Integer, Error)`) are
    /// resolved to `Known` with `type_args`. Type parameters of enclosing
    /// generic classes (e.g., `T` in `Result(T, E)`) resolve to `Dynamic` when
    /// no substitution context is available.
    /// Registering untyped params is necessary to prevent the bare-identifier
    /// state-field fallback in `infer_expr` from mis-inferring an untyped param
    /// as `self.<field>` when the parameter name shadows a state field name.
    pub(super) fn set_param_types(
        env: &mut TypeEnv,
        parameters: &[crate::ast::ParameterDefinition],
    ) {
        for param in parameters {
            let ty = match &param.type_annotation {
                Some(ann) => Self::resolve_type_annotation(ann),
                None => InferredType::Dynamic, // preserve parameter shadowing of state fields
            };
            env.set(&param.name.name, ty);
        }
    }

    /// Resolves a [`TypeAnnotation`] to an [`InferredType`].
    ///
    /// Handles all annotation variants:
    /// - `Simple` → `Known { class_name, .. }` with keyword resolution (`nil` → `UndefinedObject`, etc.)
    /// - `Generic` → `Known { class_name: base, type_args: [resolved...] }`
    /// - `Union` → `Union(resolved_members)`
    /// - `FalseOr` → `Union([inner, False])`
    /// - `SelfType` → `Dynamic` (resolved at call site, not here)
    /// - `Singleton` → `Dynamic` (not yet supported)
    pub(super) fn resolve_type_annotation(ann: &TypeAnnotation) -> InferredType {
        match ann {
            TypeAnnotation::Simple(type_id) => {
                let name = Self::resolve_type_keyword(&type_id.name);
                InferredType::known(name)
            }
            TypeAnnotation::Generic {
                base, parameters, ..
            } => {
                let type_args: Vec<InferredType> = parameters
                    .iter()
                    .map(Self::resolve_type_annotation)
                    .collect();
                InferredType::Known {
                    class_name: base.name.clone(),
                    type_args,
                    provenance: super::TypeProvenance::Declared(ann.span()),
                }
            }
            TypeAnnotation::Union { types, .. } => {
                let members: Vec<InferredType> =
                    types.iter().map(Self::resolve_type_annotation).collect();
                InferredType::union_of(&members)
            }
            TypeAnnotation::FalseOr { inner, .. } => {
                let inner_ty = Self::resolve_type_annotation(inner);
                InferredType::union_of(&[inner_ty, InferredType::known("False")])
            }
            TypeAnnotation::SelfType { .. } | TypeAnnotation::Singleton { .. } => {
                InferredType::Dynamic
            }
        }
    }

    /// Resolves type-position keywords to their class names.
    ///
    /// - `nil` → `UndefinedObject`
    /// - `false` → `False`
    /// - `true` → `True`
    /// - Everything else passes through unchanged.
    fn resolve_type_keyword(name: &EcoString) -> EcoString {
        match name.as_str() {
            "nil" => "UndefinedObject".into(),
            "false" => "False".into(),
            "true" => "True".into(),
            _ => name.clone(),
        }
    }

    /// Converts a type name string (from `ClassHierarchy::state_field_type`)
    /// to a proper `InferredType`, splitting unions on `|`.
    ///
    /// Examples:
    /// - `"Integer"` → `Known("Integer")`
    /// - `"String | nil"` → `Union(["String", "UndefinedObject"])`
    pub(super) fn resolve_type_name_string(type_name: &EcoString) -> InferredType {
        if type_name.contains('|') {
            let members: Vec<InferredType> = type_name
                .split('|')
                .map(|s| {
                    let trimmed = s.trim();
                    InferredType::known(Self::resolve_type_keyword(&EcoString::from(trimmed)))
                })
                .collect();
            InferredType::union_of(&members)
        } else {
            InferredType::known(Self::resolve_type_keyword(type_name))
        }
    }

    /// Infer the type of an expression, emitting diagnostics for invalid sends.
    ///
    /// `in_abstract_method` suppresses warnings for `self` class-side sends in
    /// abstract classes, since subclasses may provide class-side methods.
    #[allow(clippy::too_many_lines)] // one arm per AST variant — irreducible
    pub(super) fn infer_expr(
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
                    "true" | "false" => InferredType::known("Boolean"),
                    "nil" => InferredType::known("UndefinedObject"),
                    "self" => env.get("self").unwrap_or(InferredType::Dynamic),
                    _ => {
                        // First check environment for local variables or parameters
                        if let Some(ty) = env.get(name) {
                            ty
                        } else {
                            // Bare identifier might be implicit self field access
                            // (e.g., `getValue => value` is sugar for `getValue => self.value`)
                            if let Some(InferredType::Known { class_name, .. }) = env.get("self") {
                                if let Some(field_type) =
                                    hierarchy.state_field_type(&class_name, name)
                                {
                                    Self::resolve_type_name_string(&field_type)
                                } else {
                                    InferredType::Dynamic
                                }
                            } else {
                                InferredType::Dynamic
                            }
                        }
                    }
                }
            }

            // Class references are the class itself (class-side receiver)
            Expression::ClassReference { name, .. } => InferredType::known(name.name.clone()),

            // Field access — infer type from declared state type for self.field
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                let mut result = InferredType::Dynamic;
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    if recv_id.name == "self" {
                        if let Some(InferredType::Known { class_name, .. }) = env.get("self") {
                            if let Some(field_type) =
                                hierarchy.state_field_type(&class_name, &field.name)
                            {
                                result = Self::resolve_type_name_string(&field_type);
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
            | Expression::Spread { .. }
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
                            self.diagnostics.push(
                                Diagnostic::warning(
                                    format!(
                                        "Cannot assign to `{recv_name}.{field_name}` — objects cannot mutate another object's state"
                                    ),
                                    *span,
                                )
                                .with_hint(format!(
                                    "Use `{recv_name} := {recv_name} {with_sel} newValue` to get an updated copy"
                                )),
                            );
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
                                &[], // cascade return type is receiver, not send result
                            );
                        }
                    } else if let InferredType::Known { ref class_name, .. } = receiver_ty {
                        if env.in_class_method && Self::is_self_receiver(receiver) {
                            if !in_abstract_method {
                                self.check_class_side_send(
                                    class_name,
                                    &selector_name,
                                    msg.span,
                                    hierarchy,
                                    &[], // cascade return type is receiver, not send result
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
                    } else if let InferredType::Union(ref members) = receiver_ty {
                        // Union cascades: validate selector on all members
                        self.infer_union_message_send(members, &selector_name, msg.span, hierarchy);
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
                InferredType::known("Block")
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
                InferredType::known("Dictionary")
            }

            // List literal → List
            Expression::ListLiteral { elements, tail, .. } => {
                for elem in elements {
                    self.infer_expr(elem, hierarchy, env, in_abstract_method);
                }
                if let Some(t) = tail {
                    self.infer_expr(t, hierarchy, env, in_abstract_method);
                }
                InferredType::known("List")
            }

            // Array literal → Array
            Expression::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    self.infer_expr(elem, hierarchy, env, in_abstract_method);
                }
                InferredType::known("Array")
            }

            // String interpolation → String
            Expression::StringInterpolation { segments, .. } => {
                for seg in segments {
                    if let crate::ast::StringSegment::Interpolation(inner_expr) = seg {
                        self.infer_expr(inner_expr, hierarchy, env, in_abstract_method);
                    }
                }
                InferredType::known("String")
            }

            // Super — resolve to parent class type for method validation
            Expression::Super(_) => {
                // Look up current class from 'self' type, then find parent
                if let Some(InferredType::Known { class_name, .. }) = env.get("self") {
                    if let Some(class_info) = hierarchy.get_class(&class_name) {
                        if let Some(ref parent) = class_info.superclass {
                            InferredType::known(parent.clone())
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

            // Destructure assignment — infer value type, bind pattern variables into TypeEnv
            Expression::DestructureAssignment { pattern, value, .. } => {
                self.infer_expr(value, hierarchy, env, in_abstract_method);
                Self::bind_pattern_vars(pattern, env);
                InferredType::Dynamic
            }
        };

        // Record inferred type for the expression's full span for LSP queries
        self.type_map.insert(expr.span(), ty.clone());
        ty
    }

    /// Bind all named variables in a destructuring `pattern` into `env` as `Dynamic`.
    ///
    /// Delegates to [`crate::semantic_analysis::extract_pattern_bindings`] to walk
    /// all pattern variants (including `Binary` segments) consistently. Wildcards
    /// and literals are skipped; duplicates are silently ignored (the name resolver
    /// already reports them as errors earlier in the pipeline).
    pub(super) fn bind_pattern_vars(pattern: &Pattern, env: &mut TypeEnv) {
        let (bindings, _diagnostics) = crate::semantic_analysis::extract_pattern_bindings(pattern);
        for id in bindings {
            env.set(&id.name, InferredType::Dynamic);
        }
    }

    /// Infer the type of a message send and validate the selector.
    #[allow(clippy::too_many_arguments)] // hierarchy + env + flag needed for recursive checking
    #[allow(clippy::too_many_lines)] // generic substitution adds necessary branches
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

        // Control-flow narrowing (ADR 0068 Phase 1g):
        // When the selector is ifTrue:, ifFalse:, or ifTrue:ifFalse:, detect
        // type-testing patterns in the receiver and narrow variable types inside
        // block arguments.
        let narrowing = if matches!(
            selector_name.as_str(),
            "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:"
        ) {
            Self::detect_narrowing(receiver)
        } else {
            None
        };

        // Infer argument types, applying narrowing to block arguments when detected.
        let arg_types: Vec<InferredType> = if let Some(ref info) = narrowing {
            self.infer_args_with_narrowing(
                arguments,
                &selector_name,
                info,
                hierarchy,
                env,
                in_abstract_method,
            )
        } else {
            arguments
                .iter()
                .map(|arg| self.infer_expr(arg, hierarchy, env, in_abstract_method))
                .collect()
        };

        // Handle asType: compile-time type assertion (ADR 0025 Phase 2b)
        // `expr asType: SomeClass` asserts expr is SomeClass, returns Known(SomeClass)
        if selector_name == "asType:" {
            if let Some(Expression::ClassReference { name, .. }) = arguments.first() {
                return InferredType::known(name.name.clone());
            }
            return receiver_ty;
        }

        // Validate binary operand types when both sides are known
        // Only check if the receiver type actually defines the operator (avoids
        // duplicate warnings when the selector is already unknown).
        if let MessageSelector::Binary(op) = selector {
            if let (
                InferredType::Known {
                    class_name: recv_ty,
                    ..
                },
                Some(InferredType::Known {
                    class_name: arg_ty, ..
                }),
            ) = (&receiver_ty, arg_types.first())
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
            return self.check_class_side_send(
                class_name,
                &selector_name,
                span,
                hierarchy,
                &arg_types,
            );
        }

        // For instance-side sends on known types
        if let InferredType::Known {
            ref class_name,
            ref type_args,
            ..
        } = receiver_ty
        {
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
                    return self.check_class_side_send(
                        class_name,
                        &selector_name,
                        span,
                        hierarchy,
                        &arg_types,
                    );
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
                    // `Self` resolves to the static receiver class (with type args)
                    if ret_ty.as_str() == "Self" {
                        if type_args.is_empty() {
                            return InferredType::known(class_name.clone());
                        }
                        return InferredType::Known {
                            class_name: class_name.clone(),
                            type_args: type_args.clone(),
                            provenance: super::TypeProvenance::Substituted(
                                crate::source_analysis::Span::default(),
                            ),
                        };
                    }

                    // Build substitution map, composing through inheritance chain
                    // if the method is inherited (ADR 0068 Phase 1b, BT-1577)
                    let subst = Self::build_inherited_substitution_map(
                        hierarchy,
                        class_name,
                        type_args,
                        &method.defined_in,
                    );

                    // Apply generic substitution if we have type args
                    if !subst.is_empty() {
                        let method_subst = Self::infer_method_local_params(
                            &method,
                            &arg_types,
                            &subst,
                            hierarchy,
                            &method.defined_in,
                        );
                        return Self::substitute_return_type(ret_ty, &subst, &method_subst);
                    }

                    // BT-1576: If the return type is a generic like "Array(R)",
                    // extract the base class name so completion/chain resolution works.
                    if let Some(open) = ret_ty.find('(') {
                        return InferredType::known(EcoString::from(&ret_ty[..open]));
                    }
                    return InferredType::known(ret_ty.clone());
                }
            }

            // BT-1047: Fall back to return types inferred earlier in this same pass.
            // Method bodies are processed before top-level expressions, so inferred
            // return types are available for chain resolution without a second pass.
            let key = (class_name.clone(), selector_name.clone(), false);
            if let Some(ret_ty) = self.method_return_types.get(&key) {
                return InferredType::known(ret_ty.clone());
            }
        }

        // Union-typed receiver: check selector on ALL members, warn if any lacks it.
        // Return type is the union of member return types.
        if let InferredType::Union(ref members) = receiver_ty {
            return self.infer_union_message_send(members, &selector_name, span, hierarchy);
        }

        InferredType::Dynamic
    }

    /// Returns true if the expression is `self` (direct identifier reference).
    fn is_self_receiver(expr: &Expression) -> bool {
        matches!(expr, Expression::Identifier(ident) if ident.name == "self")
    }

    /// Infer the result type of a message send on a union-typed receiver.
    ///
    /// Checks the selector against ALL union members. If any member lacks the
    /// selector, emits a warning with a nullable hint when `UndefinedObject` is
    /// the missing member. The return type is the union of member return types
    /// (simplified to a single type if all agree).
    fn infer_union_message_send(
        &mut self,
        members: &[EcoString],
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        let mut missing_members: Vec<&EcoString> = Vec::new();
        let mut return_types: Vec<InferredType> = Vec::new();

        for member in members {
            if !hierarchy.has_class(member) {
                // Unknown class in union — go Dynamic for this member
                return_types.push(InferredType::Dynamic);
                continue;
            }
            if hierarchy.has_instance_dnu_override(member) {
                // DNU override accepts any message
                return_types.push(InferredType::Dynamic);
                continue;
            }
            if hierarchy.resolves_selector(member, selector) {
                // Selector resolves — compute return type
                if let Some(method) = hierarchy.find_method(member, selector) {
                    if let Some(ref ret_ty) = method.return_type {
                        let resolved = if ret_ty.as_str() == "Self" {
                            member.clone()
                        } else {
                            ret_ty.clone()
                        };
                        return_types.push(InferredType::known(resolved));
                    } else {
                        return_types.push(InferredType::Dynamic);
                    }
                } else {
                    return_types.push(InferredType::Dynamic);
                }
            } else {
                missing_members.push(member);
                // Still compute a Dynamic return for this member
                return_types.push(InferredType::Dynamic);
            }
        }

        if !missing_members.is_empty() {
            let union_display = members.join(" | ");
            let missing_display: Vec<&str> = missing_members.iter().map(|m| m.as_str()).collect();
            let is_nullable = missing_members
                .iter()
                .any(|m| m.as_str() == "UndefinedObject");

            let message = if missing_members.len() == 1 {
                format!(
                    "{} does not understand '{selector}' (in union {union_display})",
                    missing_display[0]
                )
            } else {
                format!(
                    "{} do not understand '{selector}' (in union {union_display})",
                    missing_display.join(", ")
                )
            };

            let mut diag = Diagnostic::hint(message, span)
                .with_category(crate::source_analysis::DiagnosticCategory::Dnu);
            if is_nullable {
                diag = diag.with_hint(format!("Check for nil before sending '{selector}'"));
            }
            self.diagnostics.push(diag);
        }

        InferredType::union_of(&return_types)
    }

    /// Detect control-flow narrowing from the receiver of `ifTrue:`/`ifFalse:`.
    ///
    /// Recognises a fixed set of type-testing patterns (ADR 0068 Phase 1g):
    ///
    /// | Pattern | Detected as |
    /// |---|---|
    /// | `x class = ClassName` / `x class =:= ClassName` | class identity check |
    /// | `(x class) = ClassName` / `(x class) =:= ClassName` | class identity check (parens) |
    /// | `x isKindOf: ClassName` | kind check |
    /// | `x isNil` | nil check |
    pub(super) fn detect_narrowing(receiver: &Expression) -> Option<NarrowingInfo> {
        // Unwrap parentheses
        let receiver = match receiver {
            Expression::Parenthesized { expression, .. } => expression.as_ref(),
            _ => receiver,
        };

        match receiver {
            // Pattern: `x isNil`
            Expression::MessageSend {
                receiver: inner_recv,
                selector: MessageSelector::Unary(sel),
                ..
            } if sel.as_str() == "isNil" => {
                let var_name = Self::extract_variable_name(inner_recv)?;
                Some(NarrowingInfo {
                    variable: var_name,
                    true_type: InferredType::known("UndefinedObject"),
                    is_nil_check: true,
                    responded_selector: None,
                })
            }

            // Pattern: `x respondsTo: #selector` (ADR 0068 Phase 2e)
            Expression::MessageSend {
                receiver: inner_recv,
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } if parts.len() == 1 && parts[0].keyword == "respondsTo:" => {
                let var_name = Self::extract_variable_name(inner_recv)?;
                // Extract the selector name from a symbol literal argument (#selector)
                if let Some(Expression::Literal(Literal::Symbol(sel_name), _)) = arguments.first() {
                    Some(NarrowingInfo {
                        variable: var_name,
                        // Narrow to Dynamic — we know the object responds to the selector,
                        // but not its concrete class. Dynamic suppresses DNU warnings.
                        true_type: InferredType::Dynamic,
                        is_nil_check: false,
                        responded_selector: Some(sel_name.clone()),
                    })
                } else {
                    None
                }
            }

            // Pattern: `x isKindOf: ClassName`
            Expression::MessageSend {
                receiver: inner_recv,
                selector: MessageSelector::Keyword(parts),
                arguments,
                ..
            } if parts.len() == 1 && parts[0].keyword == "isKindOf:" => {
                let var_name = Self::extract_variable_name(inner_recv)?;
                if let Some(Expression::ClassReference { name, .. }) = arguments.first() {
                    Some(NarrowingInfo {
                        variable: var_name,
                        true_type: InferredType::known(name.name.clone()),
                        is_nil_check: false,
                        responded_selector: None,
                    })
                } else {
                    None
                }
            }

            // Pattern: `x class = ClassName` or `x class =:= ClassName`
            // Also handles `(x class) = ClassName` via parenthesized unwrap
            Expression::MessageSend {
                receiver: inner_recv,
                selector: MessageSelector::Binary(op),
                arguments,
                ..
            } if op.as_str() == "=" || op.as_str() == "=:=" => {
                // The inner receiver should be `x class` or `(x class)`
                let class_send = match inner_recv.as_ref() {
                    Expression::Parenthesized { expression, .. } => expression.as_ref(),
                    other => other,
                };
                if let Expression::MessageSend {
                    receiver: var_expr,
                    selector: MessageSelector::Unary(sel),
                    ..
                } = class_send
                {
                    if sel.as_str() == "class" {
                        let var_name = Self::extract_variable_name(var_expr)?;
                        if let Some(Expression::ClassReference { name, .. }) = arguments.first() {
                            return Some(NarrowingInfo {
                                variable: var_name,
                                true_type: InferredType::known(name.name.clone()),
                                is_nil_check: false,
                                responded_selector: None,
                            });
                        }
                    }
                }
                None
            }

            _ => None,
        }
    }

    /// Infer argument types for `ifTrue:` / `ifFalse:` / `ifTrue:ifFalse:` with
    /// narrowed type environments for block arguments.
    ///
    /// For `ifTrue:`, the true-block gets the narrowed type.
    /// For `ifFalse:`, the false-block gets the complement (non-nil for nil checks).
    /// For `ifTrue:ifFalse:`, both blocks get their respective narrowings.
    fn infer_args_with_narrowing(
        &mut self,
        arguments: &[Expression],
        selector_name: &str,
        info: &NarrowingInfo,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        let mut arg_types = Vec::new();

        match selector_name {
            "ifTrue:" => {
                // Single argument: narrow in the true branch
                if let Some(arg) = arguments.first() {
                    let ty = self.infer_block_with_narrowing(
                        arg,
                        &info.variable,
                        &info.true_type,
                        hierarchy,
                        env,
                        in_abstract_method,
                    );
                    arg_types.push(ty);
                }
            }
            "ifFalse:" => {
                // Single argument: narrow in the false branch (complement)
                if let Some(arg) = arguments.first() {
                    if info.is_nil_check {
                        // isNil ifFalse: → variable is non-nil
                        let current_ty = env.get(&info.variable).unwrap_or(InferredType::Dynamic);
                        let non_nil = Self::non_nil_type(&current_ty);
                        let ty = self.infer_block_with_narrowing(
                            arg,
                            &info.variable,
                            &non_nil,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else {
                        // class = / isKindOf: ifFalse: → no useful narrowing
                        let ty = self.infer_expr(arg, hierarchy, env, in_abstract_method);
                        arg_types.push(ty);
                    }
                }
            }
            "ifTrue:ifFalse:" => {
                // Two arguments: true block then false block
                if let Some(true_arg) = arguments.first() {
                    let ty = self.infer_block_with_narrowing(
                        true_arg,
                        &info.variable,
                        &info.true_type,
                        hierarchy,
                        env,
                        in_abstract_method,
                    );
                    arg_types.push(ty);
                }
                if let Some(false_arg) = arguments.get(1) {
                    if info.is_nil_check {
                        // isNil ifTrue: [...] ifFalse: [block] → non-nil in false block
                        let current_ty = env.get(&info.variable).unwrap_or(InferredType::Dynamic);
                        let non_nil = Self::non_nil_type(&current_ty);
                        let ty = self.infer_block_with_narrowing(
                            false_arg,
                            &info.variable,
                            &non_nil,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else {
                        // class = / isKindOf: ifTrue: [...] ifFalse: [...] — no useful narrowing for false block
                        let ty = self.infer_expr(false_arg, hierarchy, env, in_abstract_method);
                        arg_types.push(ty);
                    }
                }
                // Handle any remaining arguments (shouldn't happen, but be safe)
                for arg in arguments.iter().skip(2) {
                    arg_types.push(self.infer_expr(arg, hierarchy, env, in_abstract_method));
                }
            }
            _ => {
                // Fallback: no narrowing
                for arg in arguments {
                    arg_types.push(self.infer_expr(arg, hierarchy, env, in_abstract_method));
                }
            }
        }

        arg_types
    }

    /// Type-check a block expression (or any expression) with a variable narrowed
    /// to a specific type in a child environment.
    fn infer_block_with_narrowing(
        &mut self,
        arg: &Expression,
        var_name: &str,
        narrowed_type: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        if let Expression::Block(block) = arg {
            let mut block_env = env.child();
            block_env.set(var_name, narrowed_type.clone());
            for param in &block.parameters {
                block_env.set(param.name.as_str(), InferredType::Dynamic);
            }
            self.infer_stmts(&block.body, hierarchy, &mut block_env, in_abstract_method);
            let ty = InferredType::known("Block");
            self.type_map.insert(arg.span(), ty.clone());
            ty
        } else {
            // Not a block literal — just infer normally
            self.infer_expr(arg, hierarchy, env, in_abstract_method)
        }
    }

    /// Extract a variable name from an expression, supporting identifiers
    /// and parenthesized identifiers.
    fn extract_variable_name(expr: &Expression) -> Option<EcoString> {
        match expr {
            Expression::Identifier(ident) => Some(ident.name.clone()),
            Expression::Parenthesized { expression, .. } => Self::extract_variable_name(expression),
            _ => None,
        }
    }

    /// Check whether a block contains a non-local return (`^`).
    fn block_has_return(block: &crate::ast::Block) -> bool {
        block
            .body
            .iter()
            .any(|stmt| matches!(stmt.expression, Expression::Return { .. }))
    }

    /// Remove `UndefinedObject` (nil) from a union type or convert a known type
    /// to itself if it is non-nil.
    pub(super) fn non_nil_type(ty: &InferredType) -> InferredType {
        match ty {
            InferredType::Union(members) => {
                let non_nil: Vec<EcoString> = members
                    .iter()
                    .filter(|m| m.as_str() != "UndefinedObject")
                    .cloned()
                    .collect();
                match non_nil.len() {
                    0 => InferredType::Dynamic,
                    1 => InferredType::known(non_nil.into_iter().next().unwrap()),
                    _ => InferredType::Union(non_nil),
                }
            }
            // If the variable is not a union, narrowing away nil for a non-union
            // type means it stays the same (we can't make it "more non-nil").
            _ => ty.clone(),
        }
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

            // Early-return narrowing (ADR 0068 Phase 1g):
            // After `x isNil ifTrue: [^...]`, narrow x to non-nil for the rest.
            Self::apply_early_return_narrowing(expr, env);

            if matches!(expr, Expression::Return { .. }) {
                break;
            }
        }

        body_type
    }

    /// Apply early-return narrowing to the environment after a statement.
    ///
    /// Detects `x isNil ifTrue: [^...]` — if the true-block contains a non-local
    /// return, the variable must be non-nil in subsequent statements (because if
    /// it were nil, we would have already returned).
    fn apply_early_return_narrowing(expr: &Expression, env: &mut TypeEnv) {
        // Match: `<receiver> ifTrue: [block with ^]`
        if let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        {
            // Only applies to `ifTrue:` (single block with early return)
            if !(parts.len() == 1 && parts[0].keyword == "ifTrue:") {
                return;
            }
            if let Some(info) = Self::detect_narrowing(receiver) {
                if !info.is_nil_check {
                    return;
                }
                // Check if the block contains a non-local return
                if let Some(Expression::Block(block)) = arguments.first() {
                    if Self::block_has_return(block) {
                        // After this statement, the variable is non-nil
                        let current_ty = env.get(&info.variable).unwrap_or(InferredType::Dynamic);
                        let non_nil = Self::non_nil_type(&current_ty);
                        env.set(&info.variable, non_nil);
                    }
                }
            }
        }
    }

    /// Build a substitution map from a class's type parameters and concrete type arguments.
    ///
    /// Given a class like `Result(T, E)` and concrete args `[Integer, IOError]`,
    /// builds `{T → Integer, E → IOError}`.
    ///
    /// Returns an empty map if the class has no type params or the args are empty.
    fn build_substitution_map(
        hierarchy: &ClassHierarchy,
        class_name: &str,
        type_args: &[InferredType],
    ) -> HashMap<EcoString, InferredType> {
        let mut map = HashMap::new();
        if type_args.is_empty() {
            return map;
        }
        if let Some(class_info) = hierarchy.get_class(class_name) {
            for (param, arg) in class_info.type_params.iter().zip(type_args.iter()) {
                map.insert(param.clone(), arg.clone());
            }
        }
        map
    }

    /// Build a substitution map that composes through the inheritance chain.
    ///
    /// When a method is inherited from a superclass, the type parameter names in
    /// the method signature refer to the superclass's type params, not the receiver's.
    /// This method walks from `receiver_class` up to `method_class`, composing
    /// the `superclass_type_args` at each level to produce a substitution map
    /// whose keys are the `method_class`'s type params.
    ///
    /// Example: `Collection(E) subclass: Array(E)` with receiver `Array(Integer)`:
    /// - Array's `type_args`: `[Integer]` produces Array's subst: `{E → Integer}`
    /// - Array's `superclass_type_args`: `[ParamRef(0)]` produces Collection's args: `[Integer]`
    /// - Collection's subst: `{E → Integer}` (returned)
    ///
    /// Falls back to `build_substitution_map(receiver_class, type_args)` when
    /// `method_class == receiver_class` (no inheritance to compose through).
    ///
    /// **References:** ADR 0068 Challenge 4 (BT-1577)
    fn build_inherited_substitution_map(
        hierarchy: &ClassHierarchy,
        receiver_class: &str,
        receiver_type_args: &[InferredType],
        method_class: &str,
    ) -> HashMap<EcoString, InferredType> {
        use crate::semantic_analysis::class_hierarchy::SuperclassTypeArg;

        // Fast path: method is defined on the receiver class itself
        if receiver_class == method_class {
            return Self::build_substitution_map(hierarchy, receiver_class, receiver_type_args);
        }

        // Check if there's anything to compose — either receiver has type args,
        // or some class in the chain has concrete superclass_type_args.
        if receiver_type_args.is_empty() {
            // Even without receiver type args, a non-generic class may have
            // concrete superclass_type_args (e.g., IntArray extends Collection(Integer)).
            let has_concrete_super_args = hierarchy
                .get_class(receiver_class)
                .is_some_and(|info| !info.superclass_type_args.is_empty());
            if !has_concrete_super_args {
                return HashMap::new();
            }
        }

        // Walk the inheritance chain from receiver to method_class,
        // composing type args at each level.
        let mut current_class = receiver_class.to_string();
        let mut current_args = receiver_type_args.to_vec();
        let mut visited = std::collections::HashSet::new();

        while current_class != method_class {
            if !visited.insert(current_class.clone()) {
                break; // cycle guard
            }

            let Some(info) = hierarchy.get_class(&current_class) else {
                break;
            };

            let Some(ref superclass) = info.superclass else {
                break;
            };

            if info.superclass_type_args.is_empty() {
                // No type arg mapping at this level.
                // Try direct name-based matching as fallback for same-named params
                // (e.g., when both child and parent use `E` but no explicit mapping).
                let receiver_subst =
                    Self::build_substitution_map(hierarchy, receiver_class, receiver_type_args);
                if !receiver_subst.is_empty() {
                    if let Some(method_info) = hierarchy.get_class(method_class) {
                        let mut result = HashMap::new();
                        for param in &method_info.type_params {
                            if let Some(val) = receiver_subst.get(param) {
                                result.insert(param.clone(), val.clone());
                            }
                        }
                        if !result.is_empty() {
                            return result;
                        }
                    }
                }
                // Continue walking up — a higher ancestor might have type args
                current_class = superclass.to_string();
                continue;
            }

            // Compose: resolve each superclass_type_arg using current_args
            let current_subst =
                Self::build_substitution_map(hierarchy, &current_class, &current_args);
            let mut super_args = Vec::new();
            for sta in &info.superclass_type_args {
                match sta {
                    SuperclassTypeArg::ParamRef { param_index } => {
                        if let Some(arg) = current_args.get(*param_index) {
                            super_args.push(arg.clone());
                        } else {
                            super_args.push(InferredType::Dynamic);
                        }
                    }
                    SuperclassTypeArg::Concrete { type_name } => {
                        let eco_name: EcoString = type_name.clone();
                        if let Some(resolved) = current_subst.get(&eco_name) {
                            super_args.push(resolved.clone());
                        } else {
                            super_args.push(InferredType::known(eco_name));
                        }
                    }
                }
            }

            current_class = superclass.to_string();
            current_args = super_args;
        }

        // Build the final substitution map for the method's defining class
        Self::build_substitution_map(hierarchy, method_class, &current_args)
    }

    /// Substitute type parameters in a return type string using the substitution map.
    ///
    /// Handles simple cases like `T` → `Integer` and generic return types like
    /// `Result(R, E)` where each parameter is individually substituted.
    fn substitute_return_type(
        ret_ty: &str,
        subst: &HashMap<EcoString, InferredType>,
        method_local_subst: &HashMap<EcoString, InferredType>,
    ) -> InferredType {
        let ret_eco: EcoString = ret_ty.into();

        // Check method-local params first (e.g., R in map:)
        if let Some(resolved) = method_local_subst.get(&ret_eco) {
            return resolved.clone();
        }

        // Check class-level params (e.g., T, E)
        if let Some(resolved) = subst.get(&ret_eco) {
            return resolved.clone();
        }

        // Check for generic return type like "Result(R, E)"
        if let Some(open) = ret_ty.find('(') {
            let base = &ret_ty[..open];
            let inner = &ret_ty[open + 1..ret_ty.len() - 1]; // strip parens
            let params = Self::split_type_params(inner);
            let mut resolved_args = Vec::new();
            for p in &params {
                let p_eco: EcoString = (*p).into();
                if let Some(resolved) = method_local_subst.get(&p_eco) {
                    resolved_args.push(resolved.clone());
                } else if let Some(resolved) = subst.get(&p_eco) {
                    resolved_args.push(resolved.clone());
                } else {
                    // Recursively substitute nested generics
                    resolved_args.push(Self::substitute_return_type(p, subst, method_local_subst));
                }
            }
            return InferredType::Known {
                class_name: base.into(),
                type_args: resolved_args,
                provenance: super::TypeProvenance::Substituted(Span::default()),
            };
        }

        // Not a type param — return as-is
        InferredType::known(ret_eco)
    }

    /// Split a comma-separated list of type parameters, respecting nested parentheses.
    ///
    /// `"T, E"` → `["T", "E"]`
    /// `"GenResult(A, B), E"` → `["GenResult(A, B)", "E"]`
    fn split_type_params(s: &str) -> Vec<&str> {
        let mut result = Vec::new();
        let mut depth = 0;
        let mut start = 0;
        for (i, c) in s.char_indices() {
            match c {
                '(' => depth += 1,
                ')' => depth -= 1,
                ',' if depth == 0 => {
                    result.push(s[start..i].trim());
                    start = i + 1;
                }
                _ => {}
            }
        }
        let last = s[start..].trim();
        if !last.is_empty() {
            result.push(last);
        }
        result
    }

    /// Infer method-local type parameters (like `R` in `map:`) from call-site arguments.
    ///
    /// When a method parameter is annotated as `Block(T, R)`, and `T` is already known
    /// from the class substitution, the block's actual return type can solve for `R`.
    fn infer_method_local_params(
        method: &crate::semantic_analysis::class_hierarchy::MethodInfo,
        arg_types: &[InferredType],
        _class_subst: &HashMap<EcoString, InferredType>,
        hierarchy: &ClassHierarchy,
        class_name: &str,
    ) -> HashMap<EcoString, InferredType> {
        let mut method_subst = HashMap::new();

        // Identify which single-letter uppercase identifiers in param/return types
        // are NOT class-level type params — those are method-local.
        let class_type_params: HashSet<&EcoString> =
            if let Some(info) = hierarchy.get_class(class_name) {
                info.type_params.iter().collect()
            } else {
                HashSet::new()
            };

        for (i, param_type_opt) in method.param_types.iter().enumerate() {
            let Some(param_type) = param_type_opt else {
                continue;
            };
            let Some(arg_ty) = arg_types.get(i) else {
                continue;
            };

            // Handle Block(A, B, ..., R) parameter types
            if param_type.starts_with("Block(") && param_type.ends_with(')') {
                let inner = &param_type[6..param_type.len() - 1];
                let block_params = Self::split_type_params(inner);
                if let Some(last) = block_params.last() {
                    let last_eco: EcoString = (*last).into();
                    // If last param is not a class-level type param and not a known class,
                    // it's a method-local type param (like R)
                    if !class_type_params.contains(&last_eco) && !hierarchy.has_class(&last_eco) {
                        // Try to infer R from the block argument's actual return type
                        // For now, if the arg is a Block expression, infer from its body
                        // This is a simplified version — blocks are Dynamic unless annotated
                        if let InferredType::Known {
                            class_name,
                            type_args,
                            ..
                        } = arg_ty
                        {
                            if class_name.as_str() == "Block" && !type_args.is_empty() {
                                // Block type_args: last is return type
                                if let Some(ret) = type_args.last() {
                                    method_subst.insert(last_eco, ret.clone());
                                }
                            }
                        }
                    }
                }
            }
        }

        method_subst
    }

    /// Infer the type of a literal value.
    pub(super) fn infer_literal(lit: &Literal) -> InferredType {
        match lit {
            Literal::Integer(_) => InferredType::known("Integer"),
            Literal::Float(_) => InferredType::known("Float"),
            Literal::String(_) => InferredType::known("String"),
            Literal::Symbol(_) => InferredType::known("Symbol"),
            Literal::Character(_) => InferredType::known("Character"),
            Literal::List(_) => InferredType::known("List"),
        }
    }
}
