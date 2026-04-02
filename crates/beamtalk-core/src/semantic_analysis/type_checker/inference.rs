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
use ecow::{EcoString, eco_format};

use super::{InferredType, TypeChecker, TypeEnv};

/// Describes a control-flow narrowing detected from a type-test expression.
///
/// When a Boolean-producing expression like `x class = Foo` or `x isNil` is used
/// as the receiver of `ifTrue:`/`ifFalse:`, the type checker narrows the tested
/// variable inside the block scope (ADR 0068 Phase 1g).
///
/// The `respondsTo:` variant (ADR 0068 Phase 2e) initially narrows to `Dynamic`,
/// then `refine_responds_to_narrowing` consults the protocol registry: if exactly
/// one protocol requires the tested selector, the type is refined to that
/// protocol (BT-1833). Multiple or zero matches fall back to `Dynamic`.
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
    /// When set, the narrowing was detected from `x respondsTo: #selector`.
    /// Used by `refine_responds_to_narrowing` to look up the matching
    /// protocol in the registry and narrow to that protocol type instead
    /// of `Dynamic` (BT-1833).
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

            // Warn about typed state fields with no default that aren't nilable
            self.check_uninitialized_state(class);
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
    /// - `Singleton` → `Known("#name")` (singleton type, compatible with Symbol)
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
            TypeAnnotation::SelfType { .. } => InferredType::Dynamic,
            TypeAnnotation::Singleton { name, .. } => InferredType::known(eco_format!("#{name}")),
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
                        // BT-1588: Track type origin for generic type params
                        if let Some(origin) = Self::describe_type_origin(value, &ty, hierarchy, env)
                        {
                            env.set_with_origin(
                                ident.name.as_str(),
                                ty.clone(),
                                origin.0,
                                origin.1,
                            );
                        } else {
                            env.set(ident.name.as_str(), ty.clone());
                        }
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
                    } else if let InferredType::Union { ref members, .. } = receiver_ty {
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

        // Record inferred type for the expression's full span for LSP queries.
        // Skip Dynamic — it carries no useful info and all callers treat
        // `None` and `Some(Dynamic)` equivalently. Avoiding the insert saves
        // both a clone and a HashMap insertion for every unresolved expression.
        if !matches!(ty, InferredType::Dynamic) {
            self.type_map.insert(expr.span(), ty.clone());
        }
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
            Self::detect_narrowing(receiver).map(|info| self.refine_responds_to_narrowing(info))
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
                    // BT-1588: Collect origin info for the argument expression
                    let arg_origin = arguments.first().and_then(|arg_expr| {
                        if let Expression::Identifier(ident) = arg_expr {
                            env.get_origin(&ident.name)
                                .map(|o| (o.description.clone(), Some(o.span)))
                        } else {
                            None
                        }
                    });
                    self.check_binary_operand_types(
                        recv_ty,
                        op,
                        arg_ty,
                        span,
                        hierarchy,
                        arg_origin.as_ref(),
                    );
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
                Some(arguments),
                Some(env),
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
                        Some(arguments),
                        Some(env),
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
                    Some(arguments),
                    Some(env),
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

                    // Infer method-local type params from arguments (works for
                    // any parametric param type: Block, Result, Array, etc.)
                    let method_subst = Self::infer_method_local_params(
                        &method,
                        &arg_types,
                        &subst,
                        hierarchy,
                        &method.defined_in,
                    );

                    // Apply generic substitution if we have type args or method-local params
                    if !subst.is_empty() || !method_subst.is_empty() {
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
        if let InferredType::Union { ref members, .. } = receiver_ty {
            return self.infer_union_message_send(members, &selector_name, span, hierarchy);
        }

        InferredType::Dynamic
    }

    /// Returns true if the expression is `self` (direct identifier reference).
    fn is_self_receiver(expr: &Expression) -> bool {
        matches!(expr, Expression::Identifier(ident) if ident.name == "self")
    }

    /// Describes where an expression's type originated (BT-1588).
    ///
    /// Returns `Some((description, span))` when the value expression is a
    /// message send that returns a generic type parameter (e.g., `V` from
    /// `Dictionary at:ifAbsent:`). Returns `None` when origin tracking
    /// isn't useful (concrete types, Dynamic, etc.).
    fn describe_type_origin(
        value: &Expression,
        ty: &super::InferredType,
        hierarchy: &ClassHierarchy,
        env: &super::TypeEnv,
    ) -> Option<(EcoString, crate::source_analysis::Span)> {
        // Only track origin for generic type params (single uppercase letter)
        let type_name = ty.as_known()?;
        if !super::is_generic_type_param(type_name) {
            return None;
        }

        // Extract the message send details
        if let Expression::MessageSend {
            receiver,
            selector,
            span,
            ..
        } = value
        {
            let selector_name = selector.name();
            // Try to get the receiver's type name for context
            let receiver_type = match receiver.as_ref() {
                Expression::Identifier(ident) => env
                    .get(&ident.name)
                    .and_then(|t| t.as_known().cloned())
                    .unwrap_or_else(|| ident.name.clone()),
                Expression::ClassReference { name, .. } => name.name.clone(),
                _ => {
                    // For other expressions, try to find a class name from hierarchy
                    EcoString::from("receiver")
                }
            };

            let desc = if hierarchy.has_class(&receiver_type) {
                EcoString::from(format!(
                    "`{receiver_type} {selector_name}` returns generic type `{type_name}`"
                ))
            } else {
                EcoString::from(format!(
                    "`{selector_name}` returns generic type `{type_name}`"
                ))
            };

            return Some((desc, *span));
        }

        None
    }

    /// Infer the result type of a message send on a union-typed receiver.
    ///
    /// Checks the selector against ALL union members. If any member lacks the
    /// selector, emits a warning with a nullable hint when `UndefinedObject` is
    /// the missing member. The return type is the union of member return types
    /// (simplified to a single type if all agree).
    fn infer_union_message_send(
        &mut self,
        members: &[InferredType],
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        let mut missing_names: Vec<EcoString> = Vec::new();
        let mut return_types: Vec<InferredType> = Vec::new();

        for member in members {
            let Some(member_name) = member.as_known() else {
                return_types.push(InferredType::Dynamic);
                continue;
            };
            if !hierarchy.has_class(member_name) {
                return_types.push(InferredType::Dynamic);
                continue;
            }
            if hierarchy.has_instance_dnu_override(member_name) {
                return_types.push(InferredType::Dynamic);
                continue;
            }
            if hierarchy.resolves_selector(member_name, selector) {
                if let Some(method) = hierarchy.find_method(member_name, selector) {
                    if let Some(ref ret_ty) = method.return_type {
                        let resolved = if ret_ty.as_str() == "Self" {
                            member_name.clone()
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
                missing_names.push(member_name.clone());
                return_types.push(InferredType::Dynamic);
            }
        }

        if !missing_names.is_empty() {
            let member_names: Vec<String> = members
                .iter()
                .filter_map(|m| m.display_name().map(|n| n.to_string()))
                .collect();
            let union_display = member_names.join(" | ");
            let missing_display: Vec<&str> = missing_names.iter().map(EcoString::as_str).collect();
            let is_nullable = missing_names
                .iter()
                .any(|m| m.as_str() == "UndefinedObject");

            let message = if missing_names.len() == 1 {
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

    /// Refines a `respondsTo:` narrowing from `Dynamic` to a protocol type
    /// when the protocol registry is available and exactly one protocol
    /// requires the tested selector (ADR 0068 Phase 2e, BT-1833).
    ///
    /// If no protocol registry is set, or zero/multiple protocols match the
    /// selector, the narrowing is returned unchanged (stays `Dynamic`).
    fn refine_responds_to_narrowing(&self, mut info: NarrowingInfo) -> NarrowingInfo {
        // Only refine to a protocol type when the variable is currently Dynamic.
        // If the variable already has a concrete type (e.g., Integer), narrowing
        // to a protocol (e.g., Printable) would lose type-specific APIs.
        if matches!(info.true_type, InferredType::Dynamic) {
            if let Some(ref selector) = info.responded_selector {
                if let Some(ref registry) = self.protocol_registry {
                    if let Some(protocol_name) =
                        registry.find_unique_protocol_for_selector(selector)
                    {
                        info.true_type = InferredType::known(protocol_name.clone());
                    }
                }
            }
        }
        info
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
            InferredType::Union {
                members,
                provenance,
            } => {
                // Fast path: if no member is UndefinedObject, return unchanged.
                let has_nil = members.iter().any(|m| {
                    m.as_known()
                        .is_some_and(|n| n.as_str() == "UndefinedObject")
                });
                if !has_nil {
                    return ty.clone();
                }
                let non_nil: Vec<InferredType> = members
                    .iter()
                    .filter(|m| {
                        m.as_known()
                            .is_none_or(|name| name.as_str() != "UndefinedObject")
                    })
                    .cloned()
                    .collect();
                match non_nil.len() {
                    0 => InferredType::Dynamic,
                    1 => non_nil.into_iter().next().unwrap(),
                    _ => InferredType::Union {
                        members: non_nil,
                        provenance: *provenance,
                    },
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

    /// Infer method-local type parameters from call-site arguments.
    ///
    /// Extracts type params from ANY parametric parameter type — e.g., `Block(T, R)`,
    /// `Result(T, E)`, `Array(T)`, `Dictionary(K, V)`. For each declared type parameter
    /// in the param type, if it is method-local (not a class-level type param and not a
    /// known class name), it is matched positionally against the argument's actual `type_args`.
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

            // Handle any parametric type: TypeName(A, B, ...) parameter types
            if let Some(open) = param_type.find('(') {
                if param_type.ends_with(')') {
                    let declared_base = &param_type[..open];
                    let inner = &param_type[open + 1..param_type.len() - 1];
                    let declared_params = Self::split_type_params(inner);

                    // Match against the argument's actual type if it's a Known type
                    if let InferredType::Known {
                        class_name: arg_class,
                        type_args,
                        ..
                    } = arg_ty
                    {
                        // Verify the base class matches (e.g., Block == Block, Result == Result)
                        if arg_class.as_str() == declared_base && !type_args.is_empty() {
                            // Zip declared params with actual type args positionally
                            for (declared, actual) in declared_params.iter().zip(type_args.iter()) {
                                let decl_eco: EcoString = (*declared).into();
                                // Only infer if this is a method-local type param
                                if !class_type_params.contains(&decl_eco)
                                    && !hierarchy.has_class(&decl_eco)
                                {
                                    method_subst.insert(decl_eco, actual.clone());
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
            Literal::Symbol(name) => InferredType::known(eco_format!("#{name}")),
            Literal::Character(_) => InferredType::known("Character"),
            Literal::List(_) => InferredType::known("List"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Block, BlockParameter, ExpectCategory, Expression, ExpressionStatement, Identifier,
        KeywordPart, Literal, MessageSelector, ParameterDefinition, TypeAnnotation,
    };
    use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, MethodInfo};
    use crate::source_analysis::Span;
    use ecow::EcoString;
    use std::collections::HashMap;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: span(),
        }
    }

    fn var(name: &str) -> Expression {
        Expression::Identifier(ident(name))
    }

    fn class_ref(name: &str) -> Expression {
        Expression::ClassReference {
            name: ident(name),
            package: None,
            span: span(),
        }
    }

    fn int_lit(n: i64) -> Expression {
        Expression::Literal(Literal::Integer(n), span())
    }

    fn str_lit(s: &str) -> Expression {
        Expression::Literal(Literal::String(s.into()), span())
    }

    // ---- infer_literal ----

    #[test]
    fn infer_literal_integer() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Integer(42)),
            InferredType::known("Integer")
        );
    }

    #[test]
    fn infer_literal_float() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Float(2.5)),
            InferredType::known("Float")
        );
    }

    #[test]
    fn infer_literal_string() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::String("hello".into())),
            InferredType::known("String")
        );
    }

    #[test]
    fn infer_literal_symbol() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Symbol("ok".into())),
            InferredType::known("#ok")
        );
    }

    #[test]
    fn infer_literal_character() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Character('x')),
            InferredType::known("Character")
        );
    }

    #[test]
    fn infer_literal_list() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::List(vec![])),
            InferredType::known("List")
        );
    }

    // ---- resolve_type_annotation ----

    #[test]
    fn resolve_simple_type_annotation() {
        let ann = TypeAnnotation::Simple(ident("Integer"));
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("Integer")
        );
    }

    #[test]
    fn resolve_nil_keyword_annotation() {
        let ann = TypeAnnotation::Simple(ident("nil"));
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("UndefinedObject")
        );
    }

    #[test]
    fn resolve_false_keyword_annotation() {
        let ann = TypeAnnotation::Simple(ident("false"));
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("False")
        );
    }

    #[test]
    fn resolve_true_keyword_annotation() {
        let ann = TypeAnnotation::Simple(ident("true"));
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("True")
        );
    }

    #[test]
    fn resolve_generic_type_annotation() {
        let ann = TypeAnnotation::Generic {
            base: ident("Result"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Simple(ident("String")),
            ],
            span: span(),
        };
        let result = TypeChecker::resolve_type_annotation(&ann);
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                assert_eq!(type_args[0], InferredType::known("Integer"));
                assert_eq!(type_args[1], InferredType::known("String"));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    #[test]
    fn resolve_union_type_annotation() {
        let ann = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("String")),
                TypeAnnotation::Simple(ident("nil")),
            ],
            span: span(),
        };
        let result = TypeChecker::resolve_type_annotation(&ann);
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert!(members.contains(&InferredType::known("String")));
                assert!(members.contains(&InferredType::known("UndefinedObject")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    #[test]
    fn resolve_false_or_type_annotation() {
        let ann = TypeAnnotation::FalseOr {
            inner: Box::new(TypeAnnotation::Simple(ident("Integer"))),
            span: span(),
        };
        let result = TypeChecker::resolve_type_annotation(&ann);
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert!(members.contains(&InferredType::known("Integer")));
                assert!(members.contains(&InferredType::known("False")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    #[test]
    fn resolve_self_type_annotation() {
        let ann = TypeAnnotation::SelfType { span: span() };
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::Dynamic
        );
    }

    #[test]
    fn resolve_singleton_type_annotation() {
        let ann = TypeAnnotation::Singleton {
            name: "north".into(),
            span: span(),
        };
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("#north")
        );
    }

    // ---- resolve_type_name_string ----

    #[test]
    fn resolve_type_name_string_simple() {
        assert_eq!(
            TypeChecker::resolve_type_name_string(&"Integer".into()),
            InferredType::known("Integer")
        );
    }

    #[test]
    fn resolve_type_name_string_nil_keyword() {
        assert_eq!(
            TypeChecker::resolve_type_name_string(&"nil".into()),
            InferredType::known("UndefinedObject")
        );
    }

    #[test]
    fn resolve_type_name_string_union() {
        let result = TypeChecker::resolve_type_name_string(&"String | nil".into());
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert!(members.contains(&InferredType::known("String")));
                assert!(members.contains(&InferredType::known("UndefinedObject")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    #[test]
    fn resolve_type_name_string_three_way_union() {
        let result = TypeChecker::resolve_type_name_string(&"Integer | String | nil".into());
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 3);
                assert!(members.contains(&InferredType::known("Integer")));
                assert!(members.contains(&InferredType::known("String")));
                assert!(members.contains(&InferredType::known("UndefinedObject")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    // ---- detect_narrowing ----

    #[test]
    fn detect_narrowing_is_nil() {
        // x isNil
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("isNil".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect isNil");
        assert_eq!(info.variable.as_str(), "x");
        assert_eq!(info.true_type, InferredType::known("UndefinedObject"));
        assert!(info.is_nil_check);
        assert!(info.responded_selector.is_none());
    }

    #[test]
    fn detect_narrowing_is_kind_of() {
        // x isKindOf: Integer
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Keyword(vec![KeywordPart {
                keyword: "isKindOf:".into(),
                span: span(),
            }]),
            arguments: vec![class_ref("Integer")],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect isKindOf:");
        assert_eq!(info.variable.as_str(), "x");
        assert_eq!(info.true_type, InferredType::known("Integer"));
        assert!(!info.is_nil_check);
    }

    #[test]
    fn detect_narrowing_class_eq() {
        // x class = String
        let class_send = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("class".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let expr = Expression::MessageSend {
            receiver: Box::new(class_send),
            selector: MessageSelector::Binary("=".into()),
            arguments: vec![class_ref("String")],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect class =");
        assert_eq!(info.variable.as_str(), "x");
        assert_eq!(info.true_type, InferredType::known("String"));
        assert!(!info.is_nil_check);
    }

    #[test]
    fn detect_narrowing_class_identity_eq() {
        // x class =:= Float
        let class_send = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("class".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let expr = Expression::MessageSend {
            receiver: Box::new(class_send),
            selector: MessageSelector::Binary("=:=".into()),
            arguments: vec![class_ref("Float")],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect class =:=");
        assert_eq!(info.variable.as_str(), "x");
        assert_eq!(info.true_type, InferredType::known("Float"));
    }

    #[test]
    fn detect_narrowing_responds_to() {
        // x respondsTo: #doSomething
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Keyword(vec![KeywordPart {
                keyword: "respondsTo:".into(),
                span: span(),
            }]),
            arguments: vec![Expression::Literal(
                Literal::Symbol("doSomething".into()),
                span(),
            )],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect respondsTo:");
        assert_eq!(info.variable.as_str(), "x");
        assert_eq!(info.true_type, InferredType::Dynamic);
        assert!(!info.is_nil_check);
        assert_eq!(info.responded_selector.as_deref(), Some("doSomething"));
    }

    #[test]
    fn detect_narrowing_responds_to_non_symbol() {
        // x respondsTo: someVar (not a symbol literal — should not match)
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Keyword(vec![KeywordPart {
                keyword: "respondsTo:".into(),
                span: span(),
            }]),
            arguments: vec![var("someVar")],
            is_cast: false,
            span: span(),
        };
        assert!(TypeChecker::detect_narrowing(&expr).is_none());
    }

    #[test]
    fn detect_narrowing_no_match() {
        // x size (not a type-testing pattern)
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("size".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        assert!(TypeChecker::detect_narrowing(&expr).is_none());
    }

    #[test]
    fn detect_narrowing_parenthesized_class_eq() {
        // (x class) = Integer
        let class_send = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("class".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let parens = Expression::Parenthesized {
            expression: Box::new(class_send),
            span: span(),
        };
        let expr = Expression::MessageSend {
            receiver: Box::new(parens),
            selector: MessageSelector::Binary("=".into()),
            arguments: vec![class_ref("Integer")],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect (x class) = Type");
        assert_eq!(info.variable.as_str(), "x");
        assert_eq!(info.true_type, InferredType::known("Integer"));
    }

    // ---- non_nil_type ----

    #[test]
    fn non_nil_type_removes_nil_from_union() {
        let ty = InferredType::simple_union(&["String", "nil"]);
        let result = TypeChecker::non_nil_type(&ty);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn non_nil_type_preserves_non_nil_union() {
        let ty = InferredType::simple_union(&["String", "Integer"]);
        let result = TypeChecker::non_nil_type(&ty);
        assert_eq!(result, ty);
    }

    #[test]
    fn non_nil_type_all_nil_becomes_dynamic() {
        // union_of with single member returns the member itself, so build manually
        let ty = InferredType::Union {
            members: vec![InferredType::known("UndefinedObject")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let result = TypeChecker::non_nil_type(&ty);
        assert_eq!(result, InferredType::Dynamic);
    }

    #[test]
    fn non_nil_type_known_type_unchanged() {
        let ty = InferredType::known("Integer");
        let result = TypeChecker::non_nil_type(&ty);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn non_nil_type_dynamic_unchanged() {
        let result = TypeChecker::non_nil_type(&InferredType::Dynamic);
        assert_eq!(result, InferredType::Dynamic);
    }

    #[test]
    fn non_nil_type_three_member_union() {
        let ty = InferredType::simple_union(&["String", "Integer", "nil"]);
        let result = TypeChecker::non_nil_type(&ty);
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert!(members.contains(&InferredType::known("String")));
                assert!(members.contains(&InferredType::known("Integer")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    // ---- extract_variable_name ----

    #[test]
    fn extract_variable_name_from_ident() {
        let expr = var("foo");
        assert_eq!(
            TypeChecker::extract_variable_name(&expr),
            Some("foo".into())
        );
    }

    #[test]
    fn extract_variable_name_from_parenthesized() {
        let expr = Expression::Parenthesized {
            expression: Box::new(var("bar")),
            span: span(),
        };
        assert_eq!(
            TypeChecker::extract_variable_name(&expr),
            Some("bar".into())
        );
    }

    #[test]
    fn extract_variable_name_from_non_ident() {
        let expr = int_lit(42);
        assert!(TypeChecker::extract_variable_name(&expr).is_none());
    }

    // ---- block_has_return ----

    #[test]
    fn block_has_return_true() {
        let block = Block::new(
            vec![],
            vec![ExpressionStatement::bare(Expression::Return {
                value: Box::new(int_lit(1)),
                span: span(),
            })],
            span(),
        );
        assert!(TypeChecker::block_has_return(&block));
    }

    #[test]
    fn block_has_return_false() {
        let block = Block::new(vec![], vec![ExpressionStatement::bare(int_lit(42))], span());
        assert!(!TypeChecker::block_has_return(&block));
    }

    #[test]
    fn block_has_return_empty() {
        let block = Block::new(vec![], vec![], span());
        assert!(!TypeChecker::block_has_return(&block));
    }

    // ---- split_type_params ----

    #[test]
    fn split_type_params_simple() {
        let result = TypeChecker::split_type_params("T, E");
        assert_eq!(result, vec!["T", "E"]);
    }

    #[test]
    fn split_type_params_single() {
        let result = TypeChecker::split_type_params("Integer");
        assert_eq!(result, vec!["Integer"]);
    }

    #[test]
    fn split_type_params_nested() {
        let result = TypeChecker::split_type_params("GenResult(A, B), E");
        assert_eq!(result, vec!["GenResult(A, B)", "E"]);
    }

    #[test]
    fn split_type_params_empty() {
        let result = TypeChecker::split_type_params("");
        assert!(result.is_empty());
    }

    #[test]
    fn split_type_params_deeply_nested() {
        let result = TypeChecker::split_type_params("Outer(Inner(A, B), C), D");
        assert_eq!(result, vec!["Outer(Inner(A, B), C)", "D"]);
    }

    // ---- substitute_return_type ----

    #[test]
    fn substitute_direct_param() {
        let mut subst = HashMap::new();
        subst.insert(EcoString::from("T"), InferredType::known("Integer"));
        let result = TypeChecker::substitute_return_type("T", &subst, &HashMap::new());
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn substitute_method_local_param() {
        let mut method_subst = HashMap::new();
        method_subst.insert(EcoString::from("R"), InferredType::known("String"));
        let result = TypeChecker::substitute_return_type("R", &HashMap::new(), &method_subst);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn substitute_method_local_takes_priority() {
        let mut subst = HashMap::new();
        subst.insert(EcoString::from("R"), InferredType::known("Integer"));
        let mut method_subst = HashMap::new();
        method_subst.insert(EcoString::from("R"), InferredType::known("String"));
        let result = TypeChecker::substitute_return_type("R", &subst, &method_subst);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn substitute_generic_return_type() {
        let mut subst = HashMap::new();
        subst.insert(EcoString::from("T"), InferredType::known("Integer"));
        subst.insert(EcoString::from("E"), InferredType::known("IOError"));
        let result = TypeChecker::substitute_return_type("Result(T, E)", &subst, &HashMap::new());
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                assert_eq!(type_args[0], InferredType::known("Integer"));
                assert_eq!(type_args[1], InferredType::known("IOError"));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    #[test]
    fn substitute_no_match_passes_through() {
        let result =
            TypeChecker::substitute_return_type("String", &HashMap::new(), &HashMap::new());
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn substitute_generic_base_extracted() {
        // When return type is "Array(R)" and R is not in subst, base "Array" is still extracted
        let result =
            TypeChecker::substitute_return_type("Array(R)", &HashMap::new(), &HashMap::new());
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Array");
                assert_eq!(type_args.len(), 1);
                // R not in subst, so it becomes Known("R")
                assert_eq!(type_args[0], InferredType::known("R"));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    // ---- set_param_types ----

    #[test]
    fn set_param_types_untyped() {
        let params = vec![ParameterDefinition::new(ident("x"))];
        let mut env = TypeEnv::new();
        TypeChecker::set_param_types(&mut env, &params);
        assert_eq!(env.get("x"), Some(InferredType::Dynamic));
    }

    #[test]
    fn set_param_types_typed() {
        let params = vec![ParameterDefinition {
            name: ident("x"),
            type_annotation: Some(TypeAnnotation::Simple(ident("Integer"))),
        }];
        let mut env = TypeEnv::new();
        TypeChecker::set_param_types(&mut env, &params);
        assert_eq!(env.get("x"), Some(InferredType::known("Integer")));
    }

    #[test]
    fn set_param_types_mixed() {
        let params = vec![
            ParameterDefinition {
                name: ident("x"),
                type_annotation: Some(TypeAnnotation::Simple(ident("String"))),
            },
            ParameterDefinition::new(ident("y")),
        ];
        let mut env = TypeEnv::new();
        TypeChecker::set_param_types(&mut env, &params);
        assert_eq!(env.get("x"), Some(InferredType::known("String")));
        assert_eq!(env.get("y"), Some(InferredType::Dynamic));
    }

    // ---- infer_expr: core expression type inference ----

    #[test]
    fn infer_expr_integer_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&int_lit(42), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Integer"));
    }

    #[test]
    fn infer_expr_string_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&str_lit("hello"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("String"));
    }

    #[test]
    fn infer_expr_true_identifier() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&var("true"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Boolean"));
    }

    #[test]
    fn infer_expr_false_identifier() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&var("false"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Boolean"));
    }

    #[test]
    fn infer_expr_nil_identifier() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&var("nil"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("UndefinedObject"));
    }

    #[test]
    fn infer_expr_self_identifier() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set("self", InferredType::known("Counter"));
        let ty = checker.infer_expr(&var("self"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Counter"));
    }

    #[test]
    fn infer_expr_env_variable() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set("myVar", InferredType::known("String"));
        let ty = checker.infer_expr(&var("myVar"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("String"));
    }

    #[test]
    fn infer_expr_unknown_var_is_dynamic() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&var("unknownVar"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Dynamic);
    }

    #[test]
    fn infer_expr_class_reference() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&class_ref("Integer"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Integer"));
    }

    #[test]
    fn infer_expr_assignment_tracks_type() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Assignment {
            target: Box::new(var("x")),
            value: Box::new(int_lit(42)),
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Integer"));
        // The variable should now be tracked in the environment
        assert_eq!(env.get("x"), Some(InferredType::known("Integer")));
    }

    #[test]
    fn infer_expr_block_returns_block_type() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let block = Expression::Block(Block::new(
            vec![BlockParameter::new("x", span())],
            vec![ExpressionStatement::bare(int_lit(1))],
            span(),
        ));
        let ty = checker.infer_expr(&block, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Block"));
    }

    #[test]
    fn infer_expr_map_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::MapLiteral {
            pairs: vec![],
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Dictionary"));
    }

    #[test]
    fn infer_expr_array_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::ArrayLiteral {
            elements: vec![int_lit(1), int_lit(2)],
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Array"));
    }

    #[test]
    fn infer_expr_list_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::ListLiteral {
            elements: vec![int_lit(1)],
            tail: None,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("List"));
    }

    #[test]
    fn infer_expr_string_interpolation() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::StringInterpolation {
            segments: vec![
                crate::ast::StringSegment::Literal("hello ".into()),
                crate::ast::StringSegment::Interpolation(var("name")),
            ],
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("String"));
    }

    #[test]
    fn infer_expr_return_propagates_value_type() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Return {
            value: Box::new(str_lit("done")),
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("String"));
    }

    #[test]
    fn infer_expr_parenthesized_unwraps() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Parenthesized {
            expression: Box::new(int_lit(7)),
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Integer"));
    }

    #[test]
    fn infer_expr_primitive_is_dynamic() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Primitive {
            name: "add".into(),
            is_quoted: false,
            is_intrinsic: false,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Dynamic);
    }

    #[test]
    fn infer_expr_match_is_dynamic() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Match {
            value: Box::new(int_lit(1)),
            arms: vec![],
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Dynamic);
    }

    // ---- build_substitution_map ----

    #[test]
    fn build_substitution_map_empty_args() {
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::build_substitution_map(&hierarchy, "Array", &[]);
        assert!(result.is_empty());
    }

    // ---- infer_stmts ----

    #[test]
    fn infer_stmts_empty_is_dynamic() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let result = checker.infer_stmts(&[], &hierarchy, &mut env, false);
        assert_eq!(result, InferredType::Dynamic);
    }

    #[test]
    fn infer_stmts_returns_last_type() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let stmts = vec![
            ExpressionStatement::bare(int_lit(1)),
            ExpressionStatement::bare(str_lit("hello")),
        ];
        let result = checker.infer_stmts(&stmts, &hierarchy, &mut env, false);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn infer_stmts_skips_expect_directives() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let stmts = vec![
            ExpressionStatement::bare(int_lit(42)),
            ExpressionStatement::bare(Expression::ExpectDirective {
                category: ExpectCategory::Dnu,
                span: span(),
            }),
        ];
        // The last non-directive is int_lit(42), so result should be Integer
        let result = checker.infer_stmts(&stmts, &hierarchy, &mut env, false);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn infer_stmts_stops_at_return() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let stmts = vec![
            ExpressionStatement::bare(Expression::Return {
                value: Box::new(int_lit(1)),
                span: span(),
            }),
            ExpressionStatement::bare(str_lit("unreachable")),
        ];
        let result = checker.infer_stmts(&stmts, &hierarchy, &mut env, false);
        assert_eq!(result, InferredType::known("Integer"));
    }

    // ---- is_self_receiver ----

    #[test]
    fn is_self_receiver_true() {
        assert!(TypeChecker::is_self_receiver(&var("self")));
    }

    #[test]
    fn is_self_receiver_false_for_other_ident() {
        assert!(!TypeChecker::is_self_receiver(&var("other")));
    }

    #[test]
    fn is_self_receiver_false_for_non_ident() {
        assert!(!TypeChecker::is_self_receiver(&int_lit(1)));
    }

    // ---- infer_method_local_params ----

    /// Helper: build a `MethodInfo` with the given param types and return type.
    fn method_info(
        selector: &str,
        param_types: Vec<Option<&str>>,
        return_type: Option<&str>,
    ) -> MethodInfo {
        let arity = param_types.len();
        MethodInfo {
            selector: selector.into(),
            arity,
            kind: crate::ast::MethodKind::Primary,
            defined_in: "TestClass".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: return_type.map(EcoString::from),
            param_types: param_types
                .into_iter()
                .map(|p| p.map(EcoString::from))
                .collect(),
            doc: None,
        }
    }

    #[test]
    fn infer_method_local_params_result_t_e() {
        // assertOk: has param type Result(T, E), arg is Result(Integer, Error) → T=Integer, E=Error
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![InferredType::known("Integer"), InferredType::known("Error")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
        assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
    }

    #[test]
    fn infer_method_local_params_result_dictionary_string() {
        // assertOk: on Result(Dictionary, String) → T=Dictionary, E=String
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![
                InferredType::known("Dictionary"),
                InferredType::known("String"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("T"), Some(&InferredType::known("Dictionary")));
        assert_eq!(result.get("E"), Some(&InferredType::known("String")));
    }

    #[test]
    fn infer_method_local_params_assert_error_returns_e() {
        // assertError:equals: has param type Result(T, E), return type E
        let method = method_info(
            "assertError:equals:",
            vec![Some("Result(T, E)"), Some("Object")],
            Some("E"),
        );
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("Symbol"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg, InferredType::known("Symbol")],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("E"), Some(&InferredType::known("Symbol")));
        assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_array_t() {
        // Method with Array(T) param, arg is Array(String) → T=String
        let method = method_info("process:", vec![Some("Array(T)")], Some("T"));
        let arg = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 1);
        assert_eq!(result.get("T"), Some(&InferredType::known("String")));
    }

    #[test]
    fn infer_method_local_params_dictionary_k_v() {
        // Method with Dictionary(K, V) param → K=String, V=Integer
        let method = method_info("lookup:", vec![Some("Dictionary(K, V)")], Some("V"));
        let arg = InferredType::Known {
            class_name: "Dictionary".into(),
            type_args: vec![
                InferredType::known("String"),
                InferredType::known("Integer"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result.get("K"), Some(&InferredType::known("String")));
        assert_eq!(result.get("V"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_nested_result_array() {
        // Result(Array(Integer), Error) — nested parametric type
        let method = method_info("process:", vec![Some("Result(T, E)")], Some("T"));
        let inner_array = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("Integer")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![inner_array.clone(), InferredType::known("Error")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("T"), Some(&inner_array));
        assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
    }

    #[test]
    fn infer_method_local_params_no_type_args_on_arg() {
        // Bare Result without type args — no inference possible
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::known("Result"); // no type_args
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(result.is_empty(), "No inference when arg has no type_args");
    }

    #[test]
    fn infer_method_local_params_dynamic_arg() {
        // Dynamic argument — no inference possible
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::Dynamic;
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(result.is_empty(), "No inference from Dynamic arg");
    }

    #[test]
    fn infer_method_local_params_base_class_mismatch() {
        // Param declares Result(T, E) but arg is Array(Integer) — base mismatch
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("Integer")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(
            result.is_empty(),
            "No inference when base class name doesn't match"
        );
    }

    #[test]
    fn infer_method_local_params_multiple_parametric_params() {
        // Method with two parametric params: process:with: Array(T), Dictionary(K, V)
        let method = method_info(
            "process:with:",
            vec![Some("Array(T)"), Some("Dictionary(K, V)")],
            Some("T"),
        );
        let arg1 = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let arg2 = InferredType::Known {
            class_name: "Dictionary".into(),
            type_args: vec![
                InferredType::known("Symbol"),
                InferredType::known("Integer"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg1, arg2],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 3);
        assert_eq!(result.get("T"), Some(&InferredType::known("String")));
        assert_eq!(result.get("K"), Some(&InferredType::known("Symbol")));
        assert_eq!(result.get("V"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_skips_known_classes_in_param_type() {
        // Param type is Result(Integer, E) — Integer is a known class, not a type param
        let method = method_info("check:", vec![Some("Result(Integer, E)")], Some("E"));
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![InferredType::known("Integer"), InferredType::known("Error")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        // "Integer" is a known class, so it should NOT be inferred as a type param
        assert!(!result.contains_key("Integer"));
        assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
    }

    #[test]
    fn infer_method_local_params_non_parametric_param_ignored() {
        // Method param type is just "Integer" (not parametric) — nothing to infer
        let method = method_info("add:", vec![Some("Integer")], Some("Integer"));
        let arg = InferredType::known("Integer");
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(
            result.is_empty(),
            "Non-parametric param type yields no inference"
        );
    }

    #[test]
    fn infer_method_local_params_missing_arg() {
        // Method expects 2 args but only 1 provided — second param skipped
        let method = method_info(
            "process:with:",
            vec![Some("Array(T)"), Some("Dictionary(K, V)")],
            Some("T"),
        );
        let arg1 = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg1],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 1);
        assert_eq!(result.get("T"), Some(&InferredType::known("String")));
        assert!(!result.contains_key("K"));
        assert!(!result.contains_key("V"));
    }

    #[test]
    fn infer_method_local_params_untyped_param_skipped() {
        // Method has a None param type — skipped
        let method = method_info("process:", vec![None], Some("Dynamic"));
        let arg = InferredType::known("Integer");
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(result.is_empty());
    }

    // ---- Block inference regression tests ----
    // These test that Block(T, R) param types still work correctly.

    #[test]
    fn infer_method_local_params_block_t_r() {
        // collect: has param type Block(T, R), arg is Block with type_args
        let method = method_info("collect:", vec![Some("Block(T, R)")], Some("Array(R)"));
        let arg = InferredType::Known {
            class_name: "Block".into(),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("String"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
        assert_eq!(result.get("R"), Some(&InferredType::known("String")));
    }

    #[test]
    fn infer_method_local_params_map_block() {
        // map: has Block(T, R) param type
        let method = method_info("map:", vec![Some("Block(T, R)")], Some("Array(R)"));
        let arg = InferredType::Known {
            class_name: "Block".into(),
            type_args: vec![
                InferredType::known("String"),
                InferredType::known("Integer"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("T"), Some(&InferredType::known("String")));
        assert_eq!(result.get("R"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_inject_into_block() {
        // inject:into: has two params — initial value and Block(T, R)
        let method = method_info(
            "inject:into:",
            vec![Some("Object"), Some("Block(T, R)")],
            Some("R"),
        );
        let arg_init = InferredType::known("Integer");
        let arg_block = InferredType::Known {
            class_name: "Block".into(),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("Integer"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg_init, arg_block],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        // Only the Block param yields inference; Object is non-parametric
        assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
        assert_eq!(result.get("R"), Some(&InferredType::known("Integer")));
    }

    // ---- substitute_return_type with method-local params (end-to-end flow) ----

    #[test]
    fn substitute_return_type_from_method_local_result() {
        // Simulates: assertOk: with Result(Integer, Error) → return type T → Integer
        let mut method_subst = HashMap::new();
        method_subst.insert(EcoString::from("T"), InferredType::known("Integer"));
        method_subst.insert(EcoString::from("E"), InferredType::known("Error"));
        let result = TypeChecker::substitute_return_type("T", &HashMap::new(), &method_subst);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn substitute_return_type_generic_array_r() {
        // collect: returns Array(R) where R=String → Array(String)
        let mut method_subst = HashMap::new();
        method_subst.insert(EcoString::from("R"), InferredType::known("String"));
        let result =
            TypeChecker::substitute_return_type("Array(R)", &HashMap::new(), &method_subst);
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Array");
                assert_eq!(type_args.len(), 1);
                assert_eq!(type_args[0], InferredType::known("String"));
            }
            other => panic!("Expected Known Array(String), got {other:?}"),
        }
    }

    #[test]
    fn substitute_return_type_nested_result() {
        // Return type Result(T, E) with T=Array(Integer), E=Error
        // Substitution on "Result(T, E)" should produce Result(Array(Integer), Error)
        let mut method_subst = HashMap::new();
        method_subst.insert(
            EcoString::from("T"),
            InferredType::Known {
                class_name: "Array".into(),
                type_args: vec![InferredType::known("Integer")],
                provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
            },
        );
        method_subst.insert(EcoString::from("E"), InferredType::known("Error"));
        let result =
            TypeChecker::substitute_return_type("Result(T, E)", &HashMap::new(), &method_subst);
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                match &type_args[0] {
                    InferredType::Known {
                        class_name,
                        type_args,
                        ..
                    } => {
                        assert_eq!(class_name.as_str(), "Array");
                        assert_eq!(type_args.len(), 1);
                        assert_eq!(type_args[0], InferredType::known("Integer"));
                    }
                    other => panic!("Expected Known Array(Integer), got {other:?}"),
                }
                assert_eq!(type_args[1], InferredType::known("Error"));
            }
            other => panic!("Expected Known Result(Array(Integer), Error), got {other:?}"),
        }
    }
}
