// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Protocol conformance, type parameter bounds, and generic variance checking.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module contains the module-walking orchestration methods for:
//! - Protocol conformance checking at call sites (ADR 0068 Phase 2b)
//! - Type parameter bounds validation (ADR 0068 Phase 2d)
//! - Generic variance checking (ADR 0068 Phase 2f)

use crate::ast::{Module, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use ecow::EcoString;

use super::{InferredType, TypeChecker, TypeEnv};

impl TypeChecker {
    /// Check protocol conformance for type annotations across a module.
    ///
    /// Walks all message send call sites: when a target method's parameter type
    /// annotation is a protocol name, and we have an inferred argument type,
    /// perform structural conformance checking. This is the call-site check
    /// described in ADR 0068 Phase 2b.
    pub(super) fn check_protocol_conformance_in_module(
        &mut self,
        module: &Module,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        // Walk all expressions looking for message sends with protocol-typed parameters
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for stmt in &method.body {
                    self.check_protocol_conformance_in_expr(
                        &stmt.expression,
                        hierarchy,
                        protocol_registry,
                    );
                }
            }
        }

        for standalone in &module.method_definitions {
            for stmt in &standalone.method.body {
                self.check_protocol_conformance_in_expr(
                    &stmt.expression,
                    hierarchy,
                    protocol_registry,
                );
            }
        }

        for stmt in &module.expressions {
            self.check_protocol_conformance_in_expr(&stmt.expression, hierarchy, protocol_registry);
        }
    }

    /// Recursively check protocol conformance in expressions.
    #[allow(clippy::too_many_lines)] // match over all Expression variants
    fn check_protocol_conformance_in_expr(
        &mut self,
        expr: &crate::ast::Expression,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        use crate::ast::Expression;

        match expr {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                span,
                ..
            } => {
                // Check if the receiver's method has protocol-typed params
                if let Some(receiver_type) = self.type_map.get(receiver.span()) {
                    if let InferredType::Known { class_name, .. } = receiver_type.clone() {
                        let sel_name = selector.name();
                        let method = hierarchy.find_method(&class_name, &sel_name);
                        if let Some(method) = method {
                            for (i, arg) in arguments.iter().enumerate() {
                                if let Some(Some(expected_ty)) = method.param_types.get(i) {
                                    if Self::is_protocol_type(
                                        expected_ty,
                                        hierarchy,
                                        protocol_registry,
                                    ) {
                                        if let Some(arg_type) = self.type_map.get(arg.span()) {
                                            self.check_protocol_argument_conformance(
                                                &arg_type.clone(),
                                                expected_ty,
                                                *span,
                                                hierarchy,
                                                protocol_registry,
                                            );
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Recurse into sub-expressions
                self.check_protocol_conformance_in_expr(receiver, hierarchy, protocol_registry);
                for arg in arguments {
                    self.check_protocol_conformance_in_expr(arg, hierarchy, protocol_registry);
                }
            }
            Expression::Block(block) => {
                for stmt in &block.body {
                    self.check_protocol_conformance_in_expr(
                        &stmt.expression,
                        hierarchy,
                        protocol_registry,
                    );
                }
            }
            Expression::Assignment { value, .. }
            | Expression::DestructureAssignment { value, .. }
            | Expression::Return { value, .. } => {
                self.check_protocol_conformance_in_expr(value, hierarchy, protocol_registry);
            }
            Expression::Parenthesized { expression, .. } => {
                self.check_protocol_conformance_in_expr(expression, hierarchy, protocol_registry);
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                self.check_protocol_conformance_in_expr(receiver, hierarchy, protocol_registry);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.check_protocol_conformance_in_expr(arg, hierarchy, protocol_registry);
                    }
                }
            }
            Expression::FieldAccess { receiver, .. } => {
                self.check_protocol_conformance_in_expr(receiver, hierarchy, protocol_registry);
            }
            Expression::Match { value, arms, .. } => {
                self.check_protocol_conformance_in_expr(value, hierarchy, protocol_registry);
                for arm in arms {
                    self.check_protocol_conformance_in_expr(
                        &arm.body,
                        hierarchy,
                        protocol_registry,
                    );
                }
            }
            Expression::StringInterpolation { segments, .. } => {
                for seg in segments {
                    if let crate::ast::StringSegment::Interpolation(expr) = seg {
                        self.check_protocol_conformance_in_expr(expr, hierarchy, protocol_registry);
                    }
                }
            }
            Expression::ArrayLiteral { elements, .. } => {
                for elem in elements {
                    self.check_protocol_conformance_in_expr(elem, hierarchy, protocol_registry);
                }
            }
            Expression::ListLiteral { elements, tail, .. } => {
                for elem in elements {
                    self.check_protocol_conformance_in_expr(elem, hierarchy, protocol_registry);
                }
                if let Some(tail_expr) = tail {
                    self.check_protocol_conformance_in_expr(
                        tail_expr,
                        hierarchy,
                        protocol_registry,
                    );
                }
            }
            Expression::MapLiteral { pairs, .. } => {
                for pair in pairs {
                    self.check_protocol_conformance_in_expr(
                        &pair.key,
                        hierarchy,
                        protocol_registry,
                    );
                    self.check_protocol_conformance_in_expr(
                        &pair.value,
                        hierarchy,
                        protocol_registry,
                    );
                }
            }
            // Leaf expressions — no sub-expressions to recurse into
            _ => {}
        }
    }

    /// Check type parameter bounds for all generic type annotations in a module (ADR 0068 Phase 2d).
    ///
    /// Walks class definitions, standalone methods, and protocol definitions looking for
    /// generic type annotations (e.g., `:: Logger(Integer)`). For each, checks that the
    /// concrete type arguments conform to any protocol bounds declared on the type parameters.
    pub(super) fn check_type_param_bounds_in_module(
        &mut self,
        module: &Module,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        // Check type annotations in class state declarations, method params, and return types
        for class in &module.classes {
            // Check state field type annotations
            for state_decl in &class.state {
                if let Some(ref ann) = state_decl.type_annotation {
                    self.check_bounds_in_type_annotation(ann, hierarchy, protocol_registry);
                }
            }

            // Check method parameter and return type annotations
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for param in &method.parameters {
                    if let Some(ref ann) = param.type_annotation {
                        self.check_bounds_in_type_annotation(ann, hierarchy, protocol_registry);
                    }
                }
                if let Some(ref ann) = method.return_type {
                    self.check_bounds_in_type_annotation(ann, hierarchy, protocol_registry);
                }
            }

            // Check superclass type args (e.g., `Logger(Integer) subclass: SpecialLogger`)
            for arg in &class.superclass_type_args {
                self.check_bounds_in_type_annotation(arg, hierarchy, protocol_registry);
            }
        }

        // Check standalone method definitions
        for standalone in &module.method_definitions {
            for param in &standalone.method.parameters {
                if let Some(ref ann) = param.type_annotation {
                    self.check_bounds_in_type_annotation(ann, hierarchy, protocol_registry);
                }
            }
            if let Some(ref ann) = standalone.method.return_type {
                self.check_bounds_in_type_annotation(ann, hierarchy, protocol_registry);
            }
        }
    }

    /// Check type parameter bounds within a single type annotation.
    ///
    /// For `TypeAnnotation::Generic`, resolves the type args and checks each
    /// against its corresponding bound (if any) from the class's `type_param_bounds`.
    fn check_bounds_in_type_annotation(
        &mut self,
        ann: &crate::ast::TypeAnnotation,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        use crate::ast::TypeAnnotation;

        match ann {
            TypeAnnotation::Generic {
                base,
                parameters,
                span,
            } => {
                // Resolve the type args to InferredTypes
                let type_args: Vec<InferredType> = parameters
                    .iter()
                    .map(Self::resolve_type_annotation)
                    .collect();

                // BT-1861: Warn when type args are provided for a class with no type params.
                // Block is exempt — parameterized Block annotations (e.g., Block(E, Boolean))
                // are a documentation convention for describing closure signatures.
                let has_type_params = if base.name == "Block" {
                    true // Block uses type args as documentation convention
                } else if let Some(class_info) = hierarchy.get_class(&base.name) {
                    !class_info.type_params.is_empty()
                } else if let Some(proto_info) = protocol_registry.get(&base.name) {
                    !proto_info.type_params.is_empty()
                } else {
                    true // Unknown type — don't warn (may be defined elsewhere)
                };

                if has_type_params {
                    // Check bounds against the base class's type_param_bounds
                    self.check_type_param_bounds(
                        &base.name,
                        &type_args,
                        *span,
                        hierarchy,
                        protocol_registry,
                    );
                } else {
                    let args_str = parameters
                        .iter()
                        .map(|p| p.type_name().to_string())
                        .collect::<Vec<_>>()
                        .join(", ");
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "{} has no type parameters but was given type arguments",
                                base.name
                            ),
                            *span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!(
                            "Remove the type arguments: use {0} instead of {0}({args_str})",
                            base.name,
                        )),
                    );
                }

                // Recurse into nested type annotations (e.g., `Result(Array(Integer), Error)`)
                for param in parameters {
                    self.check_bounds_in_type_annotation(param, hierarchy, protocol_registry);
                }
            }
            TypeAnnotation::Union { types, .. } => {
                for ty in types {
                    self.check_bounds_in_type_annotation(ty, hierarchy, protocol_registry);
                }
            }
            TypeAnnotation::FalseOr { inner, .. } => {
                self.check_bounds_in_type_annotation(inner, hierarchy, protocol_registry);
            }
            // Simple, SelfType, Singleton — no nested generics to check
            _ => {}
        }
    }

    /// Check generic type argument variance across a module (ADR 0068 Phase 2f).
    ///
    /// Walks message sends and state declarations looking for places where a generic
    /// type (e.g., `Array(Integer)`) is used where a differently-parameterized generic
    /// is expected (e.g., `Array(Printable)`). Checks variance rules:
    /// - Sealed Value classes are covariant: `Array(Integer)` assignable to `Array(Printable)`
    ///   if Integer conforms to Printable
    /// - Actor classes are invariant: type args must match exactly
    pub(super) fn check_generic_variance_in_module(
        &mut self,
        module: &Module,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        // Check state field declarations with generic type annotations
        for class in &module.classes {
            for state_decl in &class.state {
                if let Some(TypeAnnotation::Generic { .. }) = state_decl.type_annotation {
                    self.check_state_variance(class, state_decl, hierarchy, protocol_registry);
                }
            }
        }

        // Check message send arguments with generic parameter types
        for class in &module.classes {
            for method in class.methods.iter().chain(class.class_methods.iter()) {
                for stmt in &method.body {
                    self.check_variance_in_expr(&stmt.expression, hierarchy, protocol_registry);
                }
            }
        }

        for standalone in &module.method_definitions {
            for stmt in &standalone.method.body {
                self.check_variance_in_expr(&stmt.expression, hierarchy, protocol_registry);
            }
        }

        for stmt in &module.expressions {
            self.check_variance_in_expr(&stmt.expression, hierarchy, protocol_registry);
        }
    }

    /// Check variance for a state field with a generic type annotation.
    ///
    /// When a state field is declared as `field: items :: Array(Printable) = Array new`,
    /// checks that the default value's type (if inferred) is compatible with variance rules.
    fn check_state_variance(
        &mut self,
        class: &crate::ast::ClassDefinition,
        state_decl: &crate::ast::StateDeclaration,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        let Some(ref default_value) = state_decl.default_value else {
            return;
        };
        let Some(ref type_annotation) = state_decl.type_annotation else {
            return;
        };

        let declared_type = type_annotation.type_name();
        let mut env = TypeEnv::new();
        env.set("self", InferredType::known(class.name.name.clone()));
        let inferred = self.infer_expr(default_value, hierarchy, &mut env, false);

        let InferredType::Known {
            class_name: value_type,
            ..
        } = &inferred
        else {
            return;
        };

        // Build a full type string for the value type to compare with declared
        let value_type_str = Self::inferred_type_to_string(&inferred);
        if !Self::is_assignable_to_with_variance(
            &value_type_str,
            &declared_type,
            hierarchy,
            protocol_registry,
        ) {
            self.diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Type mismatch: state `{}` declared as {declared_type}, default is {value_type}",
                        state_decl.name.name
                    ),
                    state_decl.span,
                )
                .with_category(DiagnosticCategory::Type)
                .with_hint(format!(
                    "Default value type {value_type} is not compatible with {declared_type}"
                )),
            );
        }
    }

    /// Convert an `InferredType` to its string representation for variance checking.
    pub(super) fn inferred_type_to_string(ty: &InferredType) -> EcoString {
        match ty {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                if type_args.is_empty() {
                    class_name.clone()
                } else {
                    let args: Vec<String> = type_args
                        .iter()
                        .map(|a| Self::inferred_type_to_string(a).to_string())
                        .collect();
                    EcoString::from(format!("{}({})", class_name, args.join(", ")))
                }
            }
            InferredType::Dynamic(_) => EcoString::from("Dynamic"),
            InferredType::Union { members, .. } => EcoString::from(
                members
                    .iter()
                    .map(|m| Self::inferred_type_to_string(m).to_string())
                    .collect::<Vec<_>>()
                    .join(" | "),
            ),
        }
    }

    /// Recursively check variance in expressions (for message send arguments).
    fn check_variance_in_expr(
        &mut self,
        expr: &crate::ast::Expression,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        use crate::ast::Expression;

        match expr {
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                span,
                ..
            } => {
                // Check if any argument has generic type args that need variance checking
                if let Some(receiver_type) = self.type_map.get(receiver.span()) {
                    if let InferredType::Known { class_name, .. } = receiver_type.clone() {
                        let sel_name = selector.name();
                        let method = hierarchy.find_method(&class_name, &sel_name);
                        if let Some(method) = method {
                            for (i, arg) in arguments.iter().enumerate() {
                                if let Some(Some(expected_ty)) = method.param_types.get(i) {
                                    // Only check when expected type is generic
                                    if expected_ty.contains('(') {
                                        if let Some(arg_type) = self.type_map.get(arg.span()) {
                                            if let InferredType::Known { type_args, .. } = arg_type
                                            {
                                                if !type_args.is_empty() {
                                                    let arg_str = Self::inferred_type_to_string(
                                                        &arg_type.clone(),
                                                    );
                                                    if !Self::is_assignable_to_with_variance(
                                                        &arg_str,
                                                        expected_ty,
                                                        hierarchy,
                                                        protocol_registry,
                                                    ) {
                                                        let param_pos = i + 1;
                                                        self.diagnostics.push(
                                                            Diagnostic::warning(
                                                                format!(
                                                                    "Argument {param_pos} of '{sel_name}' on {class_name} expects {expected_ty}, got {arg_str}"
                                                                ),
                                                                *span,
                                                            )
                                                            .with_category(DiagnosticCategory::Type)
                                                            .with_hint(format!(
                                                                "Type arguments are not compatible: expected {expected_ty}, got {arg_str}"
                                                            )),
                                                        );
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                // Recurse
                self.check_variance_in_expr(receiver, hierarchy, protocol_registry);
                for arg in arguments {
                    self.check_variance_in_expr(arg, hierarchy, protocol_registry);
                }
            }
            Expression::Block(block) => {
                for stmt in &block.body {
                    self.check_variance_in_expr(&stmt.expression, hierarchy, protocol_registry);
                }
            }
            Expression::Assignment { value, .. }
            | Expression::DestructureAssignment { value, .. }
            | Expression::Return { value, .. } => {
                self.check_variance_in_expr(value, hierarchy, protocol_registry);
            }
            Expression::Parenthesized { expression, .. } => {
                self.check_variance_in_expr(expression, hierarchy, protocol_registry);
            }
            Expression::Cascade {
                receiver, messages, ..
            } => {
                self.check_variance_in_expr(receiver, hierarchy, protocol_registry);
                for msg in messages {
                    for arg in &msg.arguments {
                        self.check_variance_in_expr(arg, hierarchy, protocol_registry);
                    }
                }
            }
            // Leaf expressions — no sub-expressions to recurse into
            _ => {}
        }
    }
}
