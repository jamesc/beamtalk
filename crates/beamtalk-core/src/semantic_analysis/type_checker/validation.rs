// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type validation — checking types against declarations and constraints.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module contains the validation methods of [`TypeChecker`]:
//! - Class-side and instance-side selector validation
//! - Type annotation checking for typed classes
//! - Argument and return type compatibility
//! - Binary operand type checking
//! - Field assignment and state default validation
//! - "Did you mean?" suggestions for unknown selectors

use crate::ast::{Expression, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::string_utils::edit_distance;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

use super::{InferredType, TypeChecker, TypeEnv};

impl TypeChecker {
    /// Check a class-side message send (e.g., `Counter spawn`, `Object new`).
    pub(super) fn check_class_side_send(
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
            // BT-1548: spawn/spawnWith: are still instance methods on Actor but routed
            // class-side by beamtalk_class_dispatch.erl — they need the bypass.
            // new/new: are now proper class methods on Value (BT-1548) and found
            // via find_class_method through the hierarchy, so no bypass needed.
            let is_factory_selector = matches!(selector, "spawn" | "spawnWith:");
            let has_class_chain_method = hierarchy.resolves_selector("Class", selector)
                || (is_factory_selector && hierarchy.resolves_selector(class_name, selector));
            if !has_class_chain_method {
                self.emit_unknown_selector_warning(class_name, selector, span, hierarchy, true);
            }
        }

        // Infer return type for known factory methods
        match selector {
            "spawn" | "spawnWith:" | "new" | "new:" => InferredType::known(class_name.clone()),
            _ => {
                if let Some(method) = hierarchy.find_class_method(class_name, selector) {
                    if let Some(ref ret_ty) = method.return_type {
                        // `Self` resolves to the static receiver class
                        let resolved = if ret_ty.as_str() == "Self" {
                            class_name.clone()
                        } else {
                            ret_ty.clone()
                        };
                        return InferredType::known(resolved);
                    }
                }
                // BT-1047: Fall back to return types inferred earlier in this pass.
                let key = (class_name.clone(), EcoString::from(selector), true);
                if let Some(ret_ty) = self.method_return_types.get(&key) {
                    return InferredType::known(ret_ty.clone());
                }
                InferredType::Dynamic
            }
        }
    }

    /// Check if an instance-side selector is valid for a known class.
    pub(super) fn check_instance_selector(
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

        // Cross-file inheritance: if the parent class is not in the hierarchy,
        // we can't know the full method set — suppress false-positive DNU hints.
        if hierarchy.has_cross_file_parent(class_name) {
            return;
        }

        if !hierarchy.resolves_selector(class_name, selector) {
            self.emit_unknown_selector_warning(class_name, selector, span, hierarchy, false);
        }
    }

    /// Check that methods in typed classes have proper type annotations.
    pub(super) fn check_typed_method_annotations(
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
    pub(super) fn check_argument_types(
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
            let InferredType::Known {
                class_name: actual_ty,
                ..
            } = arg_ty
            else {
                continue; // Dynamic or Union arguments — skip (conservative)
            };
            if !Self::is_type_compatible(actual_ty, expected_ty, hierarchy) {
                let param_pos = i + 1;
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Argument {param_pos} of '{selector}' on {class_name} expects {expected_ty}, got {actual_ty}"
                        ),
                        span,
                    )
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(format!("Expected {expected_ty} (or a subclass), got {actual_ty}")),
                );
            }
        }
    }

    /// Check that a method body's inferred return type matches its declared return type.
    pub(super) fn check_return_type(
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
        let InferredType::Known {
            class_name: actual_ty,
            ..
        } = body_type
        else {
            return; // Dynamic or Union body — can't reliably check
        };

        // For `-> Self`, the expected type is the class itself
        // For `-> Generic(...)`, extract the base type name for compatibility checking
        // For `-> Union` / `-> FalseOr`, resolve via resolve_type_annotation
        let expected = match declared {
            TypeAnnotation::Simple(type_id) => InferredType::known(type_id.name.clone()),
            TypeAnnotation::SelfType { .. } => InferredType::known(class_name.clone()),
            TypeAnnotation::Generic { base, .. } => InferredType::known(base.name.clone()),
            TypeAnnotation::Union { .. } | TypeAnnotation::FalseOr { .. } => {
                Self::resolve_type_annotation(declared)
            }
            _ => return, // Singleton — not yet supported
        };

        match &expected {
            InferredType::Known { class_name: expected_ty, .. } => {
                if !Self::is_type_compatible(actual_ty, expected_ty, hierarchy) {
                    let selector = method.selector.name();
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Method '{selector}' in {class_name} declares return type {expected_ty}, but body returns {actual_ty}"
                            ),
                            method.span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!("Declared -> {expected_ty}, inferred body type is {actual_ty}")),
                    );
                }
            }
            InferredType::Union(members) => {
                // Body type must be compatible with at least one union member
                let compatible = members
                    .iter()
                    .any(|member| Self::is_type_compatible(actual_ty, member, hierarchy));
                if !compatible {
                    let selector = method.selector.name();
                    let union_display = members.join(" | ");
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Method '{selector}' in {class_name} declares return type {union_display}, but body returns {actual_ty}"
                            ),
                            method.span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!("Declared -> {union_display}, inferred body type is {actual_ty}")),
                    );
                }
            }
            InferredType::Dynamic => {} // Can't check
        }
    }

    /// Emit an error for any parameter annotated with `Self`.
    ///
    /// `Self` is only valid in return position (TypeScript `this` return type).
    /// Using it as a parameter type is unsound in the presence of subclassing
    /// (Eiffel's mistake) and is rejected at the semantic analysis level.
    pub(super) fn check_no_self_in_params(
        &mut self,
        method: &crate::ast::MethodDefinition,
        class_name: &EcoString,
    ) {
        let selector = method.selector.name();
        for param in &method.parameters {
            if matches!(param.type_annotation, Some(TypeAnnotation::SelfType { .. })) {
                self.diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "`Self` cannot be used as a parameter type in `{class_name}::{selector}` (only valid in return position)"
                        ),
                        param.name.span,
                    )
                    .with_category(DiagnosticCategory::Type),
                );
            }
        }
    }

    /// Check that a child method's parameter types are compatible with its parent's.
    pub(super) fn check_override_param_compatibility(
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
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Parameter {param_pos} of '{selector}' in {class_name} has type {child_t}, incompatible with parent's {parent_t}"
                        ),
                        method.span,
                    )
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(format!("Parent class {superclass} declares parameter type {parent_t}")),
                );
            }
        }
    }

    /// Validate binary message operand types for arithmetic and string concatenation.
    ///
    /// When both receiver and argument types are known, checks that the argument type
    /// is compatible with the operator. Only emits warnings (not errors) to allow
    /// for dynamic dispatch.
    pub(super) fn check_binary_operand_types(
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
            self.diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "`{operator}` on {receiver_ty} expects a numeric argument, got {arg_ty}"
                    ),
                    span,
                )
                .with_category(DiagnosticCategory::Type)
                .with_hint("Arithmetic operators require Integer or Float operands"),
            );
            return;
        }

        // String concatenation with ++ expects a String argument
        if operator == "++"
            && receiver_ty.as_str() == "String"
            && arg_ty.as_str() != "String"
            && arg_ty.as_str() != "Symbol"
        {
            self.diagnostics.push(
                Diagnostic::warning(
                    format!("`++` on String expects a String argument, got {arg_ty}"),
                    span,
                )
                .with_category(DiagnosticCategory::Type)
                .with_hint("Convert the argument to String first"),
            );
            return;
        }

        // Comparison operators on numeric types require numeric arguments
        if is_comparison && is_numeric(receiver_ty) && !is_numeric(arg_ty) {
            self.diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "`{operator}` on {receiver_ty} expects a numeric argument, got {arg_ty}"
                    ),
                    span,
                )
                .with_category(DiagnosticCategory::Type)
                .with_hint("Comparison operators require compatible types"),
            );
        }
    }

    /// Check a field assignment `self.field := value` against the declared state type.
    ///
    /// If the field has a type annotation in the class hierarchy and the inferred
    /// value type is known and incompatible, emits a warning.
    pub(super) fn check_field_assignment(
        &mut self,
        field: &crate::ast::Identifier,
        value_ty: &InferredType,
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &TypeEnv,
    ) {
        let InferredType::Known {
            class_name: value_type,
            ..
        } = value_ty
        else {
            return; // Dynamic or Union values — skip (conservative)
        };
        let Some(InferredType::Known { class_name, .. }) = env.get("self") else {
            return;
        };
        let Some(declared_type) = hierarchy.state_field_type(&class_name, &field.name) else {
            return; // No type annotation on this field
        };
        if !Self::is_assignable_to(value_type, &declared_type, hierarchy) {
            self.diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Type mismatch: field `{}` declared as {declared_type}, got {value_type}",
                        field.name
                    ),
                    span,
                )
                .with_category(DiagnosticCategory::Type)
                .with_hint(format!(
                    "Expected {declared_type} but assigning {value_type}"
                )),
            );
        }
    }

    /// Check state default values match declared types at class definition time.
    pub(super) fn check_state_defaults(
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
            env.set("self", InferredType::known(class.name.name.clone()));
            let inferred = self.infer_expr(default_value, hierarchy, &mut env, false);
            let InferredType::Known {
                class_name: value_type,
                ..
            } = &inferred
            else {
                continue; // Dynamic defaults are fine
            };
            if !Self::is_assignable_to(value_type, &declared_type, hierarchy) {
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Type mismatch: state `{}` declared as {declared_type}, default is {value_type}",
                            decl.name.name
                        ),
                        decl.span,
                    )
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(format!(
                        "Default value type {value_type} is not compatible with {declared_type}"
                    )),
                );
            }
        }
    }

    /// Returns true if `value_type` is assignable to `declared_type`.
    ///
    /// A type is assignable if it is the same type or a subclass of the declared type.
    /// For example, Integer is assignable to Number because Integer's superclass
    /// chain includes Number.
    ///
    /// For union declared types (containing `|`), the value is assignable if it's
    /// compatible with any member. For generic types (`Result(Integer, Error)`),
    /// compares the base type name. Singleton (`#`) types are still permissive.
    pub(super) fn is_assignable_to(
        value_type: &EcoString,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        if value_type == declared_type {
            return true;
        }
        // Singleton types — permissive until that phase lands
        if declared_type.starts_with('#') {
            return true;
        }
        // Union declared types: value must be compatible with at least one member
        if declared_type.contains('|') {
            return declared_type.split('|').map(str::trim).any(|member| {
                let member_eco: EcoString = Self::resolve_type_keyword_static(member);
                Self::is_type_compatible(value_type, &member_eco, hierarchy)
            });
        }
        // For generic declared types like "Result(Integer, Error)", extract
        // the base type name and compare structurally.
        let declared_base = if let Some(open) = declared_type.find('(') {
            &declared_type[..open]
        } else {
            declared_type.as_str()
        };
        let value_base = if let Some(open) = value_type.find('(') {
            &value_type[..open]
        } else {
            value_type.as_str()
        };
        if value_base == declared_base {
            return true;
        }
        // Check if value_type's base is a subclass of declared_type's base
        let value_eco: EcoString = value_base.into();
        let declared_eco: EcoString = declared_base.into();
        hierarchy
            .superclass_chain(&value_eco)
            .iter()
            .any(|ancestor| ancestor == &declared_eco)
    }

    /// Resolves type-position keywords to their class names (static version).
    ///
    /// Same as `resolve_type_keyword` but takes `&str` for use in validation contexts.
    fn resolve_type_keyword_static(name: &str) -> EcoString {
        match name {
            "nil" => "UndefinedObject".into(),
            "false" => "False".into(),
            "true" => "True".into(),
            _ => EcoString::from(name),
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

        let mut diag = Diagnostic::hint(message, span).with_category(DiagnosticCategory::Dnu);

        // Try to suggest similar selectors
        if let Some(suggestion) =
            Self::find_similar_selector(class_name, selector, hierarchy, is_class_side)
        {
            diag = diag.with_hint(format!("Did you mean '{suggestion}'?"));
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
}
