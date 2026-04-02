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

use std::collections::HashMap;

use crate::ast::{Expression, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::semantic_analysis::string_utils::edit_distance;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::{EcoString, eco_format};

use super::{InferredType, TypeChecker, TypeEnv};

impl TypeChecker {
    /// Classify how well a union's known members match a predicate.
    ///
    /// Returns `None` if any member is Dynamic (conservative skip).
    /// Otherwise returns the count of members that satisfy `pred` and the total.
    fn classify_union_members<F>(
        members: &[InferredType],
        pred: F,
    ) -> Option<(usize, usize, Vec<&EcoString>)>
    where
        F: Fn(&EcoString) -> bool,
    {
        let known_members: Vec<&EcoString> = members.iter().filter_map(|m| m.as_known()).collect();
        if known_members.len() < members.len() {
            return None; // Contains Dynamic — skip
        }
        let compatible = known_members.iter().filter(|m| pred(m)).count();
        let incompatible: Vec<&EcoString> =
            known_members.iter().filter(|m| !pred(m)).copied().collect();
        Some((compatible, known_members.len(), incompatible))
    }

    /// Collect deduplicated missing protocol methods across union members.
    fn collect_missing_protocol_methods(
        members: &[InferredType],
        protocol: &str,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) -> String {
        let mut all_missing: Vec<EcoString> = Vec::new();
        for member in members {
            if let Some(name) = member.as_known() {
                if let Err(missing) = protocol_registry.check_conformance(name, protocol, hierarchy)
                {
                    for m in &missing {
                        if !all_missing.iter().any(|existing| existing == m) {
                            all_missing.push(m.clone());
                        }
                    }
                }
            }
        }
        all_missing
            .iter()
            .map(|s| format!("'{s}'"))
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Check a class-side message send (e.g., `Counter spawn`, `Object new`).
    ///
    /// When `arg_types` is non-empty and the class has type parameters,
    /// constructor inference (ADR 0068 Phase 1c) kicks in: the type checker
    /// maps the class method's parameter types (e.g., `T`) against the concrete
    /// argument types (e.g., `Integer`) to infer the class's type arguments.
    /// Unresolved params default to `Dynamic`.
    pub(super) fn check_class_side_send(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
        arg_types: &[InferredType],
    ) -> InferredType {
        if !hierarchy.has_class(class_name) {
            return InferredType::Dynamic;
        }

        // ADR 0071 Phase 3 (BT-1702): E0403 — cross-package send to internal class method
        self.check_internal_method_access(class_name, selector, span, hierarchy, true);

        // Check if class-side method exists (skip warning for DNU override classes)
        let has_class_method = hierarchy.find_class_method(class_name, selector).is_some();

        // BT-1763: Sealed value types (like Erlang) dispatch class-side messages
        // through instance dispatch. Instance-side DNU suppresses class-side
        // warnings only for sealed classes — not all classes with instance DNU,
        // which would hide valid diagnostics on normal classes.
        let is_sealed_with_instance_dnu = hierarchy.has_instance_dnu_override(class_name)
            && hierarchy
                .get_class(class_name)
                .is_some_and(|info| info.is_sealed);

        if !has_class_method
            && !hierarchy.has_class_dnu_override(class_name)
            && !is_sealed_with_instance_dnu
            // BT-1736: Cross-file inheritance — if the parent class is not in
            // the hierarchy, we can't know the full class-side method set.
            // Instance-side already checks this; class-side must do the same.
            && !hierarchy.has_cross_file_parent(class_name)
        {
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
            "spawn" | "spawnWith:" | "new" | "new:" => {
                Self::infer_constructor_type(class_name, hierarchy, selector, arg_types)
            }
            _ => {
                if let Some(method) = hierarchy.find_class_method(class_name, selector) {
                    if let Some(ref ret_ty) = method.return_type {
                        // `Self` resolves to the static receiver class —
                        // with constructor inference for generic classes (ADR 0068 Phase 1c)
                        if ret_ty.as_str() == "Self" {
                            return Self::infer_constructor_type(
                                class_name, hierarchy, selector, arg_types,
                            );
                        }
                        return InferredType::known(ret_ty.clone());
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

    /// Infer the constructor return type for a generic class (ADR 0068 Phase 1c).
    ///
    /// Given a class method call like `GenResult ok: 42`, maps the method's
    /// parameter types (e.g., `T`) against the concrete argument types (e.g.,
    /// `Integer`) to produce `GenResult(Integer, Dynamic)`.
    ///
    /// For non-generic classes, returns `Known(class_name)` with no type args.
    fn infer_constructor_type(
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
        selector: &str,
        arg_types: &[InferredType],
    ) -> InferredType {
        let Some(class_info) = hierarchy.get_class(class_name) else {
            return InferredType::known(class_name.clone());
        };

        // Non-generic class — no type args to infer
        if class_info.type_params.is_empty() {
            return InferredType::known(class_name.clone());
        }

        // Build inference map from class method param types → argument types
        let mut inferred: HashMap<EcoString, InferredType> = HashMap::new();

        if let Some(method) = hierarchy.find_class_method(class_name, selector) {
            for (param_ty_opt, arg_ty) in method.param_types.iter().zip(arg_types.iter()) {
                if let Some(param_ty) = param_ty_opt {
                    // If param type is a class type param (e.g., "T"), map it to the arg type
                    if class_info.type_params.contains(param_ty) {
                        inferred.insert(param_ty.clone(), arg_ty.clone());
                    }
                }
            }
        }

        // Build type_args in class param order, defaulting unresolved to Dynamic
        let type_args: Vec<InferredType> = class_info
            .type_params
            .iter()
            .map(|p| inferred.remove(p).unwrap_or(InferredType::Dynamic))
            .collect();

        InferredType::Known {
            class_name: class_name.clone(),
            type_args,
            provenance: super::TypeProvenance::Inferred(Span::default()),
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
            // BT-1833: If the type is a protocol (from respondsTo: narrowing),
            // validate the selector against the protocol's required methods.
            if let Some(ref registry) = self.protocol_registry {
                if let Some(proto_info) = registry.get(class_name) {
                    let required = proto_info.all_required_selectors(registry);
                    if !required.iter().any(|s| s.as_str() == selector) {
                        self.emit_unknown_selector_warning(
                            class_name, selector, span, hierarchy, false,
                        );
                    }
                }
            }
            return;
        }

        // ADR 0071 Phase 3 (BT-1702): E0403 — cross-package send to internal method
        self.check_internal_method_access(class_name, selector, span, hierarchy, false);

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

    /// ADR 0071 Phase 3 (BT-1702): E0403 — cross-package send to internal method.
    ///
    /// When the receiver type is known and the target method is `internal` to
    /// another package, emit a hard error. Skipped when no current package is
    /// set (REPL, single-file scripts) or when sending to a method in the
    /// same package.
    pub(super) fn check_internal_method_access(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
    ) {
        let Some(ref caller_package) = self.current_package else {
            return;
        };

        let method = if is_class_side {
            hierarchy.find_class_method(class_name, selector)
        } else {
            hierarchy.find_method(class_name, selector)
        };

        let Some(method) = method else {
            return;
        };

        if !method.is_internal {
            return;
        }

        // Determine the package of the class that defines the method
        let method_package = hierarchy
            .get_class(&method.defined_in)
            .and_then(|c| c.package.as_ref());

        let Some(method_package) = method_package else {
            // No package info — cannot enforce, skip
            return;
        };

        // Same package — access is allowed
        if caller_package == method_package {
            return;
        }

        let message: EcoString = format!(
            "Method '{selector}' is internal to package '{method_package}' and cannot be called from '{caller_package}'"
        )
        .into();
        let defined_in = &method.defined_in;
        self.diagnostics.push(
            Diagnostic::error(message, span)
                .with_hint(format!(
                    "'{selector}' is declared 'internal' in '{defined_in}'"
                ))
                .with_category(DiagnosticCategory::Visibility),
        );
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
        // Singleton types: #foo is compatible with Symbol (its supertype)
        // and with itself (already handled by equality above).
        // Generic type params (K, V, T, etc.) are always compatible (conservative).
        if expected.starts_with('#') {
            return actual == "Symbol"
                || actual == expected
                || super::is_generic_type_param(actual);
        }
        if actual.starts_with('#') {
            // Singletons are subtypes of Symbol — check if expected is Symbol
            // or an ancestor of Symbol in the class hierarchy.
            if super::is_generic_type_param(expected) {
                return true;
            }
            let symbol: EcoString = "Symbol".into();
            return Self::is_type_compatible(&symbol, expected, hierarchy);
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
    #[allow(clippy::too_many_arguments)] // BT-1588: arg_exprs + env needed for origin tracing
    pub(super) fn check_argument_types(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
        arg_exprs: Option<&[Expression]>,
        env: Option<&super::TypeEnv>,
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
            match arg_ty {
                InferredType::Known {
                    class_name: actual_ty,
                    ..
                } => {
                    if !Self::is_type_compatible(actual_ty, expected_ty, hierarchy) {
                        let param_pos = i + 1;
                        // BT-1588: Use hint severity for generic type params (likely false positive)
                        let is_generic = super::is_generic_type_param(actual_ty);
                        let mut diag = if is_generic {
                            Diagnostic::hint(
                                format!(
                                    "Argument {param_pos} of '{selector}' on {class_name} expects {expected_ty}, got {actual_ty}"
                                ),
                                span,
                            )
                            .with_hint(format!(
                                "This is likely a false positive — `{actual_ty}` is a generic type parameter that may be {expected_ty} at runtime. \
                                 Use `@expect type` to suppress"
                            ))
                        } else {
                            Diagnostic::warning(
                                format!(
                                    "Argument {param_pos} of '{selector}' on {class_name} expects {expected_ty}, got {actual_ty}"
                                ),
                                span,
                            )
                            .with_hint(format!("Expected {expected_ty} (or a subclass), got {actual_ty}"))
                        };
                        // BT-1588: Attach origin note if available
                        if let (Some(exprs), Some(e)) = (arg_exprs, env) {
                            if let Some(Expression::Identifier(ident)) = exprs.get(i) {
                                if let Some(origin) = e.get_origin(&ident.name) {
                                    diag = diag
                                        .with_note(origin.description.clone(), Some(origin.span));
                                }
                            }
                        }
                        self.diagnostics
                            .push(diag.with_category(DiagnosticCategory::Type));
                    }
                }
                InferredType::Union { members, .. } => {
                    // BT-1832: Check all union members against the expected type.
                    let Some((compat, total, incompatible)) =
                        Self::classify_union_members(members, |m| {
                            Self::is_type_compatible(m, expected_ty, hierarchy)
                        })
                    else {
                        continue; // Contains Dynamic — skip
                    };
                    if compat == total {
                        continue; // All match → pass
                    }
                    let param_pos = i + 1;
                    let union_display = arg_ty
                        .display_name()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let mut diag = if compat == 0 {
                        Diagnostic::warning(
                            format!("Argument {param_pos} of '{selector}' on {class_name} expects {expected_ty}, got {union_display}"),
                            span,
                        )
                        .with_hint(format!("No member of {union_display} is compatible with {expected_ty}"))
                    } else {
                        let list = incompatible
                            .iter()
                            .map(|m| m.as_str())
                            .collect::<Vec<_>>()
                            .join(", ");
                        Diagnostic::hint(
                            format!("Argument {param_pos} of '{selector}' on {class_name} expects {expected_ty}, got {union_display}"),
                            span,
                        )
                        .with_hint(format!("Some members of the union are not compatible with {expected_ty}: {list}"))
                    };
                    if let (Some(exprs), Some(e)) = (arg_exprs, env) {
                        if let Some(Expression::Identifier(ident)) = exprs.get(i) {
                            if let Some(origin) = e.get_origin(&ident.name) {
                                diag =
                                    diag.with_note(origin.description.clone(), Some(origin.span));
                            }
                        }
                    }
                    self.diagnostics
                        .push(diag.with_category(DiagnosticCategory::Type));
                }
                InferredType::Dynamic => {} // Dynamic arguments — skip (conservative)
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
            TypeAnnotation::Singleton { name, .. } => InferredType::known(eco_format!("#{name}")),
        };

        match &expected {
            InferredType::Known {
                class_name: expected_ty,
                ..
            } => {
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
            InferredType::Union { members, .. } => {
                // Body type must be compatible with at least one union member
                let compatible = members.iter().any(|member| {
                    member
                        .as_known()
                        .is_none_or(|name| Self::is_type_compatible(actual_ty, name, hierarchy))
                });
                if !compatible {
                    let selector = method.selector.name();
                    let union_display = expected
                        .display_name()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
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
        arg_origin: Option<&(EcoString, Option<Span>)>,
    ) {
        let is_numeric = |ty: &str| hierarchy.is_numeric_type(ty);
        let is_arithmetic = matches!(operator, "+" | "-" | "*" | "/");
        let is_comparison = matches!(operator, "<" | ">" | "<=" | ">=");
        let is_generic = super::is_generic_type_param(arg_ty);

        // Arithmetic operators on numeric types require numeric arguments
        if is_arithmetic && is_numeric(receiver_ty) && !is_numeric(arg_ty) {
            // BT-1588: Use hint severity for generic type params (likely false positive)
            let mut diag = if is_generic {
                Diagnostic::hint(
                    format!(
                        "`{operator}` on {receiver_ty} expects a numeric argument, got {arg_ty}"
                    ),
                    span,
                )
            } else {
                Diagnostic::warning(
                    format!(
                        "`{operator}` on {receiver_ty} expects a numeric argument, got {arg_ty}"
                    ),
                    span,
                )
            };
            diag = diag
                .with_category(DiagnosticCategory::Type)
                .with_hint("Arithmetic operators require Integer or Float operands");
            if let Some((desc, origin_span)) = arg_origin {
                diag = diag.with_note(desc.clone(), *origin_span);
            }
            self.diagnostics.push(diag);
            return;
        }

        // String concatenation with ++ expects a String argument
        if operator == "++"
            && receiver_ty.as_str() == "String"
            && arg_ty.as_str() != "String"
            && arg_ty.as_str() != "Symbol"
            && !arg_ty.starts_with('#')
        {
            // BT-1588: Use hint severity for generic type params (likely false positive)
            let mut diag = if is_generic {
                Diagnostic::hint(
                    format!("`++` on String expects a String argument, got {arg_ty}"),
                    span,
                )
                .with_hint(
                    "This is likely a false positive — the generic type may be String at runtime. \
                     Use `displayString` or `@expect type` to suppress",
                )
            } else {
                Diagnostic::warning(
                    format!("`++` on String expects a String argument, got {arg_ty}"),
                    span,
                )
                .with_hint("Convert the argument to String first")
            };
            diag = diag.with_category(DiagnosticCategory::Type);
            if let Some((desc, origin_span)) = arg_origin {
                diag = diag.with_note(desc.clone(), *origin_span);
            }
            self.diagnostics.push(diag);
            return;
        }

        // Comparison operators on numeric types require numeric arguments
        if is_comparison && is_numeric(receiver_ty) && !is_numeric(arg_ty) {
            let mut diag = if is_generic {
                Diagnostic::hint(
                    format!(
                        "`{operator}` on {receiver_ty} expects a numeric argument, got {arg_ty}"
                    ),
                    span,
                )
            } else {
                Diagnostic::warning(
                    format!(
                        "`{operator}` on {receiver_ty} expects a numeric argument, got {arg_ty}"
                    ),
                    span,
                )
            };
            diag = diag
                .with_category(DiagnosticCategory::Type)
                .with_hint("Comparison operators require compatible types");
            if let Some((desc, origin_span)) = arg_origin {
                diag = diag.with_note(desc.clone(), *origin_span);
            }
            self.diagnostics.push(diag);
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
        let Some(InferredType::Known { class_name, .. }) = env.get("self") else {
            return;
        };
        let Some(declared_type) = hierarchy.state_field_type(&class_name, &field.name) else {
            return; // No type annotation on this field
        };
        match value_ty {
            InferredType::Known {
                class_name: value_type,
                ..
            } => {
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
            InferredType::Union { members, .. } => {
                // BT-1832: Check all union members against the declared field type.
                let Some((compat, total, incompatible)) =
                    Self::classify_union_members(members, |m| {
                        Self::is_assignable_to(m, &declared_type, hierarchy)
                    })
                else {
                    return; // Contains Dynamic — skip
                };
                if compat == total {
                    return; // All match → pass
                }
                let union_display = value_ty
                    .display_name()
                    .unwrap_or_else(|| EcoString::from("Dynamic"));
                let diag = if compat == 0 {
                    Diagnostic::warning(
                        format!("Type mismatch: field `{}` declared as {declared_type}, got {union_display}", field.name),
                        span,
                    )
                    .with_hint(format!("No member of {union_display} is compatible with {declared_type}"))
                } else {
                    let list = incompatible
                        .iter()
                        .map(|m| m.as_str())
                        .collect::<Vec<_>>()
                        .join(", ");
                    Diagnostic::hint(
                        format!("Type mismatch: field `{}` declared as {declared_type}, got {union_display}", field.name),
                        span,
                    )
                    .with_hint(format!("Some members of the union are not compatible with {declared_type}: {list}"))
                };
                self.diagnostics
                    .push(diag.with_category(DiagnosticCategory::Type));
            }
            InferredType::Dynamic => {} // Dynamic values — skip (conservative)
        }
    }

    /// Check state default values match declared types at class definition time.
    pub(super) fn check_state_defaults(
        &mut self,
        class: &crate::ast::ClassDefinition,
        hierarchy: &ClassHierarchy,
    ) {
        // Collect type parameter names once — we can't validate concrete
        // assignability for generic fields at class definition time.
        let type_param_names: Vec<&str> = class
            .type_params
            .iter()
            .map(|tp| tp.name.name.as_str())
            .collect();
        for decl in &class.state {
            let Some(ref type_annotation) = decl.type_annotation else {
                continue;
            };
            let Some(ref default_value) = decl.default_value else {
                continue;
            };
            let declared_type = type_annotation.type_name();
            if type_param_names.contains(&declared_type.as_str()) {
                continue;
            }
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

    /// Warn about typed state fields that have no default value and are not nilable.
    ///
    /// `state: name :: String` (no default) may be used uninitialized — emit a warning.
    /// `state: name :: String | Nil` is fine (nil is valid), as is `state: name :: String = ""`.
    pub(super) fn check_uninitialized_state(&mut self, class: &crate::ast::ClassDefinition) {
        // Sibling-default heuristic (BT-1837): only warn about uninitialized fields
        // when the class has a mix of defaulted and undefaulted typed fields. If NO
        // typed fields have defaults, the class is factory-constructed (spawnWith:/new:)
        // and all fields are set by the factory — warning would be a false positive.
        let has_any_typed_default = class
            .state
            .iter()
            .any(|d| d.type_annotation.is_some() && d.default_value.is_some());
        if !has_any_typed_default {
            return; // All-factory class — no warnings
        }

        // Collect type parameter names — generic fields can't be validated.
        let type_param_names: Vec<&str> = class
            .type_params
            .iter()
            .map(|tp| tp.name.name.as_str())
            .collect();
        for decl in &class.state {
            let Some(ref type_annotation) = decl.type_annotation else {
                continue; // No type annotation — nothing to check
            };
            if decl.default_value.is_some() {
                continue; // Has a default — won't be uninitialized
            }
            let declared_type = type_annotation.type_name();
            if type_param_names.contains(&declared_type.as_str()) {
                continue; // Generic type parameter — skip
            }
            if Self::is_nilable_type(type_annotation) {
                continue; // Nilable union — nil is a valid initial value
            }
            self.diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "State field `{}` declared as {declared_type} has no default value and may be used uninitialized",
                        decl.name.name
                    ),
                    decl.span,
                )
                .with_category(DiagnosticCategory::Type)
                .with_hint(format!(
                    "Add a default value (e.g., `state: {} :: {declared_type} = ...`) or make it nilable (`{declared_type} | Nil`)",
                    decl.name.name
                )),
            );
        }
    }

    /// Returns true if a type annotation includes `Nil` as a union member,
    /// making nil a valid value for the field.
    fn is_nilable_type(annotation: &TypeAnnotation) -> bool {
        match annotation {
            TypeAnnotation::Simple(id) => id.name == "Nil",
            TypeAnnotation::Union { types, .. } => types.iter().any(Self::is_nilable_type),
            TypeAnnotation::Singleton { .. }
            | TypeAnnotation::Generic { .. }
            | TypeAnnotation::FalseOr { .. }
            | TypeAnnotation::SelfType { .. } => false,
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
    /// compares the base type name. Singleton (`#`) types require an exact match or `Symbol`.
    pub(super) fn is_assignable_to(
        value_type: &EcoString,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        if value_type == declared_type {
            return true;
        }
        // Singleton declared types: value must be the same singleton or Symbol
        if declared_type.starts_with('#') {
            return value_type == "Symbol" || value_type == declared_type;
        }
        // Singleton value types: treat as Symbol for hierarchy assignability
        // (e.g., #ok is assignable to Object, Symbol | nil, etc.)
        if value_type.starts_with('#') {
            let symbol: EcoString = "Symbol".into();
            return Self::is_assignable_to(&symbol, declared_type, hierarchy);
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

    /// Returns true if `value_type` is assignable to `declared_type` with
    /// variance-aware generic type argument checking (ADR 0068 Phase 2f).
    ///
    /// Extends [`is_assignable_to`] with covariance support for sealed Value classes:
    /// when the declared type is generic (e.g., `Array(Printable)`) and the value type
    /// is the same generic class with different type args (e.g., `Array(Integer)`),
    /// checks each type argument covariantly — the value's type arg must be assignable
    /// to the declared type arg. This is sound because sealed Value classes are immutable.
    ///
    /// Actor classes remain invariant: type args must match exactly.
    ///
    /// Falls back to [`is_assignable_to`] when no generic type args are present or
    /// when the protocol registry is not needed.
    pub(super) fn is_assignable_to_with_variance(
        value_type: &EcoString,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) -> bool {
        // Delegate to the base check first for non-generic cases
        if !declared_type.contains('(') || !value_type.contains('(') {
            return Self::is_assignable_to(value_type, declared_type, hierarchy);
        }

        // Both are generic — extract base names and type arg strings
        let (declared_base, declared_args) = Self::parse_generic_type_string(declared_type);
        let (value_base, value_args) = Self::parse_generic_type_string(value_type);

        // Different base types — check normal assignability
        if value_base != declared_base {
            return Self::is_assignable_to(value_type, declared_type, hierarchy);
        }

        // Same base, same args — trivially compatible
        if value_args == declared_args {
            return true;
        }

        // Same base, different args — check variance
        if hierarchy.is_covariant_class(&declared_base) {
            // Covariant: each value type arg must be compatible with declared type arg.
            // A type arg is compatible if:
            // 1. It's the same type or a subclass, OR
            // 2. The declared type arg is a protocol and the value type conforms to it
            for (val_arg, decl_arg) in value_args.iter().zip(declared_args.iter()) {
                let val_eco: EcoString = val_arg.as_str().into();
                let decl_eco: EcoString = decl_arg.as_str().into();
                if val_eco == decl_eco {
                    continue;
                }
                // Check protocol conformance first — the declared type arg might be a protocol.
                // Protocol types are NOT classes in the hierarchy, so is_type_compatible
                // would return true (conservative unknown-type fallback), masking real errors.
                if Self::is_protocol_type(decl_arg, hierarchy, protocol_registry) {
                    let base_protocol = if let Some(open) = decl_arg.find('(') {
                        &decl_arg[..open]
                    } else {
                        decl_arg.as_str()
                    };
                    if protocol_registry
                        .check_conformance(&val_eco, base_protocol, hierarchy)
                        .is_ok()
                    {
                        continue;
                    }
                    // Value type does not conform to the protocol
                    return false;
                }
                // Check class hierarchy compatibility (subclass check)
                if Self::is_type_compatible(&val_eco, &decl_eco, hierarchy) {
                    continue;
                }
                // Type arg is not compatible
                return false;
            }
            true
        } else {
            // Invariant: type args must match exactly (already checked they differ above)
            // Fall back to base-name-only compatibility (current behaviour)
            Self::is_assignable_to(value_type, declared_type, hierarchy)
        }
    }

    /// Parse a generic type string like `"Array(Integer)"` into base name and type arg strings.
    ///
    /// Returns `("Array", ["Integer"])` for `"Array(Integer)"`.
    /// Returns `("Result", ["Integer", "Error"])` for `"Result(Integer, Error)"`.
    /// For non-generic types, returns the full name and an empty vec.
    ///
    /// **Limitation:** Uses a simple comma-split, which does not handle nested generic
    /// type args with multiple parameters (e.g., `Map(Result(A, B), C)` would be
    /// incorrectly split). This is acceptable because Beamtalk's current type system
    /// does not produce such deeply nested multi-parameter generic annotations in
    /// string form.
    pub(super) fn parse_generic_type_string(type_str: &str) -> (String, Vec<String>) {
        if let Some(open) = type_str.find('(') {
            let base = type_str[..open].to_string();
            let args_str = &type_str[open + 1..type_str.len() - 1]; // strip parens
            let args: Vec<String> = args_str.split(',').map(|s| s.trim().to_string()).collect();
            (base, args)
        } else {
            (type_str.to_string(), vec![])
        }
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

    /// Check protocol conformance for a call-site argument.
    ///
    /// When a parameter type is a protocol name and the argument type is a known
    /// class, checks structural conformance. Emits a warning if the class does
    /// not conform to the protocol.
    ///
    /// **References:** ADR 0068 Phase 2b — "Type checker: protocol name in type
    /// annotation → structural conformance check"
    pub(super) fn check_protocol_argument_conformance(
        &mut self,
        arg_type: &InferredType,
        expected_protocol: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        // Extract base protocol name for generic protocols (e.g., "Enumerable(T)" → "Enumerable")
        let base_protocol = if let Some(open) = expected_protocol.find('(') {
            &expected_protocol[..open]
        } else {
            expected_protocol
        };

        match arg_type {
            InferredType::Known { class_name, .. } => {
                let Some(_protocol) = protocol_registry.get(base_protocol) else {
                    return; // Not a protocol — handled by normal type checking
                };

                match protocol_registry.check_conformance(class_name, base_protocol, hierarchy) {
                    Ok(()) => {} // Conforms — no warning
                    Err(missing) => {
                        let missing_list = missing
                            .iter()
                            .map(|s| format!("'{s}'"))
                            .collect::<Vec<_>>()
                            .join(", ");
                        self.diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "{class_name} does not conform to protocol {base_protocol}"
                                ),
                                span,
                            )
                            .with_category(DiagnosticCategory::Type)
                            .with_hint(format!("Missing required method(s): {missing_list}")),
                        );
                    }
                }
            }
            InferredType::Dynamic => {
                if protocol_registry.has_protocol(base_protocol) {
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!("Cannot verify {base_protocol} conformance for Dynamic value"),
                            span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint("Add a type annotation to enable protocol conformance checking"),
                    );
                }
            }
            InferredType::Union { members, .. } => {
                if protocol_registry.get(base_protocol).is_none() {
                    return; // Not a protocol — handled by normal type checking
                }
                let Some((compat, total, non_conforming)) =
                    Self::classify_union_members(members, |m| {
                        protocol_registry
                            .check_conformance(m, base_protocol, hierarchy)
                            .is_ok()
                    })
                else {
                    return; // Contains Dynamic — skip
                };
                if compat == total {
                    return; // All conform → pass
                }
                let union_display = arg_type
                    .display_name()
                    .unwrap_or_else(|| EcoString::from("Dynamic"));
                let diag = if compat == 0 {
                    // Collect missing methods across all members
                    let all_missing = Self::collect_missing_protocol_methods(
                        members,
                        base_protocol,
                        hierarchy,
                        protocol_registry,
                    );
                    Diagnostic::warning(
                        format!("{union_display} does not conform to protocol {base_protocol}"),
                        span,
                    )
                    .with_hint(format!(
                        "No member conforms — missing required method(s): {all_missing}"
                    ))
                } else {
                    let list = non_conforming
                        .iter()
                        .map(|m| m.as_str())
                        .collect::<Vec<_>>()
                        .join(", ");
                    Diagnostic::hint(
                        format!(
                            "Not all members of {union_display} conform to protocol {base_protocol}"
                        ),
                        span,
                    )
                    .with_hint(format!("Non-conforming member(s): {list}"))
                };
                self.diagnostics
                    .push(diag.with_category(DiagnosticCategory::Type));
            }
        }
    }

    /// Returns true if the given type name refers to a protocol (not a class).
    ///
    /// Used to determine whether a type annotation should trigger structural
    /// conformance checking (protocol) or nominal subtyping (class).
    pub(super) fn is_protocol_type(
        type_name: &str,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) -> bool {
        // Extract base name for generic types
        let base_name = if let Some(open) = type_name.find('(') {
            &type_name[..open]
        } else {
            type_name
        };

        // A protocol type is one that's in the registry and NOT a class name
        protocol_registry.has_protocol(base_name) && !hierarchy.has_class(base_name)
    }

    /// Check type parameter bounds for a generic type application (ADR 0068 Phase 2d).
    ///
    /// When a generic class has bounded type parameters (e.g., `Logger(T :: Printable)`),
    /// this checks that the concrete type arguments conform to those bounds.
    ///
    /// For example, `Logger(Integer)` is valid because `Integer` conforms to `Printable`,
    /// but `Logger(SomeOpaqueType)` would warn if `SomeOpaqueType` does not.
    ///
    /// Only emits warnings (not errors) per ADR 0025 gradual typing philosophy.
    pub(super) fn check_type_param_bounds(
        &mut self,
        class_name: &EcoString,
        type_args: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) {
        // Resolve type param names and bounds from either ClassInfo or ProtocolInfo
        let (param_names, param_bounds): (Vec<&EcoString>, Vec<&Option<EcoString>>) =
            if let Some(class_info) = hierarchy.get_class(class_name) {
                (
                    class_info.type_params.iter().collect(),
                    class_info.type_param_bounds.iter().collect(),
                )
            } else if let Some(proto_info) = protocol_registry.get(class_name) {
                (
                    proto_info.type_params.iter().collect(),
                    proto_info.type_param_bounds.iter().collect(),
                )
            } else {
                return; // Unknown type — can't check bounds
            };

        if param_bounds.is_empty() || param_bounds.iter().all(|b| b.is_none()) {
            return;
        }

        for (i, (arg, bound_opt)) in type_args.iter().zip(param_bounds.iter()).enumerate() {
            let Some(bound_protocol) = bound_opt else {
                continue; // Unbounded — any type is accepted
            };

            match arg {
                InferredType::Known {
                    class_name: arg_class,
                    ..
                } => {
                    // Check if the concrete type arg conforms to the bound protocol
                    match protocol_registry.check_conformance(arg_class, bound_protocol, hierarchy)
                    {
                        Ok(()) => {} // Conforms — no warning
                        Err(missing) => {
                            let param_name = param_names.get(i).map_or("?", |p| p.as_str());
                            let missing_list = missing
                                .iter()
                                .map(|s| format!("'{s}'"))
                                .collect::<Vec<_>>()
                                .join(", ");
                            self.diagnostics.push(
                                Diagnostic::warning(
                                    format!(
                                        "Type argument {arg_class} for {param_name} in {class_name} does not conform to {bound_protocol}"
                                    ),
                                    span,
                                )
                                .with_category(DiagnosticCategory::Type)
                                .with_hint(format!(
                                    "{arg_class} is missing required method(s): {missing_list}"
                                )),
                            );
                        }
                    }
                }
                InferredType::Union { members, .. } => {
                    // BT-1832: Check all union members against the bound protocol.
                    let Some((compat, total, non_conforming)) =
                        Self::classify_union_members(members, |m| {
                            protocol_registry
                                .check_conformance(m, bound_protocol, hierarchy)
                                .is_ok()
                        })
                    else {
                        continue; // Contains Dynamic — skip
                    };
                    if compat == total {
                        continue; // All conform → pass
                    }
                    let param_name = param_names.get(i).map_or("?", |p| p.as_str());
                    let union_display = arg
                        .display_name()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let diag = if compat == 0 {
                        let missing = Self::collect_missing_protocol_methods(
                            members,
                            bound_protocol,
                            hierarchy,
                            protocol_registry,
                        );
                        Diagnostic::warning(
                            format!("Type argument {union_display} for {param_name} in {class_name} does not conform to {bound_protocol}"),
                            span,
                        )
                        .with_hint(format!("No member of {union_display} conforms — missing required method(s): {missing}"))
                    } else {
                        let list = non_conforming
                            .iter()
                            .map(|m| m.as_str())
                            .collect::<Vec<_>>()
                            .join(", ");
                        Diagnostic::hint(
                            format!("Type argument {union_display} for {param_name} in {class_name}: not all members conform to {bound_protocol}"),
                            span,
                        )
                        .with_hint(format!("Non-conforming member(s): {list}"))
                    };
                    self.diagnostics
                        .push(diag.with_category(DiagnosticCategory::Type));
                }
                // Dynamic values: can't verify bounds (skip silently).
                InferredType::Dynamic => {}
            }
        }
    }
}
