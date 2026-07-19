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

use super::well_known::WellKnownClass;
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
    ///
    /// **Receiver resolution (BT-2761):** the target method whose parameter
    /// annotations drive the check is resolved from the receiver's inferred
    /// type:
    /// - `Known{C}` (instance-side receiver) → `C`'s *instance* methods;
    /// - `Meta{C}` (class-object receiver, ADR 0083 — e.g. `Json generate: x`)
    ///   → `C`'s *class-side* methods, so protocol-typed parameters of class
    ///   methods are conformance-checked too.
    ///
    /// Other receiver types (`Dynamic`, `Union`, …) are skipped conservatively
    /// per the checker's open-world gating conventions.
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
                // Check if the receiver's method has protocol-typed params.
                // Instance-side receivers resolve against instance methods;
                // Meta (class object) receivers against class-side methods
                // (BT-2761).
                let sel_name = selector.name();
                let method = match self.type_map.get(receiver.span()) {
                    Some(InferredType::Known { class_name, .. }) => {
                        hierarchy.find_method(class_name, &sel_name)
                    }
                    Some(InferredType::Meta { class_name, .. }) => {
                        hierarchy.find_class_method(class_name, &sel_name)
                    }
                    _ => None,
                };
                if let Some(method) = method {
                    for (i, arg) in arguments.iter().enumerate() {
                        if let Some(Some(expected_ty)) = method.param_types.get(i) {
                            let Some(arg_type) = self.type_map.get(arg.span()) else {
                                continue;
                            };
                            let arg_type = arg_type.clone();
                            // ADR 0068 §Protocol Composition / ADR 0102
                            // §1/§3 (BT-2743): a parameter declared
                            // `:: P1 & P2` requires conformance to
                            // *every* protocol part. `type_name()`
                            // renders the annotation as `"P1 & P2"`;
                            // split it and check each protocol part
                            // independently instead of treating the
                            // whole compound string as one (unregistered)
                            // name.
                            if let Some(parts) = Self::split_intersection_type_string(expected_ty) {
                                for part in parts {
                                    if Self::is_protocol_type(part, hierarchy, protocol_registry) {
                                        self.check_protocol_argument_conformance(
                                            &arg_type,
                                            part,
                                            *span,
                                            hierarchy,
                                            protocol_registry,
                                        );
                                    }
                                }
                            } else if Self::is_protocol_type(
                                expected_ty,
                                hierarchy,
                                protocol_registry,
                            ) {
                                self.check_protocol_argument_conformance(
                                    &arg_type,
                                    expected_ty,
                                    *span,
                                    hierarchy,
                                    protocol_registry,
                                );
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

    /// Records every alias name a protocol's declared method signatures
    /// (instance- and class-side) transitively depend on, into
    /// `self.referenced_aliases` (ADR 0108 hot-reload re-check trigger,
    /// BT-2899 / BT-2917 follow-up).
    ///
    /// Protocol method signatures ([`crate::ast::ProtocolMethodSignature`])
    /// have no body — unlike a class method, there is nothing for the main
    /// [`Self::check_module`] pass to type-check, so
    /// [`Self::set_param_types`]'s `referenced_aliases.extend(deps)` (the
    /// call site that normally records this for a `::`-annotated class
    /// method parameter) never runs for them. Without this dedicated walk, a
    /// live redefinition of an alias named only in a `Protocol define:`
    /// body's signature (e.g. `direction :: Direction`) would never register
    /// a dependency edge for anything, even though the identical annotation
    /// on a class method's signature works correctly.
    ///
    /// Every parameter and return-type annotation is resolved unconditionally
    /// (not just `TypeAnnotation::Generic`, unlike
    /// [`Self::check_bounds_in_type_annotation`]'s bounds-only walk) since an
    /// alias dependency can appear on a plain `Simple` annotation too.
    pub(super) fn record_protocol_signature_referenced_aliases(
        &mut self,
        module: &Module,
        protocol_registry: &ProtocolRegistry,
    ) {
        let subst = super::type_resolver::SubstitutionMap::new();
        for protocol in &module.protocols {
            for sig in protocol
                .method_signatures
                .iter()
                .chain(protocol.class_method_signatures.iter())
            {
                for param in &sig.parameters {
                    if let Some(ref ann) = param.type_annotation {
                        let (_, deps) =
                            super::type_resolver::resolve_type_annotation_with_alias_deps(
                                ann,
                                &subst,
                                Some(protocol_registry),
                                self.alias_registry.as_ref(),
                            );
                        self.referenced_aliases.extend(deps);
                    }
                }
                if let Some(ref ann) = sig.return_type {
                    let (_, deps) = super::type_resolver::resolve_type_annotation_with_alias_deps(
                        ann,
                        &subst,
                        Some(protocol_registry),
                        self.alias_registry.as_ref(),
                    );
                    self.referenced_aliases.extend(deps);
                }
            }
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
                // Resolve the type args to InferredTypes. ADR 0108 (BT-2895):
                // thread the alias registry (unlike the thin
                // `Self::resolve_type_annotation` wrapper this replaced) so
                // a type argument that is itself an alias name — e.g.
                // `Logger(Small)` where `type Small = Integer` — resolves to
                // its structural expansion before being checked against the
                // class's declared bound, instead of an opaque unknown
                // class that would silently skip the bound check.
                //
                // ADR 0108 hot-reload re-check trigger (BT-2899): a plain
                // `for` loop (not `.map()`) so each iteration's dependency
                // set can be folded into `self.referenced_aliases` directly
                // — a closure capturing both `self.alias_registry` (read)
                // and `self.referenced_aliases` (write) would need disjoint
                // field capture through a `&mut self` receiver, which this
                // sidesteps entirely.
                let mut type_args: Vec<InferredType> = Vec::with_capacity(parameters.len());
                for p in parameters {
                    let (ty, deps) = super::type_resolver::resolve_type_annotation_with_alias_deps(
                        p,
                        &super::type_resolver::SubstitutionMap::new(),
                        None,
                        self.alias_registry.as_ref(),
                    );
                    self.referenced_aliases.extend(deps);
                    type_args.push(ty);
                }

                // BT-1861: Warn when type args are provided for a class with no type params.
                // Block is exempt — parameterized Block annotations (e.g., Block(E, Boolean))
                // are a documentation convention for describing closure signatures.
                let has_type_params =
                    if WellKnownClass::from_str(&base.name) == Some(WellKnownClass::Block) {
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
            TypeAnnotation::Difference { base, excluded, .. } => {
                self.check_bounds_in_type_annotation(base, hierarchy, protocol_registry);
                self.check_bounds_in_type_annotation(excluded, hierarchy, protocol_registry);
            }
            TypeAnnotation::Intersection { left, right, .. } => {
                self.check_bounds_in_type_annotation(left, hierarchy, protocol_registry);
                self.check_bounds_in_type_annotation(right, hierarchy, protocol_registry);
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

        // BT-2928: resolve the declared type through the alias table before
        // comparing against the default value's inferred type — mirroring
        // `check_state_defaults`'s alias-aware resolution (its same-file
        // sibling, fixed in BT-2923/ADR 0108). This method is only reached
        // for `TypeAnnotation::Generic` fields (see
        // `check_generic_variance_in_module`'s caller-side gate), so the
        // alias in question is typically a type *argument* — e.g. `field:
        // items :: Array(RestartStrategy) = Array new` — and
        // `resolve_type_annotation_with_alias_deps` recurses into `Generic`
        // parameters, expanding the alias before `is_assignable_to_with_variance`
        // ever sees it, instead of comparing against its opaque unresolved name.
        let (resolved_declared, alias_deps) =
            super::type_resolver::resolve_type_annotation_with_alias_deps(
                type_annotation,
                &super::type_resolver::SubstitutionMap::new(),
                None,
                self.alias_registry.as_ref(),
            );
        self.referenced_aliases.extend(alias_deps);
        let declared_type = Self::inferred_type_to_string(&resolved_declared);
        let mut env = TypeEnv::new();
        env.set_local("self", InferredType::known(class.name.name.clone()));
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
            // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
            let declared_display = InferredType::class_name_for_diagnostic(declared_type.as_str());
            let value_display = InferredType::class_name_for_diagnostic(value_type.as_str());
            self.diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Type mismatch: state `{}` declared as {declared_display}, default is {value_display}",
                        state_decl.name.name
                    ),
                    state_decl.span,
                )
                .with_category(DiagnosticCategory::Type)
                .with_hint(format!(
                    "Default value type {value_display} is not compatible with {declared_display}"
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
            // Metatype renders as the source spelling `C class` (ADR 0083),
            // matching `InferredType::display_*`.
            InferredType::Meta { class_name, .. } => EcoString::from(format!("{class_name} class")),
            InferredType::Dynamic(_) => EcoString::from("Dynamic"),
            InferredType::Never => EcoString::from("Never"),
            InferredType::Union { members, .. } => EcoString::from(
                members
                    .iter()
                    .map(|m| Self::inferred_type_to_string(m).to_string())
                    .collect::<Vec<_>>()
                    .join(" | "),
            ),
            // Negation renders as `base \ excluded` (ADR 0102), matching
            // `InferredType::display_*`. Parenthesise a union excluded so
            // `\` vs `|` precedence is unambiguous (`Symbol \ (#a | #b)`).
            InferredType::Negation { base, excluded, .. } => {
                let base_str = Self::inferred_type_to_string(base);
                let excl_str = Self::inferred_type_to_string(excluded);
                if matches!(excluded.as_ref(), InferredType::Union { .. }) {
                    EcoString::from(format!("{base_str} \\ ({excl_str})"))
                } else {
                    EcoString::from(format!("{base_str} \\ {excl_str}"))
                }
            }
            // Intersection renders as `A & B & …` (ADR 0102/BT-2743), matching
            // `InferredType::display_*`. A union member is only reachable via
            // explicit grouping; parenthesise to preserve meaning.
            InferredType::Intersection { members, .. } => EcoString::from(
                members
                    .iter()
                    .map(|m| {
                        let rendered = Self::inferred_type_to_string(m);
                        if matches!(m, InferredType::Union { .. }) {
                            format!("({rendered})")
                        } else {
                            rendered.to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join(" & "),
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
                                                        // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
                                                        let expected_display =
                                                            InferredType::class_name_for_diagnostic(
                                                                expected_ty.as_str(),
                                                            );
                                                        let arg_display =
                                                            InferredType::class_name_for_diagnostic(
                                                                arg_str.as_str(),
                                                            );
                                                        self.diagnostics.push(
                                                            Diagnostic::warning(
                                                                format!(
                                                                    "Argument {param_pos} of '{sel_name}' on {class_name} expects {expected_display}, got {arg_display}"
                                                                ),
                                                                *span,
                                                            )
                                                            .with_category(DiagnosticCategory::Type)
                                                            .with_hint(format!(
                                                                "Type arguments are not compatible: expected {expected_display}, got {arg_display}"
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
