// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Override compatibility checks: parameter and return types of an override against the method it overrides.
//!
//! **DDD Context:** Semantic Analysis

//! `TypeChecker::state_default_types` (BT-3481), keeping the direction
//! one-way instead of calling back into `inference/`.

use crate::ast::TypeAnnotation;
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::source_analysis::{Diagnostic, DiagnosticCategory};
use ecow::EcoString;

use super::super::TypeChecker;
use super::super::shape_assignability::declared_type_has_shape;
use super::super::type_resolver;
use super::super::types::OverrideReturnWidening;

impl TypeChecker {
    /// Emit an error for any parameter annotated with `Self`.
    ///
    /// `Self` is only valid in return position (TypeScript `this` return type).
    /// Using it as a parameter type is unsound in the presence of subclassing
    /// (Eiffel's mistake) and is rejected at the semantic analysis level.
    pub(in super::super) fn check_no_self_in_params(
        &mut self,
        method: &crate::ast::MethodDefinition,
        class_name: &EcoString,
    ) {
        let selector = method.selector.name();
        for param in &method.parameters {
            if matches!(
                param.type_annotation,
                Some(TypeAnnotation::SelfType { .. } | TypeAnnotation::SelfClass { .. })
            ) {
                let label = if matches!(
                    param.type_annotation,
                    Some(TypeAnnotation::SelfClass { .. })
                ) {
                    "Self class"
                } else {
                    "Self"
                };
                self.diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "`{label}` cannot be used as a parameter type in `{class_name}::{selector}` (only valid in return position)"
                        ),
                        param.name.span,
                    )
                    .with_hint(format!(
                        "Use the concrete class name instead of `{label}` for parameter types"
                    ))
                    .with_category(DiagnosticCategory::Type),
                );
            }
        }
    }

    /// Check that a child method's parameter types are compatible with its parent's.
    pub(in super::super) fn check_override_param_compatibility(
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
            // `is_type_compatible` is a string-level nominal-chain walker
            // shared across many call sites — render
            // the structured types once at this boundary.
            let child_t_str: EcoString = child_t.to_string().into();
            let parent_t_str: EcoString = parent_t.to_string().into();
            // ADR 0147: a shape on either side is judged structurally.
            let compatible =
                if declared_type_has_shape(child_t) || declared_type_has_shape(parent_t) {
                    let subst = type_resolver::SubstitutionMap::new();
                    let child_r = self.resolve_declared_recording_aliases(child_t, &subst);
                    let parent_r = self.resolve_declared_recording_aliases(parent_t, &subst);
                    Self::shape_assignable(&child_r, &parent_r, hierarchy).is_ok()
                } else {
                    Self::is_type_compatible(&child_t_str, &parent_t_str, hierarchy)
                };
            if !compatible {
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

    /// Check that a child method's *declared* return type refines the
    /// inherited method's declared return type (BT-4175).
    ///
    /// Return types are covariant: an override may narrow (`-> Integer` for
    /// a parent `-> Number`) but not widen or change it — a caller holding a
    /// parent-typed receiver statically sees the parent's return type. Only
    /// explicit annotations on the child are compared (an unannotated
    /// override has nothing declared to contradict), and the rule is
    /// [`Self::is_return_type_refinement`]. Warning severity, like the
    /// parameter check (ADR 0025); suppress a deliberate widening with
    /// `@expect type` on the method.
    pub(in super::super) fn check_override_return_compatibility(
        &mut self,
        method: &crate::ast::MethodDefinition,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
    ) {
        // Judge *this* definition's own annotation: a class body and a
        // standalone `Class >> sel` can both define `sel`, and the hierarchy
        // keeps only one of them (BT-4239).
        let Some(own_rt) = method.return_type.as_ref().map(DeclaredType::from) else {
            return;
        };
        let selector = method.selector.name();
        let Some(widening) =
            Self::override_return_widening(class_name, &selector, &own_rt, hierarchy)
        else {
            return;
        };
        let OverrideReturnWidening {
            child_rt,
            parent_rt,
            parent_class,
        } = widening;
        self.diagnostics.push(
            Diagnostic::warning(
                format!(
                    "Return type of '{selector}' in {class_name} is {child_rt}, incompatible with parent's {parent_rt}"
                ),
                method.span,
            )
            .with_category(DiagnosticCategory::Type)
            .with_hint(format!(
                "{parent_class} declares return type {parent_rt}; an override may narrow it but not widen it"
            )),
        );
    }

    /// The incompatible override `check_override_return_compatibility`
    /// reports for a `class_name>>selector` declared `-> child_rt`, if any:
    /// `child_rt` does not refine the return type inherited from the
    /// superclass. `None` when there is no annotated inherited declaration,
    /// either side names a class type parameter (no substitution here), or
    /// the override refines its parent. Shared with
    /// [`ProtocolRegistry::conformance_return_type_mismatches`], which leaves
    /// exactly these mismatches to the override site (BT-4175).
    pub(crate) fn override_return_widening(
        class_name: &str,
        selector: &str,
        child_rt: &DeclaredType,
        hierarchy: &ClassHierarchy,
    ) -> Option<OverrideReturnWidening> {
        let superclass = hierarchy.get_class(class_name)?.superclass.clone()?;
        let parent_method = hierarchy.find_method(&superclass, selector)?;
        let parent_rt = parent_method.return_type.clone()?;
        // Class type params (possibly multi-letter, which the shared rule's
        // single-letter heuristic would miss) have no substitution here.
        let parent_class = parent_method.defined_in.clone();
        let type_params: Vec<EcoString> = [class_name, parent_class.as_str()]
            .into_iter()
            .filter_map(|c| hierarchy.get_class(c))
            .flat_map(|c| c.type_params.iter().cloned())
            .collect();
        if child_rt.mentions_any(&type_params)
            || parent_rt.mentions_any(&type_params)
            || Self::is_return_type_refinement(child_rt, &parent_rt, hierarchy)
        {
            return None;
        }
        Some(OverrideReturnWidening {
            child_rt: child_rt.clone(),
            parent_rt,
            parent_class,
        })
    }
}
