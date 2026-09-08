// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type inference for assignment expressions.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Covers `Expression::Assignment` — annotation-vs-inferred-type reconciliation,
//! the generic-origin tracking that feeds it, and the two assignment targets
//! (`Identifier`, `FieldAccess`) with their distinct validation rules.

use crate::ast::{Expression, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::type_resolver;
use crate::semantic_analysis::type_checker::{EnvKey, InferredType, TypeChecker, TypeEnv};
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::EcoString;

impl TypeChecker {
    /// Describes where an expression's type originated (BT-1588).
    ///
    /// Returns `Some((description, span))` when the value expression is a
    /// message send that returns a generic type parameter (e.g., `V` from
    /// `Dictionary at:ifAbsent:`). Returns `None` when origin tracking
    /// isn't useful (concrete types, Dynamic, etc.).
    pub(in crate::semantic_analysis::type_checker) fn describe_type_origin(
        value: &Expression,
        ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &TypeEnv,
    ) -> Option<(EcoString, crate::source_analysis::Span)> {
        // Only track origin for generic type params (single uppercase letter)
        let type_name = ty.as_known()?;
        if !crate::semantic_analysis::type_checker::is_generic_type_param(type_name) {
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
                    .get_local(&ident.name)
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

    /// Infer an assignment `target := value` (optionally `target :: Type :=
    /// value`) — the `Expression::Assignment` arm of `infer_expr`'s dispatch.
    ///
    /// When a type annotation is present, the declared type wins over the
    /// inferred RHS type (with a mismatch warning when the two are
    /// unrelated). The target then determines how the resulting type is
    /// recorded: an `Identifier` binds it (with BT-1588 origin tracking) in
    /// `env`; a `self.field` `FieldAccess` validates it against the
    /// declared state type and invalidates any stale narrowing; any other
    /// `FieldAccess` (mutating another object's state) is rejected.
    #[allow(clippy::too_many_arguments)] // split from infer_expr's dispatch, mirrors its arity
    pub(in crate::semantic_analysis::type_checker) fn infer_assignment(
        &mut self,
        target: &Expression,
        value: &Expression,
        type_annotation: Option<&TypeAnnotation>,
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let inferred_ty = self.infer_expr(value, hierarchy, env, in_abstract_method);

        // If there's a type annotation, use the declared type instead
        // of the inferred type. Emit a warning if the RHS has a known
        // (non-Dynamic) type that is incompatible with the annotation.
        let ty = if let Some(ann) = type_annotation {
            // ADR 0102 §1/§3 (BT-2743): thread the protocol registry
            // through so a local `x :: P1 & P2` annotation resolves
            // class ∩ protocol correctly rather than falling to `Never`.
            // ADR 0108 (BT-2895): thread the alias registry through so
            // a local `heading :: Direction := ...` annotation
            // expands to its declared union, feeding the exact same
            // `InferredType` a spelled-out union would into
            // downstream narrowing/exhaustiveness.
            //
            // ADR 0108 hot-reload re-check trigger (BT-2899): this is
            // exactly the `heading :: Direction := ...` site the ADR's
            // REPL example builds on, so recording its alias deps here
            // is load-bearing, not incidental.
            // Constructed inline rather than via `self.resolution_context()`:
            // this call site also needs `self.protocol_registry` at the same
            // time, and a disjoint field borrow (this) composes with that
            // sibling borrow in one expression where a method call would not
            // — see `TypeChecker::resolution_context`'s doc.
            let declared = type_resolver::ResolutionContext::new(
                self.alias_registry.as_ref(),
                &mut self.referenced_aliases,
            )
            .resolve_type_annotation(
                ann,
                &type_resolver::SubstitutionMap::new(),
                self.protocol_registry.as_ref(),
            );
            // Check for type mismatch: known RHS that doesn't match declared type.
            // Dynamic RHS (the primary use case for annotations) is accepted silently.
            // Never RHS (diverging expressions like `self error:`) is compatible
            // with any declared type (bottom of the type lattice).
            // A narrowing assignment (declared type is a subtype of RHS type, e.g.
            // `Dictionary := <Object>`) is the type-erasure escape hatch the annotation
            // is designed for — the user is asserting the runtime type is more specific.
            if !matches!(inferred_ty, InferredType::Dynamic(_) | InferredType::Never) {
                let inferred_name = inferred_ty.display_name();
                let declared_name = declared.display_name();
                let rhs_assignable_to_declared =
                    Self::is_assignable_to(&inferred_name, &declared_name, hierarchy);
                let declared_assignable_to_rhs =
                    Self::is_assignable_to(&declared_name, &inferred_name, hierarchy);
                if !rhs_assignable_to_declared && !declared_assignable_to_rhs {
                    // BT-2066: Use source-sympathetic spelling (`Nil`) for user-facing messages.
                    let inferred_display = inferred_ty
                        .display_for_diagnostic()
                        .unwrap_or_else(|| inferred_name.clone());
                    let declared_display = declared
                        .display_for_diagnostic()
                        .unwrap_or_else(|| declared_name.clone());
                    self.diagnostics.push(
                                Diagnostic::warning(
                                    format!(
                                        "Type mismatch: declared as {declared_display}, got {inferred_display}"
                                    ),
                                    span,
                                )
                                .with_category(DiagnosticCategory::Type)
                                .with_hint(format!(
                                    "The right-hand side has type {inferred_display} which is not assignable to {declared_display}"
                                )),
                            );
                }
            }
            declared
        } else {
            inferred_ty
        };

        match target {
            Expression::Identifier(ident) => {
                // BT-1588: Track type origin for generic type params
                if let Some(origin) = Self::describe_type_origin(value, &ty, hierarchy, env) {
                    env.set_with_origin(
                        EnvKey::local(ident.name.clone()),
                        ty.clone(),
                        origin.0,
                        origin.1,
                    );
                } else {
                    env.set_local(ident.name.clone(), ty.clone());
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
                    self.check_field_assignment(field, &ty, span, hierarchy, env);
                    // BT-2048 / BT-2062: Invalidate any stale narrowing on
                    // `self.<field>`. After a write, the narrowed type is
                    // no longer guaranteed.
                    env.remove(&EnvKey::self_field(field.name.clone()));
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
                                    span,
                                )
                                .with_hint(format!(
                                    "Use `{recv_name} := {recv_name} {with_sel} newValue` to get an updated copy"
                                ))
                                .with_category(DiagnosticCategory::Type),
                            );
                }
            }
            _ => {}
        }
        ty
    }
}
