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
use ecow::EcoString;

use super::sendability;
use super::well_known::WellKnownClass;
use super::{DynamicReason, InferredType, TypeChecker, TypeEnv};

impl TypeChecker {
    /// Classify how well a union's known members match a predicate.
    ///
    /// Returns `None` if any member is Dynamic (conservative skip).
    /// Otherwise returns the count of members that satisfy `pred` and the total,
    /// plus the rendered names of the members that did *not* satisfy it.
    ///
    /// BT-2623: the predicate receives each member's *full* type string
    /// (via [`inferred_type_to_string`], e.g. `"Array(Integer)"`), not the bare
    /// `as_known()` class name. Before collection literals carried element types
    /// a union like `Array(Integer) | Array(String)` could not arise (both
    /// branches deduplicated to bare `Array`); now it can, and dropping the type
    /// args would let `Array(String)` silently pass a check against a declared
    /// `Array(Integer)`. The `as_known()` guard is still used solely to skip the
    /// whole union when any member is `Dynamic`/`Union`/`Meta`/`Never`.
    ///
    /// [`inferred_type_to_string`]: Self::inferred_type_to_string
    fn classify_union_members<F>(
        members: &[InferredType],
        pred: F,
    ) -> Option<(usize, usize, Vec<EcoString>)>
    where
        F: Fn(&EcoString) -> bool,
    {
        // Skip the whole union if any member is non-`Known` (Dynamic/Union/Meta/
        // Never) — we can't reason about those conservatively.
        if members.iter().any(|m| m.as_known().is_none()) {
            return None;
        }
        let member_names: Vec<EcoString> =
            members.iter().map(Self::inferred_type_to_string).collect();
        let compatible = member_names.iter().filter(|m| pred(m)).count();
        let incompatible: Vec<EcoString> =
            member_names.iter().filter(|m| !pred(m)).cloned().collect();
        Some((compatible, member_names.len(), incompatible))
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
            return InferredType::Dynamic(DynamicReason::Unknown);
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
            // ADR 0083: `new` / `new:` / `basicNew` are implicitly available on
            // every *concrete* Object subclass via the runtime's class
            // instantiation path, even when no explicit class-side `new` is
            // declared (e.g. a `sealed typed Object subclass`). Suppress the DNU
            // for these so the implicit-`new` override (previously documented
            // with `@expect dnu`, e.g. `SystemNavigation default`) is no longer
            // needed.
            //
            // Excluded:
            //  * Abstract classes — instantiating them is a genuine error
            //    (`infer_constructor_type` keeps the result Dynamic for them).
            //  * Actor subclasses — they are spawned (`spawn` / `spawnWith:`),
            //    not `new`'d; `A new` must still warn (drift-prevention pin).
            let is_implicit_constructor = matches!(selector, "new" | "new:" | "basicNew")
                && !hierarchy.is_abstract(class_name)
                && !hierarchy.is_actor_subclass(class_name);
            let has_class_chain_method = hierarchy.resolves_selector("Class", selector)
                || is_implicit_constructor
                || (is_factory_selector && hierarchy.resolves_selector(class_name, selector));
            if !has_class_chain_method {
                self.emit_unknown_selector_warning(
                    class_name, class_name, selector, span, hierarchy, true,
                );
            }
        }

        // Infer return type for known factory methods
        match selector {
            // ADR 0083: `basicNew` joins the constructor family so a metatype
            // receiver `Meta{C} basicNew` infers an instance of `C` (with the
            // abstract-class guard inside `infer_constructor_type`).
            "spawn" | "spawnWith:" | "new" | "new:" | "basicNew" => {
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
                        // BT-1836 / BT-2018: Parse parameterised return types like
                        // `Result(List(String), Error)` into a fully-nested
                        // `Known { class_name, type_args }` so that downstream
                        // sends on the bound local resolve correctly.
                        //
                        // BT-1986: `Self` appearing as a (nested) type argument —
                        // e.g. `class named: -> Result(Self, Error)` on Actor —
                        // resolves to the static receiver class. This powers
                        // ADR 0079's typed-lookup API where `Counter named: #c`
                        // should infer as `Result(Counter, Error)`.
                        //
                        // Build the class-level substitution map (for generic
                        // class methods like `class wrap: v :: T -> Box(T)`)
                        // by mapping declared param types to argument types.
                        let class_subst = Self::class_method_substitution(
                            class_name, hierarchy, &method, arg_types,
                        );
                        // Self resolves to the receiver class. For a generic
                        // receiver (e.g. `Box(T)`) we thread the inferred
                        // type args through `self_type` so nested `Self` in
                        // the return type — `-> Result(Self, Error)` —
                        // resolves to `Result(Box(Integer), Error)` rather
                        // than the erased `Result(Box, Error)`. CodeRabbit
                        // on PR #2065.
                        let self_type_args: Vec<InferredType> = hierarchy
                            .get_class(class_name)
                            .map(|info| {
                                info.type_params
                                    .iter()
                                    .map(|param| {
                                        class_subst
                                            .get(param)
                                            .cloned()
                                            .unwrap_or_else(|| InferredType::known(param.clone()))
                                    })
                                    .collect()
                            })
                            .unwrap_or_default();
                        let self_type = if self_type_args.is_empty() {
                            InferredType::known(class_name.clone())
                        } else {
                            InferredType::Known {
                                class_name: class_name.clone(),
                                type_args: self_type_args,
                                provenance: crate::semantic_analysis::TypeProvenance::Inferred(
                                    crate::source_analysis::Span::default(),
                                ),
                            }
                        };
                        return super::TypeChecker::substitute_return_type_with_self(
                            ret_ty,
                            &class_subst,
                            &HashMap::new(),
                            Some(&self_type),
                        );
                    }
                }
                // BT-1047: Fall back to return types inferred earlier in this pass.
                // BT-2022: Return the full InferredType (including type_args).
                let key = (class_name.clone(), EcoString::from(selector), true);
                if let Some(ret_ty) = self.method_return_types.get(&key) {
                    return ret_ty.clone();
                }
                // BT-2037: When no class-side method exists, the runtime
                // dispatches the message through the class object's instance
                // chain (Class → Behaviour → Object → ProtoObject; ADR 0032
                // Phase 0 fallthrough). Mirror that here for divergent
                // selectors so a class-side `self error: "..."` carries the
                // `-> Never` annotation from `Object#error:` instead of
                // resolving to Dynamic.
                //
                // Limited to `-> Never` returns: other return types on this
                // chain (e.g. `class -> Metaclass`) involve class-aware
                // semantics that aren't modelled in the static hierarchy
                // (e.g. `Metaclass` not knowing arbitrary class-side
                // selectors), so propagating them would surface false DNUs.
                if !has_class_method {
                    if let Some(method) = hierarchy.find_method("Class", selector) {
                        if method.return_type.as_ref().is_some_and(|ty| {
                            WellKnownClass::from_str(ty) == Some(WellKnownClass::Never)
                        }) {
                            return InferredType::Never;
                        }
                    }
                }
                InferredType::Dynamic(DynamicReason::UnannotatedReturn)
            }
        }
    }

    /// Build the class-level substitution map for a class-method call.
    ///
    /// Walks the method's declared parameter types and, for each parameter
    /// whose declared type is one of the class's type parameters (e.g. `T`
    /// in `Box(T)`), records the mapping `T -> arg_type`. The resulting map
    /// is threaded into [`super::TypeChecker::substitute_return_type_with_self`]
    /// so the return type's references to those params resolve to the
    /// concrete inferred types.
    ///
    /// For non-generic classes (or classes with no class-method type
    /// substitution to do), the returned map is empty.
    ///
    /// **ADR 0083 Slice 2 (BT-2256): nested element-type composition.** A
    /// class-side constructor whose parameter mentions a class type param
    /// *nested* inside a generic — e.g. `class withAll: list :: List(E) -> Self`
    /// on `Set(E)` — recovers `E` from the matching-base argument:
    /// `Set withAll: aList(Integer)` binds `E -> Integer`, so the `-> Self`
    /// return composes to `Set(Integer)` rather than the erased `Set(Dynamic)`.
    /// This mirrors the instance-side nested unification in
    /// [`super::TypeChecker::infer_method_local_params`] (which handles only
    /// *method-local* params); here we resolve *class-level* params on the
    /// class-side. Exact-match params are inserted directly and win: the nested
    /// helper's merge guard refuses to overwrite a `Known`/`Union` binding an
    /// exact match already set.
    ///
    /// **References:** BT-2018 (preserve generic return types on class-method
    /// assignments), ADR 0068 Phase 1c, BT-2256 (Slice 2 nested composition).
    fn class_method_substitution(
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
        method: &crate::semantic_analysis::class_hierarchy::MethodInfo,
        arg_types: &[InferredType],
    ) -> HashMap<EcoString, InferredType> {
        let mut subst: HashMap<EcoString, InferredType> = HashMap::new();
        let Some(class_info) = hierarchy.get_class(class_name) else {
            return subst;
        };
        if class_info.type_params.is_empty() {
            return subst;
        }
        for (param_ty_opt, arg_ty) in method.param_types.iter().zip(arg_types.iter()) {
            let Some(param_ty) = param_ty_opt else {
                continue;
            };
            // Exact match: a bare class type param (e.g. `T` in `Box(T)`).
            if class_info.type_params.contains(param_ty) {
                subst.insert(param_ty.clone(), arg_ty.clone());
                continue;
            }
            // ADR 0083 Slice 2: nested match — a param shaped like `List(E)`
            // where `E` is a class type param. Recover the binding from a
            // matching-base `Known` argument (e.g. `List(Integer)` ⇒
            // `E -> Integer`). Only fills params not already bound by an exact
            // match, so a direct `T` parameter still wins.
            Self::unify_nested_class_params(param_ty, arg_ty, class_info, &mut subst);
        }
        subst
    }

    /// Recover class-level type-param bindings nested inside a generic parameter
    /// shape (ADR 0083 Slice 2 / BT-2256).
    ///
    /// Given a declared parameter type like `List(E)` and an argument type like
    /// `List(Integer)`, binds `E -> Integer` when `E` is a class type param and
    /// the argument's base class matches the declared base. Recurses positionally
    /// so deeper nesting (`Pair(K, List(V))`) composes too. Merge precedence: a
    /// key already bound to a `Known`/`Union` (e.g. from an exact-match parameter)
    /// is kept and never overwritten; a key that is absent or only bound to
    /// `Dynamic` is filled in with this candidate.
    fn unify_nested_class_params(
        declared: &str,
        arg_ty: &InferredType,
        class_info: &crate::semantic_analysis::class_hierarchy::ClassInfo,
        subst: &mut HashMap<EcoString, InferredType>,
    ) {
        let (declared_base, declared_args) = super::type_resolver::split_generic_base(declared);
        let Some(inner) = declared_args else {
            return;
        };
        // Strip a nilable union (`List(String) | Nil`) to its non-nil member so
        // the optional-collection shape still composes (mirrors BT-2023(A)).
        let stripped;
        let effective_arg = if matches!(arg_ty, InferredType::Union { .. }) {
            stripped = super::TypeChecker::non_nil_type(arg_ty);
            &stripped
        } else {
            arg_ty
        };
        let InferredType::Known {
            class_name: arg_class,
            type_args,
            ..
        } = effective_arg
        else {
            return;
        };
        if arg_class.as_str() != declared_base || type_args.is_empty() {
            return;
        }
        let declared_params = super::TypeChecker::split_type_params(inner);
        for (declared_param, actual) in declared_params.iter().zip(type_args.iter()) {
            let decl_eco: EcoString = (*declared_param).into();
            if class_info.type_params.contains(&decl_eco) {
                // Don't clobber an existing binding; don't downgrade to Dynamic.
                match subst.get(&decl_eco) {
                    Some(InferredType::Known { .. } | InferredType::Union { .. }) => {}
                    _ => {
                        subst.insert(decl_eco, actual.clone());
                    }
                }
            } else {
                // Recurse into deeper nesting (`Pair(K, List(V))`).
                Self::unify_nested_class_params(declared_param, actual, class_info, subst);
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
    ///
    /// **ADR 0083 abstract-class guard:** `new` / `basicNew` on an *abstract*
    /// class (e.g. `Collection`, `Behaviour`) must NOT be blessed as a concrete
    /// instance — at runtime instantiating an abstract class is an error, and a
    /// metatype value of an abstract class flowing into `new` (e.g. a
    /// `Behaviour`-typed reflection result, whose instance type is statically
    /// unknown) would otherwise produce a misleadingly-precise type. For these
    /// constructor selectors we fall back to `Dynamic` so downstream sends are
    /// not checked against the abstract class's instance protocol.
    fn infer_constructor_type(
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
        selector: &str,
        arg_types: &[InferredType],
    ) -> InferredType {
        // ADR 0083: guard abstract-class instantiation. `new`/`new:`/`basicNew`
        // on an abstract class does not yield a concrete instance.
        if matches!(selector, "new" | "new:" | "basicNew") && hierarchy.is_abstract(class_name) {
            return InferredType::Dynamic(DynamicReason::Unknown);
        }

        let Some(class_info) = hierarchy.get_class(class_name) else {
            return InferredType::known(class_name.clone());
        };

        // Non-generic class — no type args to infer
        if class_info.type_params.is_empty() {
            return InferredType::known(class_name.clone());
        }

        // Build inference map from class method param types → argument types
        // (shared with non-`Self` returns via `class_method_substitution`).
        let mut inferred = if let Some(method) = hierarchy.find_class_method(class_name, selector) {
            Self::class_method_substitution(class_name, hierarchy, &method, arg_types)
        } else {
            HashMap::new()
        };

        // Build type_args in class param order, defaulting unresolved to Dynamic
        let type_args: Vec<InferredType> = class_info
            .type_params
            .iter()
            .map(|p| {
                inferred
                    .remove(p)
                    .unwrap_or(InferredType::Dynamic(DynamicReason::Unknown))
            })
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
        // BT-2647: a singleton type (`#text`) is a subtype of `Symbol` but is not
        // itself a class in the hierarchy. Resolve its selectors through
        // `Symbol`'s protocol so DNU validation still applies — narrowing a getter
        // from `Symbol` to a singleton (e.g. `#text | #json`, then a single
        // member) would otherwise silently drop the selector checking it had.
        // Mirrors the singleton-union inference handling from BT-2624.
        //
        // BT-2679: `effective_class` is what we resolve selectors/suggestions
        // against (`Symbol` for a singleton); `class_name` stays the type the user
        // wrote so DNU messages name the singleton (e.g. `#infinity`) rather than
        // `Symbol`.
        let symbol: EcoString;
        let effective_class: &EcoString =
            if class_name.starts_with('#') && !class_name.contains('|') {
                symbol = EcoString::from("Symbol");
                &symbol
            } else {
                class_name
            };
        if !hierarchy.has_class(effective_class) {
            // BT-1833: If the type is a protocol (from respondsTo: narrowing),
            // validate the selector against the protocol's required methods.
            if let Some(ref registry) = self.protocol_registry {
                if let Some(proto_info) = registry.get(effective_class) {
                    let required = proto_info.all_required_selectors(registry);
                    if !required.iter().any(|s| s.as_str() == selector) {
                        self.emit_unknown_selector_warning(
                            class_name,
                            effective_class,
                            selector,
                            span,
                            hierarchy,
                            false,
                        );
                    }
                }
            }
            return;
        }

        // ADR 0071 Phase 3 (BT-1702): E0403 — cross-package send to internal method
        self.check_internal_method_access(effective_class, selector, span, hierarchy, false);

        // Classes with instance-side doesNotUnderstand: override accept any message
        if hierarchy.has_instance_dnu_override(effective_class) {
            return;
        }

        // Cross-file inheritance: if the parent class is not in the hierarchy,
        // we can't know the full method set — suppress false-positive DNU hints.
        if hierarchy.has_cross_file_parent(effective_class) {
            return;
        }

        if !hierarchy.resolves_selector(effective_class, selector) {
            self.emit_unknown_selector_warning(
                class_name,
                effective_class,
                selector,
                span,
                hierarchy,
                false,
            );
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
                    .with_hint(format!("Add a type annotation: `{} :: Type`", param.name.name))
                    .with_category(DiagnosticCategory::TypeAnnotation),
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
                .with_hint(format!("Add a return type: `{selector} -> ReturnType =>`"))
                .with_category(DiagnosticCategory::TypeAnnotation),
            );
        }
    }

    /// Check that state fields in typed classes have type annotations.
    pub(super) fn check_typed_state_annotations(
        &mut self,
        state: &[crate::ast::StateDeclaration],
        class_name: &EcoString,
    ) {
        for field in state {
            if field.type_annotation.is_none() {
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Missing type annotation for state field `{}` in typed class `{class_name}`",
                            field.name.name
                        ),
                        field.name.span,
                    )
                    .with_hint(format!("Add a type annotation: `{} :: Type = defaultValue`", field.name.name))
                    .with_category(DiagnosticCategory::TypeAnnotation),
                );
            }
        }
    }

    /// Check if `actual` type is compatible with `expected` type.
    ///
    /// Compatibility rules:
    /// - Same type → compatible
    /// - `expected` appears in `actual`'s superclass chain → compatible (e.g., Integer for Number)
    /// - Either type is unknown to the hierarchy → compatible (conservative)
    pub(super) fn is_type_compatible(
        actual: &EcoString,
        expected: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        // BT-1877: Resolve `Nil` to `UndefinedObject` so hierarchy lookups succeed.
        let actual_resolved = Self::resolve_type_alias(actual);
        let expected_resolved = Self::resolve_type_alias(expected);
        let actual = actual_resolved.as_ref().unwrap_or(actual);
        let expected = expected_resolved.as_ref().unwrap_or(expected);

        if actual == expected {
            return true;
        }
        // Never is the bottom type — compatible with everything as actual.
        // As expected, only Never itself is compatible (a non-divergent method
        // returning Integer does not satisfy -> Never).
        if WellKnownClass::from_str(actual) == Some(WellKnownClass::Never) {
            return true;
        }
        if WellKnownClass::from_str(expected) == Some(WellKnownClass::Never) {
            return false;
        }
        // BT-1835: Union syntax in expected type (e.g., "Integer | Symbol" from builtins).
        // If expected contains `|`, split into members and check if actual matches any.
        if expected.contains(" | ") {
            return expected.split(" | ").any(|member| {
                let member_eco: EcoString = member.trim().into();
                Self::is_type_compatible(actual, &member_eco, hierarchy)
            });
        }
        // Singleton types: #foo is a subtype of Symbol, not the reverse.
        // Symbol is NOT compatible with a singleton like #ok.
        // Generic type params (K, V, T, etc.) are always compatible (conservative).
        if expected.starts_with('#') && !expected.contains('|') {
            return actual == expected || super::is_generic_type_param(actual);
        }
        if actual.starts_with('#') && !actual.contains('|') {
            // Singletons are subtypes of Symbol — check if expected is Symbol
            // or an ancestor of Symbol in the class hierarchy.
            if super::is_generic_type_param(expected) {
                return true;
            }
            let symbol: EcoString = "Symbol".into();
            return Self::is_type_compatible(&symbol, expected, hierarchy);
        }
        // BT-1877: `Class` is a meta-type — any class reference is an instance of
        // Class.  Since class references are currently inferred as their concrete
        // class name (e.g. `Supervisor`), we treat `Class` as compatible with any
        // known class to avoid false positives on `Class | Nil` parameters.
        if expected.as_str() == "Class" && hierarchy.has_class(actual) {
            return true;
        }
        // BT-2002: Strip type arguments before the hierarchy lookup so generic
        // annotations like `Block(T, R)` or `Array(Integer)` resolve to their
        // base class. Without stripping, `hierarchy.has_class("Block(T, R)")`
        // is false and the conservative "unknown → compatible" escape hatch
        // below suppresses warnings for non-Block arguments to `Block(T, R)`
        // parameters. Both sides must be normalized so override checks
        // (`check_override_param_compatibility`), which pass annotation
        // strings as `actual`, also resolve to the base class.
        let actual_base = actual
            .split_once('(')
            .map_or(actual.as_str(), |(base, _)| base);
        let expected_base = expected
            .split_once('(')
            .map_or(expected.as_str(), |(base, _)| base);

        if actual_base == expected_base {
            // BT-2623: when *both* sides carry type arguments and the bases match
            // (e.g. `Array(Integer)` vs `Array(String)`), don't blindly pass on
            // the base alone — compare the type args invariantly so a union
            // member like `Array(String)` is rejected against a declared
            // `Array(Integer)`. If only one side is parameterized we stay
            // conservative (bare `Array` may be any `Array(_)`).
            return Self::type_args_match(actual, expected, hierarchy);
        }
        // If either type isn't known to the hierarchy, don't warn (conservative)
        if !hierarchy.has_class(actual_base) || !hierarchy.has_class(expected_base) {
            return true;
        }
        // Walk superclass chain: if expected is an ancestor of actual, it's compatible
        let chain = hierarchy.superclass_chain(actual_base);
        chain
            .iter()
            .any(|ancestor| ancestor.as_str() == expected_base)
    }

    /// BT-2623: Compare the *type arguments* of two same-base generic type
    /// strings (e.g. `"Array(Integer)"` vs `"Array(String)"`).
    ///
    /// Returns `true` (compatible) unless both sides are parameterized with the
    /// same arity and at least one arg pair is incompatible. When either side is
    /// unparameterized (bare `Array`, which may stand for any `Array(_)`) or the
    /// arities differ, we stay conservative and return `true` so this never
    /// introduces a false positive on its own.
    ///
    /// Args are compared invariantly via [`is_type_compatible`], recursing into
    /// nested generics (so `Array(Array(Integer))` vs `Array(Array(String))`
    /// is also caught).
    fn type_args_match(actual: &str, expected: &str, hierarchy: &ClassHierarchy) -> bool {
        let (_, actual_args) = Self::parse_generic_type_string(actual);
        let (_, expected_args) = Self::parse_generic_type_string(expected);
        if actual_args.is_empty()
            || expected_args.is_empty()
            || actual_args.len() != expected_args.len()
        {
            return true; // Unparameterized or arity mismatch — conservative.
        }
        actual_args.iter().zip(expected_args.iter()).all(|(a, e)| {
            // Generic placeholders (T, E, K, V) are symbolic — treat as
            // compatible, matching `type_args_compatible`.
            if super::is_generic_type_param(a) || super::is_generic_type_param(e) {
                return true;
            }
            Self::is_type_compatible(&a.as_str().into(), &e.as_str().into(), hierarchy)
        })
    }

    /// BT-2022: Recursive structural compatibility for one type-arg slot.
    ///
    /// Used when comparing a declared generic return type's inner args against
    /// the body's inferred inner args. Recurses into nested `Known` so that
    /// shapes like `Result(Array(Integer), Error)` vs `Result(Array(String),
    /// Error)` are detected as mismatches at the inner level.
    ///
    /// Returns true (compatible) for any non-`Known` shape (Dynamic, Union,
    /// Never) — those are handled by the broader checker and we don't want to
    /// double-warn here. Generic type-param placeholders (T, E, K, V) are
    /// treated as compatible since they're symbolic.
    fn type_args_compatible(
        expected: &InferredType,
        actual: &InferredType,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        let (
            InferredType::Known {
                class_name: exp_name,
                type_args: exp_inner,
                ..
            },
            InferredType::Known {
                class_name: act_name,
                type_args: act_inner,
                ..
            },
        ) = (expected, actual)
        else {
            return true; // Dynamic / Union / Never on either side — skip
        };
        if super::is_generic_type_param(exp_name) || super::is_generic_type_param(act_name) {
            return true;
        }
        if !Self::is_type_compatible(act_name, exp_name, hierarchy) {
            return false;
        }
        // Base classes match — recurse into inner type args when arity lines up.
        if exp_inner.is_empty() || act_inner.is_empty() || exp_inner.len() != act_inner.len() {
            return true;
        }
        exp_inner
            .iter()
            .zip(act_inner.iter())
            .all(|(e, a)| Self::type_args_compatible(e, a, hierarchy))
    }

    /// ADR 0103 (Phase 0): warn when a process-bound handle
    /// (`HandleScoped(#process)`) is passed as an argument in an actor message.
    ///
    /// Runs independently of whether the receiver's handler has typed
    /// parameters — the hazard is about *what* is sent, not the declared
    /// parameter type — so it is invoked before the method lookup in
    /// [`check_argument_types`]. `#node`-scoped and `Unknown` arguments stay
    /// silent in v1 (no static remoteness knowledge; advisory per ADR 0100).
    ///
    /// Class-side sends (`spawnWith:` maps, `new:`) are Phase 1 (BT-2755).
    /// ADR 0103 (Phase 1): warn when a process-bound handle appears as a value
    /// in a `spawnWith:` initial-state map — the map is copied into the new
    /// actor process, so a `HandleScoped(#process)` value is only usable by its
    /// original owner. Inspects the `MapLiteral` call-site pairs (shared model
    /// with ADR 0104's `spawnWith:` key checking).
    pub(super) fn check_spawn_with_sendability(
        &mut self,
        selector: &str,
        hierarchy: &ClassHierarchy,
        receiver_is_actor: bool,
        arg_exprs: Option<&[Expression]>,
    ) {
        if !receiver_is_actor || selector != "spawnWith:" {
            return;
        }
        let Some(Expression::MapLiteral { pairs, .. }) = arg_exprs.and_then(<[_]>::first) else {
            return;
        };
        for pair in pairs {
            let Some(value_ty) = self.type_map.get(pair.value.span()) else {
                continue;
            };
            let sendability::Tier::HandleScoped(sendability::HandleScope::Process) =
                sendability::tier_of(value_ty, hierarchy)
            else {
                continue;
            };
            let ty_name = value_ty
                .as_known()
                .cloned()
                .unwrap_or_else(|| EcoString::from("handle"));
            let label = match &pair.value {
                Expression::Identifier(ident) => ident.name.clone(),
                _ => ty_name.clone(),
            };
            self.diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "`{label}` ({ty_name} — process-bound handle) passed in a `spawnWith:` \
                         map; it is only usable by its owning process"
                    ),
                    pair.value.span(),
                )
                .with_hint(
                    "The initial-state map is copied into the new actor process. Consider \
                     passing data, or an Actor that owns the handle",
                )
                .with_category(DiagnosticCategory::Sendability),
            );
        }
    }

    pub(super) fn check_arg_sendability(
        &mut self,
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
        receiver_is_actor: bool,
        arg_exprs: Option<&[Expression]>,
    ) {
        // Boundary #1 (Phase 0): actor *instance* message arguments only.
        if is_class_side || !receiver_is_actor {
            return;
        }
        for (i, arg_ty) in arg_types.iter().enumerate() {
            let sendability::Tier::HandleScoped(sendability::HandleScope::Process) =
                sendability::tier_of(arg_ty, hierarchy)
            else {
                continue;
            };
            let arg_expr = arg_exprs.and_then(|exprs| exprs.get(i));
            let arg_span = arg_expr.map_or(span, Expression::span);
            let ty_name = arg_ty
                .as_known()
                .cloned()
                .unwrap_or_else(|| EcoString::from("handle"));
            // Prefer the variable name (`port`) for the message; fall back to
            // the handle's type name for non-identifier arguments.
            let label = match arg_expr {
                Some(Expression::Identifier(ident)) => ident.name.clone(),
                _ => ty_name.clone(),
            };
            self.diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "`{label}` ({ty_name} — process-bound handle) passed in an actor \
                         message; it is only usable by its owning process"
                    ),
                    arg_span,
                )
                .with_hint(
                    "A port-like handle is bound to its owning process. Consider passing \
                     data, or an Actor that owns the handle",
                )
                .with_category(DiagnosticCategory::Sendability),
            );
        }
    }

    /// Check argument types against declared parameter types for a message send.
    #[allow(clippy::too_many_arguments)] // BT-1588: arg_exprs + env needed for origin tracing
    #[allow(clippy::too_many_lines)] // BT-2038 adds class-literal subtyping arm
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
        // ADR 0103: sendability of actor message arguments and `spawnWith:`
        // map values. Independent of the handler's declared parameter types, so
        // they run before the method lookup / early returns below.
        let receiver_is_actor = hierarchy.is_actor_subclass(class_name);
        self.check_arg_sendability(
            arg_types,
            span,
            hierarchy,
            is_class_side,
            receiver_is_actor,
            arg_exprs,
        );
        self.check_spawn_with_sendability(selector, hierarchy, receiver_is_actor, arg_exprs);

        let method = if is_class_side {
            hierarchy.find_class_method(class_name, selector)
        } else {
            hierarchy.find_method(class_name, selector)
        };
        let Some(method) = method else { return };
        if method.param_types.is_empty() {
            return;
        }

        // BT-2038: A class literal (e.g. `TestCase`) is a class value whose
        // runtime type flows through the metaclass tower
        // (Metaclass → Class → Behaviour → Object → ProtoObject). When the
        // instance-side chain fails, re-check via `Metaclass` so parameters
        // declared `:: Behaviour` / `:: Class` / `:: Object` / `:: ProtoObject`
        // accept any class literal. Hoisted out of the loop since it's
        // constant across all arguments.
        let metaclass_name: EcoString = "Metaclass".into();

        for (i, (arg_ty, expected)) in arg_types.iter().zip(method.param_types.iter()).enumerate() {
            let Some(expected_ty) = expected else {
                continue;
            };
            let is_class_ref_arg = arg_exprs
                .and_then(|exprs| exprs.get(i))
                .is_some_and(|e| matches!(e, Expression::ClassReference { .. }));
            match arg_ty {
                InferredType::Known {
                    class_name: actual_ty,
                    ..
                } => {
                    // Short-circuit: only consult the metaclass chain when the
                    // instance-side check fails. Keeps the hot path at one
                    // `is_type_compatible` call per argument.
                    //
                    // CodeRabbit on PR #2071: `is_type_compatible` has a
                    // pre-existing BT-1877 shortcut where `expected == "Class"`
                    // accepts any known class name unconditionally. That was a
                    // workaround for exactly the BT-2038 problem; with the
                    // metaclass-tower check now handling class literals
                    // properly, the shortcut must be scoped to class-literal
                    // arguments here so a plain `TestCase` instance does not
                    // satisfy a `:: Class` parameter.
                    let class_shortcut_applies =
                        expected_ty.as_str() == "Class" && !is_class_ref_arg;
                    let instance_compat = !class_shortcut_applies
                        && Self::is_type_compatible(actual_ty, expected_ty, hierarchy);
                    let class_literal_compat = !instance_compat
                        && is_class_ref_arg
                        && hierarchy.has_class(actual_ty)
                        && Self::is_type_compatible(&metaclass_name, expected_ty, hierarchy);
                    if !instance_compat && !class_literal_compat {
                        let param_pos = i + 1;
                        // BT-1588: Use hint severity for generic type params (likely false positive)
                        let is_generic = super::is_generic_type_param(actual_ty);
                        // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
                        let actual_display =
                            InferredType::class_name_for_diagnostic(actual_ty.as_str());
                        let expected_display =
                            InferredType::class_name_for_diagnostic(expected_ty.as_str());
                        let mut diag = if is_generic {
                            Diagnostic::hint(
                                format!(
                                    "Argument {param_pos} of '{selector}' on {class_name} expects {expected_display}, got {actual_display}"
                                ),
                                span,
                            )
                            .with_hint(format!(
                                "This is likely a false positive — `{actual_display}` is a generic type parameter that may be {expected_display} at runtime. \
                                 Use `@expect type` to suppress"
                            ))
                        } else {
                            Diagnostic::warning(
                                format!(
                                    "Argument {param_pos} of '{selector}' on {class_name} expects {expected_display}, got {actual_display}"
                                ),
                                span,
                            )
                            .with_hint(format!("Expected {expected_display} (or a subclass), got {actual_display}"))
                        };
                        // BT-1588: Attach origin note if available
                        if let (Some(exprs), Some(e)) = (arg_exprs, env) {
                            if let Some(Expression::Identifier(ident)) = exprs.get(i) {
                                if let Some(origin) = e.get_local_origin(&ident.name) {
                                    diag = diag
                                        .with_note(origin.description.clone(), Some(origin.span));
                                }
                            }
                        }
                        self.diagnostics
                            .push(diag.with_category(DiagnosticCategory::Type));
                    }
                }
                InferredType::Meta {
                    class_name: meta_class,
                    ..
                } => {
                    // ADR 0083: a metatype value `C class` is an instance of the
                    // metaclass tower (`Meta{C} <: Class <: Behaviour <: Object`).
                    // It satisfies a parameter whose declared type is `Class`,
                    // `Behaviour`, `Object`, `ProtoObject` — anything `Class`
                    // (the runtime class of a class object) is assignable to.
                    // We model the metatype's own type as `Class` for the
                    // hierarchy walk, so e.g. `:: Behaviour` accepts it but
                    // `:: Integer` does not.
                    let class_name_ty: EcoString = "Class".into();
                    let compat = Self::is_type_compatible(&class_name_ty, expected_ty, hierarchy);
                    if !compat {
                        let param_pos = i + 1;
                        let actual_display: EcoString = format!("{meta_class} class").into();
                        let expected_display =
                            InferredType::class_name_for_diagnostic(expected_ty.as_str());
                        self.diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "Argument {param_pos} of '{selector}' on {class_name} expects {expected_display}, got {actual_display}"
                                ),
                                span,
                            )
                            .with_hint(format!(
                                "Expected {expected_display} (or a subclass), got {actual_display}"
                            ))
                            .with_category(DiagnosticCategory::Type),
                        );
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
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    // BT-2066: Render `UndefinedObject` as `Nil` for the declared type too.
                    let expected_display =
                        InferredType::class_name_for_diagnostic(expected_ty.as_str());
                    let mut diag = if compat == 0 {
                        Diagnostic::warning(
                            format!("Argument {param_pos} of '{selector}' on {class_name} expects {expected_display}, got {union_display}"),
                            span,
                        )
                        .with_hint(format!("No member of {union_display} is compatible with {expected_display}"))
                    } else {
                        let list = incompatible
                            .iter()
                            .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        Diagnostic::hint(
                            format!("Argument {param_pos} of '{selector}' on {class_name} expects {expected_display}, got {union_display}"),
                            span,
                        )
                        .with_hint(format!("Some members of the union are not compatible with {expected_display}: {list}"))
                    };
                    if let (Some(exprs), Some(e)) = (arg_exprs, env) {
                        if let Some(Expression::Identifier(ident)) = exprs.get(i) {
                            if let Some(origin) = e.get_local_origin(&ident.name) {
                                diag =
                                    diag.with_note(origin.description.clone(), Some(origin.span));
                            }
                        }
                    }
                    self.diagnostics
                        .push(diag.with_category(DiagnosticCategory::Type));
                }
                // A `Negation` (`Symbol \ #foo`) is a narrowed `Symbol`; skip
                // conservatively like `Dynamic`/`Never` (ADR 0102). An
                // `Intersection` (`P1 & P2`, ADR 0102/BT-2743) as the
                // *argument's own* inferred type is likewise skipped — see
                // `check_protocol_argument_conformance` for the flagship
                // "parameter declared `P1 & P2`" conformance check.
                InferredType::Dynamic(_)
                | InferredType::Never
                | InferredType::Negation { .. }
                | InferredType::Intersection { .. } => {}
            }
        }
    }

    /// Check that a method body's inferred return type matches its declared return type.
    #[allow(clippy::too_many_lines)] // BT-2022 added type_args comparison arm
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
            type_args: actual_args,
            ..
        } = body_type
        else {
            return; // Dynamic or Union body — can't reliably check
        };

        // Resolve the declared return type. The `-> Self` case needs the
        // static receiver class (which the plain annotation resolver does not
        // thread), so handle it specially via `receiver_type_for_class`.
        // `-> Self class` returns a class object — existing body-type
        // comparison cannot meaningfully validate this, so skip (BT-1952).
        //
        // BT-2025: All other arms — including `Generic` — go through the
        // central `resolve_type_annotation` resolver.
        //
        // BT-2022: The resolver now preserves type_args, and the comparison
        // below checks them when both sides carry generic arguments. This
        // closes the bug where `-> Result(Integer, Error)` with body
        // `Result(String, Error)` produced no warning.
        let expected = match declared {
            TypeAnnotation::SelfType { .. } => {
                super::type_resolver::receiver_type_for_class(class_name, hierarchy)
            }
            TypeAnnotation::SelfClass { .. } | TypeAnnotation::ClassOf { .. } => return,
            _ => Self::resolve_type_annotation(declared),
        };

        match &expected {
            InferredType::Known {
                class_name: expected_ty,
                type_args: expected_args,
                ..
            } => {
                // Check base class compatibility, then inner type args (BT-2022).
                let base_mismatch = !Self::is_type_compatible(actual_ty, expected_ty, hierarchy);
                // BT-2022: When the base class matches, also verify inner type args
                // match. Recurse into nested generics so mismatches like
                // `Result(Array(Integer), Error)` vs `Result(Array(String), Error)`
                // are caught (Copilot review on PR #2059).
                let args_mismatch = !base_mismatch
                    && !expected_args.is_empty()
                    && !actual_args.is_empty()
                    && expected_args.len() == actual_args.len()
                    && expected_args
                        .iter()
                        .zip(actual_args.iter())
                        .any(|(exp_arg, act_arg)| {
                            !Self::type_args_compatible(exp_arg, act_arg, hierarchy)
                        });
                if base_mismatch || args_mismatch {
                    let expected_display = expected.display_for_diagnostic().unwrap_or_else(|| {
                        InferredType::class_name_for_diagnostic(expected_ty.as_str())
                    });
                    let actual_display = body_type.display_for_diagnostic().unwrap_or_else(|| {
                        InferredType::class_name_for_diagnostic(actual_ty.as_str())
                    });
                    let selector = method.selector.name();
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Method '{selector}' in {class_name} declares return type {expected_display}, but body returns {actual_display}"
                            ),
                            method.span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!("Declared -> {expected_display}, inferred body type is {actual_display}")),
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
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let actual_display =
                        InferredType::class_name_for_diagnostic(actual_ty.as_str());
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Method '{selector}' in {class_name} declares return type {union_display}, but body returns {actual_display}"
                            ),
                            method.span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!("Declared -> {union_display}, inferred body type is {actual_display}")),
                    );
                }
            }
            InferredType::Never => {
                // BT-2033: Declared `-> Never` means the method is divergent
                // (never returns normally). The body must also be Never for the
                // declaration to be honest. Any `Known` body reaching this arm
                // means the method *does* return a value — warn.
                //
                // Note: `Never` body-types cause the early return on line ~753
                // before this match, so we only reach this arm when the body
                // has a non-Never `Known` type (the destructure above extracted
                // `actual_ty`).
                let selector = method.selector.name();
                let actual_display = body_type
                    .display_for_diagnostic()
                    .unwrap_or_else(|| InferredType::class_name_for_diagnostic(actual_ty.as_str()));
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Method '{selector}' in {class_name} declares return type Never, but body returns {actual_display}"
                        ),
                        method.span,
                    )
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(format!(
                        "`Never` means the method is divergent (throws or loops). Either remove `-> Never` or ensure the body always diverges (e.g. ends with `self error: ...`). Inferred body type is {actual_display}"
                    )),
                );
            }
            // ADR 0083: a declared metatype return (`-> Self class` / `-> X class`)
            // already short-circuits via the `SelfClass`/`ClassOf` arm above, so
            // `Meta` is unreachable in practice; skip checking (like `Dynamic`)
            // to stay safe should a future annotation resolve to a metatype.
            // A `Negation` (`Symbol \ #foo`) is a narrowed `Symbol`; skip like
            // `Meta`/`Dynamic` (ADR 0102). An `Intersection` (`P1 & P2`, ADR
            // 0102/BT-2743) declared return type has no single `Known` body
            // shape to compare against here; skip conservatively too.
            InferredType::Meta { .. }
            | InferredType::Dynamic(_)
            | InferredType::Negation { .. }
            | InferredType::Intersection { .. } => {}
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
        // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
        let arg_display = InferredType::class_name_for_diagnostic(arg_ty.as_str());
        let receiver_display = InferredType::class_name_for_diagnostic(receiver_ty.as_str());

        // Arithmetic operators on numeric types require numeric arguments
        if is_arithmetic && is_numeric(receiver_ty) && !is_numeric(arg_ty) {
            // BT-1588: Use hint severity for generic type params (likely false positive)
            let mut diag = if is_generic {
                Diagnostic::hint(
                    format!(
                        "`{operator}` on {receiver_display} expects a numeric argument, got {arg_display}"
                    ),
                    span,
                )
            } else {
                Diagnostic::warning(
                    format!(
                        "`{operator}` on {receiver_display} expects a numeric argument, got {arg_display}"
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
            && WellKnownClass::from_str(receiver_ty) == Some(WellKnownClass::String)
            && WellKnownClass::from_str(arg_ty) != Some(WellKnownClass::String)
            && arg_ty.as_str() != "Symbol"
            && !arg_ty.starts_with('#')
        {
            // BT-1588: Use hint severity for generic type params (likely false positive)
            let mut diag = if is_generic {
                Diagnostic::hint(
                    format!("`++` on String expects a String argument, got {arg_display}"),
                    span,
                )
                .with_hint(
                    "This is likely a false positive — the generic type may be String at runtime. \
                     Use `displayString` or `@expect type` to suppress",
                )
            } else {
                Diagnostic::warning(
                    format!("`++` on String expects a String argument, got {arg_display}"),
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
                        "`{operator}` on {receiver_display} expects a numeric argument, got {arg_display}"
                    ),
                    span,
                )
            } else {
                Diagnostic::warning(
                    format!(
                        "`{operator}` on {receiver_display} expects a numeric argument, got {arg_display}"
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
        let Some(InferredType::Known { class_name, .. }) = env.get_local("self") else {
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
                    // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
                    let declared_display =
                        InferredType::class_name_for_diagnostic(declared_type.as_str());
                    let value_display =
                        InferredType::class_name_for_diagnostic(value_type.as_str());
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Type mismatch: field `{}` declared as {declared_display}, got {value_display}",
                                field.name
                            ),
                            span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!(
                            "Expected {declared_display} but assigning {value_display}"
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
                    .display_for_diagnostic()
                    .unwrap_or_else(|| EcoString::from("Dynamic"));
                // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
                let declared_display =
                    InferredType::class_name_for_diagnostic(declared_type.as_str());
                let diag = if compat == 0 {
                    Diagnostic::warning(
                        format!("Type mismatch: field `{}` declared as {declared_display}, got {union_display}", field.name),
                        span,
                    )
                    .with_hint(format!("No member of {union_display} is compatible with {declared_display}"))
                } else {
                    let list = incompatible
                        .iter()
                        .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
                        .collect::<Vec<_>>()
                        .join(", ");
                    Diagnostic::hint(
                        format!("Type mismatch: field `{}` declared as {declared_display}, got {union_display}", field.name),
                        span,
                    )
                    .with_hint(format!("Some members of the union are not compatible with {declared_display}: {list}"))
                };
                self.diagnostics
                    .push(diag.with_category(DiagnosticCategory::Type));
            }
            InferredType::Meta {
                class_name: meta_class,
                ..
            } => {
                // ADR 0083: a class object `C class` is assignable to a field
                // declared `:: Class` / `:: Behaviour` / `:: Object` (the
                // metaclass tower). Model the metatype as `Class` for the
                // assignability walk.
                let class_name_ty: EcoString = "Class".into();
                if !Self::is_assignable_to(&class_name_ty, &declared_type, hierarchy) {
                    let declared_display =
                        InferredType::class_name_for_diagnostic(declared_type.as_str());
                    let value_display: EcoString = format!("{meta_class} class").into();
                    self.diagnostics.push(
                        Diagnostic::warning(
                            format!(
                                "Type mismatch: field `{}` declared as {declared_display}, got {value_display}",
                                field.name
                            ),
                            span,
                        )
                        .with_category(DiagnosticCategory::Type)
                        .with_hint(format!(
                            "Expected {declared_display} but assigning {value_display}"
                        )),
                    );
                }
            }
            // A `Negation` (`Symbol \ #foo`) is a narrowed `Symbol`; skip
            // conservatively like `Dynamic`/`Never` (ADR 0102). Likewise an
            // `Intersection` (`P1 & P2`, ADR 0102/BT-2743) as the value's own
            // inferred type — skip conservatively.
            InferredType::Dynamic(_)
            | InferredType::Never
            | InferredType::Negation { .. }
            | InferredType::Intersection { .. } => {}
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
            env.set_local("self", InferredType::known(class.name.name.clone()));
            let inferred = self.infer_expr(default_value, hierarchy, &mut env, false);
            let InferredType::Known {
                class_name: value_type,
                ..
            } = &inferred
            else {
                continue; // Dynamic defaults are fine
            };
            if !Self::is_assignable_to(value_type, &declared_type, hierarchy) {
                // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
                let declared_display =
                    InferredType::class_name_for_diagnostic(declared_type.as_str());
                let value_display = InferredType::class_name_for_diagnostic(value_type.as_str());
                self.diagnostics.push(
                    Diagnostic::warning(
                        format!(
                            "Type mismatch: state `{}` declared as {declared_display}, default is {value_display}",
                            decl.name.name
                        ),
                        decl.span,
                    )
                    .with_category(DiagnosticCategory::Type)
                    .with_hint(format!(
                        "Default value type {value_display} is not compatible with {declared_display}"
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
    /// compares the base type name. Singleton (`#`) types require an exact match or `Symbol`.
    pub(super) fn is_assignable_to(
        value_type: &EcoString,
        declared_type: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        if value_type == declared_type {
            return true;
        }
        // Singleton declared types: value must be the exact same singleton.
        // Symbol is NOT assignable to a singleton like #ok (wrong subtyping direction).
        if declared_type.starts_with('#') && !declared_type.contains('|') {
            return value_type == declared_type;
        }
        // Singleton value types: check union membership first, then fall back to
        // Symbol hierarchy (e.g., #ok is assignable to Object, Symbol | nil, etc.)
        if value_type.starts_with('#') && !value_type.contains('|') {
            // For union declared types, check if the singleton is a member
            if declared_type.contains('|') {
                return declared_type.split('|').map(str::trim).any(|member| {
                    let member_eco: EcoString = Self::resolve_type_keyword_static(member);
                    value_type.as_str() == member_eco.as_str()
                        || Self::is_type_compatible(value_type, &member_eco, hierarchy)
                });
            }
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
        // the base type name and compare structurally. BT-2025: go through
        // the centralised `base_name_of_string` helper so the grep for
        // ad-hoc `.find('(')` slicing stays clean.
        let declared_base = super::type_resolver::base_name_of_string(declared_type);
        let value_base = super::type_resolver::base_name_of_string(value_type);
        if value_base == declared_base {
            // BT-2623: same base — compare type args so `Array(String)` is not
            // silently assignable to a declared `Array(Integer)`. Conservative
            // (returns true) when either side is unparameterized.
            return Self::type_args_match(value_type, declared_type, hierarchy);
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
                    let base_protocol = super::type_resolver::base_name_of_string(decl_arg);
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
    /// Returns `("Map", ["Result(A, B)", "C"])` for `"Map(Result(A, B), C)"` —
    /// nested multi-parameter generics split at the top level only, matching the
    /// balanced splitter used elsewhere in the type checker (BT-2025).
    /// For non-generic types, returns the full name and an empty vec.
    pub(super) fn parse_generic_type_string(type_str: &str) -> (String, Vec<String>) {
        let (base, args_slice) = super::type_resolver::split_generic_base(type_str);
        let args: Vec<String> = args_slice
            .map(|s| {
                super::TypeChecker::split_type_params(s)
                    .into_iter()
                    .map(|p| p.trim().to_string())
                    .collect()
            })
            .unwrap_or_default();
        (base.to_string(), args)
    }

    /// Resolves type-position keywords to their class names (static version).
    ///
    /// Same as `resolve_type_keyword` but takes `&str` for use in validation contexts.
    fn resolve_type_keyword_static(name: &str) -> EcoString {
        match name {
            // BT-2016: Match both `nil` and `Nil`, consistent with resolve_type_keyword.
            "nil" | "Nil" => WellKnownClass::UndefinedObject.as_str().into(),
            "false" => "False".into(),
            "true" => "True".into(),
            _ => EcoString::from(name),
        }
    }

    /// Resolves type aliases that don't exist in the class hierarchy to their
    /// canonical class names. Returns `Some` with the resolved name if an alias
    /// was applied, `None` if the name is already canonical.
    ///
    /// BT-1877: `Nil` in type annotations maps to `UndefinedObject` in the
    /// hierarchy. Without this resolution, `Nil` is treated as an unknown type
    /// and the conservative fallback disables validation.
    fn resolve_type_alias(name: &EcoString) -> Option<EcoString> {
        match name.as_str() {
            "Nil" => Some(WellKnownClass::UndefinedObject.as_str().into()),
            _ => None,
        }
    }

    /// Emit a warning diagnostic for an unknown selector.
    /// `display_name` is the type the user wrote (e.g. a singleton `#infinity`),
    /// used only in the message text. `class_name` is the type we resolve
    /// selectors and "did you mean" suggestions against (e.g. `Symbol` for a
    /// singleton — BT-2679). They coincide for ordinary class receivers.
    fn emit_unknown_selector_warning(
        &mut self,
        display_name: &EcoString,
        class_name: &EcoString,
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
    ) {
        let side = if is_class_side { " class" } else { "" };
        let message: EcoString =
            format!("{display_name}{side} does not understand '{selector}'").into();

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
        let base_protocol = super::type_resolver::base_name_of_string(expected_protocol);

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
            InferredType::Dynamic(_) => {
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
                        // BT-2623: members now carry type args (e.g. `Array(Integer)`);
                        // conformance is a property of the base class, so strip them.
                        let base = super::type_resolver::base_name_of_string(m);
                        protocol_registry
                            .check_conformance(base, base_protocol, hierarchy)
                            .is_ok()
                    })
                else {
                    return; // Contains Dynamic — skip
                };
                if compat == total {
                    return; // All conform → pass
                }
                // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
                let union_display = arg_type
                    .display_for_diagnostic()
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
                        .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
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
            // ADR 0083: structural conformance of a class object's *class-side*
            // protocol is out of scope for Slice 1 — skip the metatype rather
            // than emit a false positive (same as `Never`, which diverges).
            // A `Negation` (`Symbol \ #foo`) is a narrowed `Symbol`; skip like
            // `Meta`/`Never` (ADR 0102). An `Intersection` (`P1 & P2`, ADR
            // 0102/BT-2743) as the *argument's own* inferred type is a rarer,
            // deeper case (the value's declared type is itself a stored
            // intersection) — skip conservatively rather than guess which
            // member to check; the flagship "parameter declared `P1 & P2`"
            // case is handled by splitting `expected_protocol` in
            // `check_protocol_conformance_in_expr`, not here.
            InferredType::Meta { .. }
            | InferredType::Never
            | InferredType::Negation { .. }
            | InferredType::Intersection { .. } => {}
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
        let base_name = super::type_resolver::base_name_of_string(type_name);

        // A protocol type is one that's in the registry and either not a class name,
        // or only present as a synthetic protocol class entry (added by register_protocol_classes)
        protocol_registry.has_protocol(base_name)
            && (!hierarchy.has_class(base_name) || hierarchy.is_protocol_class(base_name))
    }

    /// Splits a `type_name()`-rendered intersection string (`"P1 & P2 & …"`,
    /// ADR 0068 §Protocol Composition / ADR 0102 §1/§3, BT-2743) into its
    /// top-level `&`-joined parts.
    ///
    /// Respects parenthesis nesting so a generic type argument's own
    /// annotation (which may itself contain `(`/`)`) is never mistaken for a
    /// top-level split point. Returns `None` when the string has no
    /// top-level `&` — i.e. it isn't an intersection annotation, and callers
    /// should fall back to single-type handling (`is_protocol_type` /
    /// `check_protocol_argument_conformance`).
    ///
    /// Used by [`check_protocol_conformance_in_expr`](super::protocol::TypeChecker::check_protocol_conformance_in_expr)
    /// so a parameter declared `:: P1 & P2` is checked for conformance to
    /// **both** protocol parts, per ADR 0068's protocol-composition use case.
    pub(super) fn split_intersection_type_string(type_name: &str) -> Option<Vec<&str>> {
        let mut depth: usize = 0;
        let mut parts = Vec::new();
        let mut start = 0usize;
        for (i, byte) in type_name.bytes().enumerate() {
            match byte {
                b'(' => depth += 1,
                b')' => depth = depth.saturating_sub(1),
                b'&' if depth == 0 => {
                    parts.push(type_name[start..i].trim());
                    start = i + 1;
                }
                _ => {}
            }
        }
        if parts.is_empty() {
            return None;
        }
        parts.push(type_name[start..].trim());
        Some(parts)
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
    #[allow(clippy::too_many_lines)] // ADR 0102 added a `Negation` skip arm
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
                            // BT-2623: strip type args (`Array(Integer)` → `Array`);
                            // conformance is checked on the base class.
                            let base = super::type_resolver::base_name_of_string(m);
                            protocol_registry
                                .check_conformance(base, bound_protocol, hierarchy)
                                .is_ok()
                        })
                    else {
                        continue; // Contains Dynamic — skip
                    };
                    if compat == total {
                        continue; // All conform → pass
                    }
                    let param_name = param_names.get(i).map_or("?", |p| p.as_str());
                    // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
                    let union_display = arg
                        .display_for_diagnostic()
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
                            .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
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
                // ADR 0083: a metatype as a type argument is unparameterized and
                // its class-side bound conformance is out of scope here — skip.
                // Dynamic/Never values: can't verify bounds (skip silently).
                // A `Negation` (`Symbol \ #foo`) is a narrowed `Symbol`; skip
                // like `Meta`/`Dynamic`/`Never` (ADR 0102). An `Intersection`
                // (`P1 & P2`, ADR 0102/BT-2743) as a type argument is likewise
                // skipped — a bound is a single protocol name, and checking
                // it against one member of the intersection risks a false
                // positive when a *different* member is the one that conforms.
                InferredType::Meta { .. }
                | InferredType::Dynamic(_)
                | InferredType::Never
                | InferredType::Negation { .. }
                | InferredType::Intersection { .. } => {}
            }
        }
    }
}
