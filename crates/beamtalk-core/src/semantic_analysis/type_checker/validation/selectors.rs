// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-side and instance-side selector validation: whether a send resolves on its receiver, access to internal methods, and typed method/state annotation checks.
//!
//! **DDD Context:** Semantic Analysis

//! `TypeChecker::state_default_types` (BT-3481), keeping the direction
//! one-way instead of calling back into `inference/`.

use std::collections::HashMap;

use crate::ast::Expression;
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::receiver_knowledge;
use crate::semantic_analysis::string_utils::is_symbol_singleton_name;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Severity, Span};
use ecow::EcoString;

use super::super::type_resolver;
use super::super::well_known::WellKnownClass;
use super::super::{DynamicReason, InferredType, TypeChecker};

/// How a send resolves on a protocol-typed receiver — see
/// [`TypeChecker::protocol_send_resolution`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in super::super) enum ProtocolSendResolution {
    /// One of the protocol's own (required or provided) selectors.
    Protocol,
    /// Not a protocol selector, but a method every object answers; its
    /// signature comes from `Object`'s method table.
    Object,
    /// Neither — a does-not-understand.
    Unknown,
}

impl TypeChecker {
    /// Check a class-side message send (e.g., `Counter spawn`, `Object new`).
    ///
    /// When `arg_types` is non-empty and the class has type parameters,
    /// constructor inference (ADR 0068 Phase 1c) kicks in: the type checker
    /// maps the class method's parameter types (e.g., `T`) against the concrete
    /// argument types (e.g., `Integer`) to infer the class's type arguments.
    /// Unresolved params default to `Dynamic`.
    #[allow(clippy::too_many_lines)] // one branch per receiver-knowledge shape — irreducible
    pub(in super::super) fn check_class_side_send(
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

        // ADR 0071 Phase 3: E0403 — cross-package send to internal class method
        self.check_internal_method_access(class_name, selector, span, hierarchy, true);

        // Check if class-side method exists (skip warning for DNU override classes)
        let has_class_method = hierarchy.find_class_method(class_name, selector).is_some();

        // Sealed value types (like Erlang) dispatch class-side messages
        // through instance dispatch. Instance-side DNU suppresses class-side
        // warnings only for sealed classes — not all classes with instance DNU,
        // which would hide valid diagnostics on normal classes. This carve-out
        // is specific to this call site (see `receiver_knowledge` doc comment
        // for why it is not folded into the shared classifier).
        let is_sealed_with_instance_dnu = hierarchy.has_instance_dnu_override(class_name)
            && hierarchy
                .get_class(class_name)
                .is_some_and(|info| info.is_sealed);

        // Whether the selector is absent from everything the checker can see
        // on this class object — no class method, no class-object-protocol
        // method, no implicit constructor, and no DNU override that would
        // answer it (BT-4526). Drives both the DNU below and the honest
        // `Dynamic` reason of the terminal fallback.
        let selector_absent = !has_class_method
            && !is_sealed_with_instance_dnu
            && !hierarchy.has_class_dnu_override(class_name)
            && !Self::class_chain_resolves(class_name, selector, hierarchy);

        // ADR 0100 Rule 1: the completeness-ladder classifier folds
        // in the class-side DNU override and cross-file-parent
        // suppression — only a `ClosedComplete` receiver's absent class-side
        // method is diagnosable.
        let dnu_reported = selector_absent
            && receiver_knowledge::classify_receiver(class_name, hierarchy, true)
                .is_closed_complete();
        if dnu_reported {
            self.emit_unknown_selector_warning(
                class_name,
                class_name,
                selector,
                span,
                hierarchy,
                true,
                Severity::Hint,
                None,
                None,
            );
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
                        if matches!(ret_ty, DeclaredType::SelfType) {
                            return Self::infer_constructor_type(
                                class_name, hierarchy, selector, arg_types,
                            );
                        }
                        // Parse parameterised return types like
                        // `Result(List(String), Error)` into a fully-nested
                        // `Known { class_name, type_args }` so that downstream
                        // sends on the bound local resolve correctly.
                        //
                        // `Self` appearing as a (nested) type argument —
                        // e.g. `class named: -> Result(Self, Error)` on Actor —
                        // resolves to the static receiver class. This powers
                        // ADR 0079's typed-lookup API where `Counter named: #c`
                        // should infer as `Result(Counter, Error)`.
                        //
                        // Build the class-level substitution map (for generic
                        // class methods like `class wrap: v :: T -> Box(T)`)
                        // by mapping declared param types to argument types.
                        let class_subst = Self::class_method_substitution(
                            class_name, hierarchy, method, arg_types,
                        );
                        // Method-local type params (`inject: initial :: A
                        // into: Block(A, E, A) -> A`) bind from the arguments
                        // exactly as on the instance side (BT-4110).
                        let method_subst = Self::infer_method_local_params(
                            method,
                            arg_types,
                            &class_subst,
                            hierarchy,
                            &method.defined_in,
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
                        // Thread `self_type` under the reserved
                        // `Self` subst key (see `resolve_declared_type`'s
                        // `SelfType` arm) so nested `Self` in the return type
                        // resolves the same way the old `self_type` parameter
                        // did.
                        return type_resolver::resolve_declared_type_in(
                            ret_ty,
                            &type_resolver::merge_substitutions(
                                &class_subst,
                                &method_subst,
                                Some(&self_type),
                            ),
                            self.protocol_registry.as_deref(),
                            self.alias_registry.as_deref(),
                            super::super::TypeStringContext::Substitution,
                            Some(hierarchy),
                        );
                    }
                }
                // Fall back to return types inferred earlier in this pass.
                // Return the full InferredType (including type_args).
                let key = (class_name.clone(), EcoString::from(selector), true);
                if let Some(ret_ty) = self.method_return_types.get(&key) {
                    return ret_ty.clone();
                }
                // When no class-side method exists, the runtime
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
                            matches!(ty, DeclaredType::Simple(n) if WellKnownClass::from_str(n) == Some(WellKnownClass::Never))
                        }) {
                            return InferredType::Never;
                        }
                    }
                }
                // BT-4526: an absent selector is not an "unannotated return".
                // Where the DNU was reported it is the root cause, so the
                // result is a non-warning `DynamicReceiver` (as on the
                // instance side); where the surface is open (an unseen
                // extension might define it), say the selector is unresolved.
                InferredType::Dynamic(if dnu_reported {
                    DynamicReason::DynamicReceiver
                } else if selector_absent {
                    self.unresolved_selector_sends.insert(span);
                    DynamicReason::UnresolvedSelector
                } else {
                    DynamicReason::UnannotatedReturn
                })
            }
        }
    }

    /// Whether a class object answers `selector` without a class method of its
    /// own: through the `Class → Behaviour → Object → ProtoObject`
    /// instance-method chain, an implicit constructor, or an actor factory.
    ///
    /// At runtime, class objects dispatch through this chain (ADR 0032 Phase 0
    /// fallthrough), so the type checker must model the same path.
    ///
    /// spawn/spawnWith: are still instance methods on Actor but routed
    /// class-side by `beamtalk_class_dispatch.erl` — they need the bypass.
    /// new/new: are proper class methods on Value and found via
    /// `find_class_method` through the hierarchy, so no bypass needed.
    ///
    /// ADR 0083: `new` / `new:` / `basicNew` are implicitly available on
    /// every *concrete* Object subclass via the runtime's class instantiation
    /// path, even when no explicit class-side `new` is declared (e.g. a
    /// `sealed typed Object subclass`), so they are not reported as DNU.
    ///
    /// Excluded:
    ///  * Abstract classes — instantiating them is a genuine error
    ///    (`infer_constructor_type` keeps the result Dynamic for them).
    ///  * Actor subclasses — they are spawned (`spawn` / `spawnWith:`), not
    ///    `new`'d. In the common case this exclusion is moot for `new`/`new:`:
    ///    actor.bt declares real `class sealed new` / `new:`, so the caller's
    ///    `has_class_method` is already `true`. It stays as defense-in-depth
    ///    for a partially-unresolved hierarchy (e.g. a cross-package actor
    ///    whose ancestor chain `find_class_method` can't see) — see
    ///    `new_on_actor_subclass_no_longer_dnu` for the still-current behavior
    ///    and `test_actor_new_error_in_standalone_method` /
    ///    `check_actor_new_usage` for the actual "use spawn" diagnostic.
    fn class_chain_resolves(
        class_name: &EcoString,
        selector: &str,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        let is_factory_selector = matches!(selector, "spawn" | "spawnWith:");
        let is_implicit_constructor = matches!(selector, "new" | "new:" | "basicNew")
            && !hierarchy.is_abstract(class_name)
            && !hierarchy.is_actor_subclass(class_name);
        hierarchy.resolves_selector("Class", selector)
            || is_implicit_constructor
            || (is_factory_selector && hierarchy.resolves_selector(class_name, selector))
    }

    /// Build the class-level substitution map for a class-method call.
    ///
    /// Walks the method's declared parameter types and, for each parameter
    /// whose declared type is one of the class's type parameters (e.g. `T`
    /// in `Box(T)`), records the mapping `T -> arg_type`. The resulting map
    /// is threaded into [`type_resolver::resolve_declared_type`]
    /// so the return type's references to those params resolve to the
    /// concrete inferred types.
    ///
    /// For non-generic classes (or classes with no class-method type
    /// substitution to do), the returned map is empty.
    ///
    /// **ADR 0083 Slice 2: nested element-type composition.** A
    /// class-side constructor whose parameter mentions a class type param
    /// *nested* inside a generic — e.g. `class withAll: list :: List(E) -> Self`
    /// on `Set(E)` — recovers `E` from the matching-base argument:
    /// `Set withAll: aList(Integer)` binds `E -> Integer`, so the `-> Self`
    /// return composes to `Set(Integer)` rather than the erased `Set(Dynamic)`.
    /// This mirrors the instance-side nested unification in
    /// [`super::super::TypeChecker::infer_method_local_params`] (which handles only
    /// *method-local* params); here we resolve *class-level* params on the
    /// class-side. Exact-match params are inserted directly and win: the nested
    /// helper's merge guard refuses to overwrite a `Known`/`Union` binding an
    /// exact match already set.
    ///
    /// **References:** preserve generic return types on class-method
    /// assignments, ADR 0068 Phase 1c, ADR 0083 Slice 2 (nested composition).
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
            if let DeclaredType::Simple(name) = param_ty {
                if class_info.type_params.contains(name) {
                    subst.insert(name.clone(), arg_ty.clone());
                    continue;
                }
            }
            // ADR 0083 Slice 2: nested match — a param shaped like `List(E)`
            // where `E` is a class type param. Recover the binding from a
            // matching-base `Known` argument (e.g. `List(Integer)` ⇒
            // `E -> Integer`). Only fills params not already bound by an exact
            // match, so a direct `T` parameter still wins.
            //
            // `unify_nested_class_params` is a string-level helper — render
            // once at this boundary.
            let param_ty_str = param_ty.to_string();
            Self::unify_nested_class_params(&param_ty_str, arg_ty, class_info, &mut subst);
        }
        subst
    }

    /// Recover class-level type-param bindings nested inside a generic parameter
    /// shape (ADR 0083 Slice 2).
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
        let (declared_base, declared_args) = type_resolver::split_generic_base(declared);
        let Some(inner) = declared_args else {
            return;
        };
        // Strip a nilable union (`List(String) | Nil`) to its non-nil member so
        // the optional-collection shape still composes.
        let stripped;
        let effective_arg = if matches!(arg_ty, InferredType::Union { .. }) {
            stripped = super::super::TypeChecker::non_nil_type(arg_ty);
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
        let declared_params = super::super::TypeChecker::split_type_params(inner);
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
            Self::class_method_substitution(class_name, hierarchy, method, arg_types)
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
            provenance: super::super::TypeProvenance::Inferred(Span::default()),
        }
    }

    /// How an instance-side send of `selector` resolves on a receiver typed
    /// as protocol `name`, or `None` when `name` is not a registered
    /// protocol (a hierarchy class — including the LSP's synthetic protocol
    /// entries, which inherit `Object` already — or an unknown name).
    ///
    /// A protocol-typed value is still an object (BT-4594): the protocol's
    /// own selectors (required ∪ provided, through `extending:`) win, then
    /// the methods every object answers (`Object`/`ProtoObject`). This is
    /// the one rule both DNU validation ([`Self::check_instance_selector`])
    /// and inference (the `Known` and union receiver paths) read, so they
    /// cannot disagree about which selectors a protocol receiver
    /// understands. An `Object` method's return type comes from `Object`'s
    /// method table; a send of the protocol's own selector still infers
    /// `Dynamic` on both paths.
    pub(in super::super) fn protocol_send_resolution(
        &self,
        name: &str,
        selector: &str,
        hierarchy: &ClassHierarchy,
    ) -> Option<ProtocolSendResolution> {
        if hierarchy.has_class(name) {
            return None;
        }
        let registry = self.protocol_registry.as_deref()?;
        let info = registry.get(name)?;
        Some(
            if info
                .all_conformance_selectors(registry)
                .iter()
                .any(|s| s.as_str() == selector)
            {
                ProtocolSendResolution::Protocol
            } else if hierarchy.resolves_selector(WellKnownClass::Object.as_str(), selector) {
                ProtocolSendResolution::Object
            } else {
                ProtocolSendResolution::Unknown
            },
        )
    }

    /// Check if an instance-side selector is valid for a known class.
    pub(in super::super) fn check_instance_selector(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        // A singleton type (`#text`) is a subtype of `Symbol` but is not
        // itself a class in the hierarchy. Resolve its selectors through
        // `Symbol`'s protocol so DNU validation still applies — narrowing a getter
        // from `Symbol` to a singleton (e.g. `#text | #json`, then a single
        // member) would otherwise silently drop the selector checking it had.
        // Mirrors the singleton-union inference handling.
        //
        // `effective_class` is what we resolve selectors/suggestions
        // against (`Symbol` for a singleton); `class_name` stays the type the user
        // wrote so DNU messages name the singleton (e.g. `#infinity`) rather than
        // `Symbol`.
        let symbol: EcoString;
        let effective_class: &EcoString =
            if is_symbol_singleton_name(class_name) && !class_name.contains('|') {
                symbol = EcoString::from("Symbol");
                &symbol
            } else {
                class_name
            };
        if !hierarchy.has_class(effective_class) {
            // A protocol-typed receiver (a `:: P` annotation, or
            // `respondsTo:` narrowing) answers its protocol's selectors and
            // every `Object` method; anything else is a DNU (BT-4594).
            if self.protocol_send_resolution(effective_class, selector, hierarchy)
                == Some(ProtocolSendResolution::Unknown)
            {
                self.emit_unknown_selector_warning(
                    class_name,
                    effective_class,
                    selector,
                    span,
                    hierarchy,
                    false,
                    Severity::Hint,
                    None,
                    None,
                );
            }
            return;
        }

        // ADR 0071 Phase 3: E0403 — cross-package send to internal method
        self.check_internal_method_access(effective_class, selector, span, hierarchy, false);

        // ADR 0100 Rule 1: suppress unless the checker's knowledge
        // of the receiver's method surface is closed and complete (folds in
        // the instance-side DNU override and cross-file-parent checks).
        if !receiver_knowledge::classify_receiver(effective_class, hierarchy, false)
            .is_closed_complete()
        {
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
                Severity::Hint,
                None,
                None,
            );
        }
    }

    /// ADR 0071 Phase 3: E0403 — cross-package send to internal method.
    ///
    /// When the receiver type is known and the target method is `internal` to
    /// another package, emit a hard error. Skipped when no current package is
    /// set (REPL, single-file scripts) or when sending to a method in the
    /// same package.
    pub(in super::super) fn check_internal_method_access(
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
    pub(in super::super) fn check_typed_method_annotations(
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
    pub(in super::super) fn check_typed_state_annotations(
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
}
