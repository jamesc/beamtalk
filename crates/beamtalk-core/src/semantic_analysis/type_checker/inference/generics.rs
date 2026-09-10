// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generic substitution and method-local type-parameter inference.
//!
//! **DDD Context:** Semantic Analysis
//!
//! Builds and resolves the substitution maps that turn a class's type
//! parameters (`Result(T, E)`) and a method's own local type parameters
//! (`inject:into:`'s `A`) into concrete [`InferredType`]s at a call site,
//! composing through the inheritance chain (ADR 0068 Phase 1b) where the
//! method is inherited from a superclass.

use std::collections::{HashMap, HashSet};

use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::type_checker::type_resolver;
use crate::semantic_analysis::type_checker::{
    DynamicReason, InferredType, TypeChecker, TypeStringContext,
};
use ecow::EcoString;

impl TypeChecker {
    /// Resolve a type parameter through class-level and method-local
    /// substitution maps. Returns the resolved type, or `Dynamic` if
    /// the parameter cannot be resolved.
    ///
    /// Handles nested generics (e.g., `List(E)`) by delegating to
    /// [`type_resolver::resolve_declared_type`], which recursively
    /// resolves inner type params.
    ///
    /// `param` is a structured [`DeclaredType`] rather than a raw
    /// string — the bare-name fast path (`method_subst` → `class_subst` →
    /// known-class → Dynamic) only ever matched a whole-string key anyway
    /// (a `Generic`'s rendered form like `"List(E)"` never appears as a
    /// substitution-map key), so it stays keyed on the `Simple` case; every
    /// other shape (`Generic`, `Union`, …) delegates to the shared resolver.
    pub(in crate::semantic_analysis::type_checker) fn resolve_type_param(
        param: &DeclaredType,
        class_subst: &HashMap<EcoString, InferredType>,
        method_subst: &HashMap<EcoString, InferredType>,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        if let DeclaredType::Simple(name) = param {
            // Check method-local substitution first (e.g., A from inject:into:)
            if let Some(ty) = method_subst.get(name) {
                return ty.clone();
            }
            // Then class-level substitution (e.g., E from List(String))
            if let Some(ty) = class_subst.get(name) {
                return ty.clone();
            }
            // If it's a known class name (e.g., Boolean, Integer), return it directly
            if hierarchy.has_class(name) {
                return InferredType::known(name.clone());
            }
            // Unresolved type param — stay Dynamic
            return InferredType::Dynamic(DynamicReason::UnannotatedParam);
        }
        // Nested generics (e.g., `List(E)`) and every other
        // non-bare shape delegate to the shared substitution-context
        // resolver, which recursively resolves inner type params. Merge
        // `method_subst` over `class_subst` (method-local wins) to match
        // this function's own bare-name precedence above.
        let mut subst: type_resolver::SubstitutionMap = class_subst.clone();
        subst.extend(method_subst.iter().map(|(k, v)| (k.clone(), v.clone())));
        type_resolver::resolve_declared_type(
            param,
            &subst,
            None,
            None,
            TypeStringContext::Substitution,
        )
    }

    /// Build a substitution map from a class's type parameters and concrete type arguments.
    ///
    /// Given a class like `Result(T, E)` and concrete args `[Integer, IOError]`,
    /// builds `{T → Integer, E → IOError}`.
    ///
    /// Returns an empty map if the class has no type params or the args are empty.
    pub(in crate::semantic_analysis::type_checker) fn build_substitution_map(
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
    /// **References:** ADR 0068 Challenge 4
    pub(in crate::semantic_analysis::type_checker) fn build_inherited_substitution_map(
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
                            super_args.push(InferredType::Dynamic(DynamicReason::Unknown));
                        }
                    }
                    SuperclassTypeArg::Concrete { declared } => {
                        if let DeclaredType::Simple(name) = declared {
                            // A "concrete" arg that's actually a bare name
                            // matching the current class's own subst keys —
                            // e.g. a param propagated through an edge case
                            // that didn't classify it as `ParamRef` at
                            // construction time. Preserves
                            // lookup-by-whole-name behaviour exactly.
                            if let Some(resolved) = current_subst.get(name) {
                                super_args.push(resolved.clone());
                            } else {
                                super_args.push(InferredType::known(name.clone()));
                            }
                        } else {
                            // A genuinely structured concrete type (e.g.
                            // `Base(List(Integer))`) — resolve through the
                            // shared resolver instead of the bare-name-only
                            // lookup above.
                            super_args.push(type_resolver::resolve_declared_type(
                                declared,
                                &current_subst,
                                None,
                                None,
                                TypeStringContext::Substitution,
                            ));
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

    /// Split a comma-separated list of type parameters, respecting nested parentheses.
    ///
    /// `"T, E"` → `["T", "E"]`
    /// `"GenResult(A, B), E"` → `["GenResult(A, B)", "E"]`
    ///
    /// Thin wrapper over the shared nesting-aware scanner
    /// (`string_utils::split_top_level`) — kept as a method so the
    /// many `Self::split_type_params` call sites in this module don't need
    /// to change.
    pub(in crate::semantic_analysis::type_checker) fn split_type_params(s: &str) -> Vec<&str> {
        crate::semantic_analysis::string_utils::split_top_level(s, ',')
    }

    /// Does the declared type mention `Self` / `Self class`
    /// *anywhere* in its tree (including, harmlessly, at the top level —
    /// callers reach this check only after already special-casing and
    /// returning on a bare top-level `Self`/`Self class`, so in practice
    /// this only ever fires for the *nested* case)?
    ///
    /// Walks the structured [`DeclaredType`] directly rather than
    /// substring-searching its rendered form.
    pub(in crate::semantic_analysis::type_checker) fn declared_type_contains_self(
        dt: &DeclaredType,
    ) -> bool {
        match dt {
            DeclaredType::SelfType | DeclaredType::SelfClass => true,
            DeclaredType::Simple(_) | DeclaredType::Singleton(_) | DeclaredType::ClassOf(_) => {
                false
            }
            DeclaredType::Union(members) => members.iter().any(Self::declared_type_contains_self),
            DeclaredType::Generic { parameters, .. } => {
                parameters.iter().any(Self::declared_type_contains_self)
            }
            DeclaredType::FalseOr(inner) => Self::declared_type_contains_self(inner),
            DeclaredType::Difference { base, excluded } => {
                Self::declared_type_contains_self(base)
                    || Self::declared_type_contains_self(excluded)
            }
            DeclaredType::Intersection { left, right } => {
                Self::declared_type_contains_self(left) || Self::declared_type_contains_self(right)
            }
        }
    }

    /// Infer method-local type parameters from call-site arguments.
    ///
    /// Extracts type params from ANY parametric parameter type — e.g., `Block(T, R)`,
    /// `Result(T, E)`, `Array(T)`, `Dictionary(K, V)`. For each declared type parameter
    /// in the param type, if it is method-local (not a class-level type param and not a
    /// known class name), it is matched positionally against the argument's actual `type_args`.
    pub(in crate::semantic_analysis::type_checker) fn infer_method_local_params(
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
            // This function's generic-param matching below is a
            // string-level split (`is_generic_type_param` / `split_generic_base`
            // over `&str`), not a `resolve_type_string` recursion — render the
            // structured `DeclaredType` once at the boundary (byte-identical to
            // the old stored string, see `DeclaredType`'s `Display`) and keep
            // the existing string-based matching logic unchanged below.
            let param_type: EcoString = param_type.to_string().into();
            let param_type = &param_type;
            let Some(arg_ty) = arg_types.get(i) else {
                continue;
            };

            // Handle plain (non-parametric) type param parameters.
            // e.g., `inject: initial :: A` — if A is method-local, map it to the arg type.
            //
            // For a bare `A` parameter, `A` represents the *whole* argument type
            // including any nilability. Don't strip nil here — that would turn
            // `identity: x :: A -> A` called with `String | Nil` into `A = String`
            // and lose the nullability in the inferred return. Nil-stripping is
            // only safe for the outer-generic path below (`List(T)` etc.) where
            // the union shape doesn't match the param shape anyway.
            if crate::semantic_analysis::type_checker::is_generic_type_param(param_type) {
                let param_eco: EcoString = param_type.clone();
                if !class_type_params.contains(&param_eco)
                    && !hierarchy.has_class(&param_eco)
                    && matches!(
                        arg_ty,
                        InferredType::Known { .. } | InferredType::Union { .. }
                    )
                {
                    Self::merge_method_local_binding(&mut method_subst, param_eco, arg_ty.clone());
                }
            }

            // Handle any parametric type: TypeName(A, B, ...) parameter types.
            // Parenthesis-aware split via the centralised helper.
            let (declared_base, declared_args_slice) =
                type_resolver::split_generic_base(param_type);
            if let Some(inner) = declared_args_slice {
                let declared_params = Self::split_type_params(inner);

                // Normalise the arg type — if it's a nullable union
                // (e.g. `List(String) | Nil`), strip nil and try to unify with
                // the non-nil member. This is the common "optional collection"
                // shape that previously fell through to Dynamic.
                let stripped;
                let effective_arg = if matches!(arg_ty, InferredType::Union { .. }) {
                    stripped = Self::non_nil_type(arg_ty);
                    &stripped
                } else {
                    arg_ty
                };

                // Match against the argument's actual type if it's a Known type
                if let InferredType::Known {
                    class_name: arg_class,
                    type_args,
                    ..
                } = effective_arg
                {
                    // Verify the base class matches (e.g., Block == Block, Result == Result)
                    if arg_class.as_str() == declared_base && !type_args.is_empty() {
                        // Zip declared params with actual type args positionally
                        for (declared, actual) in declared_params.iter().zip(type_args.iter()) {
                            let decl_eco: EcoString = (*declared).into();
                            // Only infer if this is a method-local type param
                            // (single uppercase letter, not a class-level param, not a known class)
                            if crate::semantic_analysis::type_checker::is_generic_type_param(
                                &decl_eco,
                            ) && !class_type_params.contains(&decl_eco)
                                && !hierarchy.has_class(&decl_eco)
                            {
                                Self::merge_method_local_binding(
                                    &mut method_subst,
                                    decl_eco,
                                    actual.clone(),
                                );
                            }
                        }
                    }
                }
            }
        }

        method_subst
    }

    /// Merge a new binding for a method-local type parameter, preferring Known
    /// types over Dynamic — except an *explicitly declared*
    /// `Dynamic` binding, which is authoritative and always wins.
    ///
    /// When the same type parameter appears in multiple argument
    /// positions (e.g. `Block(R) Block(R) -> R` in `ifTrue:ifFalse:`), a
    /// last-wins `insert` could collapse a Known return type to
    /// `Dynamic(UntypedFfi)` whenever one branch was an untyped FFI call.
    /// Preserve the Known binding instead so the method's declared return
    /// type survives the join. This applies broadly — every `DynamicReason`
    /// other than `ExplicitDynamic` means "we don't have enough information
    /// to give a concrete type", so Known wins regardless of *which*
    /// uninformative reason it is or which side arrived first (an earlier,
    /// narrower version of this rule only protected one arrival order and
    /// only two reasons, which under-protected the common case: most
    /// `Dynamic` bindings observed in real code are `UnannotatedParam`,
    /// `Unknown`, etc., not specifically `UntypedFfi`/`DynamicSpec`).
    ///
    /// `DynamicReason::ExplicitDynamic` means the *opposite* — the
    /// author wrote `Dynamic` in a type annotation (e.g. `T` in `Result(
    /// Dynamic, Error)`), so `okBlock :: Block(T, R)`'s inferred return is
    /// genuinely, deliberately `Dynamic`. That must survive being unified
    /// with a Known binding from a sibling argument position (e.g.
    /// `ifError:`'s block returning a concrete `nil`) — R can't soundly
    /// collapse to "definitely Nil" when one branch can produce anything.
    /// Whichever side is `ExplicitDynamic` always wins, in either order.
    ///
    /// Two Known/Union bindings (no Dynamic on either side) unify via
    /// `union_of` rather than last-wins, so neither is silently discarded.
    ///
    /// `pub(in crate::semantic_analysis::type_checker)` so `type_checker::tests` can
    /// unit-test the merge rules directly rather than only indirectly through diagnostics.
    pub(in crate::semantic_analysis::type_checker) fn merge_method_local_binding(
        method_subst: &mut HashMap<EcoString, InferredType>,
        key: EcoString,
        new_ty: InferredType,
    ) {
        let Some(existing) = method_subst.get(&key).cloned() else {
            method_subst.insert(key, new_ty);
            return;
        };
        let is_explicit_dynamic =
            |ty: &InferredType| matches!(ty, InferredType::Dynamic(DynamicReason::ExplicitDynamic));
        if is_explicit_dynamic(&existing) {
            return; // Authoritative — keep it regardless of new_ty.
        }
        if is_explicit_dynamic(&new_ty) {
            method_subst.insert(key, new_ty);
            return;
        }
        if matches!(
            existing,
            InferredType::Known { .. } | InferredType::Union { .. }
        ) && matches!(new_ty, InferredType::Dynamic(_))
        {
            return; // Keep the existing Known/Union — Dynamic loses.
        }
        if matches!(existing, InferredType::Dynamic(_))
            && matches!(
                new_ty,
                InferredType::Known { .. } | InferredType::Union { .. }
            )
        {
            method_subst.insert(key, new_ty);
            return;
        }
        method_subst.insert(key, InferredType::union_of(&[existing, new_ty]));
    }
}
