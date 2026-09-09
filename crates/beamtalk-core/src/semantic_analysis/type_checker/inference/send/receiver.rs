// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Receiver-typed message-send dispatch.
//!
//! **DDD Context:** Semantic Analysis
//!
//! The core of message-send type resolution: given a receiver expression
//! and its already-inferred type, validates the selector and resolves the
//! send's result type. Covers every receiver shape — a syntactic
//! `ClassReference`, a type-driven `Meta{C}` (ADR 0083), a concrete `Known`
//! receiver's instance-side method lookup, and a `Union`-typed receiver's
//! per-member resolution (BT-1857).

use crate::ast::{Expression, MessageSelector, WellKnownSelector};
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::receiver_knowledge;
use crate::semantic_analysis::type_checker::type_resolver;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::semantic_analysis::type_checker::{
    DynamicReason, InferredType, TypeChecker, TypeEnv, TypeStringContext, narrowing,
};
use crate::source_analysis::{Diagnostic, Severity, Span, is_equality_operator};
use ecow::EcoString;

impl TypeChecker {
    /// Variant of [`Self::infer_message_send`] that takes a pre-computed receiver
    /// type, avoiding a second walk of the receiver subtree.
    ///
    /// Used by the `Expression::Cascade` arm (BT-2035): the cascade's first send
    /// is itself a `MessageSend`, whose inner receiver type is needed both to
    /// resolve the first send and to dispatch the cascaded messages. Re-inferring
    /// the inner subtree via `infer_expr` would re-emit any DNU / type warnings
    /// it produces. By threading the receiver type through, we walk the inner
    /// subtree exactly once.
    #[allow(clippy::too_many_arguments)] // split from infer_message_send to share body
    #[allow(clippy::too_many_lines)] // generic substitution adds necessary branches
    pub(in crate::semantic_analysis::type_checker) fn infer_message_send_with_receiver_ty(
        &mut self,
        receiver: &Expression,
        receiver_ty: InferredType,
        selector: &MessageSelector,
        arguments: &[Expression],
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        // ADR 0102 §5 (BT-2744): a `Negation{base, excluded}`-typed receiver's
        // method lookup / conformance resolves through `base` — "identically
        // to a bare `base`-typed receiver" (every class admitted by the
        // negation is, by construction, a subclass of `base`, so it declares
        // no capability beyond `base`'s). The simplest faithful
        // implementation dispatches the *entire* send as if the receiver
        // were typed `base`, which also gives Q4 (ADR 0100 receiver-knowledge
        // classification) for free — DNU/argument checks below fall through
        // to exactly the same `hierarchy`/`check_instance_selector` path a
        // bare `base`-typed receiver would take. One deliberate
        // simplification: a `Self`-returning method's result widens back to
        // `base` rather than re-wrapping the exclusion — the ADR specifies
        // conformance/lookup parity with `base`, not flow-preservation of the
        // exclusion through `Self`.
        let receiver_ty = if let InferredType::Negation { base, .. } = receiver_ty {
            *base
        } else {
            receiver_ty
        };

        let selector_name = selector.name();

        // Control-flow narrowing (ADR 0068 Phase 1g):
        // When the selector is ifTrue:, ifFalse:, or ifTrue:ifFalse:, detect
        // type-testing patterns in the receiver and narrow variable types inside
        // block arguments.
        let narrowing = if matches!(
            selector.well_known(),
            Some(
                WellKnownSelector::IfTrue
                    | WellKnownSelector::IfFalse
                    | WellKnownSelector::IfTrueIfFalse
            )
        ) {
            Self::detect_narrowing(receiver)
                .map(|info| self.refine_responds_to_narrowing(info))
                .map(|info| {
                    Self::refine_result_narrowing(
                        info,
                        env,
                        hierarchy,
                        self.alias_registry.as_ref(),
                    )
                })
                .map(|info| {
                    Self::refine_singleton_narrowing(
                        info,
                        env,
                        hierarchy,
                        self.alias_registry.as_ref(),
                    )
                })
                .map(|info| self.refine_class_narrowing(info, env, hierarchy, span))
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
        } else if selector.well_known() == Some(WellKnownSelector::OnDo) {
            // BT-2045: Exception handler block parameter inference.
            // `[...] on: SomeException do: [:e | ...]` — infer `e` as `SomeException`
            // when the first argument is a class reference.
            self.infer_args_for_on_do(arguments, hierarchy, env, in_abstract_method)
        } else if matches!(
            selector.well_known(),
            Some(
                WellKnownSelector::IfNil
                    | WellKnownSelector::IfNotNil
                    | WellKnownSelector::IfNilIfNotNil
                    | WellKnownSelector::IfNotNilIfNil
            )
        ) {
            // BT-2046: Narrow block parameter of `ifNotNil: [:x | ...]` to the
            // non-nil branch of the receiver's type. Dual of the receiver-side
            // `isNil ifFalse:` narrowing (BT-2048).
            // BT-2824: Solo `ifNil:` is routed through here too, purely so its
            // niladic block's `Block(..., R)` return type is preserved — the
            // generic `infer_args_with_block_context` path requires a `Known`
            // receiver to resolve block param types from a method signature,
            // which a `T | Nil` union receiver never is.
            self.infer_args_for_if_not_nil(
                &selector_name,
                arguments,
                &receiver_ty,
                hierarchy,
                env,
                in_abstract_method,
            )
        } else if let (true, Some(var_key), Some(arg)) = (
            // BT-3462: `and:` stays a string comparison rather than a
            // `WellKnownSelector` variant, matching `state_threading_selectors`'s
            // module doc — `and:`/`or:` are ordinary self-hosted `Boolean`
            // methods (`boolean.bt`), not selectors the type checker or
            // codegen intrinsify, so adding a lone `And` variant (with no
            // `Or` counterpart) would misrepresent that boundary.
            selector_name == "and:",
            Self::detect_not_nil_and_narrowing(receiver),
            arguments.first(),
        ) {
            // BT-2872: `X notNil and: [...]` narrows `X` to non-nil inside the
            // block argument. Unlike the `ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`
            // narrowing table (which narrows to a statically known branch
            // type), the non-nil type here depends on `X`'s current type in
            // `env`, so it's resolved here rather than through `NarrowingInfo`.
            // Because `TypeEnv::child()` clones the whole binding map instead
            // of layering scopes, nested sends inside the block — including
            // further `and:` chains and binary-message argument positions
            // like `5 >= local` in `local > 0 and: [5 >= local]` — see the
            // narrowed type too, closing the gap where only receiver
            // positions were narrowed.
            let current_ty = Self::resolve_narrowing_variable_type(
                &var_key,
                env,
                hierarchy,
                self.alias_registry.as_ref(),
            );
            let non_nil_ty = Self::non_nil_type(&current_ty);
            vec![self.infer_block_with_narrowing(
                arg,
                &var_key,
                &non_nil_ty,
                hierarchy,
                env,
                in_abstract_method,
            )]
        } else {
            self.infer_generic_send_args(
                arguments,
                receiver,
                &receiver_ty,
                &selector_name,
                hierarchy,
                env,
                in_abstract_method,
            )
        };

        // Handle asType: compile-time type assertion (ADR 0025 Phase 2b)
        // `expr asType: SomeClass` asserts expr is SomeClass, returns Known(SomeClass)
        //
        // BT-3462: stays a string comparison rather than a `WellKnownSelector`
        // variant — `beamtalk-codegen`'s `expr_shape::selector_dispatches_via_self`
        // already documents `asType:` (alongside `yourself`) as a
        // "class-specific or compile-time-only construct that does not
        // warrant universal selector classification".
        if selector_name == "asType:" {
            if let Some(Expression::ClassReference { name, .. }) = arguments.first() {
                return InferredType::known(name.name.clone());
            }
            return receiver_ty;
        }

        // BT-2047: `ifNil:ifNotNil:` / `ifNotNil:ifNil:` return the union of
        // both branch bodies' return types. `infer_args_for_if_not_nil`
        // (BT-2046) already inferred both branches as `Block(..., R)` with
        // narrowed params, so we read back R from each arg and union them.
        // Blocks with a non-local return (`^`) exit the enclosing method —
        // their branch contributes `Never`, and `union_of` skips Never, so
        // the expression's type comes from the surviving branch.
        //
        // Restricted to non-class-side receivers: `ClassName ifNil: ... ifNotNil: ...`
        // and `self ifNil: ... ifNotNil: ...` inside a class method must
        // flow through `check_class_side_send` so an invalid metaclass send
        // still emits DNU. The helper unwraps parens so `(ClassName) ifNil: ...`
        // and `(self) ifNil: ...` aren't accidentally treated as non-class-side.
        let is_class_side_receiver = Self::is_class_side_receiver(receiver, env);
        if !is_class_side_receiver {
            if matches!(
                selector.well_known(),
                Some(WellKnownSelector::IfNilIfNotNil | WellKnownSelector::IfNotNilIfNil)
            ) {
                if let Some(ty) = Self::if_nil_branch_union_ret_ty(arguments, &arg_types) {
                    return ty;
                }
            } else if matches!(
                selector.well_known(),
                Some(WellKnownSelector::IfNil | WellKnownSelector::IfNotNil)
            ) {
                // BT-2824: Solo `ifNil:` / `ifNotNil:` on a `T | Nil` union
                // receiver infer as `T | R` / `R | Nil` — the union of the
                // "self" branch (executed when the nil-check doesn't match)
                // and the block branch's inferred return type.
                if let Some(ty) = Self::if_nil_solo_union_ret_ty(
                    &selector_name,
                    &receiver_ty,
                    arguments,
                    &arg_types,
                    hierarchy,
                ) {
                    return ty;
                }
            }
        }

        // Validate binary operand types when both sides are known
        // Only check if the receiver type actually defines the operator (avoids
        // duplicate warnings when the selector is already unknown).
        //
        // BT-2843: `binary_operand_check_ran` tracks whether
        // `check_binary_operand_types` actually validated this argument —
        // not merely whether it was called. `check_binary_operand_types`
        // returns `false` for Known/Known shapes it has no bespoke logic for
        // (e.g. `String>>,`, `Integer>>**` — any binary selector outside
        // +-*/ arithmetic, </>/<=/>= comparison, or `++` concat on String),
        // so those must still fall through to the generic
        // `check_argument_types` below, exactly like a `Union` argument
        // that fails the Known/Known destructure entirely.
        let mut binary_operand_check_ran = false;
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
                            env.get_local_origin(&ident.name)
                                .map(|o| (o.description.clone(), Some(o.span)))
                        } else {
                            None
                        }
                    });
                    binary_operand_check_ran = self.check_binary_operand_types(
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

        // If receiver is a class reference, check class-side methods.
        // BT-2158: unwrap parens so `(HTTPRouter) foo:` dispatches class-side
        // — matches the block-param inference normalisation above.
        if let Expression::ClassReference { name, package, .. } = receiver.unwrap_parens() {
            let class_name = &name.name;

            // ADR 0075: `Erlang <module>` — return ErlangModule<module_name> type
            // to enable FFI call type inference on the outer message send.
            // BT-1880: Class protocol selectors (class, new, superclass, etc.)
            // must NOT be intercepted as module lookups — they are handled by
            // normal class-side dispatch. BT-3079: package-qualified references
            // (`json@Erlang lists`) are excluded too — they name a package-scoped
            // class, not the compiler's built-in FFI bridge. Both rules are
            // centralized in `crate::ffi_receiver` (mirrored here via
            // `is_class_protocol_selector` since this call site splits the
            // receiver/selector apart rather than holding the combined send).
            if class_name == "Erlang" && package.is_none() {
                if let MessageSelector::Unary(module_name) = selector {
                    if !super::is_class_protocol_selector(module_name) {
                        // Static module name: `Erlang lists` → ErlangModule<lists>
                        return InferredType::Known {
                            class_name: EcoString::from(WellKnownClass::ErlangModule.as_str()),
                            type_args: vec![InferredType::Known {
                                class_name: module_name.clone(),
                                type_args: vec![],
                                provenance:
                                    crate::semantic_analysis::type_checker::TypeProvenance::Inferred(
                                        span,
                                    ),
                            }],
                            provenance:
                                crate::semantic_analysis::type_checker::TypeProvenance::Inferred(
                                    span,
                                ),
                        };
                    }
                }
                // Class protocol selector or dynamic module: fall through to
                // normal class-side dispatch.
            }

            self.check_argument_types(
                class_name,
                &selector_name,
                &arg_types,
                span,
                hierarchy,
                true,
                Some(arguments),
                Some(env),
                &[],
            );
            // ADR 0104 Phase 2 (BT-2750): `C spawnWith: #{...}` literal-map key check.
            self.check_spawn_with_map_keys(class_name, &selector_name, arguments, hierarchy);
            return self.check_class_side_send(
                class_name,
                &selector_name,
                span,
                hierarchy,
                &arg_types,
            );
        }

        // ADR 0083: type-driven class-side dispatch. When the receiver's
        // *inferred type* is a metatype `Meta{C}` — e.g. a value typed
        // `Self class` / `X class`, the result of `obj class`, or a class value
        // flowing through a variable/collection/FFI return — route the send to
        // class-side lookup on `C` exactly like a syntactic `C foo:`. This
        // generalizes `is_class_side_receiver` from a syntactic test (class
        // literal / `self` in a class method) to a type-driven one.
        if let InferredType::Meta {
            class_name: ref meta_class,
            ..
        } = receiver_ty
        {
            // The equality/identity comparison operators (`==`, `=:=`,
            // `/=`, `=/=`) are value/identity comparisons every object —
            // including a class object — supports at runtime via the universal
            // `Object`/`ProtoObject` protocol, but they are not modelled as
            // hierarchy methods. Routing them through class-side DNU lookup
            // would emit a false `C class does not understand '=:='` (it broke
            // `x class =:= Foo` narrowing). Treat ONLY these comparison
            // selectors as Boolean-returning without a class-side check —
            // matching the pre-0083 behaviour where `Self class` was Dynamic.
            // Other binary selectors (e.g. `+`) must fall through to normal
            // class-side / tower lookup (and DNU if unresolved); typing
            // `SomeClass + 1` as Boolean would be wrong and suppress the DNU.
            if let MessageSelector::Binary(op) = selector {
                if is_equality_operator(op) {
                    return InferredType::known("Boolean");
                }
            }
            // `aClass class` is the metaclass of the class object. The static
            // hierarchy doesn't track per-class metaclasses precisely, so fall
            // back to the `Metaclass` tower class (BT-1952 parity).
            if selector.well_known() == Some(WellKnownSelector::Class) {
                return InferredType::known("Metaclass");
            }
            self.check_argument_types(
                meta_class,
                &selector_name,
                &arg_types,
                span,
                hierarchy,
                true,
                Some(arguments),
                Some(env),
                &[],
            );
            // ADR 0104 Phase 2 (BT-2750): type-driven `cls spawnWith: #{...}`
            // (receiver typed `Meta{C}`) literal-map key check.
            self.check_spawn_with_map_keys(meta_class, &selector_name, arguments, hierarchy);
            let class_side =
                self.check_class_side_send(meta_class, &selector_name, span, hierarchy, &arg_types);
            // ADR 0083: when no class-side method on `C` defined the result, a
            // class object still responds to its metaclass-tower *instance*
            // protocol (`Metaclass → Class → Behaviour → Object → ProtoObject`)
            // at runtime — e.g. `Behaviour>>name -> Symbol`,
            // `Behaviour>>superclass`. `check_class_side_send` only propagates
            // `-> Never` from that chain (to avoid false DNUs on class
            // *literals*); for a metatype-typed receiver we know the value is a
            // class object, so apply the tower instance method's declared
            // return type. Only overrides a Dynamic result, never a concrete
            // class-side answer.
            if matches!(class_side, InferredType::Dynamic(_)) {
                if let Some(ty) =
                    Self::class_object_tower_return(&selector_name, hierarchy, meta_class)
                {
                    return ty;
                }
            }
            return class_side;
        }

        // BT-2868: set when a `Known`-receiver's solo `ifTrue:`/`ifFalse:`
        // resolves to a method with no declared return type by design (e.g.
        // `Boolean>>ifTrue:` — see `stdlib/src/boolean.bt`). Distinguishes
        // the terminal Dynamic fallback below from a genuinely
        // Dynamic/unresolved receiver so the reported reason is honest
        // instead of always `DynamicReceiver`. Deliberately narrow to this
        // selector family — see the comment where this is set.
        let mut known_method_unannotated_return = false;

        // For instance-side sends on known types
        if let InferredType::Known {
            ref class_name,
            ref type_args,
            ..
        } = receiver_ty
        {
            // In class methods, self sends should check class-side methods.
            // BT-2158: unwrap parens so `(self) foo:` dispatches class-side.
            if env.in_class_method && Self::is_self_receiver(receiver.unwrap_parens()) {
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
                        &[],
                    );
                    // ADR 0104 Phase 2 (BT-2750): `self spawnWith: #{...}`
                    // literal-map key check — the class-reference and
                    // Meta-typed-receiver branches above already run this;
                    // BT-3469 unified the cascade continuation loop onto
                    // this shared path, which surfaced that this branch was
                    // the one class-side shape missing it.
                    self.check_spawn_with_map_keys(
                        class_name,
                        &selector_name,
                        arguments,
                        hierarchy,
                    );
                    return self.check_class_side_send(
                        class_name,
                        &selector_name,
                        span,
                        hierarchy,
                        &arg_types,
                    );
                }
                return InferredType::Dynamic(DynamicReason::DynamicReceiver);
            }

            // ADR 0075: FFI call type inference for ErlangModule<module_name>.
            // When the receiver is typed as ErlangModule with a known module name
            // (from `Erlang lists` or a variable assigned from one), extract the
            // Erlang function name and arity, then look up in NativeTypeRegistry.
            // BT-1880: Class protocol selectors (class, new, printString, etc.)
            // and binary selectors (==, etc.) on ErlangModule instances must use
            // normal dispatch, not FFI lookup.
            if WellKnownClass::from_str(class_name) == Some(WellKnownClass::ErlangModule)
                && !super::is_class_protocol_selector(&selector_name)
                && !matches!(selector, MessageSelector::Binary(_))
            {
                return self
                    .infer_ffi_call(type_args, selector, arguments, &arg_types, span, hierarchy);
            }

            // BT-2254 (ADR 0075 amendment): literal-index tuple access.
            // `aTuple at: <literal int>` on a `Tuple(T1, …, Tn)` with known
            // positional element types infers the element type at that 1-based
            // index. A non-literal index, an out-of-range literal, or a bare
            // `Tuple` (no type_args) falls through to normal dispatch (Dynamic),
            // so this never produces a false positive.
            if let Some(elem_ty) =
                Self::infer_literal_index_tuple_at(class_name, &selector_name, type_args, arguments)
            {
                self.check_instance_selector(class_name, &selector_name, span, hierarchy);
                return elem_ty;
            }

            // Validation routes ALL singleton receivers (including binary sends)
            // through `Symbol` — that is fine: `Symbol` understands `=:=`/`=`, so
            // no spurious DNU, and the BT-2631 impossible-comparison hint fires
            // via the Dynamic fall-through below, not via validation. Only the
            // *inference* redirect (`resolve_class`) excludes binary sends.
            self.check_instance_selector(class_name, &selector_name, span, hierarchy);
            // BT-2647: a non-union singleton receiver (`#text`) is a subtype of
            // `Symbol` but not itself in the hierarchy, so method lookup and
            // argument/return-type inference would otherwise fall through to
            // Dynamic — losing the inference it had when typed `Symbol`. Resolve
            // its protocol through `Symbol`, mirroring the singleton-union member
            // handling from BT-2624. `class_name` (`#text`) is still used for
            // `Self` returns and user-facing messages.
            //
            // `!contains('|')` keeps this symmetric with `check_instance_selector`
            // (a `Known` is never a union today, but a future `Known { "#a | #b" }`
            // must not silently route here while validation leaves it untouched).
            // Binary sends are excluded: an equality op on a singleton receiver
            // (`#west =:= unionVar`) must fall through to the BT-2631
            // statically-decidable-comparison hint below rather than
            // short-circuiting on `Symbol`'s `=:=`.
            let resolve_class: EcoString = if class_name.starts_with('#')
                && !class_name.contains('|')
                && !matches!(selector, MessageSelector::Binary(_))
            {
                EcoString::from("Symbol")
            } else {
                class_name.clone()
            };
            // Skip argument type check for binary messages only when
            // `check_binary_operand_types` already ran above (the Known/Known
            // case) — it provides more specific warnings for
            // arithmetic/comparison/concat. BT-2843: when the argument's
            // inferred type didn't match that Known/Known pattern (e.g. a
            // `Union` like `String | Nil`), fall back to the generic
            // `check_argument_types` so the argument still gets *some*
            // coverage instead of none.
            if !matches!(selector, MessageSelector::Binary(_)) || !binary_operand_check_ran {
                self.check_argument_types(
                    &resolve_class,
                    &selector_name,
                    &arg_types,
                    span,
                    hierarchy,
                    false,
                    Some(arguments),
                    Some(env),
                    type_args,
                );
            }

            // Infer return type from method info
            if let Some(method) = hierarchy.find_method(&resolve_class, &selector_name) {
                if let Some(ref ret_ty) = method.return_type {
                    // BT-2751 (ADR 0104 Phase 3): `withTimeout:` return-type
                    // transparency. The generated-builtins table records the
                    // static return type `TimeoutProxy`, but the proxy is
                    // transparent — it forwards every message to the wrapped
                    // actor and a timed-out call *raises* rather than returning,
                    // so method return types are unchanged. Typing the result as
                    // the opaque `TimeoutProxy` would make every forwarded
                    // selector (`slowDb query: sql`) `Dynamic`. Instead, a
                    // `withTimeout:` send on a receiver of static type `C` is
                    // typed as `C` (with its type args preserved), so forwarded
                    // calls resolve the wrapped class's real return types. The
                    // table can only hold a static string, so the transparency
                    // rule is applied here. Restricted to `Actor` and its
                    // subclasses (`is_actor_subclass` returns true for the base
                    // `Actor` itself too) so the rule's correctness is an explicit
                    // constraint, not an implicit consequence of `TimeoutProxy`
                    // being inaccessible from user code (a user
                    // `withTimeout: -> TimeoutProxy` on a non-Actor class would
                    // otherwise be silently retyped).
                    if selector.well_known() == Some(WellKnownSelector::WithTimeout)
                        && matches!(ret_ty, DeclaredType::Simple(n) if n == "TimeoutProxy")
                        && hierarchy.is_actor_subclass(&resolve_class)
                    {
                        return receiver_ty.clone();
                    }

                    // `Self` resolves to the static receiver class (with type args)
                    if matches!(ret_ty, DeclaredType::SelfType) {
                        if type_args.is_empty() {
                            return InferredType::known(class_name.clone());
                        }
                        return InferredType::Known {
                            class_name: class_name.clone(),
                            type_args: type_args.clone(),
                            provenance:
                                crate::semantic_analysis::type_checker::TypeProvenance::Substituted(
                                    crate::source_analysis::Span::default(),
                                ),
                        };
                    }

                    // ADR 0083: `Self class` — the method returns the receiver's
                    // class object. Resolve to the metatype of the static
                    // receiver class so that downstream class-side sends
                    // (`obj class new`, `self species withAll:`) route through
                    // `find_class_method`. Pre-0083 this returned `Dynamic`
                    // (BT-1952).
                    if matches!(ret_ty, DeclaredType::SelfClass) {
                        // BT-2647: for a singleton receiver, `resolve_class` is
                        // `Symbol` so `#text class` is `Symbol class` (`#text` is a
                        // Symbol at runtime), not a phantom `Meta("#text")`.
                        return InferredType::meta(resolve_class.clone());
                    }
                    // ADR 0083: `X class` — the method returns the metatype of a
                    // specific named class (BT-2034 annotation `X class`).
                    if let DeclaredType::ClassOf(meta_class) = ret_ty {
                        if hierarchy.has_class(meta_class) {
                            return InferredType::meta(meta_class.clone());
                        }
                    }

                    // BT-1945: `Never` resolves to the bottom type (divergent methods)
                    if matches!(ret_ty, DeclaredType::Simple(n) if WellKnownClass::from_str(n) == Some(WellKnownClass::Never))
                    {
                        return InferredType::Never;
                    }

                    // Build substitution map, composing through inheritance chain
                    // if the method is inherited (ADR 0068 Phase 1b, BT-1577)
                    let subst = Self::build_inherited_substitution_map(
                        hierarchy,
                        &resolve_class,
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

                    // Apply generic substitution if we have type args, method-local
                    // params, or the return type mentions `Self` nested inside a
                    // generic/union (BT-1986). For the last case, even if no
                    // param substitutions apply, we still need to rewrite `Self`
                    // to the receiver class so callers see the narrowed type.
                    let has_nested_self = Self::declared_type_contains_self(ret_ty);
                    if !subst.is_empty() || !method_subst.is_empty() || has_nested_self {
                        // BT-1992: Thread the full receiver type (with type args)
                        // under the reserved `Self` subst key so nested `Self` in
                        // generics like `Result(Self, Error)` resolves to e.g.
                        // `Box(Integer)` not bare `Box` (BT-3076: the shared
                        // `resolve_declared_type` resolver's `SelfType` arm reads
                        // this same key — see its doc).
                        return type_resolver::resolve_declared_type(
                            ret_ty,
                            &type_resolver::merge_substitutions(
                                &subst,
                                &method_subst,
                                Some(&receiver_ty),
                            ),
                            self.protocol_registry.as_ref(),
                            self.alias_registry.as_ref(),
                            TypeStringContext::Substitution,
                        );
                    }

                    // BT-1834: If the return type is an unresolved type param
                    // (single uppercase letter like E, T, V), fall back to Dynamic
                    // so downstream sends don't get false DNU warnings.
                    if let DeclaredType::Simple(n) = ret_ty {
                        if crate::semantic_analysis::type_checker::is_generic_type_param(n)
                            && !hierarchy.has_class(n)
                        {
                            return InferredType::Dynamic(DynamicReason::Unknown);
                        }
                    }

                    // BT-2019: Resolve the return type through the centralised
                    // structured resolver, which preserves the full
                    // parameterised type — `List(String)` becomes
                    // `Known("List", [Known("String")])` rather than the
                    // pre-fix bare `Known("List", [])` that silently dropped
                    // the element type.
                    //
                    // Also handles:
                    //  * BT-2017: union return types like `Integer | Nil`
                    //    resolve into `InferredType::Union`, enabling narrowing.
                    //  * Plain class names — pass through to `Known(name, [])`.
                    //  * Nested generics — `Result(List(String), Error)`
                    //    keeps both layers.
                    //  * BT-2928: a cross-file alias name expands to its
                    //    declared union/structural type instead of staying
                    //    an opaque nominal class.
                    return type_resolver::resolve_declared_type(
                        ret_ty,
                        &type_resolver::SubstitutionMap::new(),
                        self.protocol_registry.as_ref(),
                        self.alias_registry.as_ref(),
                        TypeStringContext::Declared,
                    );
                }

                // BT-2868: `ret_ty` is None — the method exists but declares
                // no return type. For `Boolean>>ifTrue:`/`ifFalse:` this is
                // by design (see `boolean.bt`): a solo `ifTrue:`/`ifFalse:`
                // on an unnarrowed `Boolean` receiver can't soundly promise
                // `R` (the sibling branch — e.g. `False>>ifTrue:` — returns
                // `self` without invoking the block), but collapsing all the
                // way to `Dynamic` is more conservative than necessary: the
                // sound type is the union of that `Boolean` self-branch and
                // the block's own return type. Mirrors BT-2824's
                // `if_nil_solo_union_ret_ty` fix for `ifNil:`/`ifNotNil:`.
                if let Some(ty) = Self::if_true_false_solo_boolean_ret_ty(
                    &selector_name,
                    &resolve_class,
                    arguments,
                    &arg_types,
                    hierarchy,
                ) {
                    return ty;
                }

                // Only reclassify the terminal Dynamic fallback's reason for
                // the `ifTrue:`/`ifFalse:` family this issue targets — other
                // deliberately-unannotated methods (e.g. `Block>>on:do:`,
                // whose return type is a separate, unresolved soundness gap
                // predating this fix) keep the pre-existing `DynamicReceiver`
                // reason so they don't newly start firing the BT-1914
                // warning as a side effect of this narrower fix.
                if matches!(
                    selector.well_known(),
                    Some(WellKnownSelector::IfTrue | WellKnownSelector::IfFalse)
                ) {
                    known_method_unannotated_return = true;
                }
            }

            // BT-1834: Block value/value:/value:value: — return the last type arg.
            // Block is variadic: Block(R), Block(A, R), Block(A, B, R), etc.
            // The convention is that the last type arg is always the return type.
            if WellKnownClass::from_str(class_name) == Some(WellKnownClass::Block)
                && !type_args.is_empty()
                && matches!(
                    selector.well_known(),
                    Some(
                        WellKnownSelector::Value
                            | WellKnownSelector::ValueColon
                            | WellKnownSelector::ValueValue
                            | WellKnownSelector::ValueValueValue
                    )
                )
            {
                return type_args.last().unwrap().clone();
            }

            // BT-1047: Fall back to return types inferred earlier in this same pass.
            // Method bodies are processed before top-level expressions, so inferred
            // return types are available for chain resolution without a second pass.
            // BT-2022: Return the full InferredType from the cache, preserving
            // type_args so callers see e.g. List(String) instead of bare List.
            let key = (class_name.clone(), selector_name.clone(), false);
            if let Some(ret_ty) = self.method_return_types.get(&key) {
                return ret_ty.clone();
            }
        }

        // BT-2631: a standalone singleton (in)equality send (`flag := unionVar
        // =:= #west`) is statically decidable when `#west` can never be a member
        // of the union — but `infer_union_message_send` short-circuits equality
        // ops to `Boolean` before any membership check. Emit the same hint as the
        // guard-scoped path here (and only here): an `ifTrue:`-guarded comparison
        // is itself a `MessageSend` whose receiver is inferred through this path,
        // so this is the single emitter for both guarded and bare comparisons —
        // `refine_singleton_narrowing` deliberately no longer emits, avoiding a
        // double hint. The union operand may be either side (`unionVar = #west`
        // or `#west = unionVar`), so consult both operand types.
        if let MessageSelector::Binary(op) = selector {
            if let Some(eq) = narrowing::detect_singleton_eq(receiver, op, arguments) {
                let arg_ty = arg_types.first();
                let union_ty = [Some(&receiver_ty), arg_ty]
                    .into_iter()
                    .flatten()
                    .find(|t| matches!(t, InferredType::Union { .. }));
                if let Some(union_ty) = union_ty {
                    self.check_impossible_singleton_comparison(
                        union_ty,
                        &eq.info.singleton,
                        eq.info.negated,
                        span,
                        hierarchy,
                    );
                }
            }
        }

        // Union-typed receiver: check selector on ALL members, warn if any lacks it.
        // Return type is the union of member return types.
        if let InferredType::Union { ref members, .. } = receiver_ty {
            return self.infer_union_message_send(
                members,
                &selector_name,
                arguments,
                &arg_types,
                span,
                hierarchy,
            );
        }

        // BT-2868: honest reason for the terminal Dynamic fallback — a solo
        // `ifTrue:`/`ifFalse:` on a `Known` receiver whose method exists but
        // declares no return type by design reports `UnannotatedReturn`, not
        // `DynamicReceiver` (the receiver here is perfectly well-typed).
        // Every other unresolved shape (genuinely Dynamic receiver,
        // unresolvable selector, other deliberately-unannotated methods like
        // `Block>>on:do:`, …) keeps the original `DynamicReceiver` reason.
        InferredType::Dynamic(if known_method_unannotated_return {
            DynamicReason::UnannotatedReturn
        } else {
            DynamicReason::DynamicReceiver
        })
    }

    /// ADR 0083: resolve a selector against the metaclass *tower* (the instance
    /// protocol a class object inherits: `Metaclass → Class → Behaviour →
    /// Object → ProtoObject`) and return its declared return type as an
    /// `InferredType`.
    ///
    /// Used to type sends on a metatype-typed receiver (`Meta{C}`) for
    /// selectors that aren't class methods of `C` but ARE understood by every
    /// class object — e.g. `name -> Symbol` (Behaviour), `superclass`,
    /// `printString -> String`. Returns `None` when the selector is not on the
    /// tower or carries no return annotation (the caller keeps the Dynamic
    /// fallback). Resolution starts at `Metaclass` so the whole chain is walked.
    ///
    /// `receiver_meta` is the concrete metatype of the receiver class object
    /// (`Meta{C}`). When the tower method returns `Self` / `Self class` — e.g.
    /// the identity method `Object>>yourself -> Self` — the receiver *is* the
    /// class object, so the result stays `Meta{C}` (this keeps
    /// `aClass yourself new` resolving class-side rather than collapsing to
    /// Dynamic, BT-2255).
    pub(in crate::semantic_analysis::type_checker) fn class_object_tower_return(
        selector: &str,
        hierarchy: &ClassHierarchy,
        receiver_meta: &EcoString,
    ) -> Option<InferredType> {
        let method = hierarchy.find_method("Metaclass", selector)?;
        let ret_ty = method.return_type.as_ref()?;
        // `Self` / `Self class` on the tower refer to the class object itself.
        // For a metatype-typed receiver we know that object is `Meta{C}`, so
        // preserve the concrete metatype rather than dropping to Dynamic — this
        // keeps tower identity methods (`yourself`) chainable class-side.
        if matches!(ret_ty, DeclaredType::SelfType | DeclaredType::SelfClass) {
            return Some(InferredType::meta(receiver_meta.clone()));
        }
        if matches!(ret_ty, DeclaredType::Simple(n) if WellKnownClass::from_str(n) == Some(WellKnownClass::Never))
        {
            return Some(InferredType::Never);
        }
        if let DeclaredType::Simple(n) = ret_ty {
            if crate::semantic_analysis::type_checker::is_generic_type_param(n)
                && !hierarchy.has_class(n)
            {
                return None;
            }
        }
        // No alias registry threaded here: `ret_ty` comes from the built-in
        // Metaclass/Class/Behaviour/Object/ProtoObject tower, which never
        // declares an alias-typed return. BT-2936 (the general follow-up
        // that threaded `alias_registry` through the other deferred call
        // sites) revisited this one specifically and confirmed it stays
        // out of scope: the tower's method table is fixed, built-in, and
        // has no alias-typed entries to expand, so there is nothing for a
        // registry to do here.
        Some(type_resolver::resolve_declared_type(
            ret_ty,
            &type_resolver::SubstitutionMap::new(),
            None,
            None,
            TypeStringContext::Declared,
        ))
    }

    /// Resolve a message send on a union-typed receiver (BT-1857).
    ///
    /// For each member type in the union:
    /// - **Nil (`UndefinedObject`)**: skipped for method resolution. The common
    ///   pattern `x :: T | Nil` means the user is expected to nil-check before
    ///   sending, matching `isNil` narrowing semantics.
    /// - **Dynamic**: handled conservatively (no warning, returns Dynamic).
    /// - **Known types with DNU override or unknown to hierarchy**: Dynamic.
    /// - **Known types**: resolved normally; return type collected.
    ///
    /// Warnings:
    /// - ALL non-nil members respond → no warning, return union of return types.
    /// - SOME non-nil members respond → DNU hint naming the non-responding members.
    /// - NO non-nil members respond → existing DNU warning.
    ///
    /// Note: the equality / identity comparison operators (`==`, `=:=`,
    /// `/=`, `=/=`) short-circuit to `Boolean` without per-member resolution
    /// (see the inline note). A member class that overrides `=:=` to return
    /// something other than `Boolean` would therefore still infer `Boolean`
    /// here — an intentional tradeoff matching the non-union `Meta` path, since
    /// these operators are part of the universal `Object`/`ProtoObject`
    /// protocol and are not modelled as per-class hierarchy methods.
    #[allow(clippy::too_many_lines)]
    pub(in crate::semantic_analysis::type_checker) fn infer_union_message_send(
        &mut self,
        members: &[InferredType],
        selector: &str,
        arguments: &[Expression],
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        let mut missing_names: Vec<EcoString> = Vec::new();
        let mut return_types: Vec<InferredType> = Vec::new();
        let mut responding_count: usize = 0;
        let mut uncertain_member_count: usize = 0;
        let has_nil = members.iter().any(|m| {
            m.as_known().is_some_and(|n| {
                WellKnownClass::from_str(n).is_some_and(WellKnownClass::is_nil_class)
            })
        });
        let has_dynamic = members
            .iter()
            .any(|m| matches!(m, InferredType::Dynamic(_)));
        if has_dynamic {
            return InferredType::Dynamic(DynamicReason::DynamicReceiver);
        }

        // BT-2624: The equality / identity comparison operators (`==`,
        // `=:=`, `/=`, `=/=`) are universal value/identity comparisons every
        // object supports at runtime via the `Object`/`ProtoObject` protocol,
        // but they are not modelled as per-class hierarchy methods. The
        // non-union `Meta` receiver path already special-cases them for
        // exactly this reason — mirror it here so a union receiver does not
        // spuriously report that a concrete member (e.g. `Integer does not
        // understand '=:='`) or a singleton member fails to understand the
        // operator. This also keeps the idiomatic `unionVar =:= #singleton`
        // narrowing guard (BT-2617) warning-free. These selectors always
        // return `Boolean`.
        if is_equality_operator(selector) {
            return InferredType::known("Boolean");
        }

        for member in members {
            // Dynamic members: no warning, contribute Dynamic to return type.
            let Some(member_name) = member.as_known() else {
                // Dynamic member — handled above, but nested unions could
                // still reach here; treat conservatively.
                return_types.push(InferredType::Dynamic(DynamicReason::DynamicReceiver));
                continue;
            };
            // BT-1857: Skip Nil (UndefinedObject) for method resolution.
            // Nil is expected to be guarded by `isNil` checks; emitting a DNU
            // warning for every `T | Nil` union is noisy and unhelpful.
            if WellKnownClass::from_str(member_name).is_some_and(WellKnownClass::is_nil_class) {
                continue;
            }
            // BT-2624: A singleton member (`#foo`) is a subtype of `Symbol` (see
            // the singleton-as-Symbol convention in `type_resolver`). Resolve its
            // method set through `Symbol` so inherited methods (`asString`,
            // `printString`, `=:=`, …) are visible. Without this, `#foo` looks
            // like an unknown class and is treated as an *uncertain* member,
            // which both poisons the union's return type with `Dynamic` (a
            // genuine responder like `=:=` then infers `Dynamic` instead of
            // `Boolean`) and downgrades genuine non-responder warnings to hints.
            // The original `member_name` (`#foo`) is still used for the
            // user-facing missing-selector message and for `Self`-typed returns.
            let resolve_name: &str = if member_name.starts_with('#') {
                "Symbol"
            } else {
                member_name.as_str()
            };
            // ADR 0100 Rule 1 (BT-3469): route this member's completeness
            // classification through the shared classifier instead of
            // re-deriving it — the two checks this replaced (unknown class,
            // instance-side DNU override) are a strict subset of what
            // `classify_receiver` folds in; a cross-file parent, a
            // parse-error-tainted surface, or the pre-WS3 dependency guard
            // now also downgrade a member to "uncertain" here, exactly as
            // they already do for a bare (non-union) receiver.
            if !receiver_knowledge::classify_receiver(resolve_name, hierarchy, false)
                .is_closed_complete()
            {
                uncertain_member_count += 1;
                return_types.push(InferredType::Dynamic(DynamicReason::DynamicReceiver));
                continue;
            }
            if hierarchy.resolves_selector(resolve_name, selector) {
                responding_count += 1;
                if let Some(method) = hierarchy.find_method(resolve_name, selector) {
                    if let Some(ref ret_ty) = method.return_type {
                        if matches!(ret_ty, DeclaredType::SelfType) {
                            // Self resolves to the concrete member type (with type args)
                            return_types.push(member.clone());
                        } else if matches!(ret_ty, DeclaredType::SelfClass) {
                            // ADR 0083: `Self class` resolves to the metatype of
                            // the concrete union member (was Dynamic pre-0083,
                            // BT-1952). Use `resolve_name` so a singleton member
                            // yields `Symbol class` (`#foo class` is `Symbol` at
                            // runtime), not a phantom `Meta("#foo")` (BT-2624).
                            return_types.push(InferredType::meta(EcoString::from(resolve_name)));
                        } else if let DeclaredType::ClassOf(meta_class) = ret_ty
                            && hierarchy.has_class(meta_class)
                        {
                            // ADR 0083: an explicit `X class` return on a union
                            // member resolves to `Meta{X}` — mirrors the
                            // non-union path (BT-2034). Without this branch the
                            // ` class` suffix leaked through as
                            // `Known("X class")`. An unregistered `X` falls
                            // through to the arms below (like the non-union
                            // path), so the member still contributes a type
                            // instead of being silently dropped from the union.
                            return_types.push(InferredType::meta(meta_class.clone()));
                        } else if matches!(ret_ty, DeclaredType::Simple(n) if WellKnownClass::from_str(n) == Some(WellKnownClass::Never))
                        {
                            // BT-1945: Bottom type for divergent methods
                            return_types.push(InferredType::Never);
                        } else {
                            // BT-1857: Apply generic substitution for parameterised
                            // union members (e.g. Array(Integer) in a union).
                            let InferredType::Known { type_args, .. } = member else {
                                unreachable!()
                            };
                            let subst = Self::build_inherited_substitution_map(
                                hierarchy,
                                member_name,
                                type_args,
                                &method.defined_in,
                            );
                            // BT-1986 / BT-1992: also substitute nested `Self`
                            // (inside a generic) to the concrete member type
                            // (with type args), even when the substitution map
                            // is empty.
                            let has_nested_self = Self::declared_type_contains_self(ret_ty);
                            if !subst.is_empty() || has_nested_self {
                                // BT-1992: thread the concrete member type under
                                // the `Self` subst key (BT-3076, see
                                // `resolve_declared_type`'s `SelfType` arm).
                                return_types.push(type_resolver::resolve_declared_type(
                                    ret_ty,
                                    &type_resolver::merge_substitutions(
                                        &subst,
                                        &type_resolver::SubstitutionMap::new(),
                                        Some(member),
                                    ),
                                    self.protocol_registry.as_ref(),
                                    self.alias_registry.as_ref(),
                                    TypeStringContext::Substitution,
                                ));
                            } else if matches!(ret_ty, DeclaredType::Simple(n) if crate::semantic_analysis::type_checker::is_generic_type_param(n) && !hierarchy.has_class(n))
                            {
                                return_types.push(InferredType::Dynamic(DynamicReason::Unknown));
                            } else {
                                // BT-2019 / BT-2017: Resolve through the
                                // centralised structured resolver to preserve
                                // parametric type args (`List(String)` keeps
                                // its element type) and resolve union return
                                // types into `InferredType::Union`.
                                return_types.push(type_resolver::resolve_declared_type(
                                    ret_ty,
                                    &type_resolver::SubstitutionMap::new(),
                                    self.protocol_registry.as_ref(),
                                    self.alias_registry.as_ref(),
                                    TypeStringContext::Declared,
                                ));
                            }
                        }
                    } else if let Some(ty) = Self::if_true_false_solo_boolean_ret_ty(
                        selector,
                        resolve_name,
                        arguments,
                        arg_types,
                        hierarchy,
                    ) {
                        // BT-2868: a `Boolean` union member responding to a
                        // solo `ifTrue:`/`ifFalse:` with no declared return
                        // type (by design) contributes `Boolean | R` instead
                        // of poisoning the whole union to `Dynamic` — mirrors
                        // the non-union `Known` receiver fix above.
                        return_types.push(ty);
                    } else {
                        return_types.push(InferredType::Dynamic(DynamicReason::UnannotatedReturn));
                    }
                } else {
                    return_types.push(InferredType::Dynamic(DynamicReason::DynamicReceiver));
                }
            } else {
                missing_names.push(member_name.clone());
                // BT-1871: Do NOT push Dynamic here — a non-responding member
                // should not widen the return type.  If *no* members respond,
                // `union_of(&[])` returns Dynamic as a fallback.
            }
        }

        // BT-1857 / BT-2017: If nil was in the union and at least one
        // non-nil member responds, include nil's contribution to the return
        // type union.  If UndefinedObject responds to the selector (e.g.,
        // notNil, isNil, class), use its actual return type — this avoids
        // false `T | Nil` widening for methods that always return a definite
        // type.  If UndefinedObject does NOT respond, skip it: the nil case
        // is expected to be guarded by `isNil`/`notNil` checks, and adding
        // UndefinedObject here would create noisy false-positive type
        // warnings on every `T | Nil` union send.
        //
        // CodeRabbit on PR #2060: normalise the special return keywords
        // (`Self`, `Self class`, `Never`) the same way other union members are
        // normalised above, so an inherited `-> Self` selector resolves to
        // `UndefinedObject` instead of leaking `Known("Self")` into the union.
        if has_nil && responding_count > 0 {
            if let Some(method) =
                hierarchy.find_method(WellKnownClass::UndefinedObject.as_str(), selector)
            {
                if let Some(ref ret_ty) = method.return_type {
                    let nil_contribution = if matches!(ret_ty, DeclaredType::SelfType) {
                        InferredType::known(WellKnownClass::UndefinedObject.as_str())
                    } else if matches!(ret_ty, DeclaredType::SelfClass) {
                        // ADR 0083: `nil class` → metatype of `UndefinedObject`.
                        InferredType::meta(WellKnownClass::UndefinedObject.as_str())
                    } else if matches!(ret_ty, DeclaredType::Simple(n) if WellKnownClass::from_str(n) == Some(WellKnownClass::Never))
                    {
                        InferredType::Never
                    } else {
                        type_resolver::resolve_declared_type(
                            ret_ty,
                            &type_resolver::SubstitutionMap::new(),
                            self.protocol_registry.as_ref(),
                            self.alias_registry.as_ref(),
                            TypeStringContext::Declared,
                        )
                    };
                    return_types.push(nil_contribution);
                } else {
                    return_types.push(InferredType::Dynamic(DynamicReason::UnannotatedReturn));
                }
            }
            // If UndefinedObject doesn't respond: no return-type widening.
        }

        // BT-1857: Suppress DNU warnings when Dynamic is in the union —
        // Dynamic accepts any message, so we can't know the full method set.
        if !missing_names.is_empty() && !has_dynamic {
            // ADR 0100 Rule 1 (BT-3469): severity falls straight out of the
            // classify_receiver-derived counts above — "every non-nil member
            // is ClosedComplete and none responds" is the ADR's "provably
            // failing union" row (`Warning`); anything else (some member
            // responds, or a member was classified `Open`/`Dynamic` and
            // downgraded to "uncertain") stays a `Hint`, matching the
            // single-receiver ceiling. This `if` is the only place a
            // severity is chosen — `responding_count` and
            // `uncertain_member_count` are just tallies of what
            // `classify_receiver` already decided per member above.
            let severity = if responding_count == 0 && uncertain_member_count == 0 {
                Severity::Warning
            } else {
                Severity::Hint
            };

            // BT-2066: render `UndefinedObject` as `Nil` for the union
            // display, shared by both branches below — every union DNU,
            // single- or multi-culprit, names the full union it was sent
            // to, not just the non-responding member(s).
            let member_names: Vec<String> = members
                .iter()
                .filter_map(|m| m.display_for_diagnostic().map(|n| n.to_string()))
                .collect();
            let union_display = member_names.join(" | ");

            if let [only_missing] = missing_names.as_slice() {
                // BT-3469: the common one-culprit shape reuses the same
                // diagnostic builder a bare receiver's DNU uses
                // (`emit_unknown_selector_warning`) — identical message
                // shape (plus the `(in union ...)` suffix every union DNU
                // carries, via `context_suffix`), and, new for unions, the
                // same "did you mean" suggestion lookup. A singleton member
                // (`#foo`) resolves its suggestions through `Symbol` — the
                // same singleton-as-Symbol convention `resolve_name` above
                // (and `resolve_class` elsewhere in this file) applies.
                let suggestion_class = if only_missing.starts_with('#') {
                    EcoString::from("Symbol")
                } else {
                    only_missing.clone()
                };
                let display_name = InferredType::class_name_for_diagnostic(only_missing.as_str());
                // Every DNU shape carries actionable advice, so pass the
                // same generic hint the multi-culprit branch below always
                // attaches — it's used only when no "did you mean"
                // suggestion is found.
                self.emit_unknown_selector_warning(
                    &display_name,
                    &suggestion_class,
                    selector,
                    span,
                    hierarchy,
                    false,
                    severity,
                    Some(&format!(" (in union {union_display})")),
                    Some(
                        "Use `respondsTo:` to check before sending, or `@expect type` to suppress",
                    ),
                );
            } else {
                // Multiple non-responding members: `emit_unknown_selector_warning`
                // has no multi-subject mode — a per-member "did you mean"
                // can't compose into that diagnostic's single hint field —
                // so this stays its own combined-message construction.
                // BT-2066: map the missing list through the same diagnostic
                // rewriter as the union display above.
                let missing_display: Vec<EcoString> = missing_names
                    .iter()
                    .map(|n| InferredType::class_name_for_diagnostic(n.as_str()))
                    .collect();
                let message = format!(
                    "{} do not understand '{selector}' (in union {union_display})",
                    missing_display.join(", ")
                );
                let diag = match severity {
                    Severity::Warning => Diagnostic::warning(message, span),
                    Severity::Hint | Severity::Error | Severity::Lint => {
                        Diagnostic::hint(message, span)
                    }
                }
                .with_hint(
                    "Use `respondsTo:` to check before sending, or `@expect type` to suppress",
                )
                .with_category(crate::source_analysis::DiagnosticCategory::Dnu);
                self.diagnostics.push(diag);
            }
        }

        // BT-1857: If the union had Nil but all non-Nil members responded,
        // the return type is just the union of the non-Nil return types
        // (Nil was skipped, so it's not in return_types).
        InferredType::union_of(&return_types)
    }
}
