// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type inference — walking the AST to determine expression types.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module (split by expression/concern into its `literals`,
//! `assignment`, `cascade`, `send`, `blocks`, `generics`, and `flow`
//! submodules) contains the core inference entry points of [`TypeChecker`]:
//! - Module-level type checking orchestration ([`TypeChecker::check_module`])
//! - The `infer_expr` dispatch skeleton — one match arm per [`Expression`]
//!   variant, delegating the larger ones to a submodule
//! - Shared context: [`TypeStringContext`], [`TypeChecker::set_param_types`]
//!
//! Dependency direction is one-way: this module (and everything under it)
//! calls into `validation.rs`/`protocol.rs` to render diagnostics
//! (`check_argument_types`, `check_instance_selector`, …), never the other
//! way around. State-field default values are the one place validation
//! needs an inferred type it didn't compute itself; rather than calling
//! back into `infer_expr` (which would make the dependency cyclic),
//! `check_module` infers each default value once here and caches the
//! result in `TypeChecker::state_default_types` (BT-3481) for
//! `validation.rs`/`protocol.rs` to read.

use crate::ast::{Expression, ExpressionStatement, Module, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::type_resolver;
use crate::semantic_analysis::type_checker::types::DynamicInTypedClass;
use crate::semantic_analysis::type_checker::well_known::WellKnownClass;
use crate::semantic_analysis::type_checker::{
    DynamicReason, EnvKey, InferredType, TypeChecker, TypeEnv,
};
use ecow::EcoString;

mod assignment;
mod blocks;
mod cascade;
mod flow;
mod generics;
mod literals;
mod send;

/// How [`type_resolver::resolve_declared_type`] treats constructs that
/// only make sense in a particular resolution setting.
///
/// `Substitution` collapses an unresolved bare type param (`T`) to `Dynamic`
/// and stamps `Substituted` provenance on parsed generics.
/// `Declared` keeps a bare single-letter name as a nominal class — its
/// callers guard with `is_generic_type_param` themselves — and stamps
/// `Declared` provenance. `Extracted` is `Declared`'s sibling for types that
/// came from an Erlang `-spec` rather than Beamtalk source text — same
/// keyword/alias/generic handling, but stamps `Extracted` provenance instead
/// (folds `native_types::map_type_name`'s parsing into this same
/// resolver rather than reimplementing it).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum TypeStringContext {
    /// Resolving a declared signature/field type string as-is.
    Declared,
    /// Resolving a return type while substituting generic params at a send.
    Substitution,
    /// Resolving a type name string auto-extracted from `.beam` abstract
    /// code (ADR 0075) via the Erlang-FFI spec-reader protocol.
    Extracted,
}

impl TypeChecker {
    /// Checks types in a module using the class hierarchy for method resolution.
    ///
    /// Method bodies are processed first so that inferred return types are
    /// available when type-checking top-level expressions. This
    /// enables single-pass chain resolution: the `TypeChecker` consults its own
    /// `method_return_types` map when the hierarchy has no explicit annotation.
    pub fn check_module(&mut self, module: &Module, hierarchy: &ClassHierarchy) {
        // Test-only instrumentation — see `CHECK_MODULE_CALL_COUNT`'s doc.
        #[cfg(any(test, feature = "test"))]
        super::CHECK_MODULE_CALL_COUNT.with(|c| c.set(c.get() + 1));

        let mut env = TypeEnv::new();

        // Check method bodies inside class definitions first, so that inferred
        // return types are available for chain resolution in top-level code.
        for class in &module.classes {
            let is_abstract = class.is_abstract || hierarchy.is_abstract(&class.name.name);

            // Determine if this class requires type annotations (typed modifier or inherited)
            let is_typed = hierarchy.is_typed(&class.name.name);

            if is_typed {
                self.check_typed_state_annotations(&class.state, &class.name.name);
                self.check_typed_state_annotations(&class.class_variables, &class.name.name);
                // Enable Dynamic inference warnings for typed classes
                self.typed_class_context = Some(class.name.name.clone());
            }

            for method in &class.methods {
                self.check_method_body(
                    method,
                    &class.name.name,
                    false, // instance method
                    is_abstract,
                    is_typed,
                    true, // only instance methods check override compatibility
                    true,
                    hierarchy,
                );
            }
            for method in &class.class_methods {
                self.check_method_body(
                    method,
                    &class.name.name,
                    true, // class method
                    is_abstract,
                    is_typed,
                    false,
                    true,
                    hierarchy,
                );
            }

            // Infer state field default-value types once, before validating
            // them — see `state_default_types`'s doc on `TypeChecker` (BT-3481).
            self.infer_state_default_types(class, hierarchy);

            // Check state default values match declared types
            self.check_state_defaults(class, hierarchy);

            // Uninitialized state is not warned about — a type annotation
            // replaces the need for a default value.

            // Clear typed class context after processing all methods
            self.typed_class_context = None;
        }

        // Check standalone method definitions (Tonel-style: `Counter >> increment => ...`)
        for standalone in &module.method_definitions {
            let class_name = &standalone.class_name.name;
            let is_abstract = hierarchy.is_abstract(class_name);
            let is_typed = hierarchy.is_typed(class_name);

            if is_typed {
                self.typed_class_context = Some(class_name.clone());
            }

            // Standalone (Tonel-style) method definitions never run the
            // typed-annotation check here — `is_typed` above only feeds
            // `typed_class_context` (the Dynamic-in-typed-class warning) for
            // this call site, not `check_typed_method_annotations`.
            // `check_override_param_compatibility` is likewise never run for
            // standalone definitions.
            self.check_method_body(
                &standalone.method,
                class_name,
                standalone.is_class_method,
                is_abstract,
                is_typed,
                false,
                false,
                hierarchy,
            );

            self.typed_class_context = None;
        }

        // Check top-level expressions last — method return types are now available.
        self.infer_stmts(&module.expressions, hierarchy, &mut env, false);
    }

    /// Infers each state field's default-value expression type and caches it
    /// in `self.state_default_types`, keyed by `(class name, field name)`
    /// (BT-3481).
    ///
    /// Only infers fields that declare *both* a type annotation and a
    /// default value — `validation.rs::check_state_defaults` and
    /// `protocol.rs::check_state_variance` (this map's only two readers)
    /// both require both to be present before they look anything up, so a
    /// field missing either would never be read back and inferring it here
    /// would be wasted work — and could surface diagnostics (e.g. an
    /// unknown selector inside the default expression) that no code path
    /// previously observed for that field.
    ///
    /// Called once per class from [`Self::check_module`], immediately before
    /// `check_state_defaults` (Phase 1) — strictly before
    /// `check_module_with_protocols_and_aliases`'s Phase 2f
    /// `check_generic_variance_in_module` pass, which reads the same
    /// entries via `check_state_variance` instead of re-inferring them.
    fn infer_state_default_types(
        &mut self,
        class: &crate::ast::ClassDefinition,
        hierarchy: &ClassHierarchy,
    ) {
        for decl in &class.state {
            if decl.type_annotation.is_none() {
                continue;
            }
            let Some(ref default_value) = decl.default_value else {
                continue;
            };
            let mut env = TypeEnv::new();
            env.set_local("self", InferredType::known(class.name.name.clone()));
            let inferred = self.infer_expr(default_value, hierarchy, &mut env, false);
            self.state_default_types
                .insert((class.name.name.clone(), decl.name.name.clone()), inferred);
        }
    }

    /// Type-checks one method body and records its inferred return type —
    /// the shared implementation behind `check_module`'s three call sites
    /// (instance methods, class methods, and standalone Tonel-style
    /// definitions), previously near-identical loop bodies.
    ///
    /// `is_class_method` sets the method's `TypeEnv::in_class_method` flag
    /// and selects which half of the `(class, selector, is_class_method)`
    /// key the inferred return type is cached under.
    /// `check_override` gates the override-compatibility check (instance
    /// methods only — a class method or standalone definition has no
    /// override relationship to validate here). `check_typed_annotations`
    /// gates the typed-class annotation check independently of `is_typed`
    /// (standalone definitions never ran it, even when `is_typed` was true
    /// for `typed_class_context` purposes — preserved verbatim for
    /// diagnostic parity).
    #[allow(clippy::too_many_arguments)] // one flag per axis the 3 call sites differ on
    #[allow(clippy::fn_params_excessive_bools)] // each bool is an independent, named axis
    fn check_method_body(
        &mut self,
        method: &crate::ast::MethodDefinition,
        class_name: &EcoString,
        is_class_method: bool,
        is_abstract: bool,
        is_typed: bool,
        check_override: bool,
        check_typed_annotations: bool,
        hierarchy: &ClassHierarchy,
    ) {
        let mut method_env = TypeEnv::new();
        method_env.in_class_method = is_class_method;
        method_env.set_local(
            "self",
            type_resolver::receiver_type_for_class(class_name, hierarchy),
        );
        // Constructed inline rather than via `self.resolution_context()`:
        // this call site also needs `self.protocol_registry` at the same
        // time, and a disjoint field borrow (this) composes with that
        // sibling borrow in one expression where a method call (opaque to
        // the borrow checker) would not — see `resolution_context`'s doc.
        let mut ctx = type_resolver::ResolutionContext::new(
            self.alias_registry.as_ref(),
            &mut self.referenced_aliases,
        );
        Self::set_param_types(
            &mut method_env,
            &method.parameters,
            self.protocol_registry.as_ref(),
            &mut ctx,
        );
        let body_type = self.infer_stmts(&method.body, hierarchy, &mut method_env, is_abstract);
        let body_type =
            self.resolve_self_delegate_return_type(method, body_type, class_name, hierarchy);
        self.check_return_type(method, &body_type, class_name, hierarchy);
        if check_override {
            self.check_override_param_compatibility(method, class_name, hierarchy);
        }
        self.check_no_self_in_params(method, class_name);
        if is_typed && check_typed_annotations {
            self.check_typed_method_annotations(method, class_name);
        }
        // Cache the full InferredType (including type_args)
        // so callers see e.g. List(String) instead of bare List.
        if method.return_type.is_none()
            && !method
                .body
                .iter()
                .any(|s| matches!(s.expression, Expression::Primitive { .. }))
        {
            match &body_type {
                InferredType::Known { .. } | InferredType::Meta { .. } | InferredType::Never => {
                    self.method_return_types.insert(
                        (class_name.clone(), method.selector.name(), is_class_method),
                        body_type.clone(),
                    );
                }
                _ => {}
            }
        }
    }

    /// Sets parameter types in the type environment from annotations.
    ///
    /// All parameters are always registered. Typed parameters are resolved
    /// via [`type_resolver::resolve_type_annotation`]; untyped
    /// parameters are registered as `Dynamic`. Generic annotations (e.g.,
    /// `:: Result(Integer, Error)`) are resolved to `Known` with `type_args`.
    /// Type parameters of enclosing generic classes (e.g., `T` in
    /// `Result(T, E)`) resolve to `Dynamic` when no substitution context is
    /// available.
    /// Registering untyped params is necessary to prevent the bare-identifier
    /// state-field fallback in `infer_expr` from mis-inferring an untyped param
    /// as `self.<field>` when the parameter name shadows a state field name.
    ///
    /// `protocol_registry` (ADR 0102 §1/§3) is passed straight
    /// through to the resolver so a parameter typed `:: P1 & P2` resolves
    /// class ∩ protocol correctly; pass `None` when no registry is available.
    ///
    /// `alias_registry` (ADR 0108) is likewise passed straight
    /// through so a parameter typed `:: RestartStrategy` expands to its
    /// declared union; pass `None` when no registry is available.
    ///
    /// `referenced_aliases` (ADR 0108 hot-reload re-check trigger)
    /// accumulates every alias name touched while resolving each typed
    /// parameter's annotation — see
    /// [`type_resolver::resolve_type_annotation_with_alias_deps`]'s
    /// doc for why this already covers the full transitive expansion walk.
    /// An associated function (not `&mut self`) since it also takes `env:
    /// &mut TypeEnv`; callers pass `&mut self.referenced_aliases` directly
    /// (a disjoint field borrow from `self.protocol_registry`/
    /// `self.alias_registry`, so no aliasing conflict).
    pub(super) fn set_param_types(
        env: &mut TypeEnv,
        parameters: &[crate::ast::ParameterDefinition],
        protocol_registry: Option<&crate::semantic_analysis::protocol_registry::ProtocolRegistry>,
        ctx: &mut type_resolver::ResolutionContext<'_>,
    ) {
        let subst = type_resolver::SubstitutionMap::new();
        for param in parameters {
            let ty = match &param.type_annotation {
                Some(ann) => ctx.resolve_type_annotation(ann, &subst, protocol_registry),
                None => InferredType::Dynamic(DynamicReason::UnannotatedParam), // preserve parameter shadowing of state fields
            };
            env.set_local(param.name.name.clone(), ty);
        }
    }

    /// Resolves a [`TypeAnnotation`] to an [`InferredType`].
    ///
    /// Thin wrapper around
    /// [`type_resolver::resolve_type_annotation`] that supplies an
    /// empty substitution map and no protocol registry or alias registry
    /// (ADR 0108). Test-only: every production call site needs at
    /// least one of method-local / class-level type-parameter substitution,
    /// correct resolution of `&`-typed protocol intersections (ADR 0102
    /// §1/§3), or type-alias expansion, so they all call the
    /// resolver function directly with a populated
    /// [`type_resolver::SubstitutionMap`] / protocol registry / alias
    /// registry instead. Kept as a convenience for tests that only care
    /// about the "no registries" resolution path.
    ///
    /// **References:** centralised parametric type resolution.
    #[cfg(test)]
    pub(super) fn resolve_type_annotation(ann: &TypeAnnotation) -> InferredType {
        let subst = type_resolver::SubstitutionMap::new();
        type_resolver::resolve_type_annotation(ann, &subst, None, None)
    }

    /// When a method body is exactly `self delegate` (the ADR 0056 /
    /// ADR 0101 native-facade marker pattern) and the enclosing method
    /// declares an explicit return-type annotation, trust that annotation for
    /// the expression's inferred type instead of the `Dynamic` that `delegate`'s
    /// own (deliberately untyped) `sealed delegate => @intrinsic "actorDelegate"`
    /// signature would otherwise produce.
    ///
    /// `delegate` dispatches into a single generic `handle_call/3` callback
    /// shared by every selector on the native-backed class, so its own
    /// signature can never carry a per-selector return type. The author
    /// already declares the real type once, on the enclosing method
    /// (`port -> Integer => self delegate`) — this mirrors the gradual-typing
    /// "trust the annotation" model used everywhere else (ADR 0025) rather
    /// than requiring hand-written per-selector Erlang specs.
    ///
    /// Returns `body_type` unchanged when the method isn't a `self delegate`
    /// body, or when it has no return-type annotation (still whatever it
    /// inferred before — `Dynamic` for an Actor-backed class, `Never` for an
    /// Object-backed class via the `Object>>delegate -> Never` sentinel — no
    /// regression either way). Otherwise resolves the annotation and
    /// overwrites the `self delegate` expression's `type_map` entry so LSP
    /// hover and `beamtalk type-coverage` see the trusted type too.
    ///
    /// Limitation: non-`Self`/`Self class`/`X class` annotations resolve
    /// with an empty substitution map and no protocol registry — an ADR 0102
    /// intersection (`A & B`) or difference (`A \ B`) return-type annotation
    /// on a `self delegate` body won't resolve precisely. This is a narrow
    /// case with no known `self delegate` use today. Alias resolution (ADR
    /// 0108) *is* threaded through via `self.alias_registry`, so a
    /// `self delegate` method declaring `-> RestartStrategy` resolves
    /// correctly.
    pub(super) fn resolve_self_delegate_return_type(
        &mut self,
        method: &crate::ast::MethodDefinition,
        body_type: InferredType,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        if !method.is_self_delegate() {
            return body_type;
        }
        let Some(ref declared) = method.return_type else {
            return body_type;
        };
        let resolved = match declared {
            TypeAnnotation::SelfType { .. } => {
                type_resolver::receiver_type_for_class(class_name, hierarchy)
            }
            // `Self class` / `X class` return a class object — no concrete
            // instance type to trust here, so leave the Dynamic body alone
            // (mirrors `check_return_type`'s handling of the same shapes).
            TypeAnnotation::SelfClass { .. } | TypeAnnotation::ClassOf { .. } => {
                return body_type;
            }
            _ => self.resolution_context().resolve_type_annotation(
                declared,
                &type_resolver::SubstitutionMap::new(),
                None,
            ),
        };
        // The method body is exactly one statement (`self delegate`) per
        // `is_self_delegate`'s definition — record the trusted type against
        // that expression's own span, not just the method's cached return type.
        if let Some(stmt) = method.body.first() {
            self.type_map
                .insert(stmt.expression.span(), resolved.clone());
        }
        resolved
    }

    /// Infer the type of an expression, emitting diagnostics for invalid sends.
    ///
    /// `in_abstract_method` suppresses warnings for `self` class-side sends in
    /// abstract classes, since subclasses may provide class-side methods.
    ///
    /// This is the dispatch skeleton: one arm per [`Expression`] variant.
    /// Small, single-concern arms are inlined here; larger ones
    /// (`Assignment`, `Cascade`, `Match`, the collection literals) delegate
    /// to a helper method in the relevant submodule.
    #[allow(clippy::too_many_lines)] // one arm per AST variant — irreducible
    pub(super) fn infer_expr(
        &mut self,
        expr: &Expression,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let ty = match expr {
            // Literals have known types
            Expression::Literal(lit, _span) => Self::infer_literal(lit),
            // Identifiers look up the environment
            Expression::Identifier(ident) => {
                let name = ident.name.as_str();
                match name {
                    "true" | "false" => InferredType::known(WellKnownClass::Boolean.as_str()),
                    "nil" => InferredType::known(WellKnownClass::UndefinedObject.as_str()),
                    "self" => env
                        .get_local("self")
                        .unwrap_or(InferredType::Dynamic(DynamicReason::Unknown)),
                    _ => {
                        // First check environment for local variables or parameters
                        if let Some(ty) = env.get_local(name) {
                            ty
                        } else {
                            // Bare identifier might be implicit self field access
                            // (e.g., `getValue => value` is sugar for `getValue => self.value`)
                            if let Some(InferredType::Known { class_name, .. }) =
                                env.get_local("self")
                            {
                                // Check the synthetic `self.<field>`
                                // key first so the bare and explicit spellings narrow
                                // consistently.
                                if let Some(narrowed) = env.get(&EnvKey::self_field(name)) {
                                    narrowed
                                } else if let Some(field_type) =
                                    hierarchy.state_field_type(&class_name, name)
                                {
                                    type_resolver::resolve_declared_type(
                                        &field_type,
                                        &type_resolver::SubstitutionMap::new(),
                                        None,
                                        self.alias_registry.as_ref(),
                                        TypeStringContext::Declared,
                                    )
                                } else {
                                    InferredType::Dynamic(DynamicReason::Unknown)
                                }
                            } else {
                                InferredType::Dynamic(DynamicReason::Unknown)
                            }
                        }
                    }
                }
            }
            // A bare class literal `Foo` is the class *object*, whose type is the
            // metatype `Meta{Foo}` (ADR 0083) — *not* an instance of
            // `Foo`. Typing it `Meta{C}` makes a class value route class-side
            // wherever it flows (through a variable, collection, or FFI return),
            // not just when used syntactically as a direct receiver (`Foo new`).
            //
            // The syntactic class-side path (`Expression::ClassReference`
            // receiver in `infer_message_send`) still fires for direct sends, and
            // the type-driven `Meta{C}` path covers receivers reached through a
            // binding. `Meta{C} <: Class <: Behaviour` subtyping (validation.rs)
            // keeps `:: Class` / `:: Behaviour` parameter checks and `isKindOf:`
            // satisfied; `x class = Foo` narrowing is AST-driven (class_eq.rs) and
            // unaffected by this inference change.
            Expression::ClassReference { name, .. } => InferredType::meta(name.name.clone()),
            // Field access — infer type from declared state type for self.field
            // Check env first for narrowed type (e.g. inside isNil ifFalse: block)
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                let mut result = InferredType::Dynamic(DynamicReason::Unknown);
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    if recv_id.name == "self" {
                        // Check for a narrowed type in the env
                        // first. Inside `self.field isNil ifFalse: [...]`, the
                        // block env will have `SelfField("field")` → narrowed
                        // non-nil type. Assign to `result` (rather than returning
                        // early) so the shared post-processing hook still runs on
                        // narrowed reads.
                        if let Some(narrowed) = env.get(&EnvKey::self_field(field.name.clone())) {
                            result = narrowed;
                        } else if let Some(InferredType::Known { class_name, .. }) =
                            env.get_local("self")
                        {
                            if let Some(field_type) =
                                hierarchy.state_field_type(&class_name, &field.name)
                            {
                                result = type_resolver::resolve_declared_type(
                                    &field_type,
                                    &type_resolver::SubstitutionMap::new(),
                                    None,
                                    self.alias_registry.as_ref(),
                                    TypeStringContext::Declared,
                                );
                            }
                        }
                    }
                }
                result
            }
            // Primitives and errors — no type info available
            Expression::Primitive { .. }
            | Expression::Error { .. }
            | Expression::ExpectDirective { .. }
            | Expression::Spread { .. } => InferredType::Dynamic(DynamicReason::Unknown),
            // Cast / async send (`receiver selector!`) — the postfix `!` form is
            // fire-and-forget: it enqueues the message and evaluates to `nil`
            // rather than the (asynchronous) reply. Type it as `Nil`
            // (`UndefinedObject`) so a bare cast statement is `Nil`-valued
            // (ADR 0104 Phase 1).
            Expression::MessageSend { is_cast: true, .. } => {
                InferredType::known(WellKnownClass::UndefinedObject.as_str())
            }
            // Message sends — the core of type checking
            Expression::MessageSend {
                receiver,
                selector,
                arguments,
                is_cast: false,
                span,
                ..
            } => self.infer_message_send(
                receiver,
                selector,
                arguments,
                *span,
                hierarchy,
                env,
                in_abstract_method,
            ),
            // Assignments track the type of the value — see
            // `assignment::infer_assignment`.
            Expression::Assignment {
                target,
                value,
                type_annotation,
                span,
            } => self.infer_assignment(
                target,
                value,
                type_annotation.as_ref(),
                *span,
                hierarchy,
                env,
                in_abstract_method,
            ),
            // Returns propagate the value type
            Expression::Return { value, .. } => {
                self.infer_expr(value, hierarchy, env, in_abstract_method)
            }
            // Cascades: all messages dispatch to the UNDERLYING receiver of
            // the first send, not to its return value — see
            // `cascade::infer_cascade`.
            Expression::Cascade {
                receiver, messages, ..
            } => self.infer_cascade(receiver, messages, hierarchy, env, in_abstract_method),
            // Parenthesized — unwrap
            Expression::Parenthesized { expression, .. } => {
                self.infer_expr(expression, hierarchy, env, in_abstract_method)
            }
            // Blocks — infer body but return Block type
            Expression::Block(block) => {
                let mut block_env = env.child();
                for param in &block.parameters {
                    block_env.set_local(
                        param.name.clone(),
                        InferredType::Dynamic(DynamicReason::UnannotatedParam),
                    );
                }
                self.infer_stmts(&block.body, hierarchy, &mut block_env, in_abstract_method);
                InferredType::known("Block")
            }
            // Match — union of arm body types (Never arms are eliminated) —
            // see `flow::infer_match`.
            Expression::Match {
                value,
                arms,
                exhaustive,
                span,
            } => self.infer_match(
                value,
                arms,
                *exhaustive,
                *span,
                hierarchy,
                env,
                in_abstract_method,
            ),
            // Map literal → Dictionary(K, V) — see `literals::infer_map_literal`.
            Expression::MapLiteral { pairs, .. } => {
                self.infer_map_literal(pairs, hierarchy, env, in_abstract_method)
            }

            // List literal → List(E) — see `literals::infer_list_literal`.
            Expression::ListLiteral { elements, tail, .. } => self.infer_list_literal(
                elements,
                tail.as_deref(),
                hierarchy,
                env,
                in_abstract_method,
            ),

            // Array literal → Array(E) — see `literals::infer_array_literal`.
            Expression::ArrayLiteral { elements, .. } => {
                self.infer_array_literal(elements, hierarchy, env, in_abstract_method)
            }

            // String interpolation → String — see
            // `literals::infer_string_interpolation`.
            Expression::StringInterpolation { segments, .. } => {
                self.infer_string_interpolation(segments, hierarchy, env, in_abstract_method)
            }
            // Super — resolve to parent class type for method validation.
            //
            // Uses `super_receiver_type` so the parent
            // receiver threads the *child's* type-arg bindings into the
            // parent's type-param positions, mapped via the child's
            // `superclass_type_args` (`ParamRef` for `Sub(R) extends Base(R)`,
            // `Concrete` for `IntBase extends Base(Integer)`). Falls back to
            // the parent's symbolic placeholders when no extends-annotation
            // mapping is recorded.
            Expression::Super(_) => {
                if let Some(InferredType::Known {
                    class_name,
                    type_args,
                    ..
                }) = env.get_local("self")
                {
                    if let Some(class_info) = hierarchy.get_class(&class_name) {
                        if let Some(ref parent) = class_info.superclass {
                            type_resolver::super_receiver_type(
                                &class_name,
                                &type_args,
                                parent,
                                hierarchy,
                                self.alias_registry.as_ref(),
                            )
                        } else {
                            InferredType::Dynamic(DynamicReason::Unknown)
                        }
                    } else {
                        InferredType::Dynamic(DynamicReason::Unknown)
                    }
                } else {
                    InferredType::Dynamic(DynamicReason::Unknown)
                }
            }
            // Destructure assignment — infer value type, bind pattern variables into TypeEnv
            Expression::DestructureAssignment { pattern, value, .. } => {
                self.infer_expr(value, hierarchy, env, in_abstract_method);
                Self::bind_pattern_vars(pattern, env);
                InferredType::Dynamic(DynamicReason::Unknown)
            }
        };

        self.post_process_expr_type(expr, &ty);
        ty
    }

    /// Shared tail of [`Self::infer_expr`] — record the inferred type in the
    /// LSP type map and emit the "Dynamic in typed class" warning.
    ///
    /// Factored out so the cascade fast-path can apply the same
    /// post-processing to the first-send `MessageSend` node, which it resolves
    /// via `infer_message_send_with_receiver_ty` instead of routing through
    /// `infer_expr`.
    pub(in crate::semantic_analysis::type_checker) fn post_process_expr_type(
        &mut self,
        expr: &Expression,
        ty: &InferredType,
    ) {
        // Record inferred type for the expression's full span for LSP queries.
        // Dynamic types with a known reason (e.g., UnannotatedParam) are included
        // so that hover can display "Dynamic (reason)".
        // Only Dynamic(Unknown) is skipped since it carries no useful provenance.
        if !matches!(ty, InferredType::Dynamic(DynamicReason::Unknown)) {
            self.type_map.insert(expr.span(), ty.clone());
        }

        // Detecting whether this Dynamic warrants the
        // "Dynamic in typed class" warning is pure data (see
        // `detect_dynamic_in_typed_class`); rendering it as a diagnostic is
        // `validation.rs`'s job.
        if let Some(fact) =
            Self::detect_dynamic_in_typed_class(ty, self.typed_class_context.as_ref())
        {
            self.emit_dynamic_in_typed_class(&fact, expr.span());
        }
    }

    /// Detect the "Dynamic in typed class" fact for `ty` (an
    /// expression's freshly-inferred type) under `typed_class_context` (the
    /// enclosing `typed` class's name, if any) — pure data, no diagnostic
    /// construction; see `validation.rs::emit_dynamic_in_typed_class` for
    /// the rendering.
    ///
    /// Only warns for root-cause Dynamic reasons: not `DynamicReceiver`
    /// (propagated from a receiver that already produced its own warning),
    /// not `Unknown` (no actionable message), and not `ExplicitDynamic`
    /// (the author already wrote `Dynamic` in a type annotation,
    /// so "add a type annotation" would be nonsensical advice for something
    /// that already has one).
    fn detect_dynamic_in_typed_class(
        ty: &InferredType,
        typed_class_context: Option<&EcoString>,
    ) -> Option<DynamicInTypedClass> {
        let InferredType::Dynamic(reason) = ty else {
            return None;
        };
        if matches!(
            reason,
            DynamicReason::DynamicReceiver
                | DynamicReason::DynamicSpec
                | DynamicReason::Unknown
                | DynamicReason::ExplicitDynamic
        ) {
            return None;
        }
        let class_name = typed_class_context?.clone();
        let description = reason.description()?;
        Some(DynamicInTypedClass {
            class_name,
            description,
        })
    }

    /// Infer types for a sequence of expression statements.
    ///
    /// Skips `@expect` directive nodes so they don't reset the inferred body type
    /// to `Dynamic` and interfere with return-type checking.  Suppression of matching
    /// diagnostics is handled separately by `apply_expect_directives` in
    /// `diagnostic_provider` after all diagnostics have been collected.
    ///
    /// Returns the inferred type of the last non-directive expression, or `Dynamic`
    /// for an empty list.
    pub(super) fn infer_stmts(
        &mut self,
        stmts: &[ExpressionStatement],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let mut body_type = InferredType::Dynamic(DynamicReason::Unknown);

        for stmt in stmts {
            let expr = &stmt.expression;

            // @expect directives are compile-time only; skip them so they don't
            // clobber body_type and affect return-type inference.
            if matches!(expr, Expression::ExpectDirective { .. }) {
                continue;
            }

            body_type = self.infer_expr(expr, hierarchy, env, in_abstract_method);

            // Early-return narrowing (ADR 0068 Phase 1g):
            // After `x isNil ifTrue: [<diverge>]`, narrow x to non-nil for the
            // rest.  Divergence covers both `^` returns and calls to
            // `-> Never` methods like `self error: "..."`.
            self.apply_early_return_narrowing(expr, env, hierarchy);

            if matches!(expr, Expression::Return { .. }) {
                break;
            }
        }

        body_type
    }
}
