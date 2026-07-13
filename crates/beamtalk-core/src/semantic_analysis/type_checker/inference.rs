// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type inference — walking the AST to determine expression types.
//!
//! **DDD Context:** Semantic Analysis
//!
//! This module contains the core inference methods of [`TypeChecker`]:
//! - Module-level type checking orchestration
//! - Expression type inference
//! - Message send type resolution
//! - Literal type mapping

use std::collections::{HashMap, HashSet};

use crate::ast::{
    Expression, ExpressionStatement, Literal, MatchArm, MessageSelector, Module, Pattern,
    TypeAnnotation,
};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};
use ecow::{EcoString, eco_format};

use super::narrowing::extract::extract_variable_name;
use super::narrowing::extract::unwrap_parens;
use super::narrowing::refinement::RefinementLayer;
use super::narrowing::visitors::{block_has_any_return, block_has_return, block_may_reassign};
use super::narrowing::{ClassTestInfo, ClassTestKind, NarrowingInfo};
use super::well_known::WellKnownClass;
use super::{DynamicReason, EnvKey, InferredType, TypeChecker, TypeEnv, narrowing};

impl TypeChecker {
    /// Joins the inferred element types of a collection literal into a single
    /// element type (BT-2620).
    ///
    /// Delegates to [`InferredType::union_of`], which collapses a homogeneous
    /// literal to its single element type (`#[1, 2, 3]` → `Integer`), joins a
    /// heterogeneous one into a union (`#[1, "a"]` → `Integer | String`),
    /// degrades to `Dynamic` if any element is itself `Dynamic`, and — for an
    /// empty literal (no elements) — returns `Dynamic`, so `#[]` infers
    /// `Array(Dynamic)`.
    fn join_element_types(elements: &[InferredType]) -> InferredType {
        InferredType::union_of(elements)
    }

    /// Extracts the element type contributed by a list literal `tail` (cons)
    /// (BT-2620).
    ///
    /// A BEAM cons tail must be a list, so only a known `List(T)` contributes
    /// its element type `T` to the literal's element join. Any other tail
    /// carries no usable element information and widens the element to
    /// `Dynamic` — including an `Array` (a distinct, tuple-backed type that
    /// would form an *improper* list in cons position) and a bare, unannotated
    /// `List`.
    fn tail_element_type(tail_ty: &InferredType) -> InferredType {
        match tail_ty {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } if class_name == "List" && !type_args.is_empty() => type_args[0].clone(),
            _ => InferredType::Dynamic(DynamicReason::Unknown),
        }
    }

    /// Reports whether a list-literal cons tail is a *known, non-`List`* type —
    /// i.e. one that would form an improper BEAM list at runtime (BT-2623).
    ///
    /// A cons tail (`[1 | tail]`) must evaluate to a proper list. A known type
    /// whose base class is not `List` (most notably `Array`, which is
    /// tuple-backed) would instead build an improper list. We surface this as a
    /// diagnostic so the user sees a likely runtime bug rather than the silent
    /// degradation to `List(Dynamic)` that [`tail_element_type`] performs.
    ///
    /// Returns `None` (no diagnostic) for `List` tails — proper by construction —
    /// and for `Dynamic`/`Union`/`Meta`/`Never`, which are too uncertain to flag
    /// without risking noise. When a diagnostic is warranted, returns the
    /// offending type's user-facing display name.
    fn improper_cons_tail_display(tail_ty: &InferredType) -> Option<EcoString> {
        match tail_ty {
            InferredType::Known { class_name, .. } if class_name != "List" => Some(
                tail_ty
                    .display_for_diagnostic()
                    .unwrap_or_else(|| class_name.clone()),
            ),
            _ => None,
        }
    }

    /// Checks types in a module using the class hierarchy for method resolution.
    ///
    /// Method bodies are processed first so that inferred return types are
    /// available when type-checking top-level expressions (BT-1047). This
    /// enables single-pass chain resolution: the `TypeChecker` consults its own
    /// `method_return_types` map when the hierarchy has no explicit annotation.
    #[allow(clippy::too_many_lines)] // struct patterns expanded by BT-1569 refactor
    pub fn check_module(&mut self, module: &Module, hierarchy: &ClassHierarchy) {
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
                // Enable Dynamic inference warnings for typed classes (BT-1914)
                self.typed_class_context = Some(class.name.name.clone());
            }

            for method in &class.methods {
                let mut method_env = TypeEnv::new();
                method_env.set_local(
                    "self",
                    super::type_resolver::receiver_type_for_class(&class.name.name, hierarchy),
                );
                Self::set_param_types(
                    &mut method_env,
                    &method.parameters,
                    self.protocol_registry.as_ref(),
                );
                let body_type =
                    self.infer_stmts(&method.body, hierarchy, &mut method_env, is_abstract);
                self.check_return_type(method, &body_type, &class.name.name, hierarchy);
                self.check_override_param_compatibility(method, &class.name.name, hierarchy);
                self.check_no_self_in_params(method, &class.name.name);
                if is_typed {
                    self.check_typed_method_annotations(method, &class.name.name);
                }
                // BT-2022: Cache the full InferredType (including type_args)
                // so callers see e.g. List(String) instead of bare List.
                if method.return_type.is_none()
                    && !method
                        .body
                        .iter()
                        .any(|s| matches!(s.expression, Expression::Primitive { .. }))
                {
                    match &body_type {
                        InferredType::Known { .. }
                        | InferredType::Meta { .. }
                        | InferredType::Never => {
                            self.method_return_types.insert(
                                (class.name.name.clone(), method.selector.name(), false),
                                body_type.clone(),
                            );
                        }
                        _ => {}
                    }
                }
            }
            for method in &class.class_methods {
                let mut method_env = TypeEnv::new();
                method_env.in_class_method = true;
                method_env.set_local(
                    "self",
                    super::type_resolver::receiver_type_for_class(&class.name.name, hierarchy),
                );
                Self::set_param_types(
                    &mut method_env,
                    &method.parameters,
                    self.protocol_registry.as_ref(),
                );
                let body_type =
                    self.infer_stmts(&method.body, hierarchy, &mut method_env, is_abstract);
                self.check_return_type(method, &body_type, &class.name.name, hierarchy);
                self.check_no_self_in_params(method, &class.name.name);
                if is_typed {
                    self.check_typed_method_annotations(method, &class.name.name);
                }
                // BT-2022: Cache the full InferredType (including type_args)
                if method.return_type.is_none()
                    && !method
                        .body
                        .iter()
                        .any(|s| matches!(s.expression, Expression::Primitive { .. }))
                {
                    match &body_type {
                        InferredType::Known { .. }
                        | InferredType::Meta { .. }
                        | InferredType::Never => {
                            self.method_return_types.insert(
                                (class.name.name.clone(), method.selector.name(), true),
                                body_type.clone(),
                            );
                        }
                        _ => {}
                    }
                }
            }

            // Check state default values match declared types
            self.check_state_defaults(class, hierarchy);

            // BT-1947: Uninitialized state warning removed — type annotation
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

            let mut method_env = TypeEnv::new();
            method_env.in_class_method = standalone.is_class_method;
            method_env.set_local(
                "self",
                super::type_resolver::receiver_type_for_class(class_name, hierarchy),
            );
            Self::set_param_types(
                &mut method_env,
                &standalone.method.parameters,
                self.protocol_registry.as_ref(),
            );
            let body_type = self.infer_stmts(
                &standalone.method.body,
                hierarchy,
                &mut method_env,
                is_abstract,
            );
            self.check_return_type(&standalone.method, &body_type, class_name, hierarchy);
            self.check_no_self_in_params(&standalone.method, class_name);
            // BT-2022: Cache the full InferredType (including type_args)
            if standalone.method.return_type.is_none()
                && !standalone
                    .method
                    .body
                    .iter()
                    .any(|s| matches!(s.expression, Expression::Primitive { .. }))
            {
                match &body_type {
                    InferredType::Known { .. }
                    | InferredType::Meta { .. }
                    | InferredType::Never => {
                        self.method_return_types.insert(
                            (
                                class_name.clone(),
                                standalone.method.selector.name(),
                                standalone.is_class_method,
                            ),
                            body_type.clone(),
                        );
                    }
                    _ => {}
                }
            }

            self.typed_class_context = None;
        }

        // Check top-level expressions last — method return types are now available.
        self.infer_stmts(&module.expressions, hierarchy, &mut env, false);
    }

    /// Sets parameter types in the type environment from annotations.
    ///
    /// All parameters are always registered. Typed parameters are resolved
    /// via [`super::type_resolver::resolve_type_annotation`]; untyped
    /// parameters are registered as `Dynamic`. Generic annotations (e.g.,
    /// `:: Result(Integer, Error)`) are resolved to `Known` with `type_args`.
    /// Type parameters of enclosing generic classes (e.g., `T` in
    /// `Result(T, E)`) resolve to `Dynamic` when no substitution context is
    /// available.
    /// Registering untyped params is necessary to prevent the bare-identifier
    /// state-field fallback in `infer_expr` from mis-inferring an untyped param
    /// as `self.<field>` when the parameter name shadows a state field name.
    ///
    /// `protocol_registry` (ADR 0102 §1/§3, BT-2743) is passed straight
    /// through to the resolver so a parameter typed `:: P1 & P2` resolves
    /// class ∩ protocol correctly; pass `None` when no registry is available.
    pub(super) fn set_param_types(
        env: &mut TypeEnv,
        parameters: &[crate::ast::ParameterDefinition],
        protocol_registry: Option<&crate::semantic_analysis::protocol_registry::ProtocolRegistry>,
    ) {
        let subst = super::type_resolver::SubstitutionMap::new();
        for param in parameters {
            let ty = match &param.type_annotation {
                Some(ann) => {
                    super::type_resolver::resolve_type_annotation(ann, &subst, protocol_registry)
                }
                None => InferredType::Dynamic(DynamicReason::UnannotatedParam), // preserve parameter shadowing of state fields
            };
            env.set_local(param.name.name.clone(), ty);
        }
    }

    /// Resolves a [`TypeAnnotation`] to an [`InferredType`].
    ///
    /// Thin wrapper around
    /// [`super::type_resolver::resolve_type_annotation`] that supplies an
    /// empty substitution map and no protocol registry. Call sites that need
    /// method-local / class-level type-parameter substitution, or correct
    /// resolution of `&`-typed protocol intersections (ADR 0102 §1/§3,
    /// BT-2743), should call the resolver function directly with a populated
    /// [`super::type_resolver::SubstitutionMap`] / protocol registry.
    ///
    /// **References:** BT-2025 — centralised parametric type resolution.
    pub(super) fn resolve_type_annotation(ann: &TypeAnnotation) -> InferredType {
        let subst = super::type_resolver::SubstitutionMap::new();
        super::type_resolver::resolve_type_annotation(ann, &subst, None)
    }

    /// Resolves type-position keywords to their class names.
    ///
    /// - `nil` / `Nil` → `UndefinedObject`
    /// - `false` → `False`
    /// - `true` → `True`
    /// - Everything else passes through unchanged.
    ///
    /// Both lowercase (`nil`) and capitalised (`Nil`) spellings map to
    /// `UndefinedObject` so that `Integer | Nil` type annotations narrow
    /// consistently under `isNil` guards (BT-2016).
    fn resolve_type_keyword(name: &EcoString) -> EcoString {
        match name.as_str() {
            // BT-2016: Both `nil` (lowercase keyword) and `Nil` (class name) map
            // to the canonical internal name `UndefinedObject` so narrowing,
            // non_nil_type, and all downstream comparisons work consistently.
            "nil" | "Nil" => WellKnownClass::UndefinedObject.as_str().into(),
            "false" => "False".into(),
            "true" => "True".into(),
            _ => name.clone(),
        }
    }

    /// Converts a type name string (from `ClassHierarchy::state_field_type`)
    /// to a proper `InferredType`, splitting unions on `|`.
    ///
    /// Examples:
    /// - `"Integer"` → `Known("Integer")`
    /// - `"String | nil"` → `Union(["String", "UndefinedObject"])`
    /// - `"List(String)"` → `Known("List", type_args: [Known("String")])`
    pub(super) fn resolve_type_name_string(type_name: &EcoString) -> InferredType {
        if WellKnownClass::from_str(type_name) == Some(WellKnownClass::Never) {
            return InferredType::Never;
        }
        // Split on `|` respecting parenthesis nesting, so
        // `Result(String | Integer, Error)` is not split at the inner `|`.
        if type_name.contains('|') {
            let members = Self::split_union_respecting_parens(type_name);
            if members.len() > 1 {
                let resolved: Vec<InferredType> = members
                    .into_iter()
                    .map(|s| Self::resolve_type_name_string(&EcoString::from(s)))
                    .collect();
                return InferredType::union_of(&resolved);
            }
            // Single element — the `|` was inside parens, fall through
        }
        // Parametric type: e.g., "List(String)", "Dictionary(String, Integer)".
        // BT-2025: Parenthesis-aware split lives in the centralised resolver
        // helper so the `no .find('(')` grep check stays clean here.
        let (base_str, args_slice) = super::type_resolver::split_generic_base(type_name);
        if let Some(inner) = args_slice {
            let base = Self::resolve_type_keyword(&EcoString::from(base_str));
            let type_args: Vec<InferredType> = Self::split_type_params(inner)
                .into_iter()
                .map(|p| Self::resolve_type_name_string(&EcoString::from(p)))
                .collect();
            InferredType::Known {
                class_name: base,
                type_args,
                provenance: super::TypeProvenance::Declared(crate::source_analysis::Span::default()),
            }
        } else {
            InferredType::known(Self::resolve_type_keyword(type_name))
        }
    }

    /// Infer the type of an expression, emitting diagnostics for invalid sends.
    ///
    /// `in_abstract_method` suppresses warnings for `self` class-side sends in
    /// abstract classes, since subclasses may provide class-side methods.
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
                                // BT-2048 / BT-2062: Check the synthetic `self.<field>`
                                // key first so the bare and explicit spellings narrow
                                // consistently.
                                if let Some(narrowed) = env.get(&EnvKey::self_field(name)) {
                                    narrowed
                                } else if let Some(field_type) =
                                    hierarchy.state_field_type(&class_name, name)
                                {
                                    Self::resolve_type_name_string(&field_type)
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
            // metatype `Meta{Foo}` (ADR 0083 / BT-2260) — *not* an instance of
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
            // BT-2048: Check env first for narrowed type (e.g. inside isNil ifFalse: block)
            Expression::FieldAccess {
                receiver, field, ..
            } => {
                let mut result = InferredType::Dynamic(DynamicReason::Unknown);
                if let Expression::Identifier(recv_id) = receiver.as_ref() {
                    if recv_id.name == "self" {
                        // BT-2048 / BT-2062: Check for a narrowed type in the env
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
                                result = Self::resolve_type_name_string(&field_type);
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
            // (ADR 0104 Phase 1, BT-2749).
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

            // Assignments track the type of the value
            Expression::Assignment {
                target,
                value,
                type_annotation,
                span,
            } => {
                let inferred_ty = self.infer_expr(value, hierarchy, env, in_abstract_method);

                // If there's a type annotation, use the declared type instead
                // of the inferred type. Emit a warning if the RHS has a known
                // (non-Dynamic) type that is incompatible with the annotation.
                let ty = if let Some(ann) = type_annotation {
                    // ADR 0102 §1/§3 (BT-2743): thread the protocol registry
                    // through so a local `x :: P1 & P2` annotation resolves
                    // class ∩ protocol correctly rather than falling to `Never`.
                    let declared = super::type_resolver::resolve_type_annotation(
                        ann,
                        &super::type_resolver::SubstitutionMap::new(),
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
                                    *span,
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

                match target.as_ref() {
                    Expression::Identifier(ident) => {
                        // BT-1588: Track type origin for generic type params
                        if let Some(origin) = Self::describe_type_origin(value, &ty, hierarchy, env)
                        {
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
                            self.check_field_assignment(field, &ty, *span, hierarchy, env);
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
                                    *span,
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

            // Returns propagate the value type
            Expression::Return { value, .. } => {
                self.infer_expr(value, hierarchy, env, in_abstract_method)
            }

            // Cascades: all messages dispatch to the UNDERLYING receiver of
            // the first send, not to its return value. The parser bundles
            // `obj msg1; msg2; msg3` as Cascade { receiver: MessageSend(obj,
            // msg1), messages: [msg2, msg3] }, so we have to peek through the
            // first MessageSend to find the actual receiver. (BT-2017 surfaced
            // this: when `assert:equals: -> Nil` resolves to UndefinedObject,
            // the previous code dispatched cascaded messages to that return
            // type, producing spurious DNU errors.)
            Expression::Cascade {
                receiver, messages, ..
            } => {
                // BT-2035: to avoid walking the inner receiver subtree twice
                // (which would double-emit any DNU / type diagnostics it
                // produces), we infer the inner type once and thread it into
                // the first send's inference via `infer_message_send_with_receiver_ty`.
                let (send_ty, cascade_target, dispatch_ty) = if let Expression::MessageSend {
                    receiver: inner,
                    selector: inner_sel,
                    arguments: inner_args,
                    span: inner_span,
                    is_cast: false,
                    ..
                } = receiver.as_ref()
                {
                    let inner_ty = self.infer_expr(inner, hierarchy, env, in_abstract_method);
                    let send_ty = self.infer_message_send_with_receiver_ty(
                        inner,
                        inner_ty.clone(),
                        inner_sel,
                        inner_args,
                        *inner_span,
                        hierarchy,
                        env,
                        in_abstract_method,
                    );
                    // The cascade's first-send node (the outer `MessageSend`
                    // that is `receiver`) bypasses `infer_expr`, so record its
                    // type in the LSP type map and run the BT-1914 Dynamic
                    // warning for it here — mirroring `infer_expr`'s tail.
                    self.post_process_expr_type(receiver, &send_ty);
                    (send_ty, inner.as_ref(), inner_ty)
                } else {
                    // Non-MessageSend receiver (or a cast send, which short-circuits
                    // to Dynamic): fall back to a single infer_expr; the cascade
                    // dispatches messages to that same type.
                    let send_ty = self.infer_expr(receiver, hierarchy, env, in_abstract_method);
                    (send_ty.clone(), receiver.as_ref(), send_ty)
                };
                // ADR 0102 §5 (BT-2744): resolve a `Negation`-typed cascade
                // target through `base`, mirroring the same substitution in
                // `infer_message_send_with_receiver_ty` — without it,
                // cascaded messages after the first would silently skip
                // DNU/argument checking entirely (`dispatch_ty` matches
                // neither the `Known` nor `Union` arms below).
                let dispatch_ty = if let InferredType::Negation { base, .. } = dispatch_ty {
                    *base
                } else {
                    dispatch_ty
                };
                // BT-2158: normalise the cascade target so parenthesised
                // class references (`(HTTPRouter) build: [...]; ...`) are
                // treated as class-side both for block-param inference and
                // downstream selector validation.
                let unwrapped_target = unwrap_parens(cascade_target);
                let is_class_ref = matches!(unwrapped_target, Expression::ClassReference { .. });
                let is_class_side_send = Self::is_class_side_receiver(cascade_target, env);
                for msg in messages {
                    let selector_name = msg.selector.name();
                    // BT-2845: capture the inferred argument types so every
                    // cascaded message (not just the first) can be run
                    // through `check_argument_types` below — previously this
                    // return value was discarded, so a mistyped argument to a
                    // second-or-later cascade message went entirely
                    // unchecked, unlike the same send written as its own
                    // statement.
                    let arg_types = self.infer_args_with_block_context(
                        &msg.arguments,
                        &dispatch_ty,
                        &selector_name,
                        hierarchy,
                        env,
                        in_abstract_method,
                        is_class_side_send,
                    );
                    if is_class_ref {
                        if let Expression::ClassReference { name, .. } = unwrapped_target {
                            self.check_argument_types(
                                &name.name,
                                &selector_name,
                                &arg_types,
                                msg.span,
                                hierarchy,
                                true,
                                Some(&msg.arguments),
                                Some(env),
                            );
                            self.check_class_side_send(
                                &name.name,
                                &selector_name,
                                msg.span,
                                hierarchy,
                                &[], // cascade return type is receiver, not send result
                            );
                        }
                    } else if let InferredType::Known { ref class_name, .. } = dispatch_ty {
                        if env.in_class_method && Self::is_self_receiver(unwrapped_target) {
                            if !in_abstract_method {
                                self.check_argument_types(
                                    class_name,
                                    &selector_name,
                                    &arg_types,
                                    msg.span,
                                    hierarchy,
                                    true,
                                    Some(&msg.arguments),
                                    Some(env),
                                );
                                self.check_class_side_send(
                                    class_name,
                                    &selector_name,
                                    msg.span,
                                    hierarchy,
                                    &[], // cascade return type is receiver, not send result
                                );
                            }
                        } else {
                            // Skip argument type check for binary messages —
                            // `check_binary_operand_types` (run on the first
                            // cascade send via `infer_message_send_with_receiver_ty`)
                            // already provides more specific warnings for
                            // arithmetic/comparison/concat; mirrors the
                            // non-cascade skip at the bottom of
                            // `infer_message_send_with_receiver_ty`.
                            if !matches!(msg.selector, MessageSelector::Binary(_)) {
                                self.check_argument_types(
                                    class_name,
                                    &selector_name,
                                    &arg_types,
                                    msg.span,
                                    hierarchy,
                                    false,
                                    Some(&msg.arguments),
                                    Some(env),
                                );
                            }
                            self.check_instance_selector(
                                class_name,
                                &selector_name,
                                msg.span,
                                hierarchy,
                            );
                        }
                    } else if let InferredType::Union { ref members, .. } = dispatch_ty {
                        // Union cascades: validate selector on all members.
                        // Argument-type checking against a union *receiver* is
                        // out of scope here — `infer_message_send_with_receiver_ty`
                        // doesn't perform it for the first cascade message
                        // either (see `infer_union_message_send`), so this
                        // preserves parity rather than introducing new
                        // behaviour beyond BT-2845's scope (unchecked
                        // arguments on continuation messages).
                        self.infer_union_message_send(members, &selector_name, msg.span, hierarchy);
                    }
                }
                send_ty
            }

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

            // Match — union of arm body types (Never arms are eliminated)
            Expression::Match {
                value,
                arms,
                exhaustive,
                span,
            } => {
                let scrutinee_ty = self.infer_expr(value, hierarchy, env, in_abstract_method);
                // BT-2745 / ADR 0102 §4 (advisory `Warning`) vs. BT-2763 /
                // ADR 0106 (opt-in asserted `Error`, `matchExhaustive:`).
                // Distinct from (and does not replace) BT-1299's
                // pattern-based sealed-constructor check, which still runs
                // separately in `validators::match_validators`.
                if *exhaustive {
                    self.check_asserted_match_exhaustiveness(&scrutinee_ty, arms, *span);
                } else {
                    self.check_singleton_match_exhaustiveness(&scrutinee_ty, arms, *span);
                }

                // BT-2854 / ADR 0107 Phase A: a `nil` arm narrows the
                // scrutinee to `UndefinedObject` inside its own body (mirrors
                // `x isNil ifTrue:`), and — when unguarded — removes `Nil`
                // from what subsequent arms see (mirrors `x isNil ifFalse:`'s
                // `non_nil_type`), reusing the exact narrowing algebra
                // already established by `is_nil.rs`. A guarded `nil when:
                // [...] ->` arm does not guarantee coverage, so it does not
                // narrow the residual for later arms (same rule
                // `singleton_match_residual` uses for guarded arms). Only
                // applies when the scrutinee has a stable narrowable key
                // (`extract_variable_name`) — an arbitrary expression
                // scrutinee has nothing to narrow.
                let scrutinee_key = extract_variable_name(unwrap_parens(value));
                let mut residual_scrutinee_ty = scrutinee_ty.clone();

                let arm_types: Vec<InferredType> = arms
                    .iter()
                    .map(|arm| {
                        let mut arm_env = env.child();
                        Self::bind_pattern_vars(&arm.pattern, &mut arm_env);

                        // BT-2855 / ADR 0107 Phase A: a `binding :: ClassName`
                        // arm narrows `binding` to `ClassName`, computed the
                        // same way `isKindOf:`'s true branch does
                        // (`intersect_with_class`, shared with
                        // `compute_class_narrowing`) — from the scrutinee's
                        // *current* residual type, not the original
                        // `scrutinee_ty`, so a `Type` arm after an unguarded
                        // `nil ->`/`Type` arm sees the narrower residual.
                        // Overwrites the `Dynamic` `bind_pattern_vars` (above)
                        // just set for `binding`.
                        let type_pattern_narrowed =
                            if let Pattern::Type { binding, class, .. } = &arm.pattern {
                                let narrowed = Self::intersect_with_class(
                                    &residual_scrutinee_ty,
                                    &class.name,
                                    hierarchy,
                                    self.protocol_registry.as_ref(),
                                );
                                arm_env.set_local(binding.name.clone(), narrowed.clone());
                                Some(narrowed)
                            } else {
                                None
                            };

                        if let Some(key) = &scrutinee_key {
                            if matches!(arm.pattern, Pattern::Nil(_)) {
                                arm_env.set(
                                    key.clone(),
                                    InferredType::known(WellKnownClass::UndefinedObject.as_str()),
                                );
                            } else if let Some(narrowed) = &type_pattern_narrowed {
                                // The scrutinee variable (if it has a stable
                                // name) denotes the same value as `binding` —
                                // give it the identical narrowed type inside
                                // this arm, so `raw match: [path :: String ->
                                // raw ...]` sees `raw` narrowed too, not just
                                // `path`.
                                arm_env.set(key.clone(), narrowed.clone());
                            } else {
                                // Deliberately overwrites whatever
                                // `bind_pattern_vars` (above) just set for
                                // this key: today that's always `Dynamic` for
                                // a pattern-bound variable, so the residual
                                // here is strictly more precise.
                                arm_env.set(key.clone(), residual_scrutinee_ty.clone());
                            }
                        }
                        // Guard sees `binding`/scrutinee already narrowed
                        // above (ADR 0107: "scope includes the arm's `when:`
                        // guard, not just its body").
                        if let Some(guard) = &arm.guard {
                            self.infer_expr(guard, hierarchy, &mut arm_env, in_abstract_method);
                        }
                        let body_ty =
                            self.infer_expr(&arm.body, hierarchy, &mut arm_env, in_abstract_method);
                        if arm.guard.is_none() {
                            if matches!(arm.pattern, Pattern::Nil(_)) {
                                residual_scrutinee_ty = Self::non_nil_type(&residual_scrutinee_ty);
                            } else if let Pattern::Type { class, .. } = &arm.pattern {
                                // Unguarded `Type` arm guarantees coverage of
                                // `ClassName` — subsequent arms see the
                                // scrutinee narrowed by `\ ClassName` (ADR
                                // 0102 §5 nominal-class difference), mirroring
                                // `isKindOf:`'s false-branch narrowing.
                                residual_scrutinee_ty = InferredType::difference(
                                    &residual_scrutinee_ty,
                                    &InferredType::known(class.name.clone()),
                                    super::TypeProvenance::Inferred(Span::default()),
                                    Some(hierarchy),
                                );
                            }
                        }
                        body_ty
                    })
                    .collect();
                if arm_types.is_empty() {
                    InferredType::Dynamic(DynamicReason::AmbiguousControlFlow)
                } else {
                    InferredType::union_of(&arm_types)
                }
            }

            // Map literal → Dictionary(K, V)
            //
            // BT-2620: join all key types and all value types independently
            // (same union-join convention as the sequence literals below), so a
            // homogeneous `{ 1 -> "a", 2 -> "b" }` infers `Dictionary(Integer,
            // String)`. An empty `{}` infers `Dictionary(Dynamic, Dynamic)`.
            Expression::MapLiteral { pairs, .. } => {
                // Infer key/value in interleaved source order (k1, v1, k2, v2, …)
                // so any narrowing an expression applies to the shared `env`
                // propagates exactly as it did before BT-2620 — the keys and
                // values share one env (no per-pair `env.child()`), so order is
                // observable.
                let mut key_types: Vec<InferredType> = Vec::with_capacity(pairs.len());
                let mut value_types: Vec<InferredType> = Vec::with_capacity(pairs.len());
                for pair in pairs {
                    key_types.push(self.infer_expr(&pair.key, hierarchy, env, in_abstract_method));
                    value_types.push(self.infer_expr(
                        &pair.value,
                        hierarchy,
                        env,
                        in_abstract_method,
                    ));
                }
                let key_ty = Self::join_element_types(&key_types);
                let value_ty = Self::join_element_types(&value_types);
                InferredType::known_with_args("Dictionary", vec![key_ty, value_ty])
            }

            // List literal → List(E)
            //
            // BT-2620: infer every element type (already done in the loop) and
            // join them into a single element type via the union-join
            // convention. A `tail` (cons) folds its own element type in when the
            // tail is a known `List(T)`, otherwise widens the element to
            // `Dynamic`.
            Expression::ListLiteral { elements, tail, .. } => {
                let mut element_types: Vec<InferredType> = elements
                    .iter()
                    .map(|elem| self.infer_expr(elem, hierarchy, env, in_abstract_method))
                    .collect();
                if let Some(t) = tail {
                    let tail_ty = self.infer_expr(t, hierarchy, env, in_abstract_method);
                    // BT-2623: A cons tail must be a proper list. A known
                    // non-`List` tail (e.g. `Array`, tuple-backed) builds an
                    // improper list at runtime; flag it instead of silently
                    // widening the element type to `Dynamic`.
                    if let Some(tail_display) = Self::improper_cons_tail_display(&tail_ty) {
                        self.diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "Cons tail of a list literal is {tail_display}, not a List — this builds an improper list"
                                ),
                                t.span(),
                            )
                            .with_hint(format!(
                                "A `[head | tail]` tail must be a List; {tail_display} would form an improper list at runtime"
                            ))
                            .with_category(DiagnosticCategory::Type),
                        );
                    }
                    element_types.push(Self::tail_element_type(&tail_ty));
                }
                let element_ty = Self::join_element_types(&element_types);
                InferredType::known_with_args("List", vec![element_ty])
            }

            // Array literal → Array(E)
            //
            // BT-2620: join the (already inferred) element types into a single
            // element type. `#[1, 2, 3]` infers `Array(Integer)`, `#[]` infers
            // `Array(Dynamic)`.
            Expression::ArrayLiteral { elements, .. } => {
                let element_types: Vec<InferredType> = elements
                    .iter()
                    .map(|elem| self.infer_expr(elem, hierarchy, env, in_abstract_method))
                    .collect();
                let element_ty = Self::join_element_types(&element_types);
                InferredType::known_with_args("Array", vec![element_ty])
            }

            // String interpolation → String
            Expression::StringInterpolation { segments, .. } => {
                for seg in segments {
                    if let crate::ast::StringSegment::Interpolation(inner_expr) = seg {
                        self.infer_expr(inner_expr, hierarchy, env, in_abstract_method);
                    }
                }
                InferredType::known("String")
            }

            // Super — resolve to parent class type for method validation.
            //
            // BT-2025 / BT-2021: Uses `super_receiver_type` so the parent
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
                            super::type_resolver::super_receiver_type(
                                &class_name,
                                &type_args,
                                parent,
                                hierarchy,
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
    /// LSP type map and emit the BT-1914 "Dynamic in typed class" warning.
    ///
    /// Factored out so the cascade fast-path (BT-2035) can apply the same
    /// post-processing to the first-send `MessageSend` node, which it resolves
    /// via `infer_message_send_with_receiver_ty` instead of routing through
    /// `infer_expr`.
    fn post_process_expr_type(&mut self, expr: &Expression, ty: &InferredType) {
        // Record inferred type for the expression's full span for LSP queries.
        // Dynamic types with a known reason (e.g., UnannotatedParam) are included
        // so that hover can display "Dynamic (reason)" — see BT-1912.
        // Only Dynamic(Unknown) is skipped since it carries no useful provenance.
        if !matches!(ty, InferredType::Dynamic(DynamicReason::Unknown)) {
            self.type_map.insert(expr.span(), ty.clone());
        }

        // BT-1914: Warn when an expression in a typed class infers as Dynamic.
        // Only warn for root-cause Dynamic reasons (not DynamicReceiver, which is
        // propagated from a receiver that already produced its own warning).
        // Unknown is also skipped — no actionable message.
        if let InferredType::Dynamic(reason) = ty {
            if !matches!(
                reason,
                DynamicReason::DynamicReceiver
                    | DynamicReason::DynamicSpec
                    | DynamicReason::Unknown
            ) {
                if let Some(ref class_name) = self.typed_class_context {
                    if let Some(description) = reason.description() {
                        self.diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "expression inferred as Dynamic in typed class `{class_name}` ({description})"
                                ),
                                expr.span(),
                            )
                            .with_hint("Add a type annotation or use `@expect type` to suppress if intentional")
                            .with_category(DiagnosticCategory::Type),
                        );
                    }
                }
            }
        }
    }

    /// Bind all named variables in a destructuring `pattern` into `env` as `Dynamic`.
    ///
    /// Delegates to [`crate::semantic_analysis::extract_pattern_bindings`] to walk
    /// all pattern variants (including `Binary` segments) consistently. Wildcards
    /// and literals are skipped; duplicates are silently ignored (the name resolver
    /// already reports them as errors earlier in the pipeline).
    pub(super) fn bind_pattern_vars(pattern: &Pattern, env: &mut TypeEnv) {
        let (bindings, _diagnostics) = crate::semantic_analysis::extract_pattern_bindings(pattern);
        for id in bindings {
            env.set_local(
                id.name.clone(),
                InferredType::Dynamic(DynamicReason::Unknown),
            );
        }
    }

    /// Infer the type of a message send and validate the selector.
    #[allow(clippy::too_many_arguments)] // hierarchy + env + flag needed for recursive checking
    fn infer_message_send(
        &mut self,
        receiver: &Expression,
        selector: &MessageSelector,
        arguments: &[Expression],
        span: Span,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let receiver_ty = self.infer_expr(receiver, hierarchy, env, in_abstract_method);
        self.infer_message_send_with_receiver_ty(
            receiver,
            receiver_ty,
            selector,
            arguments,
            span,
            hierarchy,
            env,
            in_abstract_method,
        )
    }

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
    fn infer_message_send_with_receiver_ty(
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
            selector_name.as_str(),
            "ifTrue:" | "ifFalse:" | "ifTrue:ifFalse:"
        ) {
            Self::detect_narrowing(receiver)
                .map(|info| self.refine_responds_to_narrowing(info))
                .map(|info| Self::refine_result_narrowing(info, env, hierarchy))
                .map(|info| Self::refine_singleton_narrowing(info, env, hierarchy))
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
        } else if selector_name == "on:do:" {
            // BT-2045: Exception handler block parameter inference.
            // `[...] on: SomeException do: [:e | ...]` — infer `e` as `SomeException`
            // when the first argument is a class reference.
            self.infer_args_for_on_do(arguments, hierarchy, env, in_abstract_method)
        } else if matches!(
            selector_name.as_str(),
            "ifNil:" | "ifNotNil:" | "ifNil:ifNotNil:" | "ifNotNil:ifNil:"
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
        } else {
            // BT-2158: detect class-side sends so block-param propagation
            // uses `find_class_method` instead of `find_method`. Shares the
            // helper with the downstream `is_class_side_receiver` check below.
            // ADR 0083: a metatype-typed receiver (`Meta{C}`) is also class-side
            // — its block params should resolve against `C`'s class methods.
            let is_class_side = Self::is_class_side_receiver(receiver, env)
                || matches!(receiver_ty, InferredType::Meta { .. });
            self.infer_args_with_block_context(
                arguments,
                &receiver_ty,
                &selector_name,
                hierarchy,
                env,
                in_abstract_method,
                is_class_side,
            )
        };

        // Handle asType: compile-time type assertion (ADR 0025 Phase 2b)
        // `expr asType: SomeClass` asserts expr is SomeClass, returns Known(SomeClass)
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
                selector_name.as_str(),
                "ifNil:ifNotNil:" | "ifNotNil:ifNil:"
            ) {
                if let Some(ty) = Self::if_nil_branch_union_ret_ty(arguments, &arg_types) {
                    return ty;
                }
            } else if matches!(selector_name.as_str(), "ifNil:" | "ifNotNil:") {
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
                    self.check_binary_operand_types(
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
        if let Expression::ClassReference { name, .. } = unwrap_parens(receiver) {
            let class_name = &name.name;

            // ADR 0075: `Erlang <module>` — return ErlangModule<module_name> type
            // to enable FFI call type inference on the outer message send.
            // BT-1880: Class protocol selectors (class, new, superclass, etc.)
            // must NOT be intercepted as module lookups — they are handled by
            // normal class-side dispatch (matching codegen CLASS_PROTOCOL_SELECTORS).
            if class_name == "Erlang" {
                if let MessageSelector::Unary(module_name) = selector {
                    if !is_class_protocol_selector(module_name) {
                        // Static module name: `Erlang lists` → ErlangModule<lists>
                        return InferredType::Known {
                            class_name: EcoString::from(WellKnownClass::ErlangModule.as_str()),
                            type_args: vec![InferredType::Known {
                                class_name: module_name.clone(),
                                type_args: vec![],
                                provenance: super::TypeProvenance::Inferred(span),
                            }],
                            provenance: super::TypeProvenance::Inferred(span),
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
                if is_equality_comparison_op(op) {
                    return InferredType::known("Boolean");
                }
            }
            // `aClass class` is the metaclass of the class object. The static
            // hierarchy doesn't track per-class metaclasses precisely, so fall
            // back to the `Metaclass` tower class (BT-1952 parity).
            if selector_name == "class" {
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

        // For instance-side sends on known types
        if let InferredType::Known {
            ref class_name,
            ref type_args,
            ..
        } = receiver_ty
        {
            // In class methods, self sends should check class-side methods.
            // BT-2158: unwrap parens so `(self) foo:` dispatches class-side.
            if env.in_class_method && Self::is_self_receiver(unwrap_parens(receiver)) {
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
                && !is_class_protocol_selector(&selector_name)
                && !matches!(selector, MessageSelector::Binary(_))
            {
                return self.infer_ffi_call(
                    type_args,
                    selector,
                    &selector_name,
                    arguments,
                    &arg_types,
                    span,
                    hierarchy,
                );
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
            // Skip argument type check for binary messages — check_binary_operand_types
            // already provides more specific warnings for arithmetic/comparison/concat.
            if !matches!(selector, MessageSelector::Binary(_)) {
                self.check_argument_types(
                    &resolve_class,
                    &selector_name,
                    &arg_types,
                    span,
                    hierarchy,
                    false,
                    Some(arguments),
                    Some(env),
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
                    if selector_name == "withTimeout:"
                        && ret_ty.as_str() == "TimeoutProxy"
                        && hierarchy.is_actor_subclass(&resolve_class)
                    {
                        return receiver_ty.clone();
                    }

                    // `Self` resolves to the static receiver class (with type args)
                    if ret_ty.as_str() == "Self" {
                        if type_args.is_empty() {
                            return InferredType::known(class_name.clone());
                        }
                        return InferredType::Known {
                            class_name: class_name.clone(),
                            type_args: type_args.clone(),
                            provenance: super::TypeProvenance::Substituted(
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
                    if ret_ty.as_str() == "Self class" {
                        // BT-2647: for a singleton receiver, `resolve_class` is
                        // `Symbol` so `#text class` is `Symbol class` (`#text` is a
                        // Symbol at runtime), not a phantom `Meta("#text")`.
                        return InferredType::meta(resolve_class.clone());
                    }
                    // ADR 0083: `X class` — the method returns the metatype of a
                    // specific named class (BT-2034 annotation `X class`).
                    if let Some(meta_class) = ret_ty.as_str().strip_suffix(" class") {
                        if hierarchy.has_class(meta_class) {
                            return InferredType::meta(EcoString::from(meta_class));
                        }
                    }

                    // BT-1945: `Never` resolves to the bottom type (divergent methods)
                    if WellKnownClass::from_str(ret_ty) == Some(WellKnownClass::Never) {
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
                    let has_nested_self = Self::return_type_mentions_nested_self(ret_ty);
                    if !subst.is_empty() || !method_subst.is_empty() || has_nested_self {
                        // BT-1992: Thread the full receiver type (with type args)
                        // so nested `Self` in generics like `Result(Self, Error)`
                        // resolves to e.g. `Box(Integer)` not bare `Box`.
                        return Self::substitute_return_type_with_self(
                            ret_ty,
                            &subst,
                            &method_subst,
                            Some(&receiver_ty),
                        );
                    }

                    // BT-1834: If the return type is an unresolved type param
                    // (single uppercase letter like E, T, V), fall back to Dynamic
                    // so downstream sends don't get false DNU warnings.
                    if super::is_generic_type_param(ret_ty) && !hierarchy.has_class(ret_ty) {
                        return InferredType::Dynamic(DynamicReason::Unknown);
                    }

                    // BT-2019: Resolve the return type through the centralised
                    // string-form parser, which preserves the full
                    // parameterised type — `List(String)` becomes
                    // `Known("List", [Known("String")])` rather than the
                    // pre-fix bare `Known("List", [])` that silently dropped
                    // the element type.
                    //
                    // Also handles:
                    //  * BT-2017: union return types like `"Integer | Nil"`
                    //    parse into `InferredType::Union`, enabling narrowing.
                    //  * Plain class names — pass through to `Known(name, [])`.
                    //  * Nested generics — `Result(List(String), Error)`
                    //    keeps both layers.
                    return Self::resolve_type_name_string(ret_ty);
                }
            }

            // BT-1834: Block value/value:/value:value: — return the last type arg.
            // Block is variadic: Block(R), Block(A, R), Block(A, B, R), etc.
            // The convention is that the last type arg is always the return type.
            if WellKnownClass::from_str(class_name) == Some(WellKnownClass::Block)
                && !type_args.is_empty()
                && matches!(
                    selector_name.as_str(),
                    "value" | "value:" | "value:value:" | "value:value:value:"
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
            return self.infer_union_message_send(members, &selector_name, span, hierarchy);
        }

        InferredType::Dynamic(DynamicReason::DynamicReceiver)
    }

    /// BT-2254 (ADR 0075 amendment): infer the element type of
    /// `aTuple at: <literal int>` from a known `Tuple(T1, …, Tn)` type.
    ///
    /// Returns `Some(element_type)` only when **all** of the following hold:
    /// - the receiver class is `Tuple` and the selector is `at:`
    /// - the receiver carries positional element types (`type_args` non-empty)
    /// - the single argument is an integer literal (allowing parentheses)
    /// - the 1-based literal index is within `1..=type_args.len()`
    ///
    /// Any other shape returns `None`, so the caller falls through to normal
    /// dispatch (which yields `Dynamic` for the untyped `Tuple at:` primitive).
    /// This guarantees no false-positive type/DNU warnings for non-literal or
    /// out-of-range indices.
    fn infer_literal_index_tuple_at(
        class_name: &str,
        selector_name: &str,
        type_args: &[InferredType],
        arguments: &[Expression],
    ) -> Option<InferredType> {
        if class_name != "Tuple" || selector_name != "at:" {
            return None;
        }
        if type_args.is_empty() || arguments.len() != 1 {
            return None;
        }
        let index = match unwrap_parens(&arguments[0]) {
            Expression::Literal(Literal::Integer(n), _) => *n,
            _ => return None,
        };
        // 1-based index must be in range. `index <= 0` and `index > len` both
        // fall through to Dynamic rather than warning.
        let idx = usize::try_from(index).ok()?;
        if idx == 0 || idx > type_args.len() {
            return None;
        }
        Some(type_args[idx - 1].clone())
    }

    /// Infer the return type of an FFI call on an `ErlangModule<module_name>` receiver.
    ///
    /// Extracts the Erlang module name from the `type_args`, the function name from
    /// the first keyword of the selector, and the arity from argument count. Looks
    /// up `(module, function, arity)` in `NativeTypeRegistry` and returns the
    /// declared return type, or `Dynamic` if not found.
    ///
    /// Also emits keyword mismatch warnings when call-site keywords don't match
    /// the registry's declared parameter names (ADR 0075 — footgun prevention).
    ///
    /// **References:** ADR 0075 — Type Checker Integration, Keyword mismatch warning
    #[allow(clippy::too_many_arguments)] // BT-2846: hierarchy needed for Union-arm compatibility checks
    fn infer_ffi_call(
        &mut self,
        receiver_type_args: &[InferredType],
        selector: &MessageSelector,
        selector_name: &EcoString,
        arguments: &[Expression],
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        // Extract the module name from the receiver's type args.
        // ErlangModule<lists> → module_name = "lists"
        let Some(InferredType::Known {
            class_name: module_name,
            ..
        }) = receiver_type_args.first()
        else {
            return InferredType::Dynamic(DynamicReason::DynamicReceiver); // Dynamic module name
        };

        // Extract the canonical Erlang function name and arity from the selector.
        // The canonical name is the first keyword without colons, matching the
        // selector_to_function/1 logic in beamtalk_erlang_proxy. The spec reader
        // normalizes stored names the same way, so a single lookup suffices.
        let (function_name, arity) = Self::extract_ffi_function_info(selector_name, arguments);

        // Clone the signature to release the borrow on self before emitting diagnostics.
        let sig = self
            .native_type_registry
            .as_ref()
            .and_then(|reg| reg.lookup(module_name, &function_name, arity))
            .cloned();

        let Some(sig) = sig else {
            return InferredType::Dynamic(DynamicReason::UntypedFfi);
        };

        // Emit keyword mismatch warnings (ADR 0075 footgun prevention)
        self.check_ffi_keyword_mismatch(module_name, &function_name, arity, selector, &sig, span);

        // Check argument types positionally against declared params
        self.check_ffi_argument_types(
            module_name,
            &function_name,
            &sig,
            arg_types,
            span,
            hierarchy,
        );

        // BT-2023(C): Propagate call-site type_args into the FFI return type.
        // Erlang specs with polymorphic types (e.g., `[T] -> [T]` for lists:reverse/1)
        // are registered as bare `List -> List` with no type_args. When the call-site
        // argument carries type_args (e.g., `List(String)`), propagate them to the
        // return type so downstream sends see the element type.
        Self::substitute_ffi_return_type(&sig.return_type, &sig.params, arg_types)
    }

    /// Extracts the Erlang function name and arity from a selector and arguments.
    ///
    /// For keyword selectors like `seq: 1 to: 10`, the function name is the first
    /// keyword ("seq") and the arity is the argument count.
    /// For unary selectors like `reverse`, the function name is the selector and
    /// arity is 0 (nullary Erlang call).
    fn extract_ffi_function_info(
        selector_name: &EcoString,
        arguments: &[Expression],
    ) -> (String, u8) {
        // The selector_name for keyword messages is "reverse:" or "seq:to:"
        // The Erlang function name is the first keyword without the colon
        let function_name = selector_name
            .split(':')
            .next()
            .unwrap_or(selector_name.as_str())
            .to_string();

        let arity = u8::try_from(arguments.len()).unwrap_or(u8::MAX);
        (function_name, arity)
    }

    /// Checks argument types against declared parameter types in an FFI signature.
    ///
    /// Types are matched positionally (ADR 0075 — FFI calls are positional).
    pub(super) fn check_ffi_argument_types(
        &mut self,
        module_name: &str,
        function_name: &str,
        sig: &super::native_type_registry::FunctionSignature,
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        for (i, (param, arg_ty)) in sig.params.iter().zip(arg_types.iter()).enumerate() {
            // Skip Dynamic args — we don't know the type
            if matches!(arg_ty, InferredType::Dynamic(_)) {
                continue;
            }
            // Skip Dynamic param types — anything is accepted
            if matches!(param.type_, InferredType::Dynamic(_)) {
                continue;
            }

            let InferredType::Known {
                class_name: expected,
                ..
            } = &param.type_
            else {
                continue;
            };

            let param_pos = i + 1;
            let fallback_label = format!("parameter {param_pos}");
            let param_label = param.keyword.as_deref().unwrap_or(&fallback_label);
            // BT-2066: Render `UndefinedObject` as `Nil` in user-facing messages.
            let expected_display = InferredType::class_name_for_diagnostic(expected.as_str());

            // Object is the root of the BT class hierarchy — any class is a
            // subtype. This arises when Erlang specs use beamtalk_object()
            // (which maps to Object) and the call site passes a concrete class.
            let expected_is_object =
                WellKnownClass::from_str(expected) == Some(WellKnownClass::Object);

            match arg_ty {
                InferredType::Known {
                    class_name: actual, ..
                } => {
                    if actual == expected || expected_is_object {
                        continue;
                    }
                    let actual_display = InferredType::class_name_for_diagnostic(actual.as_str());
                    self.diagnostics.push(Diagnostic::warning(
                        format!(
                            "{module_name}:{function_name}/{arity} {param_label} expects {expected_display}, got {actual_display}",
                            arity = sig.arity,
                        ),
                        span,
                    ).with_hint("Use `@expect type` to suppress if the call is intentional")
                    .with_category(DiagnosticCategory::Type));
                }
                InferredType::Union { members, .. } => {
                    // BT-2846: mirrors check_argument_types's Union handling
                    // (BT-1832) — every member of the argument's union is
                    // checked against the declared FFI parameter type.
                    if expected_is_object {
                        continue;
                    }
                    let Some((compat, total, incompatible)) =
                        Self::classify_union_members(members, |m| {
                            Self::is_type_compatible(m, expected, hierarchy)
                        })
                    else {
                        continue; // Contains Dynamic/Union/Meta/Never member — skip conservatively
                    };
                    if compat == total {
                        continue; // All members compatible → pass
                    }
                    let union_display = arg_ty
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let base_message = format!(
                        "{module_name}:{function_name}/{arity} {param_label} expects {expected_display}, got {union_display}",
                        arity = sig.arity,
                    );
                    let diag = if compat == 0 {
                        Diagnostic::warning(base_message, span).with_hint(format!(
                            "No member of {union_display} is compatible with {expected_display}"
                        ))
                    } else {
                        let list = incompatible
                            .iter()
                            .map(|m| InferredType::class_name_for_diagnostic(m.as_str()))
                            .collect::<Vec<_>>()
                            .join(", ");
                        Diagnostic::hint(base_message, span).with_hint(format!(
                            "Some members of the union are not compatible with {expected_display}: {list}"
                        ))
                    };
                    self.diagnostics
                        .push(diag.with_category(DiagnosticCategory::Type));
                }
                _ => {
                    // Meta/Negation/Intersection/Never argument shapes are not
                    // handled by this check — same conservative skip as before.
                }
            }
        }
    }

    /// BT-2023(C): Propagate call-site `type_args` into an FFI return type.
    ///
    /// Erlang specs lose type-variable identity during extraction -- a spec like
    /// `-spec reverse([T]) -> [T]` becomes `List -> List` with empty `type_args`
    /// on both sides. When the call-site argument carries concrete `type_args`
    /// (e.g., `List(String)`) and the return type has the same base class as a
    /// parameter, we copy the argument's `type_args` to the return type.
    ///
    /// This is a heuristic: it assumes that when the param and return share a
    /// base class, the return preserves the same `type_args`. To stay sound,
    /// we restrict it to unary functions (single param) — the `[T] -> [T]`
    /// pattern of `lists:reverse/1`, `lists:sort/1`, etc. Multi-arg functions
    /// like `lists:map/2` (`Fun, [A] -> [B]`) would be unsound under this
    /// rule, so we leave their return types alone.
    fn substitute_ffi_return_type(
        return_type: &InferredType,
        params: &[super::native_type_registry::ParamType],
        arg_types: &[InferredType],
    ) -> InferredType {
        // Only applies to unary functions — see doc comment.
        if params.len() != 1 || arg_types.len() != 1 {
            return return_type.clone();
        }

        // Only applies when the return type is a Known type with no type_args
        let InferredType::Known {
            class_name: ret_class,
            type_args: ret_args,
            provenance,
        } = return_type
        else {
            return return_type.clone();
        };

        // If the return type already has type_args, nothing to propagate
        if !ret_args.is_empty() {
            return return_type.clone();
        }

        let param = &params[0];
        let arg_ty = &arg_types[0];

        let InferredType::Known {
            class_name: param_class,
            ..
        } = &param.type_
        else {
            return return_type.clone();
        };

        if param_class != ret_class {
            return return_type.clone();
        }

        let InferredType::Known {
            type_args: arg_type_args,
            ..
        } = arg_ty
        else {
            return return_type.clone();
        };

        if arg_type_args.is_empty() {
            return return_type.clone();
        }

        InferredType::Known {
            class_name: ret_class.clone(),
            type_args: arg_type_args.clone(),
            provenance: *provenance,
        }
    }

    /// Emits a warning when call-site keywords don't match the registry's declared
    /// parameter names (ADR 0075 — keyword mismatch warning).
    ///
    /// Suppressed for the universal `with:` fallback (ADR 0028 convention).
    fn check_ffi_keyword_mismatch(
        &mut self,
        module_name: &str,
        function_name: &str,
        arity: u8,
        selector: &MessageSelector,
        sig: &super::native_type_registry::FunctionSignature,
        span: Span,
    ) {
        let MessageSelector::Keyword(parts) = selector else {
            return; // Unary/binary — no keyword mismatch possible
        };

        // Compare each keyword (except the first, which IS the function name)
        // against the declared parameter names (starting from index 1).
        for (i, part) in parts.iter().enumerate().skip(1) {
            let call_keyword = part.keyword.trim_end_matches(':');

            // Suppress warning for universal `with:` fallback (ADR 0028)
            if call_keyword == "with" {
                continue;
            }

            // Check against the declared keyword at this position
            if let Some(param) = sig.params.get(i) {
                if let Some(ref declared_keyword) = param.keyword {
                    // Skip generic/non-canonical param names — "arg" is used
                    // by beamtalk_spec_reader for placeholder parameters, and
                    // normalization also lowercases an explicit `Arg` to "arg".
                    if declared_keyword == "arg" {
                        continue;
                    }
                    if call_keyword != declared_keyword.as_str() {
                        let param_pos = i + 1;
                        self.diagnostics.push(
                            Diagnostic::warning(
                                format!(
                                    "FFI keyword '{call_keyword}:' does not match declaration '{declared_keyword}:' \
                                     for {module_name}:{function_name}/{arity} parameter {param_pos}"
                                ),
                                span,
                            )
                            .with_hint(format!(
                                "FFI calls are positional — keyword names don't affect dispatch. \
                                 Preferred form: {}",
                                sig.display_signature(),
                            ))
                            .with_category(DiagnosticCategory::Type),
                        );
                    }
                }
            }
        }
    }

    /// Returns true if the expression is `self` (direct identifier reference).
    fn is_self_receiver(expr: &Expression) -> bool {
        matches!(expr, Expression::Identifier(ident) if ident.name == "self")
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
    fn class_object_tower_return(
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
        if ret_ty.as_str() == "Self" || ret_ty.as_str() == "Self class" {
            return Some(InferredType::meta(receiver_meta.clone()));
        }
        if WellKnownClass::from_str(ret_ty) == Some(WellKnownClass::Never) {
            return Some(InferredType::Never);
        }
        if super::is_generic_type_param(ret_ty) && !hierarchy.has_class(ret_ty) {
            return None;
        }
        Some(Self::resolve_type_name_string(ret_ty))
    }

    /// Returns true if `expr` resolves to a class-side receiver — either a
    /// direct `ClassReference` or `self` inside a class method. Unwraps
    /// parentheses so `(HTTPRouter) foo:` and `(self) foo:` are treated
    /// identically to the un-parenthesised forms (BT-2158).
    fn is_class_side_receiver(expr: &Expression, env: &TypeEnv) -> bool {
        let unwrapped = unwrap_parens(expr);
        matches!(unwrapped, Expression::ClassReference { .. })
            || (env.in_class_method && Self::is_self_receiver(unwrapped))
    }

    /// Describes where an expression's type originated (BT-1588).
    ///
    /// Returns `Some((description, span))` when the value expression is a
    /// message send that returns a generic type parameter (e.g., `V` from
    /// `Dictionary at:ifAbsent:`). Returns `None` when origin tracking
    /// isn't useful (concrete types, Dynamic, etc.).
    fn describe_type_origin(
        value: &Expression,
        ty: &super::InferredType,
        hierarchy: &ClassHierarchy,
        env: &super::TypeEnv,
    ) -> Option<(EcoString, crate::source_analysis::Span)> {
        // Only track origin for generic type params (single uppercase letter)
        let type_name = ty.as_known()?;
        if !super::is_generic_type_param(type_name) {
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
    fn infer_union_message_send(
        &mut self,
        members: &[InferredType],
        selector: &str,
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
        if is_equality_comparison_op(selector) {
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
            if !hierarchy.has_class(resolve_name) {
                uncertain_member_count += 1;
                return_types.push(InferredType::Dynamic(DynamicReason::DynamicReceiver));
                continue;
            }
            if hierarchy.has_instance_dnu_override(resolve_name) {
                uncertain_member_count += 1;
                return_types.push(InferredType::Dynamic(DynamicReason::DynamicReceiver));
                continue;
            }
            if hierarchy.resolves_selector(resolve_name, selector) {
                responding_count += 1;
                if let Some(method) = hierarchy.find_method(resolve_name, selector) {
                    if let Some(ref ret_ty) = method.return_type {
                        if ret_ty.as_str() == "Self" {
                            // Self resolves to the concrete member type (with type args)
                            return_types.push(member.clone());
                        } else if ret_ty.as_str() == "Self class" {
                            // ADR 0083: `Self class` resolves to the metatype of
                            // the concrete union member (was Dynamic pre-0083,
                            // BT-1952). Use `resolve_name` so a singleton member
                            // yields `Symbol class` (`#foo class` is `Symbol` at
                            // runtime), not a phantom `Meta("#foo")` (BT-2624).
                            return_types.push(InferredType::meta(EcoString::from(resolve_name)));
                        } else if let Some(meta_class) = ret_ty
                            .as_str()
                            .strip_suffix(" class")
                            .filter(|name| hierarchy.has_class(name))
                        {
                            // ADR 0083: an explicit `X class` return on a union
                            // member resolves to `Meta{X}` — mirrors the
                            // non-union path (BT-2034). Without this branch the
                            // ` class` suffix leaked through as
                            // `Known("X class")`.
                            return_types.push(InferredType::meta(EcoString::from(meta_class)));
                        } else if WellKnownClass::from_str(ret_ty) == Some(WellKnownClass::Never) {
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
                            let has_nested_self = Self::return_type_mentions_nested_self(ret_ty);
                            if !subst.is_empty() || has_nested_self {
                                return_types.push(Self::substitute_return_type_with_self(
                                    ret_ty,
                                    &subst,
                                    &HashMap::new(),
                                    Some(member),
                                ));
                            } else if super::is_generic_type_param(ret_ty)
                                && !hierarchy.has_class(ret_ty)
                            {
                                return_types.push(InferredType::Dynamic(DynamicReason::Unknown));
                            } else {
                                // BT-2019 / BT-2017: Resolve through the
                                // centralised string-form parser to preserve
                                // parametric type args (`List(String)` keeps
                                // its element type) and parse union return
                                // types into `InferredType::Union`.
                                return_types.push(Self::resolve_type_name_string(ret_ty));
                            }
                        }
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
                    let nil_contribution = if ret_ty.as_str() == "Self" {
                        InferredType::known(WellKnownClass::UndefinedObject.as_str())
                    } else if ret_ty.as_str() == "Self class" {
                        // ADR 0083: `nil class` → metatype of `UndefinedObject`.
                        InferredType::meta(WellKnownClass::UndefinedObject.as_str())
                    } else if WellKnownClass::from_str(ret_ty) == Some(WellKnownClass::Never) {
                        InferredType::Never
                    } else {
                        Self::resolve_type_name_string(ret_ty)
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
            // BT-2066: Render `UndefinedObject` as `Nil` for user-facing messages.
            let member_names: Vec<String> = members
                .iter()
                .filter_map(|m| m.display_for_diagnostic().map(|n| n.to_string()))
                .collect();
            let union_display = member_names.join(" | ");
            // BT-2066: Also map missing-member names through the diagnostic rewriter.
            let missing_display: Vec<EcoString> = missing_names
                .iter()
                .map(|n| InferredType::class_name_for_diagnostic(n.as_str()))
                .collect();

            let message = if missing_names.len() == 1 {
                format!(
                    "{} does not understand '{selector}' (in union {union_display})",
                    missing_display[0]
                )
            } else {
                format!(
                    "{} do not understand '{selector}' (in union {union_display})",
                    missing_display.join(", ")
                )
            };

            // BT-1872: Use warning severity when no non-nil members respond
            // AND no uncertain members (unknown classes, DNU overrides) exist
            // (the message send will definitely fail at runtime). Use hint when
            // only some members lack the selector or uncertainty exists.
            let diag = if responding_count == 0 && uncertain_member_count == 0 {
                Diagnostic::warning(message, span)
            } else {
                Diagnostic::hint(message, span)
            }
            .with_hint("Use `respondsTo:` to check before sending, or `@expect type` to suppress")
            .with_category(crate::source_analysis::DiagnosticCategory::Dnu);
            self.diagnostics.push(diag);
        }

        // BT-1857: If the union had Nil but all non-Nil members responded,
        // the return type is just the union of the non-Nil return types
        // (Nil was skipped, so it's not in return_types).
        InferredType::union_of(&return_types)
    }

    /// Detect control-flow narrowing from the receiver of `ifTrue:`/`ifFalse:`.
    ///
    /// Dispatches through the [`narrowing::rules::RULES`] table (BT-2050).
    /// Kept as a thin wrapper so existing test helpers that call
    /// `TypeChecker::detect_narrowing` stay working without import churn.
    pub(super) fn detect_narrowing(receiver: &Expression) -> Option<NarrowingInfo> {
        narrowing::detect(receiver)
    }

    /// Refines a `respondsTo:` narrowing from `Dynamic` to a protocol type
    /// when the protocol registry is available and exactly one protocol
    /// requires the tested selector (ADR 0068 Phase 2e, BT-1833).
    ///
    /// If no protocol registry is set, or zero/multiple protocols match the
    /// selector, the narrowing is returned unchanged (stays `Dynamic`).
    fn refine_responds_to_narrowing(&self, mut info: NarrowingInfo) -> NarrowingInfo {
        // Only refine to a protocol type when the variable is currently Dynamic.
        // If the variable already has a concrete type (e.g., Integer), narrowing
        // to a protocol (e.g., Printable) would lose type-specific APIs.
        if matches!(info.true_type, InferredType::Dynamic(_)) {
            if let Some(ref selector) = info.responded_selector {
                if let Some(ref registry) = self.protocol_registry {
                    if let Some(protocol_name) =
                        registry.find_unique_protocol_for_selector(selector)
                    {
                        info.true_type = InferredType::known(protocol_name.clone());
                    }
                }
            }
        }
        info
    }

    /// Refines a Result `isOk` / `ok` / `isError` narrowing (BT-1859).
    ///
    /// When the variable has type `Result(T, E)`, both true and false branches
    /// keep the full `Result(T, E)` type — generic substitution already resolves
    /// `value -> T` and `error -> E`.  The narrowing ensures the branches get
    /// typed environments (via `infer_block_with_narrowing`) rather than falling
    /// through to the no-narrowing path.
    ///
    /// If the variable is not typed as `Result`, the result-specific flags are
    /// cleared and `true_type` is set to the variable's current type so the
    /// narrowing is effectively a no-op (preserves the existing type in blocks).
    fn refine_result_narrowing(
        mut info: NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
    ) -> NarrowingInfo {
        if !info.is_result_ok_check && !info.is_result_error_check {
            return info;
        }
        let current_ty = Self::resolve_narrowing_variable_type(&info.variable, env, hierarchy);
        let is_result = matches!(
            &current_ty,
            InferredType::Known { class_name, .. }
                if WellKnownClass::from_str(class_name) == Some(WellKnownClass::Result)
        );
        if is_result {
            // Both branches keep the full Result(T, E) type so generic
            // substitution continues to resolve value->T / error->E.
            info.true_type = current_ty.clone();
            info.false_type = Some(current_ty);
        } else {
            // Not a Result — clear the result flags so downstream code doesn't
            // treat this as a Result narrowing.  Set true_type to the variable's
            // current type to preserve type info in the block env.
            info.is_result_ok_check = false;
            info.is_result_error_check = false;
            info.true_type = current_ty;
            info.false_type = None;
        }
        info
    }

    /// Refines a singleton (in)equality narrowing `x = #foo` / `#foo = x`
    /// (BT-2617).
    ///
    /// `detect` only sees the AST, so it leaves the branch types provisional
    /// and records the tested singleton in `singleton_eq`. Here we resolve the
    /// variable's current type and split it: the branch where the test holds
    /// narrows to the singleton, and the complementary branch narrows to the
    /// variable's type with that singleton removed (`Integer | #infinity` minus
    /// `#infinity` ⇒ `Integer`). For an inequality (`/=`, `=/=`) the two
    /// branches are swapped.
    ///
    /// BT-2624 (item 1) / BT-2631: the "comparison can never be true / always
    /// true" hint for a statically decidable singleton test is *not* emitted
    /// here. A guarded comparison (`unionVar = #foo ifTrue: …`) has its receiver
    /// — the `=` send — inferred through `infer_message_send_with_receiver_ty`,
    /// whose `check_impossible_singleton_comparison` is the single emitter for
    /// both guarded and bare comparisons. Emitting here too would duplicate the
    /// hint. This method only refines the branch types.
    pub(super) fn refine_singleton_narrowing(
        mut info: NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
    ) -> NarrowingInfo {
        let Some(eq) = info.singleton_eq.clone() else {
            return info;
        };
        let current_ty = Self::resolve_narrowing_variable_type(&info.variable, env, hierarchy);

        let matched = InferredType::known(eq.singleton.as_type_name().clone());
        let provenance = super::TypeProvenance::Inferred(Span::default());
        // ADR 0102 §2: the equality branches are the set-theoretic intersection
        // and difference of the variable's current type with the tested
        // singleton. `intersect(T, #foo)` is the "test holds" type (the values of
        // `T` that could be `#foo`); `difference(T, #foo)` is the complementary
        // type with `#foo` removed. An impossible test (`x :: Integer; x = #foo`)
        // yields `intersect = Never` for the unreachable branch — the diagnostic
        // for that case is emitted separately by
        // `check_impossible_singleton_comparison`.
        // Out of scope for narrowing (ADR 0102 §2 group 3 / BT-2743): singleton
        // narrowing never intersects with a protocol name, so `None` here.
        let holds =
            InferredType::intersect(&current_ty, &matched, provenance, Some(hierarchy), None);
        let removed = InferredType::difference(&current_ty, &matched, provenance, Some(hierarchy));
        if eq.negated {
            // `x /= #foo`: the true branch removes the singleton; the false
            // branch is the singleton.
            info.true_type = removed;
            info.false_type = Some(holds);
        } else {
            // `x = #foo`: the true branch is the singleton; the false branch
            // removes it.
            info.true_type = holds;
            info.false_type = Some(removed);
        }
        info
    }

    /// Refines a `class = C` / `isKindOf: C` narrowing (ADR 0102 §2 group 2,
    /// §5, BT-2741, BT-2744).
    ///
    /// `detect` only sees the AST, so it records the tested class name in
    /// `class_test` and leaves `true_type` provisional. Here we resolve the
    /// variable's current type and delegate to the diagnostic-free
    /// [`Self::compute_class_narrowing`] (BT-2825 factored this out so
    /// [`Self::apply_early_return_narrowing`] can reuse the same math without
    /// re-emitting the "comparison can never be true" hint below for a guard
    /// that was already fully type-checked), then reports that hint using
    /// the resolved true branch.
    pub(super) fn refine_class_narrowing(
        &mut self,
        info: NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
        test_span: Span,
    ) -> NarrowingInfo {
        let Some(ClassTestInfo { class_name, .. }) = info.class_test.clone() else {
            return info;
        };
        let current_ty = Self::resolve_narrowing_variable_type(&info.variable, env, hierarchy);
        let refined_info = Self::compute_class_narrowing(
            info,
            &current_ty,
            hierarchy,
            self.protocol_registry.as_ref(),
        );
        self.check_impossible_class_comparison(
            &current_ty,
            &class_name,
            &refined_info.true_type,
            test_span,
        );
        refined_info
    }

    /// Pure narrowing math for a `class = C` / `isKindOf: C` test — no
    /// diagnostics (BT-2825). Shared by [`Self::refine_class_narrowing`] (the
    /// primary `ifTrue:`/`ifFalse:` dispatch site, which additionally emits
    /// the "comparison can never be true" hint) and
    /// [`Self::apply_early_return_narrowing`] (the guard-and-early-return
    /// post-guard case, which must *not* re-emit that hint since the guard
    /// expression was already fully type-checked by the time post-guard
    /// narrowing runs).
    ///
    /// Routes the **true** branch through the hierarchy-and-protocol-aware
    /// `intersect(current, C)` for *both* idioms, and the **false** branch
    /// through `difference(current, C)` for `isKindOf:` *only* — see
    /// `ClassTestKind` for why `class =:=`'s false branch must stay
    /// unnarrowed.
    ///
    /// The true branch narrows precisely (`x :: Number; x isKindOf: Integer`
    /// true branch is `Integer`, not `Number`), and a test against a
    /// hierarchy-unrelated class types the (unreachable) true branch `Never`
    /// (reported by the caller via `check_impossible_class_comparison`).
    ///
    /// **Protocol collapse (BT-2825):** when `current` is a protocol (or a
    /// union containing one) and `C` is an unrelated concrete class,
    /// `intersect` conservatively returns the irreducible `current & C`
    /// (ADR 0102 §1/§3) — sound for a *declared* `P1 & P2` annotation, but
    /// `isKindOf: C` is a positive **runtime** proof that the value literally
    /// is a `C` (or subclass), which is strictly stronger. The true branch
    /// collapses that `Intersection` down to the bare `C` so downstream
    /// assignability (`is_assignable_to`, which has no notion of `&`) and DNU
    /// checks see a plain nominal type instead of a compound one they don't
    /// otherwise understand — this is what lets a `Printable`-declared local
    /// satisfy a `List`-typed assignment after `(x isKindOf: List) ifTrue:
    /// [...]` / `ifFalse: [^...]` without an `@expect type` escape hatch.
    ///
    /// The `isKindOf:` false branch closes the group-2 gap ADR 0102 §1
    /// deliberately left open (nominal-class difference needed its own
    /// design, §5): `x :: Number; x isKindOf: Integer` false branch narrows
    /// to `Number \ Integer` (previously untouched — `false_type` stayed
    /// `None`, and `ifFalse:`/the else-arm of `ifTrue:ifFalse:` fell back to
    /// no narrowing). `class =:=`'s false branch stays `None`, exactly as
    /// before BT-2744.
    fn compute_class_narrowing(
        mut info: NarrowingInfo,
        current_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&crate::semantic_analysis::protocol_registry::ProtocolRegistry>,
    ) -> NarrowingInfo {
        let Some(ClassTestInfo { class_name, kind }) = info.class_test.clone() else {
            return info;
        };
        let pattern = InferredType::known(class_name.clone());
        info.true_type =
            Self::intersect_with_class(current_ty, &class_name, hierarchy, protocol_registry);
        // BT-2744: only `isKindOf:`'s false branch can be narrowed via
        // nominal-class `difference` — `Negation{base, excluded}` always
        // excludes `excluded`'s *entire* subtree, which matches `isKindOf:`'s
        // negation ("not C and not any subclass of C") but not `class =:=`'s
        // ("not exactly C" — C's subclasses are still live possibilities;
        // narrowing them away would produce a false "comparison can never be
        // true" hint on a later, satisfiable `isKindOf:` test). See
        // `ClassTestKind`.
        if kind == ClassTestKind::KindOf {
            info.false_type = Some(InferredType::difference(
                current_ty,
                &pattern,
                super::TypeProvenance::Inferred(Span::default()),
                Some(hierarchy),
            ));
        }
        info
    }

    /// `intersect(current, Known(class_name))`, collapsing a compound
    /// `Intersection` result down to the bare nominal `class_name` (BT-2825)
    /// — shared by `isKindOf:`/`class =` guard narrowing (above) and
    /// `Pattern::Type` match-arm binding narrowing (BT-2855, ADR 0107), both
    /// of which need the same "this value literally is `class_name`"
    /// true-branch collapse so downstream `is_assignable_to`/DNU checks see
    /// a plain nominal type rather than an `Intersection` they don't
    /// otherwise understand.
    fn intersect_with_class(
        current_ty: &InferredType,
        class_name: &EcoString,
        hierarchy: &ClassHierarchy,
        protocol_registry: Option<&crate::semantic_analysis::protocol_registry::ProtocolRegistry>,
    ) -> InferredType {
        let pattern = InferredType::known(class_name.clone());
        let provenance = super::TypeProvenance::Inferred(Span::default());
        let refined = InferredType::intersect(
            current_ty,
            &pattern,
            provenance,
            Some(hierarchy),
            protocol_registry,
        );
        if matches!(refined, InferredType::Intersection { .. }) {
            pattern
        } else {
            refined
        }
    }

    /// ADR 0102 §2 group 2 / BT-2741: emits the "comparison can never be
    /// true" hint when a `class = C` / `isKindOf: C` test is statically
    /// decidable impossible — `C` is hierarchy-unrelated to `current_ty`, so
    /// `intersect(current_ty, C)` is `Never`.
    ///
    /// Parallels `check_impossible_singleton_comparison`'s gating (silent on
    /// `Dynamic` and `Never` receivers, silent whenever the intersect result
    /// is not `Never`) with one deliberate extra gate: **provenance**. The
    /// hint fires only when the receiver's type was *inferred* from actual
    /// value flow (`x := 42. x isKindOf: String`). A *declared* annotation
    /// (`aClass :: Behaviour`) is an unverified promise under gradual typing,
    /// and `isKindOf:` is precisely how code verifies it at runtime — stdlib
    /// defensive guards like `SystemNavigation referencesTo:`'s
    /// `(aClass isKindOf: Symbol)` check are legitimate and must stay silent.
    /// Conservative rule: only provably-`Inferred` provenance fires;
    /// `Declared`, `Substituted`, `Extracted`, or absent provenance is
    /// silent. (The true-branch narrowing to `Never` is *not* gated — sends
    /// in the unreachable branch are silent per the `Never`-receiver policy,
    /// so it stays harmless.) Unlike the singleton case, there is no "always
    /// true" counterpart — `isKindOf:`/`class =` have no negated form to
    /// swap the message for.
    fn check_impossible_class_comparison(
        &mut self,
        current_ty: &InferredType,
        class_name: &EcoString,
        refined_ty: &InferredType,
        test_span: Span,
    ) {
        if matches!(current_ty, InferredType::Dynamic(_) | InferredType::Never)
            || !matches!(refined_ty, InferredType::Never)
        {
            return;
        }
        // Provenance gate (see doc comment): declared annotations are
        // runtime-unverified promises — a defensive `isKindOf:` guard against
        // them is legitimate, so only value-flow-inferred types fire.
        if !matches!(
            current_ty.provenance(),
            Some(super::TypeProvenance::Inferred(_))
        ) {
            return;
        }
        let ty_display = current_ty.display_for_diagnostic().unwrap_or_default();
        let message =
            format!("comparison can never be true: `{ty_display}` is never a `{class_name}`");
        self.diagnostics.push(
            Diagnostic::hint(message, test_span)
                .with_category(crate::source_analysis::DiagnosticCategory::Type),
        );
    }

    /// BT-2624 / BT-2631: emits the "comparison can never be true" / "always
    /// true" hint when a singleton (in)equality test (`var =:= #foo`) is
    /// statically decidable — the singleton can never be a value of `current_ty`.
    ///
    /// Called from `infer_message_send_with_receiver_ty`, which is the single
    /// emitter for both the guarded path (`(unionVar = #foo) ifTrue: [...]`,
    /// where the inner `=` send is inferred through that method) and the bare
    /// standalone path (`unionVar = #foo` outside a guard), so the membership
    /// rule (`type_admits_singleton`) and the diagnostic wording live in one
    /// place. Stays conservative: silent on `Dynamic` (unknown) and `Never`
    /// (already-unreachable) receivers, and silent when any member admits the
    /// singleton (it *is* the singleton, or `Symbol`/one of its supertypes).
    pub(super) fn check_impossible_singleton_comparison(
        &mut self,
        current_ty: &InferredType,
        singleton: &narrowing::SingletonName,
        negated: bool,
        test_span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        if matches!(current_ty, InferredType::Dynamic(_) | InferredType::Never)
            || Self::type_admits_singleton(current_ty, singleton, hierarchy)
        {
            return;
        }
        let ty_display = current_ty.display_for_diagnostic().unwrap_or_default();
        let message = if negated {
            format!("comparison is always true: `{singleton}` is never a value of `{ty_display}`")
        } else {
            format!("comparison can never be true: `{singleton}` is not a value of `{ty_display}`")
        };
        self.diagnostics.push(
            Diagnostic::hint(message, test_span)
                .with_category(crate::source_analysis::DiagnosticCategory::Type),
        );
    }

    /// BT-2624 / ADR 0102 §2: whether the singleton `singleton` (`#foo`) could
    /// be a runtime value of `ty` — defined as
    /// `intersect(ty, #foo, hierarchy) != Never`.
    ///
    /// Intersection already models singleton membership (`Symbol ∩ #foo = #foo`,
    /// `Object ∩ #foo = #foo`, `Integer ∩ #foo = Never`, and it distributes over
    /// unions), so this is the single source of truth for "the test could hold".
    /// `Dynamic` intersects to the singleton (non-`Never`), so callers stay
    /// conservative when the type is unknown.
    ///
    /// The [`narrowing::SingletonName`] parameter guarantees at the type level
    /// (BT-2764) that the pattern is a bare `#foo` singleton, never a nominal
    /// class name — singletons are not hierarchy entries, so a nominal name
    /// here would silently mis-answer membership.
    ///
    /// The hierarchy is threaded through so *supertypes* of `Symbol` other
    /// than `Object` (e.g. an abstract `ProtoObject`-typed receiver) also
    /// admit singletons (BT-2764): `intersect`'s symbol-singleton arms consult
    /// the hierarchy to reduce `ProtoObject ∩ #foo` to `#foo` rather than
    /// falling through to `Never`, matching the pre-ADR-0102 hierarchy walk.
    fn type_admits_singleton(
        ty: &InferredType,
        singleton: &narrowing::SingletonName,
        hierarchy: &ClassHierarchy,
    ) -> bool {
        let matched = InferredType::known(singleton.as_type_name().clone());
        let provenance = super::TypeProvenance::Inferred(Span::default());
        !matches!(
            // No protocol registry needed — a bare singleton is never a
            // protocol name.
            InferredType::intersect(ty, &matched, provenance, Some(hierarchy), None),
            InferredType::Never
        )
    }

    /// BT-2745 / ADR 0102 §4: `true` when `ty` is a *known-closed* singleton
    /// union — an `InferredType::Union` whose every member is a bare
    /// `#symbol` singleton (`Known` with a `#`-prefixed name and no type
    /// args).
    ///
    /// This single structural condition is the entire gate for
    /// [`check_singleton_match_exhaustiveness`](Self::check_singleton_match_exhaustiveness),
    /// and it is what keeps the check silent under every open-world case
    /// ADR 0100 requires (mirroring `check_impossible_singleton_comparison`'s
    /// conservatism):
    /// - `Dynamic` is not a `Union` at all — silent.
    /// - A bare/open `Symbol` (`Known("Symbol")`) is not a `Union` — silent.
    /// - `Negation` (`Symbol \ #foo`, a co-finite set) is not a `Union` — silent.
    /// - A `perform:`-typed or DNU-overriding receiver types as `Dynamic`
    ///   upstream, so it never reaches here as a singleton union — silent.
    /// - A union with *any* non-singleton member — including a single
    ///   `Dynamic` arm — fails the `all()` check, so the *whole* union stays
    ///   silent, matching ADR 0100 Rule 1's "any `Dynamic` in a union
    ///   downgrades the whole union to `Open`".
    fn is_closed_singleton_union(ty: &InferredType) -> bool {
        matches!(ty, InferredType::Union { members, .. }
        if !members.is_empty() && members.iter().all(|m| matches!(
            m,
            InferredType::Known { class_name, type_args, .. }
                if type_args.is_empty() && class_name.starts_with('#')
        )))
    }

    /// BT-2745 / ADR 0102 §4: advisory `match:` exhaustiveness for
    /// singleton-union scrutinees.
    ///
    /// **Distinct from BT-1299.** `validators::match_validators::check_match_exhaustiveness`
    /// is *pattern-based* — it keys on `Result ok:`/`Result error:` constructor
    /// patterns in the arms and emits a hard `Diagnostic::error()`, because
    /// (per its own doc comment) "there is no resolved scrutinee type available
    /// at compile time" for sealed constructor types. This check is the
    /// opposite: it is *type-based*, consulting the scrutinee's **inferred**
    /// type and computing the residual via `difference` (ADR 0102 §1) —
    /// `difference(scrutinee_type, covered)`. The two checks run independently,
    /// side by side, and neither suppresses the other.
    ///
    /// **Severity is `Warning`, never `Error`** (ADR 0102 §4 / ADR 0100):
    /// gradual-typing annotations are not runtime-enforced, so a "provably
    /// exhaustive" static claim can never be a soundness guarantee — an FFI
    /// call typed `#north | #south` can still return `#west` at runtime.
    ///
    /// **Gating:** see [`is_closed_singleton_union`](Self::is_closed_singleton_union).
    ///
    /// **Known discoverability cliff** (ADR 0102 §4, documented here rather
    /// than fixed): this warning silently disappears the moment inference
    /// *widens* the scrutinee — one `Dynamic`-returning arm upstream, a
    /// reassignment to a wider type, or annotating the variable as bare
    /// `Symbol` — and the user has no way to distinguish "provably exhaustive"
    /// from "the checker gave up". An opt-in `match:` assertion (analogous to
    /// TypeScript's `satisfies never` idiom) is the natural follow-up; it is
    /// deliberately out of scope for this ADR so the check stays advisory-only.
    pub(super) fn check_singleton_match_exhaustiveness(
        &mut self,
        scrutinee_ty: &InferredType,
        arms: &[MatchArm],
        match_span: Span,
    ) {
        if !Self::is_closed_singleton_union(scrutinee_ty) {
            return;
        }
        let Some((residual_display, missing)) = Self::singleton_match_residual(scrutinee_ty, arms)
        else {
            return;
        };
        let missing_str = Self::format_missing_members(&missing);
        let verb = if missing.len() == 1 { "is" } else { "are" };

        self.diagnostics.push(
            Diagnostic::warning(
                format!(
                    "non-exhaustive match: {missing_str} {verb} not handled \
                     (residual type: `{residual_display}`)"
                ),
                match_span,
            )
            .with_hint(
                "Add an arm for the remaining case(s), or a `_ ->` wildcard \
                 to handle them."
                    .to_string(),
            )
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// BT-2763 / ADR 0106: `matchExhaustive:` — an opt-in **assertion** that a
    /// `match:` is provably exhaustive, at asserted `Error` severity (the user
    /// opted in by writing `matchExhaustive:` instead of `match:`, so ADR
    /// 0100's "escalation to a build-failing error is always opt-in" rule is
    /// satisfied here, not violated).
    ///
    /// **Distinct from, and does not replace,**
    /// [`check_singleton_match_exhaustiveness`](Self::check_singleton_match_exhaustiveness)
    /// (BT-2745's advisory `Warning` path for plain `match:`), which is
    /// unchanged and still runs whenever `exhaustive` is `false` — see the
    /// call site in `infer_expr`'s `Expression::Match` arm.
    ///
    /// Two failure modes, both `Error`:
    /// - **Residual non-empty on a closed singleton union**: same residual
    ///   computation as the advisory check, naming the uncovered members.
    /// - **Scrutinee is not a *known-closed* singleton union at all**
    ///   (`Dynamic`, an open/bare `Symbol`, a `Negation` co-finite set, a
    ///   union with any non-singleton member, or an ordinary nominal type) —
    ///   the assertion cannot be verified, so it fails loudly rather than
    ///   silently downgrading to advisory. This is the behaviour BT-2745 /
    ///   ADR 0102 §4 left as a "known discoverability cliff": once the
    ///   scrutinee widens, `matchExhaustive:` stops being provable and must
    ///   say so, not go quiet.
    pub(super) fn check_asserted_match_exhaustiveness(
        &mut self,
        scrutinee_ty: &InferredType,
        arms: &[MatchArm],
        match_span: Span,
    ) {
        if !Self::is_closed_singleton_union(scrutinee_ty) {
            let ty_display = scrutinee_ty.display_for_diagnostic().unwrap_or_default();
            self.diagnostics.push(
                Diagnostic::error(
                    format!(
                        "cannot verify `matchExhaustive:` is exhaustive — scrutinee type \
                         `{ty_display}` is not a closed union of symbol singletons"
                    ),
                    match_span,
                )
                .with_hint(
                    "matchExhaustive: only proves exhaustiveness over a closed union of \
                     `#symbol` singletons (e.g. `x :: #north | #south`). Annotate the \
                     scrutinee with such a type, or use `match:` if exhaustiveness cannot \
                     be guaranteed statically."
                        .to_string(),
                )
                .with_category(DiagnosticCategory::Type),
            );
            return;
        }

        let Some((residual_display, missing)) = Self::singleton_match_residual(scrutinee_ty, arms)
        else {
            return;
        };
        let missing_str = Self::format_missing_members(&missing);
        let verb = if missing.len() == 1 { "is" } else { "are" };

        self.diagnostics.push(
            Diagnostic::error(
                format!(
                    "non-exhaustive matchExhaustive: {missing_str} {verb} not handled \
                     (residual type: `{residual_display}`)"
                ),
                match_span,
            )
            .with_hint(
                "Add an arm for the remaining case(s), or a `_ ->` wildcard \
                 to handle them."
                    .to_string(),
            )
            .with_category(DiagnosticCategory::Type),
        );
    }

    /// Shared residual computation for both the advisory (BT-2745) and
    /// asserted (BT-2763) singleton-union `match:` exhaustiveness checks.
    ///
    /// Callers must already have checked
    /// [`is_closed_singleton_union`](Self::is_closed_singleton_union) —
    /// this function assumes `scrutinee_ty` is one.
    ///
    /// Returns `None` when the match is exhaustive (an unguarded
    /// wildcard/variable-binding arm, or a `Never` residual after subtracting
    /// covered singleton arms). Otherwise returns `(residual_display,
    /// missing_members)`.
    fn singleton_match_residual(
        scrutinee_ty: &InferredType,
        arms: &[MatchArm],
    ) -> Option<(EcoString, Vec<EcoString>)> {
        // An unguarded wildcard arm is full coverage — mirrors BT-1299's
        // suppression rule. An unguarded variable-binding arm (`x -> ...`)
        // always matches too, so it counts the same. A *guarded* catch-all
        // (`_ when: [cond] -> ...`) does NOT guarantee coverage of the
        // remaining cases.
        if arms.iter().any(|arm| {
            arm.guard.is_none()
                && matches!(arm.pattern, Pattern::Wildcard(_) | Pattern::Variable(_))
        }) {
            return None;
        }

        // Collect covered singletons from unguarded symbol-literal arms only —
        // a guarded arm (`#north when: [cond] -> ...`) does not guarantee
        // coverage of that variant, same rule as BT-1299's constructor-arm
        // coverage.
        let mut covered: Vec<InferredType> = Vec::new();
        for arm in arms {
            if arm.guard.is_some() {
                continue;
            }
            if let Pattern::Literal(Literal::Symbol(name), _) = &arm.pattern {
                covered.push(InferredType::known(eco_format!("#{name}")));
            }
        }

        let provenance = super::TypeProvenance::Inferred(Span::default());
        let covered_ty = InferredType::union_of(&covered);
        // Singleton scrutinees only (guaranteed by `is_closed_singleton_union`
        // callers) — singletons are never hierarchy entries, so no hierarchy
        // is needed here.
        let residual = InferredType::difference(scrutinee_ty, &covered_ty, provenance, None);

        // `Never` residual ⇒ every member is covered ⇒ exhaustive.
        if matches!(residual, InferredType::Never) {
            return None;
        }

        let residual_display = residual.display_for_diagnostic().unwrap_or_default();
        let missing: Vec<EcoString> = match &residual {
            InferredType::Union { members, .. } => members
                .iter()
                .filter_map(InferredType::as_known)
                .cloned()
                .collect(),
            InferredType::Known { class_name, .. } => vec![class_name.clone()],
            // Not reachable in practice: `difference` over a union of bare
            // singletons only ever normalises to `Never`, a single `Known`,
            // or a `Union` of `Known`s — but stay conservative rather than
            // panicking if the algebra's normal form ever changes.
            _ => vec![],
        };
        Some((residual_display, missing))
    }

    /// Formats a list of missing singleton member names as a
    /// backtick-quoted, comma-separated list for a diagnostic message.
    fn format_missing_members(missing: &[EcoString]) -> String {
        missing
            .iter()
            .map(|m| format!("`{m}`"))
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// BT-2045: Infer argument types for `on:do:` with exception class propagation.
    ///
    /// When the first argument is a class reference (e.g., `Exception`, `Error`),
    /// the handler block's parameter is typed as that class instead of
    /// `Dynamic(UnannotatedParam)`.
    ///
    /// `[...] on: Error do: [:e | e message]` → `e :: Error`
    fn infer_args_for_on_do(
        &mut self,
        arguments: &[Expression],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        // on:do: expects exactly 2 arguments: exClass and handler
        if arguments.len() != 2 {
            return arguments
                .iter()
                .map(|arg| self.infer_expr(arg, hierarchy, env, in_abstract_method))
                .collect();
        }

        let ex_class_arg = &arguments[0];
        let handler_arg = &arguments[1];

        // Infer the exception class argument normally
        let ex_class_ty = self.infer_expr(ex_class_arg, hierarchy, env, in_abstract_method);

        // Unwrap parentheses so `on: (Error) do: ([:e | ...])` also gets the
        // contextual block-param typing.
        let ex_class_inner = unwrap_parens(ex_class_arg);
        let handler_inner = unwrap_parens(handler_arg);

        // Extract class name from ClassReference for block param typing
        let exception_class_name = if let Expression::ClassReference { name, .. } = ex_class_inner {
            Some(name.name.clone())
        } else {
            None
        };

        // If handler is a block and we have a class name, type the block param
        let handler_ty = if let (Some(class_name), Expression::Block(block)) =
            (&exception_class_name, handler_inner)
        {
            let param_types = if block.parameters.is_empty() {
                vec![]
            } else {
                // Type the first (and typically only) block parameter as the exception class
                let mut types = vec![InferredType::known(class_name.clone())];
                // Any additional params beyond the first stay Dynamic
                for _ in 1..block.parameters.len() {
                    types.push(InferredType::Dynamic(DynamicReason::UnannotatedParam));
                }
                types
            };
            self.infer_block_with_typed_params(
                block,
                handler_arg.span(),
                &param_types,
                hierarchy,
                env,
                in_abstract_method,
            )
        } else {
            self.infer_expr(handler_arg, hierarchy, env, in_abstract_method)
        };

        vec![ex_class_ty, handler_ty]
    }

    /// BT-2046: Infer argument types for `ifNotNil:` / `ifNil:ifNotNil:` /
    /// `ifNotNil:ifNil:` with non-nil narrowing of the receiver propagated to
    /// the not-nil block's parameter.
    ///
    /// When the receiver is `T | Nil`, the block parameter in
    /// `ifNotNil: [:x | ...]` should be typed `T` (the non-nil branch),
    /// instead of `Dynamic(UnannotatedParam)`. For non-nullable receivers the
    /// parameter is typed as the full receiver type (not a regression from
    /// prior behaviour, which also produced `Dynamic`).
    ///
    /// Nil-branch blocks (`ifNil:`) and blocks with no declared parameter get
    /// the default inference path — solo `ifNil:` (BT-2824) also lands here
    /// (`not_nil_index` is `None` for it) purely to reuse that default path,
    /// which preserves the block's `Block(..., R)` return type.
    fn infer_args_for_if_not_nil(
        &mut self,
        selector_name: &str,
        arguments: &[Expression],
        receiver_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        // Compute the non-nil branch type once. `non_nil_type` strips
        // `UndefinedObject` / `Nil` from a union and returns other types
        // unchanged (matches the `isNil ifFalse:` narrowing — BT-2048).
        let non_nil_ty = Self::non_nil_type(receiver_ty);

        // Positions of the `ifNotNil:` block in the argument list per selector.
        let not_nil_index: Option<usize> = match selector_name {
            "ifNil:ifNotNil:" => Some(1),
            "ifNotNil:" | "ifNotNil:ifNil:" => Some(0),
            _ => None,
        };

        arguments
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                if Some(i) == not_nil_index {
                    self.infer_if_not_nil_block(
                        arg,
                        receiver_ty,
                        &non_nil_ty,
                        hierarchy,
                        env,
                        in_abstract_method,
                    )
                } else {
                    // Preserve block-context inference for the `ifNil:` arm in
                    // `ifNil:ifNotNil:` / `ifNotNil:ifNil:` — a bare
                    // `infer_expr` would drop the `Block(..., R)` return type,
                    // degrading the whole send on statically-known receivers.
                    let inner = unwrap_parens(arg);
                    if let Expression::Block(block) = inner {
                        self.infer_block_with_typed_params(
                            block,
                            arg.span(),
                            &[],
                            hierarchy,
                            env,
                            in_abstract_method,
                        )
                    } else {
                        self.infer_expr(arg, hierarchy, env, in_abstract_method)
                    }
                }
            })
            .collect()
    }

    /// Infer the `ifNotNil:` block, typing its first parameter (if any) as the
    /// non-nil branch of the receiver's type.
    ///
    /// Falls back to the normal expression inference path when the argument
    /// isn't a block literal (e.g. `receiver ifNotNil: aSymbol` is legal but
    /// non-local-inferable here).
    fn infer_if_not_nil_block(
        &mut self,
        arg: &Expression,
        receiver_ty: &InferredType,
        non_nil_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        // Unwrap parens: `ifNotNil: ([:x | ...])` should narrow the same as
        // the unparenthesised form.
        let inner = unwrap_parens(arg);
        let Expression::Block(block) = inner else {
            return self.infer_expr(arg, hierarchy, env, in_abstract_method);
        };

        // Zero-arity `ifNotNil: [ ... ]` — still call the typed-param helper
        // with an empty param list so the returned `Block(..., R)` type
        // carries the body's return type (consistent with
        // `infer_block_with_narrowing`; relevant for BT-2047).
        let param_types: Vec<InferredType> = if block.parameters.is_empty() {
            vec![]
        } else {
            // If the RECEIVER is nil-only (e.g. receiver is a literal `nil`,
            // `UndefinedObject | Nil`), the block is dead code. Check against
            // the original receiver — `non_nil_type` collapses a nil-only
            // union to `Dynamic(Unknown)`, so checking `non_nil_ty` here would
            // miss the case. Leave the param as `Dynamic(UnannotatedParam)`
            // so unreachable bodies still compile without DNU noise.
            let first_param_ty = if Self::is_nil_only(receiver_ty) {
                InferredType::Dynamic(DynamicReason::UnannotatedParam)
            } else {
                non_nil_ty.clone()
            };
            let mut types = Vec::with_capacity(block.parameters.len());
            types.push(first_param_ty);
            // Any additional params beyond the first stay Dynamic. A
            // well-formed `ifNotNil:` block has 0 or 1 parameter (validated by
            // `validate_if_not_nil_block` in codegen), so this is defensive.
            for _ in 1..block.parameters.len() {
                types.push(InferredType::Dynamic(DynamicReason::UnannotatedParam));
            }
            types
        };

        self.infer_block_with_typed_params(
            block,
            arg.span(),
            &param_types,
            hierarchy,
            env,
            in_abstract_method,
        )
    }

    /// BT-2047: Compute the return type of `ifNil:ifNotNil:` /
    /// `ifNotNil:ifNil:` as the union of both branch bodies' return types.
    ///
    /// `arg_types` must be the pair of `Block(..., R)` types produced by
    /// [`Self::infer_args_for_if_not_nil`]; the last type-arg of each Block is
    /// the branch body's inferred return type. A block literal containing a
    /// non-local return (`^`) exits the enclosing method, so its branch
    /// contributes `Never` to the union regardless of the returned value's
    /// type — matching the semantics noted in the issue's AC #3.
    ///
    /// Returns `None` if either arg isn't a well-formed `Block(...)` (e.g. the
    /// caller passed a symbol or bare value instead of a block literal), so
    /// the caller falls back to the generic method-lookup path for those cases.
    fn if_nil_branch_union_ret_ty(
        arguments: &[Expression],
        arg_types: &[InferredType],
    ) -> Option<InferredType> {
        if arg_types.len() < 2 || arguments.len() < 2 {
            return None;
        }
        let branch_ret = |arg: &Expression, ty: &InferredType| -> Option<InferredType> {
            let InferredType::Known {
                class_name,
                type_args,
                ..
            } = ty
            else {
                return None;
            };
            if class_name.as_str() != "Block" {
                return None;
            }
            // Non-local `^` anywhere inside the branch (including nested
            // sub-expressions like `[[^1] value]` or `foo: (^bar)`) exits
            // the method before the expression value is observed — treat
            // the branch as Never so `union_of` skips it.
            if let Expression::Block(block) = unwrap_parens(arg) {
                if block_has_any_return(block) {
                    return Some(InferredType::Never);
                }
            }
            type_args.last().cloned()
        };
        let a = branch_ret(&arguments[0], &arg_types[0])?;
        let b = branch_ret(&arguments[1], &arg_types[1])?;
        Some(InferredType::union_of(&[a, b]))
    }

    /// BT-2824: Compute the return type of a solo `ifNil:` / `ifNotNil:` send
    /// on a `T | Nil` union receiver as the union of the "self" branch
    /// (executed when the nil-check condition doesn't hold) and the block
    /// branch's inferred return type `R`.
    ///
    /// For `ifNil:`, the self branch is the receiver's non-nil type `T`
    /// (`Object>>ifNil:` returns `Self` when not nil) and the block branch is
    /// the nil block's `R`. For `ifNotNil:`, the self branch is `Nil`
    /// (`UndefinedObject>>ifNotNil:` returns `self`) and the block branch is
    /// the not-nil block's `R` (already narrowed to the non-nil receiver type
    /// by `infer_if_not_nil_block`).
    ///
    /// Only fires when `receiver_ty` is actually a `Nil`-containing union — a
    /// plain `Known` receiver (nilable or not) falls through to the
    /// pre-existing dispatch path unchanged, since the "impossible" branch
    /// can't be ruled out generically for those without risking a
    /// false-positive widening (e.g. `NonNilT ifNotNil: [...]` must not gain
    /// a spurious `Nil` member).
    ///
    /// The "self branch" semantics (`Object>>ifNil: -> Self`,
    /// `UndefinedObject>>ifNotNil: -> Nil`) are verified against the actual
    /// resolved stdlib signature rather than assumed, so a future edit to
    /// either method's declared return type in `Object.bt` / `UndefinedObject.bt`
    /// falls back to the generic dispatch path instead of silently going stale.
    ///
    /// Returns `None` when the block argument isn't a well-formed `Block(...)`
    /// type, the receiver doesn't qualify, or the stdlib contract this
    /// function relies on no longer matches, so the caller falls back to the
    /// generic dispatch path for those cases.
    fn if_nil_solo_union_ret_ty(
        selector_name: &str,
        receiver_ty: &InferredType,
        arguments: &[Expression],
        arg_types: &[InferredType],
        hierarchy: &ClassHierarchy,
    ) -> Option<InferredType> {
        let InferredType::Union { members, .. } = receiver_ty else {
            return None;
        };
        let has_nil = members.iter().any(|m| {
            m.as_known().is_some_and(|n| {
                WellKnownClass::from_str(n).is_some_and(WellKnownClass::is_nil_class)
            })
        });
        if !has_nil {
            return None;
        }
        let non_nil_ty = Self::non_nil_type(receiver_ty);
        if matches!(non_nil_ty, InferredType::Dynamic(_)) {
            return None;
        }

        let arg = arguments.first()?;
        let ty = arg_types.first()?;
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = ty
        else {
            return None;
        };
        if class_name.as_str() != "Block" {
            return None;
        }
        let block_ret = if let Expression::Block(block) = unwrap_parens(arg) {
            if block_has_any_return(block) {
                InferredType::Never
            } else {
                type_args.last().cloned()?
            }
        } else {
            type_args.last().cloned()?
        };

        let self_branch = match selector_name {
            "ifNil:" => {
                let ret_ty = hierarchy
                    .find_method(WellKnownClass::Object.as_str(), "ifNil:")?
                    .return_type?;
                if ret_ty.as_str() != "Self" {
                    return None;
                }
                non_nil_ty
            }
            "ifNotNil:" => {
                let ret_ty = hierarchy
                    .find_method(WellKnownClass::UndefinedObject.as_str(), "ifNotNil:")?
                    .return_type?;
                if WellKnownClass::from_str(&ret_ty).is_none_or(|c| !c.is_nil_class()) {
                    return None;
                }
                InferredType::known(WellKnownClass::UndefinedObject.as_str())
            }
            _ => return None,
        };
        Some(InferredType::union_of(&[self_branch, block_ret]))
    }

    /// Infer argument types for `ifTrue:` / `ifFalse:` / `ifTrue:ifFalse:` with
    /// narrowed type environments for block arguments.
    ///
    /// For `ifTrue:`, the true-block gets the narrowed type.
    /// For `ifFalse:`, the false-block gets the complement (non-nil for nil checks).
    /// For `ifTrue:ifFalse:`, both blocks get their respective narrowings.
    #[allow(clippy::too_many_lines)]
    fn infer_args_with_narrowing(
        &mut self,
        arguments: &[Expression],
        selector_name: &str,
        info: &NarrowingInfo,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        let mut arg_types = Vec::new();

        match selector_name {
            "ifTrue:" => {
                // Single argument: narrow in the true branch
                if let Some(arg) = arguments.first() {
                    let ty = self.infer_block_with_narrowing(
                        arg,
                        &info.variable,
                        &info.true_type,
                        hierarchy,
                        env,
                        in_abstract_method,
                    );
                    arg_types.push(ty);
                }
            }
            "ifFalse:" => {
                // Single argument: narrow in the false branch (complement)
                if let Some(arg) = arguments.first() {
                    if let Some(ref false_ty) = info.false_type {
                        // Explicit false type (e.g., Result isOk/isError — BT-1859;
                        // singleton (in)equality complement — BT-2617; or
                        // class = / isKindOf: nominal-class complement — BT-2744)
                        let ty = self.infer_block_with_narrowing(
                            arg,
                            &info.variable,
                            false_ty,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else if info.is_nil_check {
                        // isNil ifFalse: → variable is non-nil
                        let current_ty =
                            Self::resolve_narrowing_variable_type(&info.variable, env, hierarchy);
                        let non_nil = Self::non_nil_type(&current_ty);
                        let ty = self.infer_block_with_narrowing(
                            arg,
                            &info.variable,
                            &non_nil,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else {
                        // respondsTo: ifFalse: → no useful narrowing. (isKindOf:
                        // now populates `false_type` above — BT-2744; class =:=
                        // still leaves it None, since its false branch can't be
                        // narrowed via subtree exclusion.)
                        let ty = self.infer_expr(arg, hierarchy, env, in_abstract_method);
                        arg_types.push(ty);
                    }
                }
            }
            "ifTrue:ifFalse:" => {
                // Two arguments: true block then false block
                if let Some(true_arg) = arguments.first() {
                    let ty = self.infer_block_with_narrowing(
                        true_arg,
                        &info.variable,
                        &info.true_type,
                        hierarchy,
                        env,
                        in_abstract_method,
                    );
                    arg_types.push(ty);
                }
                if let Some(false_arg) = arguments.get(1) {
                    if let Some(ref false_ty) = info.false_type {
                        // Explicit false type (e.g., Result isOk/isError — BT-1859;
                        // singleton (in)equality complement — BT-2617; or
                        // class = / isKindOf: nominal-class complement — BT-2744)
                        let ty = self.infer_block_with_narrowing(
                            false_arg,
                            &info.variable,
                            false_ty,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else if info.is_nil_check {
                        // isNil ifTrue: [...] ifFalse: [block] → non-nil in false block
                        let current_ty =
                            Self::resolve_narrowing_variable_type(&info.variable, env, hierarchy);
                        let non_nil = Self::non_nil_type(&current_ty);
                        let ty = self.infer_block_with_narrowing(
                            false_arg,
                            &info.variable,
                            &non_nil,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                        arg_types.push(ty);
                    } else {
                        // respondsTo: ifTrue: [...] ifFalse: [...] — no useful
                        // narrowing for false block. (isKindOf: now populates
                        // `false_type` above — BT-2744; class =:= still leaves
                        // it None, since its false branch can't be narrowed via
                        // subtree exclusion.)
                        let ty = self.infer_expr(false_arg, hierarchy, env, in_abstract_method);
                        arg_types.push(ty);
                    }
                }
                // Handle any remaining arguments (shouldn't happen, but be safe)
                for arg in arguments.iter().skip(2) {
                    arg_types.push(self.infer_expr(arg, hierarchy, env, in_abstract_method));
                }
            }
            _ => {
                // Fallback: no narrowing
                for arg in arguments {
                    arg_types.push(self.infer_expr(arg, hierarchy, env, in_abstract_method));
                }
            }
        }

        arg_types
    }

    /// Type-check a block expression (or any expression) with a variable narrowed
    /// to a specific type in a child environment.
    ///
    /// BT-2020: Preserves the block body's inferred return type as a `type_arg`
    /// on the returned `Block(..., R)` type. Without this, `ifTrue:ifFalse:`
    /// return types collapsed to `Dynamic` because `infer_method_local_params`
    /// requires the Block argument to carry its return type before it can unify
    /// the method-local `R` type parameter.
    fn infer_block_with_narrowing(
        &mut self,
        arg: &Expression,
        var_key: &EnvKey,
        narrowed_type: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        if let Expression::Block(block) = arg {
            let mut block_env = env.child();
            // BT-2050: narrowing uses the unified refinement API; the layer
            // is block-scoped because the child env is dropped on return.
            block_env.push_refinement(RefinementLayer::block_scope(
                var_key.clone(),
                narrowed_type.clone(),
            ));
            // Block parameters are unannotated in the narrowing context (the
            // selectors we enter here — ifTrue:/ifFalse:/ifTrue:ifFalse: —
            // take zero-arity blocks, but be defensive for any future use).
            let mut block_param_types: Vec<InferredType> =
                Vec::with_capacity(block.parameters.len());
            for param in &block.parameters {
                let param_ty = InferredType::Dynamic(DynamicReason::UnannotatedParam);
                block_env.set_local(param.name.clone(), param_ty.clone());
                block_param_types.push(param_ty);
            }
            let body_ty =
                self.infer_stmts(&block.body, hierarchy, &mut block_env, in_abstract_method);
            // Build `Block(P1, ..., Pn, R)` so downstream generic inference
            // (e.g. `infer_method_local_params` matching `Block(R) -> R`) can
            // recover the block's return type.
            let mut block_type_args = block_param_types;
            block_type_args.push(body_ty);
            let ty = InferredType::Known {
                class_name: "Block".into(),
                type_args: block_type_args,
                provenance: super::TypeProvenance::Inferred(arg.span()),
            };
            self.type_map.insert(arg.span(), ty.clone());
            ty
        } else {
            // Not a block literal — just infer normally
            self.infer_expr(arg, hierarchy, env, in_abstract_method)
        }
    }

    /// Infer argument types, propagating block parameter types from the callee
    /// method's signature when the receiver type is known.
    ///
    /// For example, `List(String)>>sort:` declares `Block(E, E, Boolean)`.
    /// With E=String (from receiver type args), block params get typed as String
    /// instead of `Dynamic(UnannotatedParam)`.
    ///
    /// **BT-2042:** When the receiver is Dynamic (or the method can't be resolved),
    /// block arguments still need their parameters typed — otherwise each unannotated
    /// block param defaults to `Dynamic(UnannotatedParam)`, which fires the
    /// "expression inferred as Dynamic in typed class" warning at every use of the
    /// block param. In a `typed` class this forces users to annotate every block
    /// parameter whose upstream iterable happens to be Dynamic, even though the
    /// root cause is the Dynamic receiver, not the block itself. We propagate
    /// `Dynamic(DynamicReceiver)` into block params in the fallback paths so
    /// downstream uses propagate that reason (which is filtered from the warning),
    /// matching how the send result itself is already classified.
    #[allow(clippy::too_many_arguments)] // class-side flag (BT-2158) added to existing 7 args
    #[allow(clippy::too_many_lines)] // two-phase block-arg inference adds necessary branches
    fn infer_args_with_block_context(
        &mut self,
        arguments: &[Expression],
        receiver_ty: &InferredType,
        selector_name: &str,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
        is_class_side_send: bool,
    ) -> Vec<InferredType> {
        // ADR 0083: a metatype receiver `Meta{C}` is class-side — normalize it
        // to a bare `Known{C}` so the method lookup below resolves `C`'s
        // class-side methods (the class object is unparameterized, so no type
        // args). The `is_class_side_send` flag is already set by the caller.
        let normalized_receiver: InferredType;
        let receiver_ty = if let InferredType::Meta { class_name, .. } = receiver_ty {
            normalized_receiver = InferredType::known(class_name.clone());
            &normalized_receiver
        } else {
            receiver_ty
        };

        // Fast path: receiver must be Known to look up method signatures.
        // For non-Known receivers (Dynamic, Never, etc.), we can't resolve block
        // param types from the signature — but we can still propagate a reason-
        // preserving type into the block params so their uses don't re-fire the
        // "Dynamic in typed class" warning (BT-2042).
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = receiver_ty
        else {
            return self.infer_args_with_dynamic_block_params(
                arguments,
                receiver_ty,
                hierarchy,
                env,
                in_abstract_method,
            );
        };

        // Look up the method to get param types. For class-side sends
        // (`ClassName foo:` or `self foo:` inside a class method), look up the
        // class-side method; otherwise the instance method. BT-2158: without
        // this split, class-side block parameters never get their declared
        // types propagated to the call-site block params.
        let method_lookup = if is_class_side_send {
            hierarchy.find_class_method(class_name, selector_name)
        } else {
            hierarchy.find_method(class_name, selector_name)
        };
        let Some(method) = method_lookup else {
            return self.infer_args_with_dynamic_block_params(
                arguments,
                receiver_ty,
                hierarchy,
                env,
                in_abstract_method,
            );
        };

        // Check if any param type is a Block(...) type
        let has_block_param = method.param_types.iter().any(|pt| {
            pt.as_ref()
                .is_some_and(|t| t.starts_with("Block(") && t.ends_with(')'))
        });
        if !has_block_param {
            return arguments
                .iter()
                .map(|arg| self.infer_expr(arg, hierarchy, env, in_abstract_method))
                .collect();
        }

        // Phase 1: Infer non-block arguments to resolve method-local type params
        let mut arg_types: Vec<InferredType> = Vec::with_capacity(arguments.len());
        for arg in arguments {
            if matches!(arg, Expression::Block(_)) {
                // Placeholder — will be re-inferred in Phase 2
                arg_types.push(InferredType::known("Block"));
            } else {
                arg_types.push(self.infer_expr(arg, hierarchy, env, in_abstract_method));
            }
        }

        // Build substitution maps
        let class_subst = Self::build_inherited_substitution_map(
            hierarchy,
            class_name,
            type_args,
            &method.defined_in,
        );
        let method_subst = Self::infer_method_local_params(
            &method,
            &arg_types,
            &class_subst,
            hierarchy,
            &method.defined_in,
        );

        // Phase 2: Re-infer block arguments with typed params
        for (i, arg) in arguments.iter().enumerate() {
            if let Expression::Block(block) = arg {
                let param_type_str = method
                    .param_types
                    .get(i)
                    .and_then(|pt| pt.as_ref())
                    .filter(|t| t.starts_with("Block(") && t.ends_with(')'));

                if let Some(block_type_str) = param_type_str {
                    // Parse Block(X, Y, Z) → params = [X, Y], return = Z
                    let inner = &block_type_str[6..block_type_str.len() - 1];
                    let type_params = Self::split_type_params(inner);

                    if type_params.len() >= 2 {
                        // All but last are block parameter types, last is return type
                        let block_param_types: Vec<InferredType> = type_params
                            [..type_params.len() - 1]
                            .iter()
                            .map(|p| {
                                Self::resolve_type_param(p, &class_subst, &method_subst, hierarchy)
                            })
                            .collect();

                        arg_types[i] = self.infer_block_with_typed_params(
                            block,
                            arg.span(),
                            &block_param_types,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                    } else {
                        // BT-2020: Block(R) — zero-arity block with a return type param.
                        // Use `infer_block_with_typed_params` with no param types so the
                        // returned Block type_args preserve the body's return type (R).
                        // Without this, the bare `Block` returned by `infer_expr` gives
                        // `infer_method_local_params` nothing to unify `R` against, and
                        // callers see the method's return type as `Dynamic`.
                        arg_types[i] = self.infer_block_with_typed_params(
                            block,
                            arg.span(),
                            &[],
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                    }
                } else {
                    // No Block(...) param type for this position
                    arg_types[i] = self.infer_expr(arg, hierarchy, env, in_abstract_method);
                }
            }
        }

        arg_types
    }

    /// Fallback variant of [`Self::infer_args_with_block_context`] used when the
    /// receiver type isn't `Known` (Dynamic/Never/Union/…) or when the selector
    /// can't be resolved on the receiver.
    ///
    /// Walks each argument via `infer_expr`, **except** for block literals: those
    /// are walked with their parameters pre-bound to `Dynamic(DynamicReceiver)`
    /// so that usages inside the block body inherit a "propagated Dynamic" reason
    /// (which is filtered out of the BT-1914 "Dynamic in typed class" warning).
    /// Without this step, each block param would default to
    /// `Dynamic(UnannotatedParam)`, re-firing the warning at every use of the
    /// block param inside a `typed` class — see BT-2042.
    ///
    /// The chosen reason follows the send's result classification at line
    /// `infer_message_send_with_receiver_ty` fallback (see `Dynamic(DynamicReceiver)`
    /// returns): the send's result is already classified that way for the same
    /// root cause, so block params inherit the same provenance for consistency.
    fn infer_args_with_dynamic_block_params(
        &mut self,
        arguments: &[Expression],
        receiver_ty: &InferredType,
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> Vec<InferredType> {
        // Only propagate DynamicReceiver when the receiver actually is Dynamic.
        // For Never / Union / other shapes, fall back to the plain walk so we
        // don't mask unrelated root causes.
        let propagate_dynamic = matches!(receiver_ty, InferredType::Dynamic(_));

        arguments
            .iter()
            .map(|arg| {
                // Unwrap parens so `foo: ([:x | ...])` gets the same
                // Dynamic(DynamicReceiver) propagation as the unparenthesised
                // `foo: [:x | ...]` form.
                let inner = unwrap_parens(arg);
                if let Expression::Block(block) = inner {
                    if propagate_dynamic {
                        let param_types: Vec<InferredType> = (0..block.parameters.len())
                            .map(|_| InferredType::Dynamic(DynamicReason::DynamicReceiver))
                            .collect();
                        return self.infer_block_with_typed_params(
                            block,
                            arg.span(),
                            &param_types,
                            hierarchy,
                            env,
                            in_abstract_method,
                        );
                    }
                }
                self.infer_expr(arg, hierarchy, env, in_abstract_method)
            })
            .collect()
    }

    /// Infer a block expression with typed parameters resolved from the callee
    /// method's signature.
    fn infer_block_with_typed_params(
        &mut self,
        block: &crate::ast::Block,
        block_span: Span,
        param_types: &[InferredType],
        hierarchy: &ClassHierarchy,
        env: &mut TypeEnv,
        in_abstract_method: bool,
    ) -> InferredType {
        let mut block_env = env.child();
        for (param, ty) in block.parameters.iter().zip(param_types.iter()) {
            block_env.set_local(param.name.clone(), ty.clone());
        }
        // Extra params beyond resolved types stay Dynamic
        for param in block.parameters.iter().skip(param_types.len()) {
            block_env.set_local(
                param.name.clone(),
                InferredType::Dynamic(DynamicReason::UnannotatedParam),
            );
        }
        let body_ty = self.infer_stmts(&block.body, hierarchy, &mut block_env, in_abstract_method);
        // Build Block type with resolved param types + inferred return type
        let mut block_type_args: Vec<InferredType> = param_types.to_vec();
        block_type_args.push(body_ty);
        let ty = InferredType::Known {
            class_name: "Block".into(),
            type_args: block_type_args,
            provenance: super::TypeProvenance::Inferred(block_span),
        };
        self.type_map.insert(block_span, ty.clone());
        ty
    }

    /// Resolve a type parameter string through class-level and method-local
    /// substitution maps. Returns the resolved type, or `Dynamic` if
    /// the parameter cannot be resolved.
    ///
    /// BT-2023(B): Handles nested generics (e.g., `List(E)`) by delegating to
    /// `substitute_return_type_with_self`, which recursively resolves inner type params.
    fn resolve_type_param(
        param: &str,
        class_subst: &HashMap<EcoString, InferredType>,
        method_subst: &HashMap<EcoString, InferredType>,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        let eco: EcoString = EcoString::from(param);
        // Check method-local substitution first (e.g., A from inject:into:)
        if let Some(ty) = method_subst.get(&eco) {
            return ty.clone();
        }
        // Then class-level substitution (e.g., E from List(String))
        if let Some(ty) = class_subst.get(&eco) {
            return ty.clone();
        }
        // If it's a known class name (e.g., Boolean, Integer), return it directly
        if hierarchy.has_class(&eco) {
            return InferredType::known(eco);
        }
        // BT-2023(B): If the param contains nested generics (e.g., `List(E)`),
        // delegate to `substitute_return_type_with_self` which handles recursive resolution.
        if param.contains('(') {
            return Self::substitute_return_type_with_self(param, class_subst, method_subst, None);
        }
        // Unresolved type param — stay Dynamic
        InferredType::Dynamic(DynamicReason::UnannotatedParam)
    }

    /// Check whether a type is *only* the nil type (`UndefinedObject` or
    /// its legacy `Nil` alias). Returns `true` for the bare nil type itself,
    /// or a union whose members are all nil. Used by the `ifNotNil:` block-
    /// param narrowing (BT-2046) to avoid typing the param as `UndefinedObject`
    /// when the non-nil branch is dead code.
    fn is_nil_only(ty: &InferredType) -> bool {
        match ty {
            InferredType::Known { class_name, .. } => {
                WellKnownClass::from_str(class_name).is_some_and(WellKnownClass::is_nil_class)
            }
            InferredType::Union { members, .. } => members.iter().all(|m| {
                m.as_known().is_some_and(|n| {
                    WellKnownClass::from_str(n).is_some_and(WellKnownClass::is_nil_class)
                })
            }),
            _ => false,
        }
    }

    /// Check whether a block's execution cannot fall through to the enclosing
    /// method's next statement (BT-2049, extended in BT-2051).
    ///
    /// A block "diverges" when any of the following hold:
    /// - It contains a non-local return `^expr` (already handled by
    ///   [`block_has_return`]).
    /// - Its body's inferred return type is [`InferredType::Never`] — i.e. the
    ///   last expression is a call to a `-> Never`-returning method such as
    ///   `Object>>error:` or `Object>>notImplemented`.
    /// - Any statement inside the body has — or *contains* as a descendant —
    ///   an expression of inferred type `Never`. This covers both
    ///   `[self error: "…". ^nil]` (trailing `^nil` unreachable) and
    ///   `[logger info: (self error: "…")]` (the diverging call is buried
    ///   in a method-send argument). BT-2051 walks descendants via
    ///   [`Self::expr_contains_never`], symmetric with the `^`-walker
    ///   [`Self::expr_contains_return`].
    ///
    /// The block's inferred type is read from [`TypeChecker::type_map`] as the
    /// final type-arg of its `Block(...)` representation (populated by
    /// [`Self::infer_block_with_narrowing`]). The block must therefore have
    /// been type-checked already; callers run this after the enclosing
    /// expression has been inferred.
    fn block_diverges(&self, block: &crate::ast::Block) -> bool {
        if block_has_return(block) {
            return true;
        }
        // Look up the block's type from the type_map; the last type-arg of the
        // `Block(P1, ..., Pn, R)` representation is the body's return type.
        if let Some(InferredType::Known {
            class_name,
            type_args,
            ..
        }) = self.type_map.get(block.span)
        {
            if WellKnownClass::from_str(class_name) == Some(WellKnownClass::Block) {
                if let Some(body_ty) = type_args.last() {
                    if matches!(body_ty, InferredType::Never) {
                        return true;
                    }
                }
            }
        }
        block
            .body
            .iter()
            .any(|stmt| self.expr_contains_never(&stmt.expression))
    }

    /// Recursively check whether `expr` — or any sub-expression — has
    /// inferred type [`InferredType::Never`] in the type map (BT-2051).
    ///
    /// This is the `Never`-typed companion to
    /// [`super::narrowing::visitors::expr_contains_return`]: both share the
    /// exhaustive [`crate::ast::visitor`] walker so every sub-expression
    /// variant gets covered (BT-2063). A diverging call such as
    /// `self error: "…"` is detected whether it appears as the whole
    /// statement (`[self error: "…"]`), as a receiver, buried in a message
    /// send argument (`[logger info: (self error: "…")]`), inside a
    /// `DestructureAssignment`, `Match`, `MapLiteral`, `ListLiteral`, etc.
    ///
    /// **Nested block literals are opaque**: a block value is *constructed*,
    /// not *executed*, at this position. Guards like
    /// `[callbacks add: [self error: "later"]]` would otherwise be
    /// mis-classified as diverging even though the outer block still falls
    /// through. This is the [`crate::ast::visitor::Visitor`] default.
    fn expr_contains_never(&self, expr: &Expression) -> bool {
        use crate::ast::visitor::{Visitor, walk_expr};

        struct Finder<'a> {
            type_map: &'a crate::semantic_analysis::type_checker::TypeMap,
            found: bool,
        }
        impl<'ast> Visitor<'ast> for Finder<'_> {
            fn visit_expr(&mut self, e: &'ast Expression) {
                if self.found {
                    return;
                }
                if matches!(self.type_map.get(e.span()), Some(InferredType::Never)) {
                    self.found = true;
                    return;
                }
                walk_expr(self, e);
            }
            // `visit_block` default (opaque) preserves the BT-2051 rule:
            // inert block literals must NOT count toward divergence.
        }

        let mut finder = Finder {
            type_map: &self.type_map,
            found: false,
        };
        finder.visit_expr(expr);
        finder.found
    }

    /// Remove `UndefinedObject` (nil) from a union type or convert a known type
    /// to itself if it is non-nil — the `isNil ifFalse:` branch type.
    ///
    /// ADR 0102 §2: for a union receiver this routes through the set-theoretic
    /// `difference` operator with `P = UndefinedObject` (nil's type). `"Nil"` is
    /// subtracted alongside the canonical `UndefinedObject` as a defensive alias
    /// (BT-2016) — `resolve_type_keyword` should canonicalize to
    /// `"UndefinedObject"`, but downstream callers may encounter `"Nil"` from
    /// BEAM metadata or return-type strings.
    ///
    /// Two behaviours are preserved from the pre-operator implementation:
    /// - **Nil-only-union → `Dynamic` softening.** A union whose members are all
    ///   nil would collapse to `Never` under `difference`; instead it softens to
    ///   `Dynamic(Unknown)` so an unreachable `ifNotNil:` body still compiles.
    /// - **Non-union pass-through.** A non-union type is returned unchanged (we
    ///   cannot make a non-union "more non-nil"), rather than subtracting nil —
    ///   which would turn a bare `UndefinedObject` receiver into `Never`.
    pub(super) fn non_nil_type(ty: &InferredType) -> InferredType {
        match ty {
            InferredType::Union { .. } => {
                let provenance = super::TypeProvenance::Inferred(Span::default());
                // `P = UndefinedObject | Nil` (both nil spellings, BT-2016).
                let nil = InferredType::simple_union(&["nil", "Nil"]);
                // `nil`/`Nil` are matched by exact equality, not subclassing,
                // so no hierarchy is needed here.
                let stripped = InferredType::difference(ty, &nil, provenance, None);
                // Nil-only union collapses to `Never`; soften to `Dynamic`.
                if matches!(stripped, InferredType::Never) {
                    InferredType::Dynamic(DynamicReason::Unknown)
                } else {
                    stripped
                }
            }
            // If the variable is not a union, narrowing away nil for a non-union
            // type means it stays the same (we can't make it "more non-nil").
            _ => ty.clone(),
        }
    }

    /// Resolve the current type of a narrowing variable from the environment.
    ///
    /// For locals, this is a simple env lookup. For
    /// [`EnvKey::SelfField`] (BT-2048 / BT-2062) we prefer a previously
    /// pushed narrowing, falling back to the declared state type resolved
    /// through the class hierarchy when none is present.
    fn resolve_narrowing_variable_type(
        var_key: &EnvKey,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
    ) -> InferredType {
        // Regular env lookup first (handles locals and previously-narrowed fields).
        if let Some(ty) = env.get(var_key) {
            return ty;
        }
        // BT-2048 / BT-2062: for `self.<field>` keys, resolve via the class
        // hierarchy using the `self` binding in the env.
        if let EnvKey::SelfField(field_name) = var_key {
            if let Some(InferredType::Known { class_name, .. }) = env.get_local("self") {
                if let Some(field_type) = hierarchy.state_field_type(&class_name, field_name) {
                    return Self::resolve_type_name_string(&field_type);
                }
            }
        }
        InferredType::Dynamic(DynamicReason::Unknown)
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
    fn infer_stmts(
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

            // Early-return narrowing (ADR 0068 Phase 1g, extended in BT-2049):
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

    /// Apply early-return narrowing to the environment after a statement.
    ///
    /// Detects `x isNil ifTrue: [<diverge>]` and, since BT-2825, `x isKindOf:
    /// C ifTrue: [<diverge>]` / `ifFalse: [<diverge>]` / `ifTrue: [<diverge>]
    /// ifFalse: [...]` — if the branch whose test is *not* the one we fall
    /// through cannot fall through (either a non-local return `^` or a
    /// diverging call such as `self error: "..."` whose inferred type is
    /// `Never`), the variable is narrowed for subsequent statements. Covers
    /// both local variables and synthetic `self.field` keys via
    /// [`Self::resolve_narrowing_variable_type`] (BT-2049).
    fn apply_early_return_narrowing(
        &mut self,
        expr: &Expression,
        env: &mut TypeEnv,
        hierarchy: &ClassHierarchy,
    ) {
        // Match: `<receiver> ifTrue: [diverging]`, `<receiver> ifFalse:
        // [diverging]` (BT-2825), or `<receiver> ifTrue: [diverging]
        // ifFalse: [...]` — whichever block diverges, any execution reaching
        // the next statement came through the *other* path, narrowing the
        // variable accordingly.
        let Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } = expr
        else {
            return;
        };
        let is_if_true = parts.len() == 1 && parts[0].keyword == "ifTrue:";
        let is_if_false = parts.len() == 1 && parts[0].keyword == "ifFalse:";
        let is_if_true_if_false =
            parts.len() == 2 && parts[0].keyword == "ifTrue:" && parts[1].keyword == "ifFalse:";
        if !(is_if_true || is_if_false || is_if_true_if_false) {
            return;
        }
        let Some(mut info) = Self::detect_narrowing(receiver) else {
            return;
        };
        if !info.is_nil_check && info.class_test.is_none() {
            return;
        }
        if info.class_test.is_some() {
            // BT-2825: resolve `true_type`/`false_type` the same way the
            // primary `ifTrue:`/`ifFalse:` dispatch does
            // (`refine_class_narrowing`), but through the diagnostic-free
            // `compute_class_narrowing` — `expr` (this exact guard) was
            // already fully type-checked by `infer_stmts` above, so the
            // "comparison can never be true" hint already fired once.
            let current_ty = Self::resolve_narrowing_variable_type(&info.variable, env, hierarchy);
            info = Self::compute_class_narrowing(
                info,
                &current_ty,
                hierarchy,
                self.protocol_registry.as_ref(),
            );
        }

        if is_if_true || is_if_true_if_false {
            // In both shapes, the true block is argument 0.
            let Some(Expression::Block(true_block)) = arguments.first() else {
                return;
            };
            if !self.block_diverges(true_block) {
                return;
            }
            // For `ifTrue:ifFalse:`, execution may reach the next
            // statement through the `ifFalse:` block. If that block
            // reassigns the tested variable (e.g. `[self.user := nil]`
            // or `[x := nil]`), the post-statement narrowing would be
            // unsound — skip it.
            if is_if_true_if_false {
                if let Some(Expression::Block(false_block)) = arguments.get(1) {
                    if block_may_reassign(false_block, &info.variable) {
                        return;
                    }
                }
            }
            let Some(narrowed) = Self::early_return_false_branch_type(&info, env, hierarchy) else {
                return;
            };
            // BT-2050: after this statement, the variable is narrowed.
            // Use method-remainder scope: the refinement outlives the
            // guard send and applies to the rest of the enclosing method
            // body (unlike the block-scoped narrowings pushed inside
            // `infer_block_with_narrowing`).
            env.push_refinement(RefinementLayer::method_remainder(
                info.variable.clone(),
                narrowed,
            ));
        } else if is_if_false {
            // BT-2825: `<receiver> ifFalse: [diverging]` — the sole
            // argument is the false block. If it diverges, execution
            // reaching the next statement proves the guard's test held,
            // so the variable narrows to the *true*-branch type.
            //
            // For `isKindOf:` this is `compute_class_narrowing`'s resolved
            // `true_type` (set above). For plain `isNil` checks, `info` was
            // never routed through `compute_class_narrowing` — `true_type`
            // instead comes straight from `detect_narrowing`'s `isNil` rule,
            // which sets it to `InferredType::known("UndefinedObject")`
            // (see `narrowing/rules/is_nil.rs`).
            let Some(Expression::Block(false_block)) = arguments.first() else {
                return;
            };
            if !self.block_diverges(false_block) {
                return;
            }
            env.push_refinement(RefinementLayer::method_remainder(
                info.variable.clone(),
                info.true_type.clone(),
            ));
        }
    }

    /// The type the tested variable takes in the "complementary" (false)
    /// branch of a narrowing, mirroring `infer_args_with_narrowing`'s
    /// `ifFalse:` arm (BT-2825): an explicit `false_type` (class test /
    /// Result / singleton) if set, else non-nil for `isNil` checks, else
    /// `None` (no useful narrowing — e.g. `class =:=`'s false branch, which
    /// deliberately stays unnarrowed per `ClassTestKind`).
    fn early_return_false_branch_type(
        info: &NarrowingInfo,
        env: &TypeEnv,
        hierarchy: &ClassHierarchy,
    ) -> Option<InferredType> {
        if let Some(ref false_ty) = info.false_type {
            Some(false_ty.clone())
        } else if info.is_nil_check {
            let current_ty = Self::resolve_narrowing_variable_type(&info.variable, env, hierarchy);
            Some(Self::non_nil_type(&current_ty))
        } else {
            None
        }
    }

    /// Build a substitution map from a class's type parameters and concrete type arguments.
    ///
    /// Given a class like `Result(T, E)` and concrete args `[Integer, IOError]`,
    /// builds `{T → Integer, E → IOError}`.
    ///
    /// Returns an empty map if the class has no type params or the args are empty.
    fn build_substitution_map(
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
    /// **References:** ADR 0068 Challenge 4 (BT-1577)
    fn build_inherited_substitution_map(
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
                    SuperclassTypeArg::Concrete { type_name } => {
                        let eco_name: EcoString = type_name.clone();
                        if let Some(resolved) = current_subst.get(&eco_name) {
                            super_args.push(resolved.clone());
                        } else {
                            super_args.push(InferredType::known(eco_name));
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

    /// Substitute type parameters in a return type string using the substitution map.
    ///
    /// Handles simple cases like `T` → `Integer` and generic return types like
    /// `Result(R, E)` where each parameter is individually substituted. Kept
    /// as a thin wrapper over [`substitute_return_type_with_self`] for call
    /// sites and tests that do not thread a receiver class through.
    #[cfg(test)]
    fn substitute_return_type(
        ret_ty: &str,
        subst: &HashMap<EcoString, InferredType>,
        method_local_subst: &HashMap<EcoString, InferredType>,
    ) -> InferredType {
        Self::substitute_return_type_with_self(ret_ty, subst, method_local_subst, None)
    }

    /// Like [`substitute_return_type`], but also substitutes `Self` when it
    /// appears as a (possibly nested) type reference (BT-1986).
    ///
    /// Bare `-> Self` returns are handled specially at the call site to keep
    /// the receiver's type arguments attached. This path is for the nested
    /// case — e.g. `-> Result(Self, Error)` on ADR 0079's
    /// `class named: -> Result(Self, Error)`, where `Self` must resolve to
    /// the static receiver class (`Counter` in `Counter named: #c`).
    ///
    /// BT-1992: `self_type` carries the full receiver `InferredType` (including
    /// type arguments for parameterised receivers like `Box(Integer)`), so that
    /// `Self` inside a generic return like `Result(Self, Error)` resolves to
    /// `Box(Integer)` rather than bare `Box`.
    ///
    /// `self_type = None` preserves the previous behaviour (nested `Self`
    /// passes through as a class-named `Known("Self")`, which is wrong but
    /// matches historic behaviour for call sites that don't know the
    /// receiver).
    pub(super) fn substitute_return_type_with_self(
        ret_ty: &str,
        subst: &HashMap<EcoString, InferredType>,
        method_local_subst: &HashMap<EcoString, InferredType>,
        self_type: Option<&InferredType>,
    ) -> InferredType {
        let ret_eco: EcoString = ret_ty.into();

        // Check method-local params first (e.g., R in map:)
        if let Some(resolved) = method_local_subst.get(&ret_eco) {
            return resolved.clone();
        }

        // Check class-level params (e.g., T, E)
        if let Some(resolved) = subst.get(&ret_eco) {
            return resolved.clone();
        }

        // BT-1986 / BT-1992: `Self` as a nested type reference resolves to
        // the full receiver type (including type args for parameterised
        // receivers). The top-level bare-`Self` case is still handled by
        // the caller; this branch only fires when `Self` appears inside a
        // generic or union.
        if ret_ty == "Self" {
            if let Some(ty) = self_type {
                return ty.clone();
            }
        }

        // BT-1836: Handle union return types like "E | Nil" — substitute each member.
        // Use paren-aware splitting so `Result(Self | Nil, Error)` is NOT split at
        // the inner `|`. Only treat as a union if there are >1 top-level members;
        // otherwise fall through to the generic parsing path below.
        if ret_ty.contains('|') {
            let parts = Self::split_union_respecting_parens(ret_ty);
            if parts.len() > 1 {
                let members: Vec<InferredType> = parts
                    .into_iter()
                    .map(|m| {
                        Self::substitute_return_type_with_self(
                            m,
                            subst,
                            method_local_subst,
                            self_type,
                        )
                    })
                    .collect();
                return InferredType::Union {
                    members,
                    provenance: super::TypeProvenance::Substituted(Span::default()),
                };
            }
        }

        // Check for generic return type like "Result(R, E)".
        // BT-2025: Parenthesis-aware split via the centralised helper.
        let (base, args_slice) = super::type_resolver::split_generic_base(ret_ty);
        if let Some(inner) = args_slice {
            let params = Self::split_type_params(inner);
            let mut resolved_args = Vec::new();
            for p in &params {
                let p_eco: EcoString = (*p).into();
                if let Some(resolved) = method_local_subst.get(&p_eco) {
                    resolved_args.push(resolved.clone());
                } else if let Some(resolved) = subst.get(&p_eco) {
                    resolved_args.push(resolved.clone());
                } else {
                    // Recursively substitute nested generics (and nested Self).
                    resolved_args.push(Self::substitute_return_type_with_self(
                        p,
                        subst,
                        method_local_subst,
                        self_type,
                    ));
                }
            }
            return InferredType::Known {
                class_name: base.into(),
                type_args: resolved_args,
                provenance: super::TypeProvenance::Substituted(Span::default()),
            };
        }

        // BT-1834: Unresolved bare type param (single uppercase letter) → Dynamic
        if super::is_generic_type_param(&ret_eco) {
            return InferredType::Dynamic(DynamicReason::Unknown);
        }

        // Not a type param — return as-is
        InferredType::known(ret_eco)
    }

    /// Split a comma-separated list of type parameters, respecting nested parentheses.
    ///
    /// `"T, E"` → `["T", "E"]`
    /// `"GenResult(A, B), E"` → `["GenResult(A, B)", "E"]`
    pub(super) fn split_type_params(s: &str) -> Vec<&str> {
        let mut result = Vec::new();
        let mut depth = 0;
        let mut start = 0;
        for (i, c) in s.char_indices() {
            match c {
                '(' => depth += 1,
                ')' => depth -= 1,
                ',' if depth == 0 => {
                    result.push(s[start..i].trim());
                    start = i + 1;
                }
                _ => {}
            }
        }
        let last = s[start..].trim();
        if !last.is_empty() {
            result.push(last);
        }
        result
    }

    /// Split a type string on `|` while respecting parenthesis nesting.
    ///
    /// This ensures `Result(String | Integer, Error)` is NOT split at the inner `|`,
    /// but `String | nil` IS split into `["String", "nil"]`.
    pub(super) fn split_union_respecting_parens(s: &str) -> Vec<&str> {
        let mut result = Vec::new();
        let mut depth = 0i32;
        let mut start = 0;
        for (i, c) in s.char_indices() {
            match c {
                '(' => depth += 1,
                ')' => depth = depth.saturating_sub(1),
                '|' if depth == 0 => {
                    result.push(s[start..i].trim());
                    start = i + 1;
                }
                _ => {}
            }
        }
        let last = s[start..].trim();
        if !last.is_empty() {
            result.push(last);
        }
        result
    }

    /// BT-1986: Does the return type mention `Self` *nested* inside a
    /// generic or union, as opposed to a bare `Self` / `Self class` /
    /// `Self?` at the top level?
    ///
    /// The bare-top-level cases are handled by dedicated code paths
    /// that preserve the receiver's type args; this predicate identifies
    /// the "needs recursive substitution" case.
    fn return_type_mentions_nested_self(ret_ty: &EcoString) -> bool {
        ret_ty.contains("Self") && ret_ty.as_str() != "Self" && ret_ty.as_str() != "Self class"
    }

    /// Infer method-local type parameters from call-site arguments.
    ///
    /// Extracts type params from ANY parametric parameter type — e.g., `Block(T, R)`,
    /// `Result(T, E)`, `Array(T)`, `Dictionary(K, V)`. For each declared type parameter
    /// in the param type, if it is method-local (not a class-level type param and not a
    /// known class name), it is matched positionally against the argument's actual `type_args`.
    fn infer_method_local_params(
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
            let Some(arg_ty) = arg_types.get(i) else {
                continue;
            };

            // BT-1834: Handle plain (non-parametric) type param parameters.
            // e.g., `inject: initial :: A` — if A is method-local, map it to the arg type.
            //
            // For a bare `A` parameter, `A` represents the *whole* argument type
            // including any nilability. Don't strip nil here — that would turn
            // `identity: x :: A -> A` called with `String | Nil` into `A = String`
            // and lose the nullability in the inferred return. Nil-stripping is
            // only safe for the outer-generic path below (`List(T)` etc.) where
            // the union shape doesn't match the param shape anyway.
            if super::is_generic_type_param(param_type) {
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
            // BT-2025: Parenthesis-aware split via the centralised helper.
            let (declared_base, declared_args_slice) =
                super::type_resolver::split_generic_base(param_type);
            if let Some(inner) = declared_args_slice {
                let declared_params = Self::split_type_params(inner);

                // BT-2023(A): Normalise the arg type — if it's a nullable union
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
                            if super::is_generic_type_param(&decl_eco)
                                && !class_type_params.contains(&decl_eco)
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
    /// types over Dynamic.
    ///
    /// BT-2039: When the same type parameter appears in multiple argument
    /// positions (e.g. `Block(R) Block(R) -> R` in `ifTrue:ifFalse:`), a
    /// last-wins `insert` could collapse a Known return type to
    /// `Dynamic(UntypedFfi)` whenever one branch was an untyped FFI call.
    /// Preserve the Known binding instead so the method's declared return type
    /// survives the join.
    fn merge_method_local_binding(
        method_subst: &mut HashMap<EcoString, InferredType>,
        key: EcoString,
        new_ty: InferredType,
    ) {
        match method_subst.get(&key) {
            Some(InferredType::Known { .. } | InferredType::Union { .. })
                if matches!(new_ty, InferredType::Dynamic(_)) =>
            {
                // Keep the existing Known/Union — Dynamic loses.
            }
            _ => {
                method_subst.insert(key, new_ty);
            }
        }
    }

    /// Infer the type of a literal value.
    pub(super) fn infer_literal(lit: &Literal) -> InferredType {
        match lit {
            Literal::Integer(_) => InferredType::known("Integer"),
            Literal::Float(_) => InferredType::known("Float"),
            Literal::String(_) => InferredType::known("String"),
            Literal::Symbol(name) => InferredType::known(eco_format!("#{name}")),
            Literal::Character(_) => InferredType::known("Character"),
            Literal::List(_) => InferredType::known("List"),
        }
    }
}

/// Class-protocol selectors that must NOT be intercepted as FFI module lookups.
///
/// These are handled by `beamtalk_object_class:class_send/3` at runtime.
/// This list mirrors `CLASS_PROTOCOL_SELECTORS` in `dispatch_codegen.rs`
/// to keep codegen and type-checker behaviour consistent (BT-1880).
fn is_class_protocol_selector(selector: &str) -> bool {
    matches!(
        selector,
        "new"
            | "spawn"
            | "class"
            | "methods"
            | "superclass"
            | "subclasses"
            | "allSubclasses"
            | "class_name"
            | "module_name"
            | "printString"
    )
}

/// Returns `true` for the equality / identity comparison binary operators —
/// the universal value/identity comparisons every object (including a class
/// object) supports via `Object` / `ProtoObject`.
///
/// These mirror the equality operators in the parser's binding-power table
/// (`==`, `/=`, `=:=`, `=/=` at precedence 10). They are the same comparison
/// forms `x class =:= Foo` narrowing recognises
/// (`narrowing/rules/class_eq.rs`). All other binary selectors are NOT
/// comparisons and must fall through to normal class-side / tower lookup.
fn is_equality_comparison_op(op: &str) -> bool {
    matches!(op, "==" | "=:=" | "/=" | "=/=")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{
        Block, BlockParameter, ExpectCategory, Expression, ExpressionStatement, Identifier,
        KeywordPart, Literal, MessageSelector, ParameterDefinition, TypeAnnotation,
    };
    use crate::semantic_analysis::TypeProvenance;
    use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, MethodInfo};
    use crate::source_analysis::Span;
    use ecow::EcoString;
    use std::collections::HashMap;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: span(),
        }
    }

    fn var(name: &str) -> Expression {
        Expression::Identifier(ident(name))
    }

    fn class_ref(name: &str) -> Expression {
        Expression::ClassReference {
            name: ident(name),
            package: None,
            span: span(),
        }
    }

    fn int_lit(n: i64) -> Expression {
        Expression::Literal(Literal::Integer(n), span())
    }

    fn str_lit(s: &str) -> Expression {
        Expression::Literal(Literal::String(s.into()), span())
    }

    // ---- BT-2254: literal-index tuple `at:` ----

    fn tuple_args(names: &[&str]) -> Vec<InferredType> {
        names.iter().map(|n| InferredType::known(*n)).collect()
    }

    #[test]
    fn literal_index_tuple_at_in_range() {
        let args = tuple_args(&["Symbol", "Integer", "Symbol"]);
        // `at: 1` → first element type
        assert_eq!(
            TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[int_lit(1)]),
            Some(InferredType::known("Symbol"))
        );
        // `at: 2` → second element type
        assert_eq!(
            TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[int_lit(2)]),
            Some(InferredType::known("Integer"))
        );
    }

    #[test]
    fn literal_index_tuple_at_out_of_range_is_dynamic() {
        let args = tuple_args(&["Symbol", "Integer"]);
        // Index 3 is out of range (only 2 elements) → None (falls back to Dynamic)
        assert_eq!(
            TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[int_lit(3)]),
            None
        );
        // Index 0 is out of range (1-based) → None
        assert_eq!(
            TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[int_lit(0)]),
            None
        );
    }

    #[test]
    fn literal_index_tuple_at_non_literal_is_dynamic() {
        let args = tuple_args(&["Symbol", "Integer"]);
        // Non-literal index (a variable) → None
        assert_eq!(
            TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &args, &[var("i")]),
            None
        );
    }

    #[test]
    fn literal_index_tuple_at_bare_tuple_is_dynamic() {
        // No positional element types (bare `Tuple`) → None
        assert_eq!(
            TypeChecker::infer_literal_index_tuple_at("Tuple", "at:", &[], &[int_lit(1)]),
            None
        );
    }

    #[test]
    fn literal_index_tuple_at_non_tuple_receiver_ignored() {
        let args = tuple_args(&["Integer"]);
        // Not a Tuple receiver → None (List uses its own at: handling)
        assert_eq!(
            TypeChecker::infer_literal_index_tuple_at("List", "at:", &args, &[int_lit(1)]),
            None
        );
    }

    // ---- infer_literal ----

    #[test]
    fn infer_literal_integer() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Integer(42)),
            InferredType::known("Integer")
        );
    }

    #[test]
    fn infer_literal_float() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Float(2.5)),
            InferredType::known("Float")
        );
    }

    #[test]
    fn infer_literal_string() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::String("hello".into())),
            InferredType::known("String")
        );
    }

    #[test]
    fn infer_literal_symbol() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Symbol("ok".into())),
            InferredType::known("#ok")
        );
    }

    #[test]
    fn infer_literal_character() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::Character('x')),
            InferredType::known("Character")
        );
    }

    #[test]
    fn infer_literal_list() {
        assert_eq!(
            TypeChecker::infer_literal(&Literal::List(vec![])),
            InferredType::known("List")
        );
    }

    // ---- resolve_type_annotation ----

    #[test]
    fn resolve_simple_type_annotation() {
        let ann = TypeAnnotation::Simple(ident("Integer"));
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("Integer")
        );
    }

    #[test]
    fn resolve_nil_keyword_annotation() {
        let ann = TypeAnnotation::Simple(ident("nil"));
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("UndefinedObject")
        );
    }

    #[test]
    fn resolve_false_keyword_annotation() {
        let ann = TypeAnnotation::Simple(ident("false"));
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("False")
        );
    }

    #[test]
    fn resolve_true_keyword_annotation() {
        let ann = TypeAnnotation::Simple(ident("true"));
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("True")
        );
    }

    #[test]
    fn resolve_generic_type_annotation() {
        let ann = TypeAnnotation::Generic {
            base: ident("Result"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Simple(ident("String")),
            ],
            span: span(),
        };
        let result = TypeChecker::resolve_type_annotation(&ann);
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                assert_eq!(type_args[0], InferredType::known("Integer"));
                assert_eq!(type_args[1], InferredType::known("String"));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    #[test]
    fn resolve_union_type_annotation() {
        let ann = TypeAnnotation::Union {
            types: vec![
                TypeAnnotation::Simple(ident("String")),
                TypeAnnotation::Simple(ident("nil")),
            ],
            span: span(),
        };
        let result = TypeChecker::resolve_type_annotation(&ann);
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert!(members.contains(&InferredType::known("String")));
                assert!(members.contains(&InferredType::known("UndefinedObject")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    #[test]
    fn resolve_false_or_type_annotation() {
        let ann = TypeAnnotation::FalseOr {
            inner: Box::new(TypeAnnotation::Simple(ident("Integer"))),
            span: span(),
        };
        let result = TypeChecker::resolve_type_annotation(&ann);
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert!(members.contains(&InferredType::known("Integer")));
                assert!(members.contains(&InferredType::known("False")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    #[test]
    fn resolve_self_type_annotation() {
        let ann = TypeAnnotation::SelfType { span: span() };
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::Dynamic(DynamicReason::Unknown)
        );
    }

    #[test]
    fn resolve_self_class_type_annotation() {
        let ann = TypeAnnotation::SelfClass { span: span() };
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::Dynamic(DynamicReason::Unknown)
        );
    }

    #[test]
    fn resolve_singleton_type_annotation() {
        let ann = TypeAnnotation::Singleton {
            name: "north".into(),
            span: span(),
        };
        assert_eq!(
            TypeChecker::resolve_type_annotation(&ann),
            InferredType::known("#north")
        );
    }

    // ---- resolve_type_name_string ----

    #[test]
    fn resolve_type_name_string_simple() {
        assert_eq!(
            TypeChecker::resolve_type_name_string(&"Integer".into()),
            InferredType::known("Integer")
        );
    }

    #[test]
    fn resolve_type_name_string_nil_keyword() {
        assert_eq!(
            TypeChecker::resolve_type_name_string(&"nil".into()),
            InferredType::known("UndefinedObject")
        );
    }

    #[test]
    fn resolve_type_name_string_union() {
        let result = TypeChecker::resolve_type_name_string(&"String | nil".into());
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert!(members.contains(&InferredType::known("String")));
                assert!(members.contains(&InferredType::known("UndefinedObject")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    #[test]
    fn resolve_type_name_string_three_way_union() {
        let result = TypeChecker::resolve_type_name_string(&"Integer | String | nil".into());
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 3);
                assert!(members.contains(&InferredType::known("Integer")));
                assert!(members.contains(&InferredType::known("String")));
                assert!(members.contains(&InferredType::known("UndefinedObject")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    // ---- detect_narrowing ----

    #[test]
    fn detect_narrowing_is_nil() {
        // x isNil
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("isNil".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect isNil");
        assert_eq!(info.variable, EnvKey::local("x"));
        assert_eq!(info.true_type, InferredType::known("UndefinedObject"));
        assert!(info.is_nil_check);
        assert!(info.responded_selector.is_none());
    }

    #[test]
    fn detect_narrowing_is_kind_of() {
        // x isKindOf: Integer
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Keyword(vec![KeywordPart {
                keyword: "isKindOf:".into(),
                span: span(),
            }]),
            arguments: vec![class_ref("Integer")],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect isKindOf:");
        assert_eq!(info.variable, EnvKey::local("x"));
        // ADR 0102 §2 group 2: `detect` leaves `true_type` provisional and
        // records the tested class in `class_test`; `refine_class_narrowing`
        // resolves the actual narrowed type later.
        assert_eq!(
            info.true_type,
            InferredType::Dynamic(DynamicReason::Unknown)
        );
        let class_test = info.class_test.expect("should record class_test");
        assert_eq!(class_test.class_name, "Integer");
        assert_eq!(class_test.kind, ClassTestKind::KindOf);
        assert!(!info.is_nil_check);
    }

    #[test]
    fn detect_narrowing_class_identity_eq() {
        // x class =:= Float
        let class_send = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("class".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let expr = Expression::MessageSend {
            receiver: Box::new(class_send),
            selector: MessageSelector::Binary("=:=".into()),
            arguments: vec![class_ref("Float")],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect class =:=");
        assert_eq!(info.variable, EnvKey::local("x"));
        assert_eq!(
            info.true_type,
            InferredType::Dynamic(DynamicReason::Unknown)
        );
        let class_test = info.class_test.expect("should record class_test");
        assert_eq!(class_test.class_name, "Float");
        assert_eq!(class_test.kind, ClassTestKind::Exact);
    }

    #[test]
    fn detect_narrowing_responds_to() {
        // x respondsTo: #doSomething
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Keyword(vec![KeywordPart {
                keyword: "respondsTo:".into(),
                span: span(),
            }]),
            arguments: vec![Expression::Literal(
                Literal::Symbol("doSomething".into()),
                span(),
            )],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect respondsTo:");
        assert_eq!(info.variable, EnvKey::local("x"));
        assert_eq!(
            info.true_type,
            InferredType::Dynamic(DynamicReason::Unknown)
        );
        assert!(!info.is_nil_check);
        assert_eq!(info.responded_selector.as_deref(), Some("doSomething"));
    }

    #[test]
    fn detect_narrowing_responds_to_non_symbol() {
        // x respondsTo: someVar (not a symbol literal — should not match)
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Keyword(vec![KeywordPart {
                keyword: "respondsTo:".into(),
                span: span(),
            }]),
            arguments: vec![var("someVar")],
            is_cast: false,
            span: span(),
        };
        assert!(TypeChecker::detect_narrowing(&expr).is_none());
    }

    #[test]
    fn detect_narrowing_no_match() {
        // x size (not a type-testing pattern)
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("size".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        assert!(TypeChecker::detect_narrowing(&expr).is_none());
    }

    #[test]
    fn detect_narrowing_parenthesized_class_eq() {
        // (x class) =:= Integer
        let class_send = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("class".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let parens = Expression::Parenthesized {
            expression: Box::new(class_send),
            span: span(),
        };
        let expr = Expression::MessageSend {
            receiver: Box::new(parens),
            selector: MessageSelector::Binary("=:=".into()),
            arguments: vec![class_ref("Integer")],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect (x class) =:= Type");
        assert_eq!(info.variable, EnvKey::local("x"));
        assert_eq!(
            info.true_type,
            InferredType::Dynamic(DynamicReason::Unknown)
        );
        let class_test = info.class_test.expect("should record class_test");
        assert_eq!(class_test.class_name, "Integer");
        assert_eq!(class_test.kind, ClassTestKind::Exact);
    }

    // ---- detect_narrowing: isOk / ok / isError (BT-1859) ----

    #[test]
    fn detect_narrowing_is_ok() {
        // x isOk
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("isOk".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect isOk");
        assert_eq!(info.variable, EnvKey::local("x"));
        assert!(info.is_result_ok_check);
        assert!(!info.is_result_error_check);
        assert!(!info.is_nil_check);
    }

    #[test]
    fn detect_narrowing_ok() {
        // x ok
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("ok".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect ok");
        assert_eq!(info.variable, EnvKey::local("x"));
        assert!(info.is_result_ok_check);
        assert!(!info.is_result_error_check);
    }

    #[test]
    fn detect_narrowing_is_error() {
        // x isError
        let expr = Expression::MessageSend {
            receiver: Box::new(var("x")),
            selector: MessageSelector::Unary("isError".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect isError");
        assert_eq!(info.variable, EnvKey::local("x"));
        assert!(!info.is_result_ok_check);
        assert!(info.is_result_error_check);
        assert!(!info.is_nil_check);
    }

    #[test]
    fn refine_result_narrowing_with_result_type() {
        // When the variable has type Result(String, Error), refine should set
        // true_type and false_type to the full Result type.
        let mut env = TypeEnv::new();
        let result_ty = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![InferredType::known("String"), InferredType::known("Error")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        env.set_local("r", result_ty.clone());

        let info = NarrowingInfo {
            variable: EnvKey::local("r"),
            true_type: InferredType::Dynamic(DynamicReason::Unknown),
            false_type: None,
            is_nil_check: false,
            is_result_ok_check: true,
            is_result_error_check: false,
            responded_selector: None,
            singleton_eq: None,
            class_test: None,
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let refined = TypeChecker::refine_result_narrowing(info, &env, &hierarchy);
        assert_eq!(refined.true_type, result_ty);
        assert_eq!(refined.false_type, Some(result_ty));
        assert!(refined.is_result_ok_check);
    }

    #[test]
    fn refine_result_narrowing_non_result_disables() {
        // When the variable is not a Result, the result flags are cleared.
        let mut env = TypeEnv::new();
        env.set_local("x", InferredType::known("Integer"));

        let info = NarrowingInfo {
            variable: EnvKey::local("x"),
            true_type: InferredType::Dynamic(DynamicReason::Unknown),
            false_type: None,
            is_nil_check: false,
            is_result_ok_check: true,
            is_result_error_check: false,
            responded_selector: None,
            singleton_eq: None,
            class_test: None,
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let refined = TypeChecker::refine_result_narrowing(info, &env, &hierarchy);
        assert!(!refined.is_result_ok_check);
        assert!(!refined.is_result_error_check);
        assert!(refined.false_type.is_none());
        // true_type should be preserved as the current type, not Dynamic
        assert_eq!(refined.true_type, InferredType::known("Integer"));
    }

    // ---- non_nil_type ----

    #[test]
    fn non_nil_type_removes_nil_from_union() {
        let ty = InferredType::simple_union(&["String", "nil"]);
        let result = TypeChecker::non_nil_type(&ty);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn non_nil_type_preserves_non_nil_union() {
        let ty = InferredType::simple_union(&["String", "Integer"]);
        let result = TypeChecker::non_nil_type(&ty);
        assert_eq!(result, ty);
    }

    #[test]
    fn non_nil_type_all_nil_becomes_dynamic() {
        // union_of with single member returns the member itself, so build manually
        let ty = InferredType::Union {
            members: vec![InferredType::known("UndefinedObject")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let result = TypeChecker::non_nil_type(&ty);
        assert_eq!(result, InferredType::Dynamic(DynamicReason::Unknown));
    }

    #[test]
    fn non_nil_type_known_type_unchanged() {
        let ty = InferredType::known("Integer");
        let result = TypeChecker::non_nil_type(&ty);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn non_nil_type_dynamic_unchanged() {
        let result = TypeChecker::non_nil_type(&InferredType::Dynamic(DynamicReason::Unknown));
        assert_eq!(result, InferredType::Dynamic(DynamicReason::Unknown));
    }

    #[test]
    fn non_nil_type_three_member_union() {
        let ty = InferredType::simple_union(&["String", "Integer", "nil"]);
        let result = TypeChecker::non_nil_type(&ty);
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert!(members.contains(&InferredType::known("String")));
                assert!(members.contains(&InferredType::known("Integer")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    // ---- extract_variable_name ----

    #[test]
    fn extract_variable_name_from_ident() {
        let expr = var("foo");
        assert_eq!(extract_variable_name(&expr), Some(EnvKey::local("foo")));
    }

    #[test]
    fn extract_variable_name_from_parenthesized() {
        let expr = Expression::Parenthesized {
            expression: Box::new(var("bar")),
            span: span(),
        };
        assert_eq!(extract_variable_name(&expr), Some(EnvKey::local("bar")));
    }

    #[test]
    fn extract_variable_name_from_non_ident() {
        let expr = int_lit(42);
        assert!(extract_variable_name(&expr).is_none());
    }

    #[test]
    fn extract_variable_name_from_self_field() {
        // BT-2048 / BT-2062: self.supervisor → EnvKey::SelfField("supervisor")
        let expr = Expression::FieldAccess {
            receiver: Box::new(var("self")),
            field: ident("supervisor"),
            span: span(),
        };
        assert_eq!(
            extract_variable_name(&expr),
            Some(EnvKey::self_field("supervisor")),
        );
    }

    #[test]
    fn extract_variable_name_from_non_self_field() {
        // other.field → None (only self.field is supported)
        let expr = Expression::FieldAccess {
            receiver: Box::new(var("other")),
            field: ident("value"),
            span: span(),
        };
        assert!(extract_variable_name(&expr).is_none());
    }

    // ---- detect_narrowing: self.field isNil (BT-2048) ----

    #[test]
    fn detect_narrowing_self_field_is_nil() {
        // self.supervisor isNil
        let field_access = Expression::FieldAccess {
            receiver: Box::new(var("self")),
            field: ident("supervisor"),
            span: span(),
        };
        let expr = Expression::MessageSend {
            receiver: Box::new(field_access),
            selector: MessageSelector::Unary("isNil".into()),
            arguments: vec![],
            is_cast: false,
            span: span(),
        };
        let info = TypeChecker::detect_narrowing(&expr).expect("should detect self.field isNil");
        assert_eq!(info.variable, EnvKey::self_field("supervisor"));
        assert_eq!(info.true_type, InferredType::known("UndefinedObject"));
        assert!(info.is_nil_check);
    }

    // ---- block_has_return ----

    #[test]
    fn block_has_return_true() {
        let block = Block::new(
            vec![],
            vec![ExpressionStatement::bare(Expression::Return {
                value: Box::new(int_lit(1)),
                span: span(),
            })],
            span(),
        );
        assert!(block_has_return(&block));
    }

    #[test]
    fn block_has_return_false() {
        let block = Block::new(vec![], vec![ExpressionStatement::bare(int_lit(42))], span());
        assert!(!block_has_return(&block));
    }

    #[test]
    fn block_has_return_empty() {
        let block = Block::new(vec![], vec![], span());
        assert!(!block_has_return(&block));
    }

    // ---- split_type_params ----

    #[test]
    fn split_type_params_simple() {
        let result = TypeChecker::split_type_params("T, E");
        assert_eq!(result, vec!["T", "E"]);
    }

    #[test]
    fn split_type_params_single() {
        let result = TypeChecker::split_type_params("Integer");
        assert_eq!(result, vec!["Integer"]);
    }

    #[test]
    fn split_type_params_nested() {
        let result = TypeChecker::split_type_params("GenResult(A, B), E");
        assert_eq!(result, vec!["GenResult(A, B)", "E"]);
    }

    #[test]
    fn split_type_params_empty() {
        let result = TypeChecker::split_type_params("");
        assert!(result.is_empty());
    }

    #[test]
    fn split_type_params_deeply_nested() {
        let result = TypeChecker::split_type_params("Outer(Inner(A, B), C), D");
        assert_eq!(result, vec!["Outer(Inner(A, B), C)", "D"]);
    }

    // ---- substitute_return_type ----

    #[test]
    fn substitute_direct_param() {
        let mut subst = HashMap::new();
        subst.insert(EcoString::from("T"), InferredType::known("Integer"));
        let result = TypeChecker::substitute_return_type("T", &subst, &HashMap::new());
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn substitute_method_local_param() {
        let mut method_subst = HashMap::new();
        method_subst.insert(EcoString::from("R"), InferredType::known("String"));
        let result = TypeChecker::substitute_return_type("R", &HashMap::new(), &method_subst);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn substitute_method_local_takes_priority() {
        let mut subst = HashMap::new();
        subst.insert(EcoString::from("R"), InferredType::known("Integer"));
        let mut method_subst = HashMap::new();
        method_subst.insert(EcoString::from("R"), InferredType::known("String"));
        let result = TypeChecker::substitute_return_type("R", &subst, &method_subst);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn substitute_generic_return_type() {
        let mut subst = HashMap::new();
        subst.insert(EcoString::from("T"), InferredType::known("Integer"));
        subst.insert(EcoString::from("E"), InferredType::known("IOError"));
        let result = TypeChecker::substitute_return_type("Result(T, E)", &subst, &HashMap::new());
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                assert_eq!(type_args[0], InferredType::known("Integer"));
                assert_eq!(type_args[1], InferredType::known("IOError"));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    #[test]
    fn substitute_no_match_passes_through() {
        let result =
            TypeChecker::substitute_return_type("String", &HashMap::new(), &HashMap::new());
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn substitute_generic_base_extracted() {
        // When return type is "Array(R)" and R is not in subst, base "Array" is still extracted
        // BT-1834: Unresolved type param R falls back to Dynamic instead of Known("R")
        let result =
            TypeChecker::substitute_return_type("Array(R)", &HashMap::new(), &HashMap::new());
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Array");
                assert_eq!(type_args.len(), 1);
                // R not in subst → Dynamic (BT-1834)
                assert_eq!(type_args[0], InferredType::Dynamic(DynamicReason::Unknown));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    // ---- substitute_return_type_with_self (BT-1992) ----

    #[test]
    fn substitute_self_in_generic_uses_full_receiver_type() {
        // BT-1992: `Result(Self, Error)` on a parameterised receiver `Box(Integer)`
        // should produce `Result(Box(Integer), Error)`, not `Result(Box, Error)`.
        let receiver_ty = InferredType::Known {
            class_name: EcoString::from("Box"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Inferred(Span::default()),
        };
        let result = TypeChecker::substitute_return_type_with_self(
            "Result(Self, Error)",
            &HashMap::new(),
            &HashMap::new(),
            Some(&receiver_ty),
        );
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                // Self should resolve to full Box(Integer), not bare Box
                assert_eq!(type_args[0], receiver_ty);
                assert_eq!(type_args[1], InferredType::known("Error"));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    #[test]
    fn substitute_self_in_generic_non_parameterised_receiver() {
        // Non-parameterised receiver: `Result(Self, Error)` on `Counter`
        // should produce `Result(Counter, Error)`.
        let receiver_ty = InferredType::known("Counter");
        let result = TypeChecker::substitute_return_type_with_self(
            "Result(Self, Error)",
            &HashMap::new(),
            &HashMap::new(),
            Some(&receiver_ty),
        );
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                assert_eq!(type_args[0], InferredType::known("Counter"));
                assert_eq!(type_args[1], InferredType::known("Error"));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    #[test]
    fn substitute_self_in_union_uses_full_receiver_type() {
        // BT-1992: `Self | Error` on `Box(Integer)` should produce
        // `Box(Integer) | Error`.
        let receiver_ty = InferredType::Known {
            class_name: EcoString::from("Box"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Inferred(Span::default()),
        };
        let result = TypeChecker::substitute_return_type_with_self(
            "Self | Error",
            &HashMap::new(),
            &HashMap::new(),
            Some(&receiver_ty),
        );
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert!(members.contains(&receiver_ty));
                assert!(members.contains(&InferredType::known("Error")));
            }
            other => panic!("Expected Union, got {other:?}"),
        }
    }

    #[test]
    fn substitute_self_none_passes_through() {
        // When self_type is None, `Self` should pass through as Known("Self")
        // (backward-compatible behaviour).
        let result = TypeChecker::substitute_return_type_with_self(
            "Result(Self, Error)",
            &HashMap::new(),
            &HashMap::new(),
            None,
        );
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                assert_eq!(type_args[0], InferredType::known("Self"));
                assert_eq!(type_args[1], InferredType::known("Error"));
            }
            other => panic!("Expected Known, got {other:?}"),
        }
    }

    // ---- set_param_types ----

    #[test]
    fn set_param_types_untyped() {
        let params = vec![ParameterDefinition::new(ident("x"))];
        let mut env = TypeEnv::new();
        TypeChecker::set_param_types(&mut env, &params, None);
        assert_eq!(
            env.get_local("x"),
            Some(InferredType::Dynamic(DynamicReason::Unknown))
        );
    }

    #[test]
    fn set_param_types_typed() {
        let params = vec![ParameterDefinition {
            name: ident("x"),
            type_annotation: Some(TypeAnnotation::Simple(ident("Integer"))),
        }];
        let mut env = TypeEnv::new();
        TypeChecker::set_param_types(&mut env, &params, None);
        assert_eq!(env.get_local("x"), Some(InferredType::known("Integer")));
    }

    #[test]
    fn set_param_types_mixed() {
        let params = vec![
            ParameterDefinition {
                name: ident("x"),
                type_annotation: Some(TypeAnnotation::Simple(ident("String"))),
            },
            ParameterDefinition::new(ident("y")),
        ];
        let mut env = TypeEnv::new();
        TypeChecker::set_param_types(&mut env, &params, None);
        assert_eq!(env.get_local("x"), Some(InferredType::known("String")));
        assert_eq!(
            env.get_local("y"),
            Some(InferredType::Dynamic(DynamicReason::Unknown))
        );
    }

    // ---- infer_expr: core expression type inference ----

    #[test]
    fn infer_expr_integer_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&int_lit(42), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Integer"));
    }

    #[test]
    fn infer_expr_string_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&str_lit("hello"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("String"));
    }

    #[test]
    fn infer_expr_true_identifier() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&var("true"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Boolean"));
    }

    #[test]
    fn infer_expr_false_identifier() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&var("false"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Boolean"));
    }

    #[test]
    fn infer_expr_nil_identifier() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&var("nil"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("UndefinedObject"));
    }

    #[test]
    fn infer_expr_self_identifier() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("self", InferredType::known("Counter"));
        let ty = checker.infer_expr(&var("self"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Counter"));
    }

    #[test]
    fn infer_expr_env_variable() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("myVar", InferredType::known("String"));
        let ty = checker.infer_expr(&var("myVar"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("String"));
    }

    #[test]
    fn infer_expr_unknown_var_is_dynamic() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&var("unknownVar"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Dynamic(DynamicReason::Unknown));
    }

    #[test]
    fn infer_expr_class_reference() {
        // BT-2260: a bare class literal `Integer` is the class *object*, whose
        // type is the metatype `Meta{Integer}` — not an instance of `Integer`.
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let ty = checker.infer_expr(&class_ref("Integer"), &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::meta("Integer"));
    }

    #[test]
    fn infer_expr_assignment_tracks_type() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Assignment {
            target: Box::new(var("x")),
            value: Box::new(int_lit(42)),
            type_annotation: None,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Integer"));
        // The variable should now be tracked in the environment
        assert_eq!(env.get_local("x"), Some(InferredType::known("Integer")));
    }

    #[test]
    fn infer_expr_block_returns_block_type() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let block = Expression::Block(Block::new(
            vec![BlockParameter::new("x", span())],
            vec![ExpressionStatement::bare(int_lit(1))],
            span(),
        ));
        let ty = checker.infer_expr(&block, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Block"));
    }

    #[test]
    fn infer_expr_map_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::MapLiteral {
            pairs: vec![],
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        // BT-2620: empty map literal carries no key/value info → Dictionary(Dynamic, Dynamic).
        assert_eq!(
            ty,
            InferredType::known_with_args(
                "Dictionary",
                vec![
                    InferredType::Dynamic(DynamicReason::Unknown),
                    InferredType::Dynamic(DynamicReason::Unknown),
                ],
            )
        );
    }

    #[test]
    fn infer_expr_array_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::ArrayLiteral {
            elements: vec![int_lit(1), int_lit(2)],
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        // BT-2620: homogeneous Integer elements → Array(Integer).
        assert_eq!(
            ty,
            InferredType::known_with_args("Array", vec![InferredType::known("Integer")])
        );
    }

    #[test]
    fn infer_expr_list_literal() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::ListLiteral {
            elements: vec![int_lit(1)],
            tail: None,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        // BT-2620: homogeneous Integer elements → List(Integer).
        assert_eq!(
            ty,
            InferredType::known_with_args("List", vec![InferredType::known("Integer")])
        );
    }

    #[test]
    fn infer_expr_array_literal_heterogeneous_joins_to_union() {
        // BT-2620: mixed element types join into a union element type.
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::ArrayLiteral {
            elements: vec![int_lit(1), str_lit("a")],
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(
            ty,
            InferredType::known_with_args(
                "Array",
                vec![InferredType::simple_union(&["Integer", "String"])]
            )
        );
    }

    #[test]
    fn infer_expr_array_literal_empty_is_dynamic_element() {
        // BT-2620: an empty literal carries no element info → Array(Dynamic).
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::ArrayLiteral {
            elements: vec![],
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(
            ty,
            InferredType::known_with_args(
                "Array",
                vec![InferredType::Dynamic(DynamicReason::Unknown)]
            )
        );
    }

    #[test]
    fn infer_expr_list_literal_folds_typed_tail_element() {
        // BT-2620: a `List(Integer)` tail contributes its `Integer` element to
        // the join, so `[1 | someIntList]` stays `List(Integer)`.
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local(
            "rest",
            InferredType::known_with_args("List", vec![InferredType::known("Integer")]),
        );
        let expr = Expression::ListLiteral {
            elements: vec![int_lit(1)],
            tail: Some(Box::new(var("rest"))),
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(
            ty,
            InferredType::known_with_args("List", vec![InferredType::known("Integer")])
        );
    }

    #[test]
    fn infer_expr_list_literal_array_tail_widens_to_dynamic() {
        // BT-2620: an Array is not a valid BEAM cons tail (it would form an
        // improper list), so it contributes no element type — folding a
        // `Dynamic` into the join collapses the element to `Dynamic`, giving
        // `List(Dynamic)` rather than a false `List(Integer)`.
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local(
            "rest",
            InferredType::known_with_args("Array", vec![InferredType::known("Integer")]),
        );
        let expr = Expression::ListLiteral {
            elements: vec![int_lit(1)],
            tail: Some(Box::new(var("rest"))),
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(
            ty,
            InferredType::known_with_args(
                "List",
                vec![InferredType::Dynamic(DynamicReason::Unknown)]
            )
        );
        // BT-2623: the silent widening is now also surfaced as a diagnostic so
        // the user sees the likely improper-list bug.
        assert_eq!(
            checker.diagnostics().len(),
            1,
            "Array cons tail should emit one improper-list diagnostic, got: {:?}",
            checker.diagnostics()
        );
        assert!(
            checker.diagnostics()[0].message.contains("improper list"),
            "Diagnostic should mention improper list, got: {:?}",
            checker.diagnostics()[0]
        );
        assert!(
            checker.diagnostics()[0].message.contains("Array"),
            "Diagnostic should name the offending Array tail, got: {:?}",
            checker.diagnostics()[0]
        );
    }

    #[test]
    fn infer_expr_list_literal_list_tail_no_diagnostic() {
        // BT-2623: a proper `List(T)` cons tail is valid — no diagnostic.
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local(
            "rest",
            InferredType::known_with_args("List", vec![InferredType::known("Integer")]),
        );
        let expr = Expression::ListLiteral {
            elements: vec![int_lit(1)],
            tail: Some(Box::new(var("rest"))),
            span: span(),
        };
        let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert!(
            checker.diagnostics().is_empty(),
            "List cons tail is proper — no diagnostic expected, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn infer_expr_list_literal_dynamic_tail_no_diagnostic() {
        // BT-2623: a `Dynamic` tail (e.g. unannotated param) is too uncertain to
        // flag — staying silent avoids false-positive noise.
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("rest", InferredType::Dynamic(DynamicReason::Unknown));
        let expr = Expression::ListLiteral {
            elements: vec![int_lit(1)],
            tail: Some(Box::new(var("rest"))),
            span: span(),
        };
        let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert!(
            checker.diagnostics().is_empty(),
            "Dynamic cons tail should not be flagged, got: {:?}",
            checker.diagnostics()
        );
    }

    #[test]
    fn infer_expr_list_literal_non_collection_tail_warns() {
        // BT-2623: any known non-List tail (e.g. an Integer) would form an
        // improper list, so it is flagged too — not just Array.
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("rest", InferredType::known("Integer"));
        let expr = Expression::ListLiteral {
            elements: vec![int_lit(1)],
            tail: Some(Box::new(var("rest"))),
            span: span(),
        };
        let _ = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(
            checker.diagnostics().len(),
            1,
            "Integer cons tail should emit one improper-list diagnostic, got: {:?}",
            checker.diagnostics()
        );
        assert!(checker.diagnostics()[0].message.contains("Integer"));
    }

    #[test]
    fn infer_expr_string_interpolation() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::StringInterpolation {
            segments: vec![
                crate::ast::StringSegment::Literal("hello ".into()),
                crate::ast::StringSegment::Interpolation(var("name")),
            ],
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("String"));
    }

    #[test]
    fn infer_expr_return_propagates_value_type() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Return {
            value: Box::new(str_lit("done")),
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("String"));
    }

    #[test]
    fn infer_expr_parenthesized_unwraps() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Parenthesized {
            expression: Box::new(int_lit(7)),
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::known("Integer"));
    }

    #[test]
    fn infer_expr_primitive_is_dynamic() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Primitive {
            name: "add".into(),
            is_quoted: false,
            is_intrinsic: false,
            is_inferred: false,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Dynamic(DynamicReason::Unknown));
    }

    #[test]
    fn infer_expr_match_is_dynamic() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let expr = Expression::Match {
            value: Box::new(int_lit(1)),
            arms: vec![],
            exhaustive: false,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(ty, InferredType::Dynamic(DynamicReason::Unknown));
    }

    // ---- BT-2854 / ADR 0107 Phase A: `Pattern::Nil` narrowing ----

    /// A `nil` arm's body sees the scrutinee narrowed to `UndefinedObject`,
    /// mirroring `x isNil ifTrue:` (reuses the same true-branch type as
    /// `is_nil.rs`).
    #[test]
    fn infer_expr_match_nil_arm_narrows_scrutinee_to_undefined_object() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("x", InferredType::simple_union(&["String", "Nil"]));

        // x match: [nil -> x; _ -> 0]
        let expr = Expression::Match {
            value: Box::new(var("x")),
            arms: vec![
                MatchArm::new(Pattern::Nil(span()), var("x"), span()),
                MatchArm::new(Pattern::Wildcard(span()), int_lit(0), span()),
            ],
            exhaustive: false,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        // Arm types: [UndefinedObject (nil arm's `x`), Integer (wildcard arm)]
        assert_eq!(
            ty,
            InferredType::union_of(&[
                InferredType::known(WellKnownClass::UndefinedObject.as_str()),
                InferredType::known("Integer"),
            ])
        );
    }

    /// An unguarded `nil` arm removes `Nil` from what subsequent arms see,
    /// mirroring `x isNil ifFalse:`'s `non_nil_type` narrowing.
    #[test]
    fn infer_expr_match_unguarded_nil_arm_narrows_subsequent_arms() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("x", InferredType::simple_union(&["String", "Nil"]));

        // x match: [nil -> 0; y -> x]  -- second arm's body reads `x`, which
        // should be narrowed to non-nil (`String`) after the unguarded nil arm.
        let expr = Expression::Match {
            value: Box::new(var("x")),
            arms: vec![
                MatchArm::new(Pattern::Nil(span()), int_lit(0), span()),
                MatchArm::new(
                    Pattern::Variable(Identifier::new("y", span())),
                    var("x"),
                    span(),
                ),
            ],
            exhaustive: false,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        // Arm types: [Integer (nil arm), String (narrowed `x` in 2nd arm)]
        assert_eq!(
            ty,
            InferredType::union_of(&[
                InferredType::known("Integer"),
                InferredType::known("String")
            ])
        );
    }

    /// A *guarded* `nil when: [...] ->` arm does not guarantee coverage, so
    /// it must not narrow away `Nil` for subsequent arms.
    #[test]
    fn infer_expr_match_guarded_nil_arm_does_not_narrow_subsequent_arms() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("x", InferredType::simple_union(&["String", "Nil"]));

        let expr = Expression::Match {
            value: Box::new(var("x")),
            arms: vec![
                MatchArm::with_guard(Pattern::Nil(span()), var("x"), int_lit(0), span()),
                MatchArm::new(
                    Pattern::Variable(Identifier::new("y", span())),
                    var("x"),
                    span(),
                ),
            ],
            exhaustive: false,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        // Second arm still sees `x` as `String | Nil` (unchanged residual).
        assert_eq!(
            ty,
            InferredType::union_of(&[
                InferredType::known("Integer"),
                InferredType::simple_union(&["String", "Nil"]),
            ])
        );
    }

    // ---- BT-2855 / ADR 0107 Phase A: `Pattern::Type` narrowing ----

    /// A `binding :: ClassName` arm's body sees `binding` statically
    /// narrowed to `ClassName`, mirroring `isKindOf:`'s true-branch
    /// narrowing (`intersect_with_class`).
    #[test]
    fn infer_expr_match_type_arm_narrows_binding_to_class() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("x", InferredType::simple_union(&["String", "Integer"]));

        // x match: [s :: String -> s; n :: Integer -> n]
        let expr = Expression::Match {
            value: Box::new(var("x")),
            arms: vec![
                MatchArm::new(
                    Pattern::Type {
                        binding: Identifier::new("s", span()),
                        class: Identifier::new("String", span()),
                        span: span(),
                    },
                    var("s"),
                    span(),
                ),
                MatchArm::new(
                    Pattern::Type {
                        binding: Identifier::new("n", span()),
                        class: Identifier::new("Integer", span()),
                        span: span(),
                    },
                    var("n"),
                    span(),
                ),
            ],
            exhaustive: false,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        // Each arm's body is the narrowed binding, not the original union.
        assert_eq!(
            ty,
            InferredType::union_of(&[
                InferredType::known("String"),
                InferredType::known("Integer"),
            ])
        );
    }

    /// An unguarded `Type` arm removes `ClassName` from what subsequent arms
    /// see, mirroring `isKindOf:`'s false-branch `\` (difference) narrowing
    /// — and the *scrutinee's own name* (not just the pattern's binding)
    /// sees the same narrowed type, since they denote the same value.
    #[test]
    fn infer_expr_match_unguarded_type_arm_narrows_subsequent_arms() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("x", InferredType::simple_union(&["String", "Integer"]));

        // x match: [s :: String -> 0; y -> x]  -- second arm's body reads
        // `x` (the scrutinee, not `s`), which should be narrowed to
        // `Integer` after the unguarded String arm.
        let expr = Expression::Match {
            value: Box::new(var("x")),
            arms: vec![
                MatchArm::new(
                    Pattern::Type {
                        binding: Identifier::new("s", span()),
                        class: Identifier::new("String", span()),
                        span: span(),
                    },
                    int_lit(0),
                    span(),
                ),
                MatchArm::new(
                    Pattern::Variable(Identifier::new("y", span())),
                    var("x"),
                    span(),
                ),
            ],
            exhaustive: false,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        assert_eq!(
            ty,
            InferredType::union_of(&[
                InferredType::known("Integer"),
                InferredType::known("Integer"),
            ])
        );
    }

    /// A *guarded* `binding :: ClassName when: [...] ->` arm does not
    /// guarantee coverage, so it must not narrow away `ClassName` for
    /// subsequent arms (same rule the guarded `nil` case above uses).
    #[test]
    fn infer_expr_match_guarded_type_arm_does_not_narrow_subsequent_arms() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        env.set_local("x", InferredType::simple_union(&["String", "Integer"]));

        let expr = Expression::Match {
            value: Box::new(var("x")),
            arms: vec![
                MatchArm::with_guard(
                    Pattern::Type {
                        binding: Identifier::new("s", span()),
                        class: Identifier::new("String", span()),
                        span: span(),
                    },
                    var("s"),
                    int_lit(0),
                    span(),
                ),
                MatchArm::new(
                    Pattern::Variable(Identifier::new("y", span())),
                    var("x"),
                    span(),
                ),
            ],
            exhaustive: false,
            span: span(),
        };
        let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
        // Second arm still sees `x` as `String | Integer` (unchanged residual).
        assert_eq!(
            ty,
            InferredType::union_of(&[
                InferredType::known("Integer"),
                InferredType::simple_union(&["String", "Integer"]),
            ])
        );
    }

    // ---- build_substitution_map ----

    #[test]
    fn build_substitution_map_empty_args() {
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::build_substitution_map(&hierarchy, "Array", &[]);
        assert!(result.is_empty());
    }

    // ---- infer_stmts ----

    #[test]
    fn infer_stmts_empty_is_dynamic() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let result = checker.infer_stmts(&[], &hierarchy, &mut env, false);
        assert_eq!(result, InferredType::Dynamic(DynamicReason::Unknown));
    }

    #[test]
    fn infer_stmts_returns_last_type() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let stmts = vec![
            ExpressionStatement::bare(int_lit(1)),
            ExpressionStatement::bare(str_lit("hello")),
        ];
        let result = checker.infer_stmts(&stmts, &hierarchy, &mut env, false);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn infer_stmts_skips_expect_directives() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let stmts = vec![
            ExpressionStatement::bare(int_lit(42)),
            ExpressionStatement::bare(Expression::ExpectDirective {
                category: ExpectCategory::Dnu,
                reason: None,
                span: span(),
            }),
        ];
        // The last non-directive is int_lit(42), so result should be Integer
        let result = checker.infer_stmts(&stmts, &hierarchy, &mut env, false);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn infer_stmts_stops_at_return() {
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        let mut env = TypeEnv::new();
        let stmts = vec![
            ExpressionStatement::bare(Expression::Return {
                value: Box::new(int_lit(1)),
                span: span(),
            }),
            ExpressionStatement::bare(str_lit("unreachable")),
        ];
        let result = checker.infer_stmts(&stmts, &hierarchy, &mut env, false);
        assert_eq!(result, InferredType::known("Integer"));
    }

    // ---- is_self_receiver ----

    #[test]
    fn is_self_receiver_true() {
        assert!(TypeChecker::is_self_receiver(&var("self")));
    }

    #[test]
    fn is_self_receiver_false_for_other_ident() {
        assert!(!TypeChecker::is_self_receiver(&var("other")));
    }

    #[test]
    fn is_self_receiver_false_for_non_ident() {
        assert!(!TypeChecker::is_self_receiver(&int_lit(1)));
    }

    // ---- infer_method_local_params ----

    /// Helper: build a `MethodInfo` with the given param types and return type.
    fn method_info(
        selector: &str,
        param_types: Vec<Option<&str>>,
        return_type: Option<&str>,
    ) -> MethodInfo {
        let arity = param_types.len();
        MethodInfo {
            selector: selector.into(),
            arity,
            kind: crate::ast::MethodKind::Primary,
            defined_in: "TestClass".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: return_type.map(EcoString::from),
            param_types: param_types
                .into_iter()
                .map(|p| p.map(EcoString::from))
                .collect(),
            doc: None,
        }
    }

    #[test]
    fn infer_method_local_params_result_t_e() {
        // assertOk: has param type Result(T, E), arg is Result(Integer, Error) → T=Integer, E=Error
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![InferredType::known("Integer"), InferredType::known("Error")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
        assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
    }

    #[test]
    fn infer_method_local_params_result_dictionary_string() {
        // assertOk: on Result(Dictionary, String) → T=Dictionary, E=String
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![
                InferredType::known("Dictionary"),
                InferredType::known("String"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("T"), Some(&InferredType::known("Dictionary")));
        assert_eq!(result.get("E"), Some(&InferredType::known("String")));
    }

    #[test]
    fn infer_method_local_params_assert_error_returns_e() {
        // assertError:equals: has param type Result(T, E), return type E
        let method = method_info(
            "assertError:equals:",
            vec![Some("Result(T, E)"), Some("Object")],
            Some("E"),
        );
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("Symbol"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg, InferredType::known("Symbol")],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("E"), Some(&InferredType::known("Symbol")));
        assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_array_t() {
        // Method with Array(T) param, arg is Array(String) → T=String
        let method = method_info("process:", vec![Some("Array(T)")], Some("T"));
        let arg = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 1);
        assert_eq!(result.get("T"), Some(&InferredType::known("String")));
    }

    #[test]
    fn infer_method_local_params_dictionary_k_v() {
        // Method with Dictionary(K, V) param → K=String, V=Integer
        let method = method_info("lookup:", vec![Some("Dictionary(K, V)")], Some("V"));
        let arg = InferredType::Known {
            class_name: "Dictionary".into(),
            type_args: vec![
                InferredType::known("String"),
                InferredType::known("Integer"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result.get("K"), Some(&InferredType::known("String")));
        assert_eq!(result.get("V"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_nested_result_array() {
        // Result(Array(Integer), Error) — nested parametric type
        let method = method_info("process:", vec![Some("Result(T, E)")], Some("T"));
        let inner_array = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("Integer")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![inner_array.clone(), InferredType::known("Error")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("T"), Some(&inner_array));
        assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
    }

    #[test]
    fn infer_method_local_params_no_type_args_on_arg() {
        // Bare Result without type args — no inference possible
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::known("Result"); // no type_args
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(result.is_empty(), "No inference when arg has no type_args");
    }

    #[test]
    fn infer_method_local_params_dynamic_arg() {
        // Dynamic argument — no inference possible
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::Dynamic(DynamicReason::Unknown);
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(result.is_empty(), "No inference from Dynamic arg");
    }

    #[test]
    fn infer_method_local_params_base_class_mismatch() {
        // Param declares Result(T, E) but arg is Array(Integer) — base mismatch
        let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
        let arg = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("Integer")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(
            result.is_empty(),
            "No inference when base class name doesn't match"
        );
    }

    #[test]
    fn infer_method_local_params_multiple_parametric_params() {
        // Method with two parametric params: process:with: Array(T), Dictionary(K, V)
        let method = method_info(
            "process:with:",
            vec![Some("Array(T)"), Some("Dictionary(K, V)")],
            Some("T"),
        );
        let arg1 = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let arg2 = InferredType::Known {
            class_name: "Dictionary".into(),
            type_args: vec![
                InferredType::known("Symbol"),
                InferredType::known("Integer"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg1, arg2],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 3);
        assert_eq!(result.get("T"), Some(&InferredType::known("String")));
        assert_eq!(result.get("K"), Some(&InferredType::known("Symbol")));
        assert_eq!(result.get("V"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_skips_known_classes_in_param_type() {
        // Param type is Result(Integer, E) — Integer is a known class, not a type param
        let method = method_info("check:", vec![Some("Result(Integer, E)")], Some("E"));
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![InferredType::known("Integer"), InferredType::known("Error")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        // "Integer" is a known class, so it should NOT be inferred as a type param
        assert!(!result.contains_key("Integer"));
        assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
    }

    #[test]
    fn infer_method_local_params_non_parametric_param_ignored() {
        // Method param type is just "Integer" (not parametric) — nothing to infer
        let method = method_info("add:", vec![Some("Integer")], Some("Integer"));
        let arg = InferredType::known("Integer");
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(
            result.is_empty(),
            "Non-parametric param type yields no inference"
        );
    }

    #[test]
    fn infer_method_local_params_missing_arg() {
        // Method expects 2 args but only 1 provided — second param skipped
        let method = method_info(
            "process:with:",
            vec![Some("Array(T)"), Some("Dictionary(K, V)")],
            Some("T"),
        );
        let arg1 = InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg1],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 1);
        assert_eq!(result.get("T"), Some(&InferredType::known("String")));
        assert!(!result.contains_key("K"));
        assert!(!result.contains_key("V"));
    }

    #[test]
    fn infer_method_local_params_untyped_param_skipped() {
        // Method has a None param type — skipped
        let method = method_info("process:", vec![None], Some("Dynamic"));
        let arg = InferredType::known("Integer");
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(result.is_empty());
    }

    // ---- Block inference regression tests ----
    // These test that Block(T, R) param types still work correctly.

    #[test]
    fn infer_method_local_params_block_t_r() {
        // collect: has param type Block(T, R), arg is Block with type_args
        let method = method_info("collect:", vec![Some("Block(T, R)")], Some("Array(R)"));
        let arg = InferredType::Known {
            class_name: "Block".into(),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("String"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.len(), 2);
        assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
        assert_eq!(result.get("R"), Some(&InferredType::known("String")));
    }

    #[test]
    fn infer_method_local_params_map_block() {
        // map: has Block(T, R) param type
        let method = method_info("map:", vec![Some("Block(T, R)")], Some("Array(R)"));
        let arg = InferredType::Known {
            class_name: "Block".into(),
            type_args: vec![
                InferredType::known("String"),
                InferredType::known("Integer"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("T"), Some(&InferredType::known("String")));
        assert_eq!(result.get("R"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_inject_into_block() {
        // inject:into: has two params — initial value and Block(T, R)
        let method = method_info(
            "inject:into:",
            vec![Some("Object"), Some("Block(T, R)")],
            Some("R"),
        );
        let arg_init = InferredType::known("Integer");
        let arg_block = InferredType::Known {
            class_name: "Block".into(),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("Integer"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg_init, arg_block],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        // Only the Block param yields inference; Object is non-parametric
        assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
        assert_eq!(result.get("R"), Some(&InferredType::known("Integer")));
    }

    /// BT-2039: When the same method-local type parameter is unified across
    /// multiple argument positions and one resolves to `Known` while another
    /// resolves to `Dynamic`, the Known binding must win. Otherwise
    /// `Block(R) Block(R) -> R` on `ifTrue:ifFalse:` collapses to `Dynamic`
    /// whenever one branch is an untyped FFI call, triggering a spurious
    /// "expression inferred as Dynamic in typed class" warning.
    #[test]
    fn bt_2039_method_local_known_beats_dynamic_across_args() {
        let method = method_info(
            "ifTrue:ifFalse:",
            vec![Some("Block(R)"), Some("Block(R)")],
            Some("R"),
        );
        let known_block = InferredType::Known {
            class_name: "Block".into(),
            type_args: vec![InferredType::known("Integer")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let dynamic_block = InferredType::Known {
            class_name: "Block".into(),
            type_args: vec![InferredType::Dynamic(DynamicReason::UntypedFfi)],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();

        // Known branch first, Dynamic second — naive last-wins would pick Dynamic.
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[known_block.clone(), dynamic_block.clone()],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(
            result.get("R"),
            Some(&InferredType::known("Integer")),
            "Known branch must win when second arg is Dynamic, got {:?}",
            result.get("R")
        );

        // Reverse order — Dynamic first, Known second — also resolves to Known.
        let result_rev = TypeChecker::infer_method_local_params(
            &method,
            &[dynamic_block, known_block],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(
            result_rev.get("R"),
            Some(&InferredType::known("Integer")),
            "Known branch must win when first arg is Dynamic, got {:?}",
            result_rev.get("R")
        );
    }

    #[test]
    fn infer_method_local_params_skips_non_type_param_in_parametric() {
        // BT-1895: Param type is Result(Enumerable, E) where Enumerable is a protocol name,
        // not a type parameter. It should NOT be substituted even though it's not in the hierarchy.
        let method = method_info("check:", vec![Some("Result(Enumerable, E)")], Some("E"));
        let arg = InferredType::Known {
            class_name: "Result".into(),
            type_args: vec![InferredType::known("List"), InferredType::known("Error")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        // "Enumerable" is not a single-letter type param, so it must NOT be inferred
        assert!(
            !result.contains_key("Enumerable"),
            "Non-type-param identifier 'Enumerable' should not be substituted"
        );
        // "E" IS a valid single-letter type param — it should still be inferred
        assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
    }

    // ---- substitute_return_type with method-local params (end-to-end flow) ----

    #[test]
    fn substitute_return_type_from_method_local_result() {
        // Simulates: assertOk: with Result(Integer, Error) → return type T → Integer
        let mut method_subst = HashMap::new();
        method_subst.insert(EcoString::from("T"), InferredType::known("Integer"));
        method_subst.insert(EcoString::from("E"), InferredType::known("Error"));
        let result = TypeChecker::substitute_return_type("T", &HashMap::new(), &method_subst);
        assert_eq!(result, InferredType::known("Integer"));
    }

    #[test]
    fn substitute_return_type_generic_array_r() {
        // collect: returns Array(R) where R=String → Array(String)
        let mut method_subst = HashMap::new();
        method_subst.insert(EcoString::from("R"), InferredType::known("String"));
        let result =
            TypeChecker::substitute_return_type("Array(R)", &HashMap::new(), &method_subst);
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Array");
                assert_eq!(type_args.len(), 1);
                assert_eq!(type_args[0], InferredType::known("String"));
            }
            other => panic!("Expected Known Array(String), got {other:?}"),
        }
    }

    #[test]
    fn substitute_return_type_nested_result() {
        // Return type Result(T, E) with T=Array(Integer), E=Error
        // Substitution on "Result(T, E)" should produce Result(Array(Integer), Error)
        let mut method_subst = HashMap::new();
        method_subst.insert(
            EcoString::from("T"),
            InferredType::Known {
                class_name: "Array".into(),
                type_args: vec![InferredType::known("Integer")],
                provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
            },
        );
        method_subst.insert(EcoString::from("E"), InferredType::known("Error"));
        let result =
            TypeChecker::substitute_return_type("Result(T, E)", &HashMap::new(), &method_subst);
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Result");
                assert_eq!(type_args.len(), 2);
                match &type_args[0] {
                    InferredType::Known {
                        class_name,
                        type_args,
                        ..
                    } => {
                        assert_eq!(class_name.as_str(), "Array");
                        assert_eq!(type_args.len(), 1);
                        assert_eq!(type_args[0], InferredType::known("Integer"));
                    }
                    other => panic!("Expected Known Array(Integer), got {other:?}"),
                }
                assert_eq!(type_args[1], InferredType::known("Error"));
            }
            other => panic!("Expected Known Result(Array(Integer), Error), got {other:?}"),
        }
    }

    #[test]
    fn substitute_return_type_union_with_type_param() {
        // BT-1836: "E | Nil" with E=Integer should produce Union(Integer, Nil)
        let mut subst = HashMap::new();
        subst.insert(EcoString::from("E"), InferredType::known("Integer"));
        let result = TypeChecker::substitute_return_type("E | Nil", &subst, &HashMap::new());
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert_eq!(members[0], InferredType::known("Integer"));
                assert_eq!(members[1], InferredType::known("Nil"));
            }
            other => panic!("Expected Union(Integer, Nil), got {other:?}"),
        }
    }

    #[test]
    fn substitute_return_type_union_no_params() {
        // "Behaviour | Nil" with no substitutions should pass through as Union
        let result = TypeChecker::substitute_return_type(
            "Behaviour | Nil",
            &HashMap::new(),
            &HashMap::new(),
        );
        match result {
            InferredType::Union { members, .. } => {
                assert_eq!(members.len(), 2);
                assert_eq!(members[0], InferredType::known("Behaviour"));
                assert_eq!(members[1], InferredType::known("Nil"));
            }
            other => panic!("Expected Union(Behaviour, Nil), got {other:?}"),
        }
    }

    // ---- BT-2023(A): Nullable-union arguments unify with generic params ----

    #[test]
    fn infer_method_local_params_nullable_list_union() {
        // Param type List(T), arg is List(String) | Nil → should extract T=String
        let method = method_info("process:", vec![Some("List(T)")], Some("List(T)"));
        let arg = InferredType::Union {
            members: vec![
                InferredType::Known {
                    class_name: "List".into(),
                    type_args: vec![InferredType::known("String")],
                    provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
                },
                InferredType::known("UndefinedObject"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(
            result.get("T"),
            Some(&InferredType::known("String")),
            "Should extract T=String from List(String) | Nil"
        );
    }

    #[test]
    fn infer_method_local_params_nullable_dictionary_union() {
        // Dictionary(K, V) | Nil → should extract K=String, V=Integer
        let method = method_info("lookup:", vec![Some("Dictionary(K, V)")], Some("V"));
        let arg = InferredType::Union {
            members: vec![
                InferredType::Known {
                    class_name: "Dictionary".into(),
                    type_args: vec![
                        InferredType::known("String"),
                        InferredType::known("Integer"),
                    ],
                    provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
                },
                InferredType::known("UndefinedObject"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("K"), Some(&InferredType::known("String")));
        assert_eq!(result.get("V"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_nullable_set_union() {
        // Set(T) | Nil → should extract T=Symbol
        let method = method_info("process:", vec![Some("Set(T)")], Some("T"));
        let arg = InferredType::Union {
            members: vec![
                InferredType::Known {
                    class_name: "Set".into(),
                    type_args: vec![InferredType::known("Symbol")],
                    provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
                },
                InferredType::known("UndefinedObject"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("T"), Some(&InferredType::known("Symbol")));
    }

    #[test]
    fn infer_method_local_params_nil_alias_in_union() {
        // Also handle "Nil" (not just "UndefinedObject") in unions
        let method = method_info("process:", vec![Some("List(T)")], Some("T"));
        let arg = InferredType::Union {
            members: vec![
                InferredType::Known {
                    class_name: "List".into(),
                    type_args: vec![InferredType::known("Integer")],
                    provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
                },
                InferredType::known("Nil"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
    }

    #[test]
    fn infer_method_local_params_non_nil_union_no_match() {
        // Union without nil members (e.g., String | Integer) should NOT match List(T)
        let method = method_info("process:", vec![Some("List(T)")], Some("T"));
        let arg = InferredType::Union {
            members: vec![
                InferredType::known("String"),
                InferredType::known("Integer"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[arg],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert!(
            result.is_empty(),
            "Non-nil union members with no matching base class should not unify"
        );
    }

    #[test]
    fn infer_method_local_params_plain_type_param_nullable_union() {
        // Plain type param A with nullable union arg: A binds to the whole
        // union (String | Nil), preserving nilability through the return type.
        // CodeRabbit on PR #2058: stripping Nil here would unsoundly turn
        // `identity: x :: A -> A` called with `String | Nil` into `A = String`.
        let method = method_info("inject:", vec![Some("A")], Some("A"));
        let nullable = InferredType::Union {
            members: vec![
                InferredType::known("String"),
                InferredType::known("UndefinedObject"),
            ],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::infer_method_local_params(
            &method,
            std::slice::from_ref(&nullable),
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(
            result.get("A"),
            Some(&nullable),
            "Plain type param A should bind to the full nullable union"
        );
    }

    // ---- BT-2023(B): Nested-generic block params resolve ----

    #[test]
    fn resolve_type_param_nested_list_e() {
        // Block param type "List(E)" where E=String → should resolve to List(String)
        let mut class_subst = HashMap::new();
        class_subst.insert(EcoString::from("E"), InferredType::known("String"));
        let hierarchy = ClassHierarchy::with_builtins();
        let result =
            TypeChecker::resolve_type_param("List(E)", &class_subst, &HashMap::new(), &hierarchy);
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "List");
                assert_eq!(type_args.len(), 1);
                assert_eq!(type_args[0], InferredType::known("String"));
            }
            other => panic!("Expected Known List(String), got {other:?}"),
        }
    }

    #[test]
    fn resolve_type_param_nested_dictionary_k_list_v() {
        // Two levels: "Dictionary(K, List(V))" where K=String, V=Integer
        let mut method_subst = HashMap::new();
        method_subst.insert(EcoString::from("K"), InferredType::known("String"));
        method_subst.insert(EcoString::from("V"), InferredType::known("Integer"));
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::resolve_type_param(
            "Dictionary(K, List(V))",
            &HashMap::new(),
            &method_subst,
            &hierarchy,
        );
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "Dictionary");
                assert_eq!(type_args.len(), 2);
                assert_eq!(type_args[0], InferredType::known("String"));
                match &type_args[1] {
                    InferredType::Known {
                        class_name,
                        type_args,
                        ..
                    } => {
                        assert_eq!(class_name.as_str(), "List");
                        assert_eq!(type_args.len(), 1);
                        assert_eq!(type_args[0], InferredType::known("Integer"));
                    }
                    other => panic!("Expected Known List(Integer), got {other:?}"),
                }
            }
            other => panic!("Expected Known Dictionary(String, List(Integer)), got {other:?}"),
        }
    }

    #[test]
    fn resolve_type_param_bare_param_unchanged() {
        // A bare "E" still resolves via the substitution map (existing behaviour)
        let mut subst = HashMap::new();
        subst.insert(EcoString::from("E"), InferredType::known("String"));
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::resolve_type_param("E", &subst, &HashMap::new(), &hierarchy);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn resolve_type_param_known_class_unchanged() {
        // A known class name stays the same
        let hierarchy = ClassHierarchy::with_builtins();
        let result = TypeChecker::resolve_type_param(
            "Integer",
            &HashMap::new(),
            &HashMap::new(),
            &hierarchy,
        );
        assert_eq!(result, InferredType::known("Integer"));
    }

    // ---- BT-2023(C): FFI polymorphic return type substitution ----

    #[test]
    fn substitute_ffi_return_type_list_propagation() {
        // FFI sig: List -> List, call-site arg: List(String)
        // → return should be List(String)
        let ret = InferredType::known("List");
        let params = vec![super::super::native_type_registry::ParamType {
            keyword: Some("list".into()),
            type_: InferredType::known("List"),
        }];
        let arg = InferredType::Known {
            class_name: "List".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
        match result {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                assert_eq!(class_name.as_str(), "List");
                assert_eq!(type_args.len(), 1);
                assert_eq!(type_args[0], InferredType::known("String"));
            }
            other => panic!("Expected Known List(String), got {other:?}"),
        }
    }

    #[test]
    fn substitute_ffi_return_type_no_match() {
        // FFI sig: Integer -> String (different base classes), call-site arg: Integer
        // → return should stay String (no propagation)
        let ret = InferredType::known("String");
        let params = vec![super::super::native_type_registry::ParamType {
            keyword: Some("n".into()),
            type_: InferredType::known("Integer"),
        }];
        let arg = InferredType::known("Integer");
        let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
        assert_eq!(result, InferredType::known("String"));
    }

    #[test]
    fn substitute_ffi_return_type_already_has_type_args() {
        // FFI sig: List(Integer) -> List(Integer) (already concrete)
        // → return should stay unchanged
        let ret = InferredType::Known {
            class_name: "List".into(),
            type_args: vec![InferredType::known("Integer")],
            provenance: crate::semantic_analysis::TypeProvenance::Extracted,
        };
        let params = vec![super::super::native_type_registry::ParamType {
            keyword: Some("list".into()),
            type_: InferredType::known("List"),
        }];
        let arg = InferredType::Known {
            class_name: "List".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
        assert_eq!(result.as_known().unwrap().as_str(), "List");
        // Should keep the original type_args, not the arg's
        match result {
            InferredType::Known { type_args, .. } => {
                assert_eq!(type_args[0], InferredType::known("Integer"));
            }
            _ => unreachable!(),
        }
    }

    #[test]
    fn substitute_ffi_return_type_arg_has_no_type_args() {
        // FFI sig: List -> List, call-site arg: List (no type_args)
        // → return should stay List (no type_args to propagate)
        let ret = InferredType::known("List");
        let params = vec![super::super::native_type_registry::ParamType {
            keyword: Some("list".into()),
            type_: InferredType::known("List"),
        }];
        let arg = InferredType::known("List");
        let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
        assert_eq!(result, InferredType::known("List"));
    }

    #[test]
    fn substitute_ffi_return_type_dynamic_return() {
        // FFI sig: Dynamic return type → stays Dynamic
        let ret = InferredType::Dynamic(DynamicReason::Unknown);
        let params = vec![super::super::native_type_registry::ParamType {
            keyword: Some("list".into()),
            type_: InferredType::known("List"),
        }];
        let arg = InferredType::Known {
            class_name: "List".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
        assert!(matches!(result, InferredType::Dynamic(_)));
    }

    #[test]
    fn substitute_ffi_return_type_skips_multi_arg() {
        // Models lists:map/2: Fun, [A] -> [B]. The element type of the input
        // list must NOT be propagated to the return — the return's element
        // type is the block's result type, not the input element type.
        let ret = InferredType::known("List");
        let params = vec![
            super::super::native_type_registry::ParamType {
                keyword: Some("fun".into()),
                type_: InferredType::known("Block"),
            },
            super::super::native_type_registry::ParamType {
                keyword: Some("list".into()),
                type_: InferredType::known("List"),
            },
        ];
        let fun_arg = InferredType::known("Block");
        let list_arg = InferredType::Known {
            class_name: "List".into(),
            type_args: vec![InferredType::known("String")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        };
        let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[fun_arg, list_arg]);
        // Stays bare List — no unsound propagation
        assert_eq!(result, InferredType::known("List"));
    }
}
