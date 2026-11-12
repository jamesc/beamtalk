// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Argument checking (declared parameter types, sendability, list-literal elements) and binary-operand / comparison operand checks.
//!
//! **DDD Context:** Semantic Analysis

//! `TypeChecker::state_default_types` (BT-3481), keeping the direction
//! one-way instead of calling back into `inference/`.

use std::collections::HashMap;

use crate::announce_selectors::is_announce_selector;
use crate::ast::{Expression, Literal, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, DeclaredType};
use crate::semantic_analysis::string_utils::is_symbol_singleton_name;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span, equality_operator_is_negated};
use ecow::EcoString;

use super::super::known_remote;
use super::super::sendability;
use super::super::type_resolver;
use super::super::types::ClassObjectType;
use super::super::well_known::WellKnownClass;
use super::super::{DynamicReason, InferredType, TypeChecker};
use super::UnionArgCompat;

impl TypeChecker {
    /// ADR 0103 (Phase 1): warn when a process-bound handle
    /// (`HandleScoped(#process)`) appears as a value in a `spawnWith:`
    /// initial-state map — the map is copied into the new actor process, so the
    /// value is only usable by its original owner. Inspects the `MapLiteral`
    /// call-site pairs (shared model with ADR 0104's `spawnWith:` key checking).
    pub(in super::super) fn check_spawn_with_sendability(
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
                sendability::tier_of(value_ty, hierarchy, self.alias_registry.as_deref())
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

    /// ADR 0103 (Phase 0): warn when a process-bound handle
    /// (`HandleScoped(#process)`) is passed as an argument in an actor message
    /// (boundary #1) or used as an Announcement payload (boundary #3) — both
    /// copy the term across a process boundary. The wording differs; the tier
    /// rule does not.
    ///
    /// ADR 0126 §6: also warns when a node-bound handle
    /// (`HandleScoped(#node)`) is passed in an actor message AND the
    /// receiver is known-remote (`receiver_known_remote.is_some()`) —
    /// upgraded from ADR 0103's silent-in-v1 `#node` tier because ADR 0126
    /// §5.4 makes this a deterministic `not_serialisable` runtime
    /// rejection. A `#node` handle to a receiver that is *not*
    /// known-remote (or an Announcement payload) stays silent, unchanged
    /// from ADR 0103.
    ///
    /// Runs independently of whether the receiver's handler has typed
    /// parameters — the hazard is about *what* is sent, not the declared
    /// parameter type — so it is invoked before the method lookup in
    /// [`check_argument_types`]. `Unknown` arguments stay silent in v1
    /// (advisory per ADR 0100). Class-side `spawnWith:` maps are handled by
    /// [`check_spawn_with_sendability`].
    #[allow(clippy::too_many_arguments)] // boundary context: selector + receiver flags + arg spans
    pub(in super::super) fn check_arg_sendability(
        &mut self,
        selector: &str,
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
        receiver_is_actor: bool,
        arg_exprs: Option<&[Expression]>,
        receiver_known_remote: Option<&known_remote::KnownRemote>,
    ) {
        // Two boundaries share this check (ADR 0103): actor *instance* message
        // arguments (#1) and Announcement payloads (#3) — both copy the term
        // across a process boundary. The wording differs; the tier rule does not.
        let is_announce = is_announce_selector(selector);
        let is_actor_message = !is_class_side && receiver_is_actor;
        if !is_announce && !is_actor_message {
            return;
        }
        // ADR 0126 §6: the #node upgrade applies only to the actor-message
        // boundary, on a known-remote receiver — never to an Announcement
        // payload (there is no "receiver" in the remote sense there).
        let node_upgrade_applies = is_actor_message && receiver_known_remote.is_some();
        for (i, arg_ty) in arg_types.iter().enumerate() {
            let tier = sendability::tier_of(arg_ty, hierarchy, self.alias_registry.as_deref());
            let scope = match tier {
                sendability::Tier::HandleScoped(sendability::HandleScope::Process) => {
                    sendability::HandleScope::Process
                }
                sendability::Tier::HandleScoped(sendability::HandleScope::Node)
                    if node_upgrade_applies =>
                {
                    sendability::HandleScope::Node
                }
                _ => continue,
            };
            let arg_expr = arg_exprs.and_then(|exprs| exprs.get(i));
            let arg_span = arg_expr.map_or(span, Expression::span);
            let ty_name = arg_ty
                .as_known()
                .cloned()
                .unwrap_or_else(|| EcoString::from("handle"));
            // Prefer the variable name (`port`/`cache`) for the message;
            // fall back to the handle's type name for non-identifier
            // arguments.
            let label = match arg_expr {
                Some(Expression::Identifier(ident)) => ident.name.clone(),
                _ => ty_name.clone(),
            };
            let diagnostic = if matches!(scope, sendability::HandleScope::Node) {
                Diagnostic::warning(
                    format!(
                        "`{label}` ({ty_name} — node-bound handle) sent to a remote actor; it \
                         will be rejected at runtime (not_serialisable)"
                    ),
                    arg_span,
                )
                .with_hint(
                    "A node-bound handle (an ETS table, timer, or atomic counter) does not \
                     exist on the remote node. Consider passing data, or having the remote \
                     actor own the handle",
                )
                .with_category(DiagnosticCategory::Sendability)
            } else {
                let boundary = if is_announce {
                    "used as an Announcement payload"
                } else {
                    "passed in an actor message"
                };
                Diagnostic::warning(
                    format!(
                        "`{label}` ({ty_name} — process-bound handle) {boundary}; it is only \
                         usable by its owning process"
                    ),
                    arg_span,
                )
                .with_hint(
                    "A port-like handle is bound to its owning process. Consider passing \
                     data, or an Actor that owns the handle",
                )
                .with_category(DiagnosticCategory::Sendability)
            };
            self.diagnostics.push(diagnostic);
        }
    }

    /// Check argument types against declared parameter types for a message send.
    ///
    /// Thin wrapper over [`Self::check_argument_types_impl`] with no
    /// known-remote provenance (ADR 0126 §6) — the overwhelming majority of
    /// call sites (self-sends, class-side sends, and the existing unit
    /// tests) have no receiver expression to classify. See
    /// [`Self::check_argument_types_with_known_remote`] for the one call
    /// site that does.
    #[allow(clippy::too_many_arguments)] // arg_exprs + env needed for origin tracing
    pub(in super::super) fn check_argument_types(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
        arg_exprs: Option<&[Expression]>,
        env: Option<&super::super::TypeEnv>,
        receiver_type_args: &[InferredType],
    ) {
        self.check_argument_types_impl(
            class_name,
            selector,
            arg_types,
            span,
            hierarchy,
            is_class_side,
            arg_exprs,
            env,
            receiver_type_args,
            None,
            false,
        );
    }

    /// [`Self::check_argument_types`], additionally threading the
    /// receiver's known-remote provenance (ADR 0126 §6) through to the
    /// shared ADR 0103 sendability check
    /// ([`Self::check_arg_sendability`]), which upgrades a
    /// `HandleScoped(#node)` argument to a Warning when the receiver is
    /// known-remote (silent otherwise, unchanged from ADR 0103).
    /// `receiver_is_self` marks a `self`/`super` receiver, exempt from the
    /// sendability check (ADR 0103, BT-4447): a self-send never moves its
    /// argument away from the owning actor. At home it runs in the actor's
    /// own process; from a block running away from home (ADR 0137) it
    /// messages the owner back. A block that carries the handle away is the
    /// escape, and ADR 0103's block-capture check (boundary #2) reports it,
    /// not this check.
    #[allow(clippy::too_many_arguments)]
    pub(in super::super) fn check_argument_types_with_known_remote(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
        arg_exprs: Option<&[Expression]>,
        env: Option<&super::super::TypeEnv>,
        receiver_type_args: &[InferredType],
        receiver_known_remote: Option<&known_remote::KnownRemote>,
        receiver_is_self: bool,
    ) {
        self.check_argument_types_impl(
            class_name,
            selector,
            arg_types,
            span,
            hierarchy,
            is_class_side,
            arg_exprs,
            env,
            receiver_type_args,
            receiver_known_remote,
            receiver_is_self,
        );
    }

    #[allow(clippy::too_many_arguments)] // arg_exprs + env + known-remote needed for origin tracing
    fn check_argument_types_impl(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        arg_types: &[InferredType],
        span: Span,
        hierarchy: &ClassHierarchy,
        is_class_side: bool,
        arg_exprs: Option<&[Expression]>,
        env: Option<&super::super::TypeEnv>,
        receiver_type_args: &[InferredType],
        receiver_known_remote: Option<&known_remote::KnownRemote>,
        receiver_is_self: bool,
    ) {
        // ADR 0103: sendability of actor message arguments and `spawnWith:`
        // map values. Independent of the handler's declared parameter types, so
        // they run before the method lookup / early returns below. A
        // self-send never moves its argument away from the owning actor, so
        // it is exempt (BT-4447; see `check_argument_types_with_known_remote`).
        let receiver_is_actor = hierarchy.is_actor_subclass(class_name);
        self.check_arg_sendability(
            selector,
            arg_types,
            span,
            hierarchy,
            is_class_side,
            receiver_is_actor && !receiver_is_self,
            arg_exprs,
            receiver_known_remote,
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

        // Substitution map from `class_name`'s (or its declaring
        // superclass's, if `method` is inherited) generic type-param names
        // to the receiver's concrete `receiver_type_args` — e.g. `Dictionary
        // (Symbol, JsonValue)` gives `{K -> Symbol, V -> JsonValue}`. Empty
        // (a no-op below) when the receiver has no type args or the class
        // has no type params, so a non-generic call is entirely unaffected.
        // Hoisted out of the loop since, like `metaclass_name`, it's
        // constant across all arguments.
        //
        // Adversarial-review follow-up: an earlier version of this fix
        // special-cased `class_subst.remove("K")` (Dictionary/Ets's
        // conventional key slot), reasoning that only *keys* infer as narrow
        // singleton types from literals. That was wrong on two counts: (1)
        // ANY type-arg position singleton-narrows from a literal — a `List`
        // built from symbol-literal elements (`#(#a, #b)`) or a `Result`
        // built from a symbol ok/error value (`Result ok: #active`) narrows
        // its `E`/`T` the same way `Dictionary`'s `K` does, so a name-only
        // exclusion of `"K"` still false-positived on `#(#a, #b) ++ #(#c,
        // #d)` and `(Result ok: #active) valueOr: #inactive`; (2) excluding
        // by bare param name regardless of class permanently under-checks
        // any *user-defined* generic class that happens to name a type
        // param `K` for something that isn't a key at all. `widen_singleton_type_arg`
        // below fixes the actual root cause (singleton-narrowed substitution
        // targets) for every position uniformly, so no name-based exclusion
        // is needed at all.
        let class_subst: HashMap<EcoString, InferredType> =
            super::super::TypeChecker::build_inherited_substitution_map(
                hierarchy,
                class_name,
                receiver_type_args,
                &method.defined_in,
            )
            .into_iter()
            .map(|(param, ty)| (param, Self::widen_singleton_type_arg(&ty)))
            .collect();

        self.check_args_against_param_types(
            class_name,
            selector,
            arg_types,
            &method.param_types,
            &class_subst,
            span,
            hierarchy,
            arg_exprs,
            env,
        );
    }

    /// The per-argument half of [`Self::check_argument_types`]: checks each
    /// argument type against its declared parameter type (`None` = unchecked),
    /// after substituting the receiver's generic type params via `class_subst`.
    /// `class_name` is the receiver as the diagnostics name it — a class, or a
    /// protocol metatype `P class` whose `class` requirement supplies
    /// `param_types` (ADR 0144).
    #[allow(clippy::too_many_arguments)] // arg_exprs + env needed for origin tracing
    #[allow(clippy::too_many_lines)] // includes the class-literal subtyping arm
    pub(in super::super) fn check_args_against_param_types(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        arg_types: &[InferredType],
        param_types: &[Option<DeclaredType>],
        class_subst: &HashMap<EcoString, InferredType>,
        span: Span,
        hierarchy: &ClassHierarchy,
        arg_exprs: Option<&[Expression]>,
        env: Option<&super::super::TypeEnv>,
    ) {
        // A class literal (e.g. `TestCase`) is a class value whose
        // runtime type flows through the metaclass tower
        // (Metaclass → Class → Behaviour → Object → ProtoObject). When the
        // instance-side chain fails, re-check via `Metaclass` so parameters
        // declared `:: Behaviour` / `:: Class` / `:: Object` / `:: ProtoObject`
        // accept any class literal. Hoisted out of the loop since it's
        // constant across all arguments.
        let metaclass_name: EcoString = "Metaclass".into();

        for (i, (arg_ty, expected)) in arg_types.iter().zip(param_types.iter()).enumerate() {
            let Some(expected_declared) = expected else {
                continue;
            };
            if let Some(arg) = arg_exprs.and_then(|exprs| exprs.get(i)) {
                self.check_list_literal_elements(
                    class_name,
                    selector,
                    i + 1,
                    arg,
                    expected_declared,
                    hierarchy,
                );
            }
            // Resolve a declared parameter type that names one of
            // the receiver's own generic type params — bare (`V`) or nested
            // (`List(E)`, `Dictionary(K, V)`) — to its concrete substituted
            // type before any of the compatibility checks below. Without
            // this, `at:put:`/`++`/etc. on a `Dictionary(K, V)`/`List(E)`
            // instance always passed any element argument: `V`/`E`/`K` are
            // never registered hierarchy classes, so `is_type_compatible`'s
            // "unknown declared class -> conservatively compatible" escape
            // hatch (and `type_args_match`'s generic-placeholder shortcut)
            // fired unconditionally.
            //
            // Only substitutes when `expected_ty` actually references one
            // of `class_subst`'s keys — every other declared type (plain
            // classes, unions, and in particular alias names like
            // `RestartStrategy`, which also aren't registered hierarchy
            // classes) is left byte-for-byte untouched, so this can't
            // regress the alias/protocol/union handling below by feeding
            // `resolve_type_param` an unrelated string it would otherwise
            // resolve to `Dynamic`.
            // `type_string_references_class_param` and the compatibility
            // checks below are string-level helpers — render the structured
            // type once at this boundary (byte-
            // identical to the old stored string); `resolve_type_param`
            // itself takes the structured `DeclaredType` directly.
            // ADR 0147: a shape on either side (an alias to one included) is
            // judged structurally; everything else keeps the string-level
            // nominal rules below, which know no `#{...}` class.
            let expected_resolved = (arg_ty.contains_shape()
                || class_subst.values().any(InferredType::contains_shape)
                || self.declared_may_be_shape(expected_declared))
            .then(|| self.resolve_declared_recording_aliases(expected_declared, class_subst))
            .filter(|resolved| resolved.contains_shape() || arg_ty.contains_shape());
            if let Some(expected_resolved) = expected_resolved {
                if let Err(mismatch) = Self::shape_assignable(arg_ty, &expected_resolved, hierarchy)
                {
                    let param_pos = i + 1;
                    let actual_display = arg_ty
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let expected_display = expected_resolved
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let mut diag = Self::shape_mismatch_diagnostic(
                        format!(
                            "Argument {param_pos} of '{selector}' on {class_name} expects {expected_display}, got {actual_display}"
                        ),
                        &mismatch,
                        &expected_resolved,
                        span,
                    );
                    if let (Some(exprs), Some(e)) = (arg_exprs, env) {
                        if let Some(Expression::Identifier(ident)) = exprs.get(i) {
                            if let Some(origin) = e.get_local_origin(&ident.name) {
                                diag =
                                    diag.with_note(origin.description.clone(), Some(origin.span));
                            }
                        }
                    }
                    self.diagnostics.push(diag);
                }
                continue;
            }
            let expected_declared_str: EcoString = expected_declared.to_string().into();
            let substituted_expected;
            let expected_ty: &EcoString = if !class_subst.is_empty()
                && Self::type_string_references_class_param(&expected_declared_str, class_subst)
            {
                substituted_expected = super::super::TypeChecker::resolve_type_param(
                    expected_declared,
                    class_subst,
                    &HashMap::new(),
                    hierarchy,
                )
                .display_name();
                &substituted_expected
            } else {
                &expected_declared_str
            };
            // A protocol-typed parameter is registered as a synthetic
            // sealed-abstract class entry by `register_protocol_classes`,
            // so `hierarchy.has_class` sees it and the nominal
            // walk below no longer takes the "unknown type → compatible"
            // escape hatch. Nominal subtyping doesn't apply to protocols —
            // conformance is structural — so defer entirely to
            // `check_protocol_conformance_in_module` /
            // `check_protocol_argument_conformance` (which run later in
            // `check_module_with_protocols` and consult the protocol
            // registry) rather than flagging a false "expects P, got C"
            // mismatch here for an argument that structurally conforms.
            // Applies to both class-side and instance-side params — the
            // nominal walk has no protocol awareness on either path.
            if hierarchy.is_protocol_class(type_resolver::base_name_of_string(expected_ty)) {
                continue;
            }
            // `method.param_types` predates ADR 0108 and stores a
            // bare `EcoString` with no alias provenance — unlike an
            // `InferredType`, which tags via
            // `TypeProvenance::Aliased` when resolved through
            // `resolve_type_annotation`. Re-resolve `expected_ty` through
            // `AliasRegistry::resolve_display_name` when it names a
            // registered alias so this diagnostic renders `AliasName
            // (expansion)` just like a mismatched alias-typed local/return
            // type already does; fall back to the existing `UndefinedObject`
            // -> `Nil` display otherwise. Computed once here
            // (after the protocol-type early-continue above, which never
            // needs it) and reused by every `arg_ty` arm below.
            let expected_display = self
                .alias_registry
                .as_ref()
                .and_then(|registry| registry.resolve_display_name(expected_ty))
                .unwrap_or_else(|| InferredType::class_name_for_diagnostic(expected_ty.as_str()));
            // `expected_display` above only fixes the diagnostic
            // text (display-only scope). `is_type_compatible`
            // below still has an "unknown declared class → conservatively
            // compatible" escape hatch that fires on the unresolved bare
            // alias name, making argument-type checking against an
            // alias-typed parameter a silent no-op. Resolve to the
            // structural expansion so the checks below actually validate.
            let (expected_structural, alias_deps) =
                Self::resolve_alias_structural(expected_ty, self.alias_registry.as_deref());
            self.referenced_aliases.extend(alias_deps);
            let is_class_ref_arg = arg_exprs
                .and_then(|exprs| exprs.get(i))
                .is_some_and(|e| matches!(e, Expression::ClassReference { .. }));
            // Complementary to the `expected_structural` resolution
            // above (which only resolves the *declared/expected*
            // side) — `arg_ty` itself can be a bare `InferredType::Known`
            // naming a registered alias on the *actual* side, e.g. a message
            // send whose callee's `MethodInfo::return_type` is the raw alias
            // name `RestartStrategy` (send-site resolution expands aliases
            // only when a registry is threaded through — several
            // `resolve_declared_type` call sites, e.g. the class-object tower,
            // pass `None`).
            // Structural compatibility below (`is_type_compatible` and the
            // `InferredType::Union` arm's `classify_union_members`) only
            // understands the *expansion*, not the alias name, so re-resolve
            // `arg_ty` through the same `resolve_type_annotation` entry point
            // every declared annotation uses when its class name is a known
            // alias — this reuses the existing, tested `Known`/`Union`
            // compatibility and diagnostic paths unchanged rather than
            // special-casing alias names here.
            let resolved_arg_ty = match arg_ty {
                InferredType::Known { class_name, .. } => self
                    .alias_registry
                    .as_ref()
                    .and_then(|registry| registry.get(class_name))
                    .map(|info| {
                        let reference = TypeAnnotation::Simple(crate::ast::Identifier {
                            name: info.name.clone(),
                            span: info.span,
                        });
                        let (resolved, arg_alias_deps) =
                            type_resolver::resolve_type_annotation_with_alias_deps(
                                &reference,
                                &HashMap::new(),
                                None,
                                self.alias_registry.as_deref(),
                            );
                        self.referenced_aliases.extend(arg_alias_deps);
                        resolved
                    }),
                _ => None,
            };
            let arg_ty = resolved_arg_ty.as_ref().unwrap_or(arg_ty);
            match arg_ty {
                InferredType::Known {
                    class_name: actual_ty,
                    ..
                } => {
                    // Short-circuit: only consult the metaclass chain when the
                    // instance-side check fails. Keeps the hot path at one
                    // `is_type_compatible` call per argument.
                    //
                    // `is_type_compatible` has a shortcut where
                    // `expected == "Class"` accepts any known class name
                    // unconditionally. With the metaclass-tower check
                    // handling class literals properly, that shortcut must be
                    // scoped to class-literal arguments here so a plain
                    // `TestCase` instance does not satisfy a `:: Class`
                    // parameter.
                    //
                    // This guard must
                    // check `expected_structural`, not the bare `expected_ty`
                    // — otherwise `type MyAlias = Class` would resolve to
                    // `expected_structural == "Class"` while `expected_ty`
                    // still names the alias, silently skipping the guard and
                    // letting `is_type_compatible`'s shortcut accept
                    // any known class instance against a `:: MyAlias` param.
                    //
                    // With the shortcut suppressed, a value whose own type is
                    // `Class` or a subclass of it (e.g. a forwarded `:: Class`
                    // parameter, BT-4599) is still compatible: check that by
                    // the nominal chain directly, bypassing the shortcut.
                    let class_shortcut_applies =
                        expected_structural.as_str() == "Class" && !is_class_ref_arg;
                    let instance_compat = if class_shortcut_applies {
                        InferredType::is_nominal_subtype(hierarchy, actual_ty, "Class")
                    } else {
                        Self::is_type_compatible(actual_ty, &expected_structural, hierarchy)
                    };
                    let class_literal_compat = !instance_compat
                        && is_class_ref_arg
                        && hierarchy.has_class(actual_ty)
                        && Self::is_type_compatible(
                            &metaclass_name,
                            &expected_structural,
                            hierarchy,
                        );
                    if !instance_compat && !class_literal_compat {
                        let param_pos = i + 1;
                        // Use hint severity for generic type params (likely false positive)
                        let is_generic =
                            super::super::is_type_var_placeholder(actual_ty, hierarchy);
                        // Render `UndefinedObject` as `Nil` in user-facing messages.
                        let actual_display =
                            InferredType::class_name_for_diagnostic(actual_ty.as_str());
                        let expected_display = expected_display.clone();
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
                        // Attach origin note if available
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
                InferredType::Meta { .. } | InferredType::ProtocolMeta { .. } => {
                    // ADR 0083: a metatype value `C class` is an instance of the
                    // metaclass tower (`Meta{C} <: Class <: Behaviour <: Object`).
                    // It satisfies a parameter whose declared type is `Class`,
                    // `Behaviour`, `Object`, `ProtoObject` — anything `Class`
                    // (the runtime class of a class object) is assignable to.
                    // We model the metatype's own type as `Class` for the
                    // hierarchy walk, so e.g. `:: Behaviour` accepts it but
                    // `:: Integer` does not. ADR 0144: a `P class` parameter
                    // accepts `C class` iff `C` conforms to `P`.
                    let Some(class_object) = ClassObjectType::of(arg_ty) else {
                        continue;
                    };
                    let compat = Self::metatype_assignable_to(
                        class_object,
                        &expected_structural,
                        hierarchy,
                        self.protocol_registry.as_deref(),
                        Self::is_type_compatible,
                    );
                    if !compat {
                        let param_pos = i + 1;
                        let expected_display = expected_display.clone();
                        let diag = Self::metatype_mismatch_diagnostic(
                            class_object,
                            &expected_structural,
                            hierarchy,
                            self.protocol_registry.as_deref(),
                            format!(
                                "Argument {param_pos} of '{selector}' on {class_name} expects {expected_display}, got {class_object}"
                            ),
                            format!(
                                "Expected {expected_display} (or a subclass), got {class_object}"
                            ),
                            span,
                        );
                        self.diagnostics.push(diag);
                    }
                }
                InferredType::Union { members, .. } => {
                    // BT-4454: class-object members are judged by the slot
                    // rule, as a bare class object is above.
                    let param_pos = i + 1;
                    if let Some(diag) = self.union_class_object_diagnostic(
                        members,
                        &expected_structural,
                        hierarchy,
                        Self::is_type_compatible,
                        span,
                        |class_object| {
                            (
                                format!(
                                    "Argument {param_pos} of '{selector}' on {class_name} expects {expected_display}, got {class_object}"
                                ),
                                format!(
                                    "Expected {expected_display} (or a subclass), got {class_object}"
                                ),
                            )
                        },
                    ) {
                        self.diagnostics.push(diag);
                        continue;
                    }
                    // Check every other union member against the
                    // expected type via the shared classification.
                    let instance_members = Self::non_class_object_members(members);
                    let (compat, incompatible) = match Self::classify_union_arg_compat(
                        &instance_members,
                        &expected_structural,
                        hierarchy,
                    ) {
                        UnionArgCompat::AllCompatible | UnionArgCompat::Unclassifiable => continue,
                        UnionArgCompat::SomeIncompatible {
                            compat,
                            incompatible,
                            ..
                        } => (compat, incompatible),
                    };
                    // Accepted class objects are compatible members too.
                    let compat = compat + (members.len() - instance_members.len());
                    let union_display = arg_ty
                        .display_for_diagnostic()
                        .unwrap_or_else(|| EcoString::from("Dynamic"));
                    let expected_display = expected_display.clone();
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
                // `Intersection` (`P1 & P2`, ADR 0102) as the
                // *argument's own* inferred type is likewise skipped — see
                // `check_protocol_argument_conformance` for the flagship
                // "parameter declared `P1 & P2`" conformance check.
                // A shape argument was replaced by the `Dictionary` stand-in
                // above, so it is unreachable here.
                InferredType::Dynamic(_)
                | InferredType::Never
                | InferredType::Negation { .. }
                | InferredType::Intersection { .. }
                | InferredType::Shape { .. } => {}
            }
        }
    }

    /// BT-4385: when `arg` is a non-empty list literal
    /// and the parameter declared `expected_declared` takes a list with a
    /// concrete element type, the literal's elements and that element type.
    /// The argument is then judged element by element
    /// ([`Self::check_list_literal_elements`]), and the whole-argument
    /// variance check (`check_variance_in_expr`) leaves it alone, so one
    /// mistake gets one warning.
    ///
    /// The declared type may be `List(E)`, an alias of it, or a union with
    /// exactly one `List` arm (`List(RegexOption) | Nil`). It is `None`, and
    /// the argument keeps the whole-argument checks, when `E` mentions:
    /// - a type param of the receiver class or a superclass (`E` on
    ///   `List>>++`, `Item` on a user `Box(Item)`), resolved to `Dynamic`
    ///   here, or a method-local type variable (`T`);
    /// - a protocol: conformance is structural, and the per-element
    ///   assignability rule is nominal (BT-4482).
    ///
    /// A cons literal (`#(a | rest)`) is judged too: each head is an
    /// element, and the tail's own element type (`E'` of its `List(E')`) is
    /// checked against `E` as one more entry.
    pub(in super::super) fn list_literal_element_expectation<'a>(
        &mut self,
        arg: &'a Expression,
        expected_declared: &DeclaredType,
        class_name: &str,
        hierarchy: &ClassHierarchy,
    ) -> Option<ListLiteralElements<'a>> {
        // `ast::normalize` (BT-4059) already peeled parens off a send argument.
        let Expression::ListLiteral { elements, tail, .. } = arg else {
            return None;
        };
        if elements.is_empty() {
            return None;
        }
        let class_type_params: type_resolver::SubstitutionMap =
            std::iter::once(EcoString::from(class_name))
                .chain(hierarchy.superclass_chain(class_name))
                .filter_map(|name| hierarchy.get_class(&name))
                .flat_map(|info| info.type_params.iter().cloned())
                .map(|param| (param, InferredType::Dynamic(DynamicReason::Unknown)))
                .collect();
        let (resolved, alias_deps) = type_resolver::resolve_declared_type_with_alias_deps(
            expected_declared,
            &class_type_params,
            self.protocol_registry.as_deref(),
            self.alias_registry.as_deref(),
            super::super::TypeStringContext::Declared,
        );
        self.referenced_aliases.extend(alias_deps);
        let list_element = |ty: &InferredType| match ty {
            InferredType::Known {
                class_name,
                type_args,
                ..
            } if class_name == "List" && type_args.len() == 1 => Some(type_args[0].clone()),
            _ => None,
        };
        let element = match &resolved {
            InferredType::Union { members, .. } => {
                let mut arms = members.iter().filter_map(list_element);
                let first = arms.next()?;
                if arms.next().is_some() {
                    return None;
                }
                first
            }
            other => list_element(other)?,
        };
        if Self::element_type_is_unjudged(&element, hierarchy) {
            return None;
        }
        Some(ListLiteralElements {
            elements,
            tail: tail.as_deref(),
            element,
        })
    }

    /// Whether a declared list element type is left to the whole-argument
    /// checks ([`Self::list_literal_element_expectation`]): it is `Dynamic`,
    /// or mentions a type variable placeholder anywhere in it. A protocol
    /// element type is judged: the per-element rule checks structural
    /// conformance (BT-4482).
    fn element_type_is_unjudged(ty: &InferredType, hierarchy: &ClassHierarchy) -> bool {
        match ty {
            InferredType::Dynamic(_) => true,
            InferredType::Known {
                class_name,
                type_args,
                ..
            } => {
                super::super::is_type_var_placeholder(class_name, hierarchy)
                    || type_args
                        .iter()
                        .any(|arg| Self::element_type_is_unjudged(arg, hierarchy))
            }
            InferredType::Union { members, .. } => members
                .iter()
                .any(|member| Self::element_type_is_unjudged(member, hierarchy)),
            InferredType::Shape { fields, .. } => fields
                .iter()
                .any(|f| Self::element_type_is_unjudged(&f.value, hierarchy)),
            InferredType::Meta { .. }
            | InferredType::ProtocolMeta { .. }
            | InferredType::Never
            | InferredType::Negation { .. }
            | InferredType::Intersection { .. } => false,
        }
    }

    /// BT-4385: a list literal `#(...)` passed to a parameter declared
    /// `List(E)` checks each element against `E`, by the same rule as an
    /// annotated local ([`Self::detect_assignment_type_mismatch`]). The
    /// argument's own joined `List(E')` type cannot carry this: a joined
    /// element type loses which element is wrong, and the nominal argument
    /// check compares only the `List` base. Which arguments are judged, and
    /// against what, is [`Self::list_literal_element_expectation`].
    fn check_list_literal_elements(
        &mut self,
        class_name: &EcoString,
        selector: &str,
        param_pos: usize,
        arg: &Expression,
        expected_declared: &DeclaredType,
        hierarchy: &ClassHierarchy,
    ) {
        let Some(ListLiteralElements {
            elements,
            tail,
            element: element_declared,
        }) = self.list_literal_element_expectation(arg, expected_declared, class_name, hierarchy)
        else {
            return;
        };
        let element_declared = &element_declared;
        // Each head, then the tail's element type (`E'` of its `List(E')`).
        let entries = elements
            .iter()
            .enumerate()
            .map(|(j, element)| {
                (
                    format!("Element {}", j + 1),
                    element.span(),
                    self.type_map.get(element.span()).cloned(),
                )
            })
            .chain(tail.map(|tail| {
                (
                    "Each tail element".to_string(),
                    tail.span(),
                    self.type_map.get(tail.span()).map(Self::tail_element_type),
                )
            }))
            .collect::<Vec<_>>();
        for (label, span, element_ty) in entries {
            let Some(element_ty) = element_ty else {
                continue;
            };
            let Some(mismatch) = Self::detect_assignment_type_mismatch(
                &element_ty,
                element_declared,
                hierarchy,
                self.protocol_registry.as_deref(),
            ) else {
                continue;
            };
            let diag = if let Some(nonconformance) = &mismatch.nonconformance {
                Self::protocol_metatype_nonconformance_diagnostic(
                    &mismatch.inferred_display,
                    nonconformance,
                    span,
                )
            } else {
                Diagnostic::warning(
                    format!(
                        "{label} of argument {param_pos} of '{selector}' on {class_name} expects {}, got {}",
                        mismatch.declared_display, mismatch.inferred_display
                    ),
                    span,
                )
                .with_category(DiagnosticCategory::Type)
                .with_hint(format!(
                    "Every element of the list must be assignable to {}",
                    mismatch.declared_display
                ))
            };
            self.diagnostics.push(diag);
        }
    }

    /// BT-4090: whether `expr` contains a `self deferReply` send (descending
    /// into blocks and arguments).
    pub(super) fn contains_defer_reply_send(expr: &Expression) -> bool {
        let mut found = false;
        crate::ast_walker::walk_expression(expr, &mut |e| {
            if let Expression::MessageSend {
                receiver,
                selector: crate::ast::MessageSelector::Unary(name),
                ..
            } = e
            {
                if name == "deferReply"
                    && matches!(&**receiver, Expression::Identifier(id) if id.name == "self")
                {
                    found = true;
                }
            }
        });
        found
    }

    /// Validate binary message operand types for arithmetic and string concatenation.
    ///
    /// When both receiver and argument types are known, checks that the argument type
    /// is compatible with the operator. Only emits warnings (not errors) to allow
    /// for dynamic dispatch.
    /// Returns `true` when this function has specific arithmetic/comparison/
    /// concat logic for the `(operator, receiver_ty)` shape — regardless of
    /// whether the argument turned out to be compatible (i.e. `true` even
    /// when no diagnostic was emitted). `false` means this operator/receiver
    /// combination has no bespoke check here (e.g. `String>>,`, `Integer>>**`,
    /// or any other binary selector outside +-*/ / </>/<=/>= / `++` on
    /// String) — the caller must fall back to the generic
    /// `check_argument_types` rather than treat "resolved on the
    /// receiver" as "checked".
    pub(in super::super) fn check_binary_operand_types(
        &mut self,
        receiver_ty: &EcoString,
        operator: &str,
        arg_ty: &EcoString,
        span: Span,
        hierarchy: &ClassHierarchy,
        arg_origin: Option<&(EcoString, Option<Span>)>,
    ) -> bool {
        let is_numeric = |ty: &str| hierarchy.is_numeric_type(ty);
        let is_arithmetic = matches!(operator, "+" | "-" | "*" | "/");
        let is_comparison = matches!(operator, "<" | ">" | "<=" | ">=");
        let is_concat = operator == "++"
            && WellKnownClass::from_str(receiver_ty) == Some(WellKnownClass::String);
        let is_generic = super::super::is_type_var_placeholder(arg_ty, hierarchy);
        // Render `UndefinedObject` as `Nil` in user-facing messages.
        let arg_display = InferredType::class_name_for_diagnostic(arg_ty.as_str());
        let receiver_display = InferredType::class_name_for_diagnostic(receiver_ty.as_str());

        // Comparison operators require an argument of the receiver's own kind:
        // numeric for Integer/Float, otherwise Character, String or Symbol.
        // Anything else falls back to Erlang term order at runtime (BT-3928),
        // so the compiler is the only place the mismatch can be reported.
        let comparison_expectation = Self::comparison_arg_expectation(receiver_ty, hierarchy);

        // Whether this call has bespoke logic for `operator` on
        // `receiver_ty` — computed from the same guards each branch below
        // uses, so it stays in lockstep with what's actually checked. If a
        // new operator/receiver shape gets a branch below, its guard must be
        // added here too, or the caller will wrongly treat it as `handled`
        // and skip the generic `check_argument_types` fallback.
        let handled = (is_arithmetic && is_numeric(receiver_ty))
            || is_concat
            || (is_comparison && comparison_expectation.is_some());

        // Arithmetic operators on numeric types require numeric arguments
        if is_arithmetic && is_numeric(receiver_ty) && !is_numeric(arg_ty) {
            // Use hint severity for generic type params (likely false positive)
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
            return handled;
        }

        // String concatenation with ++ expects a String argument
        if is_concat
            && WellKnownClass::from_str(arg_ty) != Some(WellKnownClass::String)
            && arg_ty.as_str() != "Symbol"
            && !is_symbol_singleton_name(arg_ty)
        {
            // Use hint severity for generic type params (likely false positive)
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
            return handled;
        }

        // Comparison operators require an argument of the receiver's kind
        if let Some(expected) =
            comparison_expectation.filter(|e| is_comparison && !e.accepts(arg_ty, hierarchy))
        {
            let expected_label = expected.label();
            let mut diag = if is_generic {
                Diagnostic::hint(
                    format!(
                        "`{operator}` on {receiver_display} expects a {expected_label} argument, got {arg_display}"
                    ),
                    span,
                )
            } else {
                Diagnostic::warning(
                    format!(
                        "`{operator}` on {receiver_display} expects a {expected_label} argument, got {arg_display}"
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
        handled
    }

    /// Warn when an `Exception`-typed expression is compared for equality
    /// with a Symbol literal (BT-4023).
    ///
    /// The FFI boundary wraps every `{error, Reason}` in an Exception
    /// (ADR 0101), so an FFI Result's `error` is typed `Exception` and
    /// `r error =:= #closed` is always false at runtime — the raw reason is
    /// `r error reason`. Covers every equality operator
    /// ([`crate::source_analysis::is_equality_operator`]: `=:=`, `==`, `=/=`,
    /// `/=`; Beamtalk has no bare `=` or `~=`) in either operand order, for
    /// `Exception` and its subclasses. Deliberately targeted: this is not a
    /// general disjoint-type comparison lint.
    #[allow(clippy::too_many_arguments)] // two operand (expr, type) pairs + context
    pub(in super::super) fn check_exception_symbol_comparison(
        &mut self,
        operator: &str,
        receiver: &Expression,
        receiver_ty: &InferredType,
        arg: &Expression,
        arg_ty: Option<&InferredType>,
        span: Span,
        hierarchy: &ClassHierarchy,
    ) {
        let Some(negated) = equality_operator_is_negated(operator) else {
            return;
        };
        let is_exception = |ty: Option<&InferredType>| {
            matches!(ty, Some(InferredType::Known { class_name, .. })
                if class_name.as_str() == "Exception"
                    || hierarchy
                        .superclass_chain(class_name)
                        .iter()
                        .any(|s| s.as_str() == "Exception"))
        };
        let is_symbol_literal = |e: &Expression| {
            matches!(
                e.unwrap_parens(),
                Expression::Literal(Literal::Symbol(_), _)
            )
        };

        let (exception_expr, exception_ty) =
            if is_exception(Some(receiver_ty)) && is_symbol_literal(arg) {
                (receiver, Some(receiver_ty))
            } else if is_exception(arg_ty) && is_symbol_literal(receiver) {
                (arg, arg_ty)
            } else {
                return;
            };
        // A structured Beamtalk error (an `Error` subclass, e.g. a shim's
        // `#beamtalk_error{}`) usually records no raw `reason`; its Symbol
        // tag is `kind`. Only a plain `Exception` is purely an FFI wrapper.
        let is_plain_exception = matches!(exception_ty,
            Some(InferredType::Known { class_name, .. }) if class_name.as_str() == "Exception");

        let render = |e: &Expression| crate::unparse::unparse_expression(e).to_pretty_string();
        // Appending the unary `reason` binds tighter than a binary/keyword
        // send, cascade or assignment, so those need parentheses.
        let needs_parens = match exception_expr {
            Expression::MessageSend { selector, .. } => {
                !matches!(selector, crate::ast::MessageSelector::Unary(_))
            }
            Expression::Assignment { .. } | Expression::Cascade { .. } => true,
            _ => false,
        };
        let exception_src = render(exception_expr);
        let fixed = if needs_parens {
            format!("({exception_src}) reason")
        } else {
            format!("{exception_src} reason")
        };
        let suggestion = if std::ptr::eq(exception_expr, receiver) {
            format!("{fixed} {operator} {}", render(arg))
        } else {
            format!("{} {operator} {fixed}", render(receiver))
        };
        let outcome = if negated { "true" } else { "false" };
        self.diagnostics.push(
            Diagnostic::warning(
                format!(
                    "comparing an Exception to a Symbol with `{operator}` is always {outcome}; use `{fixed}`"
                ),
                span,
            )
            .with_category(DiagnosticCategory::Type)
            .with_hint(if is_plain_exception {
                format!("did you mean `{suggestion}`?")
            } else {
                format!(
                    "did you mean `{suggestion}`? For a structured Beamtalk error, \
                     compare its `kind` instead"
                )
            }),
        );
    }

    /// The argument kind a comparison (`<` `>` `<=` `>=`) expects for `receiver_ty`.
    ///
    /// `None` when the receiver is not a primitive kind with a defined ordering
    /// (the generic argument check applies instead). `Character` is checked
    /// before the numeric case because it inherits from `Integer`.
    fn comparison_arg_expectation(
        receiver_ty: &str,
        hierarchy: &ClassHierarchy,
    ) -> Option<ComparisonArgKind> {
        if receiver_ty == "Character" {
            Some(ComparisonArgKind::Character)
        } else if hierarchy.is_numeric_type(receiver_ty) {
            Some(ComparisonArgKind::Numeric)
        } else if WellKnownClass::from_str(receiver_ty) == Some(WellKnownClass::String) {
            Some(ComparisonArgKind::String)
        } else if receiver_ty == "Symbol" || is_symbol_singleton_name(receiver_ty) {
            Some(ComparisonArgKind::Symbol)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ComparisonArgKind {
    Numeric,
    Character,
    String,
    Symbol,
}

impl ComparisonArgKind {
    fn label(self) -> &'static str {
        match self {
            Self::Numeric => "numeric",
            Self::Character => "Character",
            Self::String => "String",
            Self::Symbol => "Symbol",
        }
    }

    fn accepts(self, arg_ty: &str, hierarchy: &ClassHierarchy) -> bool {
        match self {
            Self::Numeric => hierarchy.is_numeric_type(arg_ty),
            Self::Character => arg_ty == "Character",
            Self::String => WellKnownClass::from_str(arg_ty) == Some(WellKnownClass::String),
            Self::Symbol => arg_ty == "Symbol" || is_symbol_singleton_name(arg_ty),
        }
    }
}

/// BT-4385: a list-literal argument judged element by element — see
/// [`TypeChecker::list_literal_element_expectation`].
pub(in super::super) struct ListLiteralElements<'a> {
    /// The literal's heads.
    elements: &'a [Expression],
    /// A cons literal's tail (`rest` in `#(a | rest)`).
    tail: Option<&'a Expression>,
    /// The declared element type `E` of the parameter's `List(E)`.
    element: InferredType,
}
