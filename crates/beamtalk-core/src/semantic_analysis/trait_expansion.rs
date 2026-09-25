// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Flattening pass — expand `uses:` into classes (ADR 0127 §3, phase 2).
//!
//! **DDD Context:** Semantic Analysis
//!
//! A class that `uses:` a protocol (a *trait* — a protocol with at least
//! one provided method, ADR 0127 §1) means exactly the class with the
//! protocol's provided methods written into its own body. This pass makes
//! that literally true at the AST level, so every consumer *of the module
//! this pass produces* — `ClassHierarchy`, the type checker, protocol
//! conformance — sees an ordinary class with ordinary methods and never
//! needs to know `uses:` exists.
//!
//! **Codegen does not see this module yet.** `expand_module` is wired into
//! `analyse_full` (below), which only ever receives `&Module` and flattens
//! a *clone* for its own internal use — the diagnostics, `ClassHierarchy`
//! and `ProtocolRegistry` this pass feeds are correct, but the caller's own
//! `Module` (the one every driver later hands to
//! `lower_module_for_codegen`/`generate_module`, e.g.
//! `beam_compiler.rs`'s `compile_module`) is never mutated, so a class's
//! flattened provisions do not yet reach compiled output. Wiring the
//! flattened module through to codegen is BT-3590's job ("Codegen for
//! flattened classes…"), not this pass's.
//!
//! This is the **expansion** half of ADR 0127 §3's two-part pass (steps
//! 1–4, 6: collect provisions, apply `excluding:`, merge with conflict
//! detection, drop provisions the class body already defines — "class
//! wins" — and treat the result as the class's own body). It runs
//! **before** `ClassHierarchy` is built, exactly where the ADR places it,
//! so every later phase (protocol conformance, type inference, sendability,
//! definite assignment) sees the flattened class. The **requirement**
//! half (ADR 0127 §3 step 5, §3a: resolving each used trait's required
//! selectors and checking the `overriding:` acknowledgement) needs
//! `ClassHierarchy::resolves_selector` over the *flattened* hierarchy, so
//! it necessarily runs afterward — that is BT-3589's pass, not this one.
//!
//! # Inputs and boundaries
//!
//! [`expand_module`] takes only `&mut Module` — a deliberately narrow,
//! explicit-input design (no hidden global registry lookup) that mirrors
//! [`crate::semantic_analysis::class_kind_writeback::apply_class_kind_writeback`].
//! A used protocol is resolved from `module.protocols` alone: only
//! same-module traits flatten today. Carrying a trait's AST across files
//! (ADR 0127 §10a, "Carrying trait ASTs to users") is BT-3591's build-graph
//! work; until then, a cross-file or cross-package `uses:` reports
//! [`Diagnostic::error`] "unknown protocol" the same way a genuinely
//! misspelled name would, rather than silently doing nothing.
//!
//! # `MethodInfo::origin`
//!
//! This pass cannot stamp [`MethodInfo::origin`] itself — `ClassHierarchy`
//! doesn't exist yet when it runs. It instead returns an [`OriginMap`]
//! (`(class, selector) -> protocol`) that the caller applies to the
//! hierarchy's `MethodInfo` entries once they exist; see that field's own
//! doc for the full reasoning and [`apply_origins`] below.
//!
//! [`MethodInfo::origin`]: crate::semantic_analysis::class_hierarchy::MethodInfo::origin

use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};

use ecow::EcoString;

use crate::ast::{
    ClassDefinition, ClassKind, Identifier, MethodDefinition, Module, ParameterDefinition,
    ProtocolDefinition, ProtocolUse, TypeAnnotation, TypeParamDecl,
};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::type_checker::is_generic_type_param;
use crate::source_analysis::Diagnostic;

/// Maps a flattened method back to the protocol that provided it, keyed by
/// `(using_class_name, selector)`. See the module doc's "`MethodInfo::origin`"
/// section for why this can't be stamped onto `MethodInfo` directly here.
pub type OriginMap = HashMap<(EcoString, EcoString), EcoString>;

/// Expand every `uses:` line in `module`'s classes in place (ADR 0127 §3
/// steps 1–4, 6).
///
/// Returns the diagnostics this pass produced (unknown protocols, provision
/// conflicts, the body-less-protocol hint) and the [`OriginMap`] the caller
/// applies to `MethodInfo::origin` after building `ClassHierarchy` from the
/// now-flattened `module` (see [`apply_origins`]).
///
/// A class with no `uses:` lines is untouched and contributes nothing to
/// either return value — this function is a no-op for every module that
/// doesn't use ADR 0127 traits, so callers can run it unconditionally.
pub fn expand_module(module: &mut Module) -> (Vec<Diagnostic>, OriginMap) {
    let mut diagnostics = Vec::new();
    let mut origins = OriginMap::new();

    if !module.classes.iter().any(|c| !c.uses.is_empty()) {
        return (diagnostics, origins);
    }

    // Look up protocols by name once, before mutating any class — a
    // protocol's provisions never change while flattening its users, and
    // borrowing `module.protocols` for the whole loop while also mutating
    // `module.classes` would need this same split regardless.
    let protocols: HashMap<&EcoString, &ProtocolDefinition> =
        module.protocols.iter().map(|p| (&p.name.name, p)).collect();

    for class in &mut module.classes {
        if class.uses.is_empty() {
            continue;
        }
        expand_class(class, &protocols, &mut diagnostics, &mut origins);
    }

    (diagnostics, origins)
}

/// Applies an [`OriginMap`] returned by [`expand_module`] to `hierarchy`'s
/// `MethodInfo` entries, once it has been built from the same (already
/// flattened) module.
///
/// Silently skips an entry whose class or selector isn't found — that can
/// only mean a downstream diagnostic already reported the class as
/// otherwise broken (e.g. a namespace collision that kept it out of the
/// hierarchy); it is not this function's place to report it again.
pub fn apply_origins(hierarchy: &mut ClassHierarchy, origins: &OriginMap) {
    if origins.is_empty() {
        return;
    }
    for ((class_name, selector), protocol_name) in origins {
        if let Some(class_info) = hierarchy.classes_mut().get_mut(class_name.as_str()) {
            if let Some(method) = class_info
                .methods
                .iter_mut()
                .find(|m| &m.selector == selector)
            {
                method.origin = Some(protocol_name.clone());
            }
        }
    }
}

/// One provision merged into `class`, tracked with the protocol it came
/// from — needed both to detect a same-selector conflict between two
/// *different* protocols (ADR 0127 §4) and to populate the [`OriginMap`].
struct MergedProvision {
    method: MethodDefinition,
    protocol_name: EcoString,
}

/// Flattens every `uses:` line of a single `class` (ADR 0127 §3 steps 1–4, 6).
fn expand_class(
    class: &mut ClassDefinition,
    protocols: &HashMap<&EcoString, &ProtocolDefinition>,
    diagnostics: &mut Vec<Diagnostic>,
    origins: &mut OriginMap,
) {
    // Step 4 precondition: everything the class body already defines "wins"
    // outright, so it must never even reach the merge step below (ADR 0127
    // §3: "Drop every trait provision whose selector … the class body
    // defines"). Synthesised `Value` slot accessors count as class body too
    // (§3, "Synthesised methods rank as class body") — reusing
    // `synthetic_selectors` rather than re-deriving the naming convention.
    // `class.class_kind` is still the parser's shallow, direct-superclass
    // placeholder here (`apply_class_kind_writeback` runs later, at
    // codegen-lowering time) — this only misses a class whose Value-ness is
    // *indirect* (`SomeValueSubclass subclass: X`), a narrow gap shared
    // with every other pre-hierarchy AST pass.
    let mut own_selectors: HashSet<EcoString> =
        class.methods.iter().map(|m| m.selector.name()).collect();
    if class.class_kind == ClassKind::Value {
        for slot in &class.state {
            own_selectors.insert(slot.name.name.clone());
            own_selectors.insert(EcoString::from(
                crate::synthetic_selectors::with_star_selector(slot.name.name.as_str()),
            ));
        }
    }

    // Steps 1–3: collect each used protocol's provisions (after
    // `excluding:`), merging into one set and detecting same-selector
    // conflicts between *different* protocols (§4). A selector the class
    // already owns (`own_selectors`) never enters this map at all, so two
    // protocols racing to provide a selector the class also defines itself
    // is not a conflict — matches the ADR's own `Report` example.
    let mut merged: HashMap<EcoString, MergedProvision> = HashMap::new();
    let mut conflicted: HashSet<EcoString> = HashSet::new();

    for use_ in &class.uses {
        // A package-qualified `uses:` (`uses: json@Parser`) or a name not
        // defined in this module is not resolvable yet — cross-file trait
        // ASTs are BT-3591 (module doc). Matching by bare name only when
        // `package` is `None` avoids accidentally flattening an unrelated
        // same-named local protocol for a qualified reference.
        let protocol = if use_.package.is_none() {
            protocols.get(&use_.protocol.name).copied()
        } else {
            None
        };
        let Some(protocol) = protocol else {
            diagnostics.push(
                Diagnostic::error(
                    format!("unknown protocol `{}`", use_.protocol.name),
                    use_.protocol.span,
                )
                .with_hint(
                    "cross-file and cross-package traits aren't supported yet — \
                     define the protocol in this file, or check the spelling",
                ),
            );
            continue;
        };

        if protocol.provided_methods.is_empty() {
            // ADR 0127 §1, §Status 6: a hint, not a warning, so a protocol
            // gaining or losing its last provision never flips a
            // `--warnings-as-errors` build.
            diagnostics.push(Diagnostic::hint(
                format!(
                    "`{}` provides no methods, so `uses:` only checks its requirements here; \
                     conformance is structural (ADR 0068)",
                    protocol.name.name
                ),
                use_.span,
            ));
            continue;
        }

        let excluding: HashSet<&str> = use_.excluding.iter().map(|id| id.name.as_str()).collect();

        for method in &protocol.provided_methods {
            let selector = method.selector.name();
            if excluding.contains(selector.as_str()) {
                continue;
            }
            if own_selectors.contains(&selector) {
                continue;
            }

            let flattened = substitute_provision(method, use_, protocol, class);

            match merged.entry(selector.clone()) {
                Entry::Occupied(existing) => {
                    if existing.get().protocol_name != protocol.name.name {
                        conflicted.insert(selector.clone());
                        diagnostics.push(conflict_diagnostic(
                            &class.name.name,
                            &selector,
                            &existing.get().protocol_name,
                            &protocol.name.name,
                            use_.span,
                        ));
                    }
                    // Same protocol reached twice (e.g. a redundant repeated
                    // `uses:` line) — not a conflict; the first copy stands.
                }
                Entry::Vacant(slot) => {
                    slot.insert(MergedProvision {
                        method: flattened,
                        protocol_name: protocol.name.name.clone(),
                    });
                }
            }
        }
    }

    // Step 6: splice every non-conflicting provision into the class's own
    // body, as if it had been written there — nothing downstream needs to
    // know it came from a protocol except via `origins`. Sorted by selector
    // first: `merged` is a `HashMap`, whose iteration order is randomised
    // per process (`RandomState`) — pushing straight from it would make
    // `class.methods`' tail order, and therefore codegen's emitted function
    // order, non-deterministic across runs of the same compiler on the same
    // source. A deterministic order also keeps diagnostics and any future
    // xref/source-map output (BT-3590) reproducible.
    let mut flattened: Vec<(EcoString, MergedProvision)> = merged
        .into_iter()
        .filter(|(selector, _)| !conflicted.contains(selector))
        .collect();
    flattened.sort_by(|(a, _), (b, _)| a.cmp(b));
    for (selector, provision) in flattened {
        origins.insert((class.name.name.clone(), selector), provision.protocol_name);
        class.methods.push(provision.method);
    }
}

/// Builds the "`sel` is provided by both A and B in C" conflict error (ADR
/// 0127 §4, §13), naming both protocols and both fixes.
fn conflict_diagnostic(
    class_name: &str,
    selector: &str,
    first_protocol: &str,
    second_protocol: &str,
    span: crate::source_analysis::Span,
) -> Diagnostic {
    Diagnostic::error(
        format!("`{selector}` is provided by both {first_protocol} and {second_protocol} in {class_name}"),
        span,
    )
    .with_hint(format!(
        "define `{selector}` in {class_name}, or exclude one: `uses: {second_protocol} excluding: #(#{selector})`"
    ))
}

/// Produces `class`'s own copy of a protocol's provided `method` — ADR 0127
/// §3's hygienic substitution: the `uses:` type arguments for the
/// protocol's type parameters, `Self` for the using class (with its own
/// type parameters for a generic user), and method-local type variables
/// alpha-renamed first so they can't be captured by either substitution.
fn substitute_provision(
    method: &MethodDefinition,
    use_: &ProtocolUse,
    protocol: &ProtocolDefinition,
    class: &ClassDefinition,
) -> MethodDefinition {
    let protocol_type_param_names: HashSet<&str> = protocol
        .type_params
        .iter()
        .map(|tp| tp.name.name.as_str())
        .collect();
    let class_type_param_names: HashSet<&str> = class
        .type_params
        .iter()
        .map(|tp| tp.name.name.as_str())
        .collect();

    // Protocol type param name -> the `uses:` type argument in that
    // position (`uses: Enumerable(Worker)` maps `E -> Worker`). A `uses:`
    // that under-supplies type arguments (fewer args than the protocol has
    // params) leaves the trailing params unsubstituted — arity mismatches
    // there are a separate, not-yet-implemented diagnostic; this pass never
    // panics or drops the method over it.
    let type_arg_subst: HashMap<EcoString, TypeAnnotation> = protocol
        .type_params
        .iter()
        .zip(use_.type_args.iter())
        .map(|(tp, arg)| (tp.name.name.clone(), arg.clone()))
        .collect();

    // Hygiene: a method-local type variable (a bare single uppercase
    // letter, `is_generic_type_param`, that isn't one of the protocol's own
    // type params) that happens to share a name with one of the *using*
    // class's type params must be alpha-renamed before substitution, or it
    // would be silently captured — after flattening, the type checker's own
    // `class_type_params` check (`infer_method_local_params`) can no longer
    // tell the method-local variable from the class's fixed one (ADR 0127
    // §3, "Type-parameter substitution and hygiene";
    // `Pair(A, B) uses: Enumerable(A)` must not let `inject:into:`'s own
    // local `A` collapse into the class's `A`).
    let mut local_type_vars: Vec<EcoString> = Vec::new();
    for param in &method.parameters {
        if let Some(ty) = &param.type_annotation {
            collect_local_type_vars(ty, &protocol_type_param_names, &mut local_type_vars);
        }
    }
    if let Some(rt) = &method.return_type {
        collect_local_type_vars(rt, &protocol_type_param_names, &mut local_type_vars);
    }

    let mut local_renames: HashMap<EcoString, EcoString> = HashMap::new();
    for name in &local_type_vars {
        if class_type_param_names.contains(name.as_str()) {
            if let Some(fresh) = fresh_type_var_letter(
                &class_type_param_names,
                &protocol_type_param_names,
                &local_type_vars,
                &local_renames,
            ) {
                local_renames.insert(name.clone(), fresh);
            }
            // Exhausted `'A'..='Z'` (26 distinct method-local + class type
            // params in one signature) — leave unrenamed. The resulting
            // capture is real but vanishingly unlikely to occur in
            // practice, and not worth a 27th fallback naming scheme that
            // `is_generic_type_param` (single-letter only) wouldn't even
            // recognise as a type variable afterward.
        }
    }

    let substitute = |ty: &TypeAnnotation| -> TypeAnnotation {
        substitute_type_annotation(
            ty,
            &type_arg_subst,
            &local_renames,
            &class.name.name,
            &class.type_params,
        )
    };

    let parameters: Vec<ParameterDefinition> = method
        .parameters
        .iter()
        .map(|p| ParameterDefinition {
            name: p.name.clone(),
            type_annotation: p.type_annotation.as_ref().map(&substitute),
        })
        .collect();
    let return_type = method.return_type.as_ref().map(&substitute);

    MethodDefinition {
        selector: method.selector.clone(),
        parameters,
        // The body's self-sends resolve against the using class once this
        // is spliced into `class.methods` (ADR 0127 §6) — no substitution
        // needed there, only in the signature.
        body: method.body.clone(),
        return_type,
        is_sealed: method.is_sealed,
        is_internal: method.is_internal,
        is_class_method: false, // class-side provisions are post-v1 (§9)
        kind: method.kind,
        expect: method.expect.clone(),
        comments: method.comments.clone(),
        doc_comment: method.doc_comment.clone(),
        // Kept as the protocol's own span for now — full source-identity
        // carry (diagnostics/BEAM line annotations pointing at the
        // protocol file) is ADR 0127 §3's "Source locations", BT-3590.
        span: method.span,
    }
}

/// Recursively collects the method-local type-variable names appearing in
/// `ty` — a bare `TypeAnnotation::Simple` identifier that
/// [`is_generic_type_param`] recognises (a single uppercase letter) and
/// that isn't one of the protocol's own declared type parameters. Skips
/// duplicates so each name is visited once regardless of how many
/// signature positions mention it.
fn collect_local_type_vars(
    ty: &TypeAnnotation,
    protocol_type_params: &HashSet<&str>,
    out: &mut Vec<EcoString>,
) {
    match ty {
        TypeAnnotation::Simple(id) => {
            if is_generic_type_param(&id.name)
                && !protocol_type_params.contains(id.name.as_str())
                && !out.contains(&id.name)
            {
                out.push(id.name.clone());
            }
        }
        TypeAnnotation::Union { types, .. } => {
            for t in types {
                collect_local_type_vars(t, protocol_type_params, out);
            }
        }
        TypeAnnotation::Generic { parameters, .. } => {
            for t in parameters {
                collect_local_type_vars(t, protocol_type_params, out);
            }
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            collect_local_type_vars(inner, protocol_type_params, out);
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            collect_local_type_vars(base, protocol_type_params, out);
            collect_local_type_vars(excluded, protocol_type_params, out);
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            collect_local_type_vars(left, protocol_type_params, out);
            collect_local_type_vars(right, protocol_type_params, out);
        }
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. }
        | TypeAnnotation::ClassOf { .. } => {}
    }
}

/// Picks the first single uppercase letter (`'A'..='Z'`) that collides with
/// none of: the using class's own type params, the protocol's own type
/// params (avoided purely to reduce confusion — they're substituted away
/// regardless), every other method-local type variable in this same
/// signature, or a rename already chosen for one of them.
fn fresh_type_var_letter(
    avoid_class_params: &HashSet<&str>,
    avoid_protocol_params: &HashSet<&str>,
    all_local_vars: &[EcoString],
    already_renamed: &HashMap<EcoString, EcoString>,
) -> Option<EcoString> {
    for c in 'A'..='Z' {
        let candidate = c.to_string();
        if avoid_class_params.contains(candidate.as_str())
            || avoid_protocol_params.contains(candidate.as_str())
            || all_local_vars.iter().any(|v| v.as_str() == candidate)
            || already_renamed.values().any(|v| v.as_str() == candidate)
        {
            continue;
        }
        return Some(EcoString::from(candidate));
    }
    None
}

/// Substitutes protocol type params and `Self` into one `TypeAnnotation`
/// tree, applying the hygienic local-variable renames first (see
/// [`substitute_provision`]'s doc). `Generic`'s own `base` identifier (e.g.
/// `Block` in `Block(E, Object)`, or `List` in `List(E)`) is never
/// substituted — it names a real class, not a type parameter.
fn substitute_type_annotation(
    ty: &TypeAnnotation,
    type_arg_subst: &HashMap<EcoString, TypeAnnotation>,
    local_renames: &HashMap<EcoString, EcoString>,
    self_class_name: &EcoString,
    self_class_type_params: &[TypeParamDecl],
) -> TypeAnnotation {
    match ty {
        TypeAnnotation::SelfType { span } => {
            self_type_replacement(*span, self_class_name, self_class_type_params)
        }
        TypeAnnotation::Simple(id) => {
            if let Some(replacement) = type_arg_subst.get(&id.name) {
                replacement.clone()
            } else if let Some(renamed) = local_renames.get(&id.name) {
                TypeAnnotation::Simple(Identifier::new(renamed.clone(), id.span))
            } else {
                ty.clone()
            }
        }
        TypeAnnotation::Union { types, span } => TypeAnnotation::union(
            types
                .iter()
                .map(|t| {
                    substitute_type_annotation(
                        t,
                        type_arg_subst,
                        local_renames,
                        self_class_name,
                        self_class_type_params,
                    )
                })
                .collect(),
            *span,
        ),
        TypeAnnotation::Generic {
            base,
            parameters,
            span,
        } => TypeAnnotation::generic(
            base.clone(),
            parameters
                .iter()
                .map(|t| {
                    substitute_type_annotation(
                        t,
                        type_arg_subst,
                        local_renames,
                        self_class_name,
                        self_class_type_params,
                    )
                })
                .collect(),
            *span,
        ),
        TypeAnnotation::FalseOr { inner, span } => TypeAnnotation::false_or(
            substitute_type_annotation(
                inner,
                type_arg_subst,
                local_renames,
                self_class_name,
                self_class_type_params,
            ),
            *span,
        ),
        TypeAnnotation::Difference {
            base,
            excluded,
            span,
        } => TypeAnnotation::difference(
            substitute_type_annotation(
                base,
                type_arg_subst,
                local_renames,
                self_class_name,
                self_class_type_params,
            ),
            substitute_type_annotation(
                excluded,
                type_arg_subst,
                local_renames,
                self_class_name,
                self_class_type_params,
            ),
            *span,
        ),
        TypeAnnotation::Intersection { left, right, span } => TypeAnnotation::intersection(
            substitute_type_annotation(
                left,
                type_arg_subst,
                local_renames,
                self_class_name,
                self_class_type_params,
            ),
            substitute_type_annotation(
                right,
                type_arg_subst,
                local_renames,
                self_class_name,
                self_class_type_params,
            ),
            *span,
        ),
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfClass { .. }
        | TypeAnnotation::ClassOf { .. } => ty.clone(),
    }
}

/// Builds the `Self` replacement for a provision flattened into `class_name`
/// (ADR 0127 §1): the bare class name for a non-generic user, or
/// `ClassName(A, B, …)` — the class applied to its own type params — for a
/// generic one (`Pair(A, B) uses: Comparable` turns `Self` into `Pair(A,
/// B)`). `span` is the original `Self` reference's location, so a
/// diagnostic on the substituted type still points near where `Self`
/// appeared in the protocol source.
fn self_type_replacement(
    span: crate::source_analysis::Span,
    class_name: &EcoString,
    class_type_params: &[TypeParamDecl],
) -> TypeAnnotation {
    if class_type_params.is_empty() {
        TypeAnnotation::simple(class_name.clone(), span)
    } else {
        TypeAnnotation::generic(
            Identifier::new(class_name.clone(), span),
            class_type_params
                .iter()
                .map(|tp| TypeAnnotation::simple(tp.name.name.clone(), span))
                .collect(),
            span,
        )
    }
}

#[cfg(test)]
mod tests;
