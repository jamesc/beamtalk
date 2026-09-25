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
//! This file holds both halves of ADR 0127 §3's two-part pass:
//!
//! - **Expansion** (steps 1–4, 6: collect provisions, apply `excluding:`,
//!   merge with conflict detection, drop provisions the class body already
//!   defines — "class wins" — and treat the result as the class's own body).
//!   [`expand_module`] runs it **before** `ClassHierarchy` is built, exactly
//!   where the ADR places it, so every later phase (protocol conformance,
//!   type inference, sendability, definite assignment) sees the flattened
//!   class.
//! - **The compile-time guarantees** (ADR 0127 §3 step 5, §3a, §5, §7;
//!   BT-3589): resolving each used trait's required selectors, checking the
//!   `overriding:` acknowledgement, and the protocol-side rules (self-send
//!   bound, statelessness, reserved selectors). [`check_after_hierarchy`]
//!   runs these **after** `ClassHierarchy` (and the `ProtocolRegistry`) are
//!   built from the flattened module, because the requirement check needs
//!   `ClassHierarchy::resolves_selector` and the `overriding:` check needs
//!   the superclass chain — see that function's own doc for the full
//!   breakdown.
//!
//! # Inputs and boundaries
//!
//! [`expand_module`] takes `&mut Module` plus an `external_protocols` map —
//! a deliberately narrow, explicit-input design (no hidden global registry
//! lookup) that mirrors
//! [`crate::semantic_analysis::class_kind_writeback::apply_class_kind_writeback`].
//! A used protocol is resolved from `module.protocols` first (current-file
//! wins on a name clash), falling back to `external_protocols` — every
//! provision-bearing protocol's full AST carried in from elsewhere: another
//! file in the same package, or a dependency package (ADR 0127 §10a,
//! "Carrying trait ASTs to users"; BT-3591's build-graph work). Each compile
//! path builds this map from whatever source it already has access to (see
//! that section for the full per-entry-point breakdown) and hands it in
//! here — `trait_expansion` itself does no file I/O and knows nothing about
//! packages or dependency checkouts. A `uses:` whose protocol resolves in
//! neither map reports [`Diagnostic::error`]: "unknown protocol" for a bare
//! name (a same-file/same-project typo, exactly as before), or a distinct
//! "no source available" error for a package-qualified name — since the
//! *only* way a cross-package protocol's `ProtocolDefinition` reaches
//! `external_protocols` is by successfully parsing that dependency's `.bt`
//! source, failing to find it there means precisely that no source (nor yet
//! a `'__beamtalk_protocol_source'/0` reader — not implemented on this,
//! Rust-side, path; see ADR 0127 §10a "Binary-only dependencies") was
//! available, so the diagnostic can report that directly rather than
//! guessing.
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
    ClassDefinition, ClassKind, Expression, Identifier, MessageSelector, MethodDefinition,
    MethodKind, Module, ParameterDefinition, ProtocolDefinition, ProtocolMethodSignature,
    ProtocolUse, TypeAnnotation, TypeParamDecl,
};
use crate::ast_walker::walk_expression;
use crate::method_source_walker::collect_self_sends;
use crate::semantic_analysis::class_hierarchy::{ClassHierarchy, ClassInfo};
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::semantic_analysis::receiver_knowledge;
use crate::semantic_analysis::type_checker::{TypeChecker, is_generic_type_param};
use crate::source_analysis::Diagnostic;

/// Maps a flattened method back to the protocol that provided it, keyed by
/// `(using_class_name, selector)`. See the module doc's "`MethodInfo::origin`"
/// section for why this can't be stamped onto `MethodInfo` directly here.
pub type OriginMap = HashMap<(EcoString, EcoString), EcoString>;

/// Expand every `uses:` line in `module`'s classes in place (ADR 0127 §3
/// steps 1–4, 6).
///
/// `external_protocols` carries every provision-bearing protocol's full AST
/// from outside this module — another file in the same package, or a
/// dependency package (ADR 0127 §10a; BT-3591) — keyed by bare protocol
/// name. Pass an empty map for a caller with no cross-file/cross-package
/// carrying to offer (a `uses:` then only resolves same-module, exactly as
/// before). A name present in both `module.protocols` and
/// `external_protocols` resolves to the current module's own definition —
/// current-file wins, matching every other pre-hierarchy pass's "current
/// module wins" convention.
///
/// Returns the diagnostics this pass produced (unknown protocols, provision
/// conflicts, the body-less-protocol hint) and the [`OriginMap`] the caller
/// applies to `MethodInfo::origin` after building `ClassHierarchy` from the
/// now-flattened `module` (see [`apply_origins`]).
///
/// A class with no `uses:` lines is untouched and contributes nothing to
/// either return value — this function is a no-op for every module that
/// doesn't use ADR 0127 traits, so callers can run it unconditionally.
#[allow(clippy::implicit_hasher)] // concrete HashMap (every caller builds one the same way) is simpler for callers
pub fn expand_module(
    module: &mut Module,
    external_protocols: &HashMap<EcoString, ProtocolDefinition>,
) -> (Vec<Diagnostic>, OriginMap) {
    let mut diagnostics = Vec::new();
    let mut origins = OriginMap::new();

    if !module.classes.iter().any(|c| !c.uses.is_empty()) {
        return (diagnostics, origins);
    }

    // Look up protocols by name once, before mutating any class — a
    // protocol's provisions never change while flattening its users, and
    // borrowing `module.protocols` for the whole loop while also mutating
    // `module.classes` would need this same split regardless.
    //
    // `external_protocols` is inserted first so the current module's own
    // `module.protocols` entries — inserted second into the same `HashMap`
    // — overwrite any same-named external entry (current-file wins, per
    // this function's own doc).
    let mut protocols: HashMap<&EcoString, &ProtocolDefinition> =
        external_protocols.iter().collect();
    protocols.extend(module.protocols.iter().map(|p| (&p.name.name, p)));

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
        // `protocols` already merges the current module's own protocols with
        // every externally-carried one (same-package other file, or a
        // dependency's protocol AST — BT-3591, module doc), so a bare-name
        // lookup resolves either kind identically regardless of whether
        // `use_` itself is package-qualified (`uses: json@Parser`) — the
        // qualifier only matters for the *diagnostic* below when resolution
        // fails.
        let protocol = protocols.get(&use_.protocol.name).copied();
        let Some(protocol) = protocol else {
            diagnostics.push(missing_protocol_diagnostic(use_));
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

/// Builds the diagnostic for a `uses:` line whose protocol resolved in
/// neither the current module nor `external_protocols` (ADR 0127 §10a;
/// BT-3591).
///
/// A package-qualified reference (`uses: json@Parser`) gets a distinct "no
/// source available" error rather than the bare-name "unknown protocol" one:
/// the only way a cross-package protocol's AST reaches `external_protocols`
/// is a caller successfully parsing that dependency's `.bt` source (see
/// [`expand_module`]'s doc), so failing to find it here means precisely that
/// — not a typo — and the diagnostic can name that directly instead of
/// guessing between the two.
fn missing_protocol_diagnostic(use_: &ProtocolUse) -> Diagnostic {
    if let Some(package) = &use_.package {
        Diagnostic::error(
            format!(
                "no source available for protocol `{}` used via `{}@{}`",
                use_.protocol.name, package.name, use_.protocol.name
            ),
            use_.protocol.span,
        )
        .with_hint(
            "a trait's provisions must be flattened from its source — the dependency must \
             ship this protocol's `.bt` source (or, for a compiled-only module, export \
             `'__beamtalk_protocol_source'/0`) so `uses:` can flatten it here",
        )
    } else {
        Diagnostic::error(
            format!("unknown protocol `{}`", use_.protocol.name),
            use_.protocol.span,
        )
        .with_hint(
            "no protocol by this name was found in this file, elsewhere in this package, or \
             among its resolved dependencies — check the spelling, or add a package qualifier \
             (`uses: pkg@Name`) if it's defined in a dependency",
        )
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

// ---------------------------------------------------------------------------
// Post-`ClassHierarchy` half (BT-3589): requirements, `overriding:`, and
// protocol-side rules (ADR 0127 §3a, §5, §7, §13).
// ---------------------------------------------------------------------------

/// Reserved selectors a protocol may not *provide* (ADR 0127 §7, §13) —
/// each one changes what the compiler or runtime does with a class (shape,
/// dispatch, lifecycle), not just what it answers, so a trait providing one
/// would change a class's shape or dispatch behind a `uses:` line.
/// `migrateFromV<N>:` is matched separately, via [`crate::ast::migrate_from_v_version`]
/// (its `N` varies, so it can't be a literal in this list).
const RESERVED_PROVISION_SELECTORS: &[&str] = &[
    "initialize",
    "terminate:",
    "doesNotUnderstand:args:",
    "supervisionPolicy",
    "supervisionSpec",
];

/// The two-selector allowlist exempt from the `overriding:` acknowledgement
/// (ADR 0127 §3a): cosmetic defaults a class is expected to replace.
const OVERRIDE_ALLOWLIST_SELECTORS: &[&str] = &["printString", "displayString"];

/// The root classes the allowlist above applies to — only when the
/// *inherited* method being replaced is itself defined on one of these, not
/// for any class that happens to be named the same.
const OVERRIDE_ALLOWLIST_ROOTS: &[&str] = &["Object", "Value"];

/// Runs every post-`ClassHierarchy` trait check over `module` (already
/// flattened by [`expand_module`]), given the `hierarchy` and
/// `protocol_registry` built from that same flattened module.
///
/// Two independent halves, per the ADR's own split:
///
/// - **Protocol-side** (§5, §7, §13): each protocol with at least one
///   provided method is checked once, regardless of whether any class uses
///   it — "a protocol with no users is still checked" (§5) — for a
///   self-send outside required ∪ provided ∪ `Object`, statelessness
///   (`self.slot`), the reserved-selector list, `@primitive`/`@intrinsic`,
///   `sealed`/`internal`, and the `equals:`/`hash` pairing.
/// - **Class-side** (§3 step 5, §3a, §8): only classes with at least one
///   `uses:` line — required-selector resolution, `excluding:`/`overriding:`
///   name validity, "excluding can break conformance", the `overriding:`
///   acknowledgement (and its staleness), and override-compatibility for a
///   class-body method that replaced a dropped provision.
///
/// Called by `analyse_full` right after `ProtocolRegistry` is registered
/// (Phase 0.5) — both class-side checks (`resolves_selector`, the
/// superclass-chain walk) and protocol-side checks (`all_conformance_selectors`,
/// which needs the registry for `extending:` transitivity) depend on it.
///
/// `external_protocols` must be the same map [`expand_module`] flattened
/// `module` against (BT-3591) — a class-side check for a `uses:` line that
/// resolved cross-file/cross-package during expansion needs the same
/// protocol definition to check requirements, `excluding:`/`overriding:`
/// names, and dropped-provision override compatibility against.
#[allow(clippy::implicit_hasher)] // concrete HashMap (every caller builds one the same way) is simpler for callers
pub fn check_after_hierarchy(
    module: &Module,
    hierarchy: &ClassHierarchy,
    protocol_registry: &ProtocolRegistry,
    external_protocols: &HashMap<EcoString, ProtocolDefinition>,
) -> Vec<Diagnostic> {
    let mut diagnostics = Vec::new();

    for protocol in &module.protocols {
        check_protocol_provisions(protocol, hierarchy, protocol_registry, &mut diagnostics);
    }

    if module.classes.iter().any(|c| !c.uses.is_empty()) {
        let mut protocols: HashMap<&EcoString, &ProtocolDefinition> =
            external_protocols.iter().collect();
        protocols.extend(module.protocols.iter().map(|p| (&p.name.name, p)));
        for class in &module.classes {
            if class.uses.is_empty() {
                continue;
            }
            check_class_trait_usage(class, &protocols, hierarchy, &mut diagnostics);
        }
    }

    diagnostics
}

// --- Protocol-side checks (§5, §7, §13) -------------------------------------

/// Runs every protocol-side check on `protocol` (ADR 0127 §5, §7, §13) —
/// a no-op for a protocol with no provisions, since every check here is
/// about a *provided* method's body or signature.
fn check_protocol_provisions(
    protocol: &ProtocolDefinition,
    hierarchy: &ClassHierarchy,
    protocol_registry: &ProtocolRegistry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if protocol.provided_methods.is_empty() {
        return;
    }

    // §5: "self bounded by the protocol's required ∪ provided selectors plus
    // `Object`'s". `all_conformance_selectors` already gives required ∪
    // provided, transitively through `extending:` (ADR 0127 §8) — the exact
    // set `check_conformance_to_protocol` uses for the same reason.
    let mut allowed_self_sends: HashSet<String> = HashSet::new();
    if let Some(info) = protocol_registry.get(&protocol.name.name) {
        for selector in info.all_conformance_selectors(protocol_registry) {
            allowed_self_sends.insert(selector.to_string());
        }
    } else {
        // Not found in the registry (e.g. a namespace collision already
        // reported elsewhere) — fall back to this definition's own
        // required/provided selectors with no `extending:` transitivity, so
        // the self-send check still runs rather than silently skipping the
        // protocol.
        for sig in &protocol.method_signatures {
            allowed_self_sends.insert(sig.selector.name().to_string());
        }
        for method in &protocol.provided_methods {
            allowed_self_sends.insert(method.selector.name().to_string());
        }
    }
    for method in hierarchy.all_methods("Object") {
        allowed_self_sends.insert(method.selector.to_string());
    }

    for method in &protocol.provided_methods {
        check_single_provision(protocol, method, &allowed_self_sends, diagnostics);
    }

    // A protocol providing exactly one of `equals:`/`hash` (§3a, §13).
    let has_equals = protocol
        .provided_methods
        .iter()
        .any(|m| m.selector.name() == "equals:");
    let has_hash = protocol
        .provided_methods
        .iter()
        .any(|m| m.selector.name() == "hash");
    if has_equals != has_hash {
        let (present, missing) = if has_equals {
            ("equals:", "hash")
        } else {
            ("hash", "equals:")
        };
        diagnostics.push(Diagnostic::warning(
            format!(
                "{} provides `{present}` but not `{missing}` — a class using it may get \
                 inconsistent equality and hashing; provide both or neither",
                protocol.name.name
            ),
            protocol.span,
        ));
    }
}

/// Returns `true` when `expr` is the bare `self` pseudo-variable.
fn is_self_identifier(expr: &Expression) -> bool {
    matches!(expr, Expression::Identifier(id) if id.name == "self")
}

/// Runs every single-provision check (ADR 0127 §1, §5, §7, §13) on `method`,
/// one of `protocol`'s provided methods.
fn check_single_provision(
    protocol: &ProtocolDefinition,
    method: &MethodDefinition,
    allowed_self_sends: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let selector = method.selector.name();

    // Reserved selectors — a provision changing class shape/dispatch (§7, §13).
    if RESERVED_PROVISION_SELECTORS.contains(&selector.as_str())
        || crate::ast::migrate_from_v_version(&selector).is_some()
    {
        diagnostics.push(Diagnostic::error(
            format!(
                "a protocol cannot provide `{selector}`; it changes how the class is built or \
                 dispatched. Declare it as a required method instead"
            ),
            method.span,
        ));
    }

    // `sealed`/`internal` on a provided method — not yet supported (§13).
    if method.is_sealed || method.is_internal {
        diagnostics.push(Diagnostic::error(
            "`sealed`/`internal` are not supported on provided methods in v1",
            method.span,
        ));
    }

    // Statelessness (`self.slot`) and `@primitive`/`@intrinsic` (§1, §7) —
    // one walk of the body via the shared expression walker so neither check
    // re-derives its own `Expression` match.
    for stmt in &method.body {
        walk_expression(&stmt.expression, &mut |expr| match expr {
            Expression::FieldAccess { receiver, span, .. } if is_self_identifier(receiver) => {
                diagnostics.push(Diagnostic::error(
                    "protocols are stateless — declare `slot -> Type` as a required method",
                    *span,
                ));
            }
            Expression::Primitive { span, .. } => {
                diagnostics.push(Diagnostic::error(
                    "provided methods cannot use primitives",
                    *span,
                ));
            }
            _ => {}
        });
    }

    // §5: self bounded by required ∪ provided ∪ `Object`.
    for hit in collect_self_sends(method) {
        if !allowed_self_sends.contains(&hit.selector) {
            diagnostics.push(Diagnostic::error(
                format!(
                    "`{}` is sent by `{selector}` but is neither required nor provided by {}; \
                     declare it as required",
                    hit.selector, protocol.name.name
                ),
                hit.span,
            ));
        }
    }
}

// --- Class-side checks (§3 step 5, §3a, §8) ---------------------------------

/// Renders a required signature for a diagnostic hint
/// (`< other :: Self -> Boolean`, `do: block :: Block(E, Object)`) — a
/// small, self-contained rendering for a one-line hint, not a full
/// `Document`-pipeline unparse (`unparse::unparse_protocol_method_signature`
/// is private to that module and returns a `Document`, which is more than a
/// hint string needs).
fn format_required_signature(sig: &ProtocolMethodSignature) -> String {
    let mut out = String::new();
    match &sig.selector {
        MessageSelector::Unary(name) => out.push_str(name),
        MessageSelector::Binary(op) => {
            out.push_str(op);
            if let Some(param) = sig.parameters.first() {
                out.push(' ');
                out.push_str(param.name.name.as_str());
            }
        }
        MessageSelector::Keyword(parts) => {
            for (i, part) in parts.iter().enumerate() {
                if i > 0 {
                    out.push(' ');
                }
                out.push_str(part.keyword.as_str());
                if let Some(param) = sig.parameters.get(i) {
                    out.push(' ');
                    out.push_str(param.name.name.as_str());
                }
            }
        }
    }
    if let Some(param) = sig.parameters.first() {
        if let Some(ty) = &param.type_annotation {
            out.push_str(" :: ");
            out.push_str(ty.type_name().as_str());
        }
    }
    if let Some(rt) = &sig.return_type {
        out.push_str(" -> ");
        out.push_str(rt.type_name().as_str());
    }
    out
}

/// Runs every class-side check (ADR 0127 §3 step 5, §3a, §8) for one class
/// that has at least one `uses:` line.
fn check_class_trait_usage(
    class: &ClassDefinition,
    protocols: &HashMap<&EcoString, &ProtocolDefinition>,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(class_info) = hierarchy.get_class(class.name.name.as_str()) else {
        return; // Not in the hierarchy — a namespace collision or similar
        // already reported elsewhere; nothing sound to check here.
    };

    // ADR 0100: when the class's own method surface isn't known with
    // certainty (a DNU override, a cross-file/parse-error-degraded
    // ancestor), every "unresolved selector" diagnostic below downgrades
    // from an error to a hint rather than risk a false positive.
    let is_open_world =
        !receiver_knowledge::classify_receiver(class.name.name.as_str(), hierarchy, false)
            .is_closed_complete();

    for use_ in &class.uses {
        // Mirrors `expand_class`'s own protocol resolution (`protocols`
        // here is the same current-module-plus-external merge) — an
        // unresolvable `uses:` was already diagnosed by `expand_module`
        // (Phase -1); nothing further to check here.
        let protocol = protocols.get(&use_.protocol.name).copied();
        let Some(protocol) = protocol else {
            continue;
        };

        check_excluding_and_overriding_names(
            class,
            class_info,
            use_,
            protocol,
            hierarchy,
            diagnostics,
        );
        check_required_selectors(class, use_, protocol, hierarchy, is_open_world, diagnostics);
        check_dropped_provision_overrides(class, use_, protocol, hierarchy, diagnostics);
    }

    check_overriding_acknowledgement(class, class_info, hierarchy, is_open_world, diagnostics);
}

/// §5 "excluding: a required selector"/"excluding: names a selector T does
/// not provide", the parallel "overriding: names a selector T does not
/// provide" (§13), §5's "excluding can break conformance" warning, and the
/// `overriding:` staleness check (§3a) for a *validly-named* entry — all
/// four read `use_.excluding`/`use_.overriding` against what `protocol`
/// actually requires/provides, so they run together over the same two lists.
fn check_excluding_and_overriding_names(
    class: &ClassDefinition,
    class_info: &ClassInfo,
    use_: &ProtocolUse,
    protocol: &ProtocolDefinition,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let required: HashSet<EcoString> = protocol
        .method_signatures
        .iter()
        .map(|s| s.selector.name())
        .collect();
    let provided: HashSet<EcoString> = protocol
        .provided_methods
        .iter()
        .map(|m| m.selector.name())
        .collect();

    for id in &use_.excluding {
        let sel = &id.name;
        if provided.contains(sel) {
            // A real exclusion — check it doesn't leave the class
            // non-conforming (§5, "Excluding can break conformance").
            if !hierarchy.resolves_selector(class.name.name.as_str(), sel) {
                diagnostics.push(Diagnostic::warning(
                    format!(
                        "{} uses {} but does not conform to {}: it excludes `{sel}` without \
                         defining or inheriting it",
                        class.name.name, protocol.name.name, protocol.name.name
                    ),
                    id.span,
                ));
            }
        } else if required.contains(sel) {
            diagnostics.push(Diagnostic::error(
                format!(
                    "`{sel}` is required by {}, not provided; requirements cannot be excluded",
                    protocol.name.name
                ),
                id.span,
            ));
        } else {
            diagnostics.push(Diagnostic::error(
                format!("{} does not provide `{sel}`", protocol.name.name),
                id.span,
            ));
        }
    }

    for id in &use_.overriding {
        if !provided.contains(&id.name) {
            diagnostics.push(Diagnostic::error(
                format!("{} does not provide `{}`", protocol.name.name, id.name),
                id.span,
            ));
            continue;
        }

        // Staleness (§3a): a validly-named `overriding:` entry that no
        // longer replaces anything — the superclass dropped the method, the
        // protocol dropped the provision (already excluded above by
        // `!provided.contains`), or the class body now defines the selector
        // itself (so `expand_class`'s class-wins step never spliced this
        // provision in, and it carries no `origin` in the hierarchy).
        let still_a_provision = class_info
            .methods
            .iter()
            .find(|m| m.selector == id.name)
            .is_some_and(|m| m.origin.as_ref() == Some(&protocol.name.name));
        let still_inherited = still_a_provision
            && class_info
                .superclass
                .as_deref()
                .is_some_and(|superclass| hierarchy.find_method(superclass, &id.name).is_some());
        if !still_inherited {
            diagnostics.push(Diagnostic::warning(
                format!(
                    "`{}` in `overriding:` does not override an inherited method; remove it",
                    id.name
                ),
                id.span,
            ));
        }
    }
}

/// §3 step 5 / §5: every required selector of `use_`'s protocol (after
/// exclusion) must resolve on the flattened class.
fn check_required_selectors(
    class: &ClassDefinition,
    use_: &ProtocolUse,
    protocol: &ProtocolDefinition,
    hierarchy: &ClassHierarchy,
    is_open_world: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let excluded: HashSet<&str> = use_.excluding.iter().map(|id| id.name.as_str()).collect();

    for sig in &protocol.method_signatures {
        let selector = sig.selector.name();
        if excluded.contains(selector.as_str()) {
            // Either a valid exclusion of a non-required selector (checked
            // above) or an invalid exclusion of this very requirement,
            // already reported by `check_excluding_and_overriding_names` —
            // either way, checking resolution of an intentionally-excluded
            // requirement would only pile on the same diagnostic.
            continue;
        }
        if hierarchy.resolves_selector(class.name.name.as_str(), &selector) {
            continue;
        }

        let message = format!(
            "{} uses {} but does not implement required `{selector}`",
            class.name.name, protocol.name.name
        );
        let hint = format!(
            "{} requires `{}`",
            protocol.name.name,
            format_required_signature(sig)
        );
        diagnostics.push(if is_open_world {
            Diagnostic::hint(message, use_.span).with_hint(hint)
        } else {
            Diagnostic::error(message, use_.span).with_hint(hint)
        });
    }
}

/// §8 "An override of a provision is checked against it": when `class`'s own
/// body defines a selector that `use_`'s protocol also provides (so
/// class-wins dropped the provision, per `expand_class` step 4), the class's
/// method is checked against the *dropped* provision's signature — the same
/// nominal-compatibility rule used for a method overriding an inherited one.
fn check_dropped_provision_overrides(
    class: &ClassDefinition,
    use_: &ProtocolUse,
    protocol: &ProtocolDefinition,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let excluded: HashSet<&str> = use_.excluding.iter().map(|id| id.name.as_str()).collect();
    let Some(class_info) = hierarchy.get_class(class.name.name.as_str()) else {
        return;
    };

    for provided in &protocol.provided_methods {
        let selector = provided.selector.name();
        if excluded.contains(selector.as_str()) {
            continue;
        }

        // Only a class-body method — not a surviving provision from this or
        // another `uses:` line — counts as "class wins" here. A synthesised
        // `Value` accessor also drops a provision (`expand_class`'s
        // `own_selectors`) but has no `MethodDefinition` in `class.methods`
        // to compare a signature against, so there is nothing to check.
        let is_own_body_method = class_info
            .methods
            .iter()
            .find(|m| m.selector.as_str() == selector.as_str())
            .is_some_and(|m| m.origin.is_none());
        if !is_own_body_method {
            continue;
        }
        let Some(own_method) = class
            .methods
            .iter()
            .find(|m| m.kind == MethodKind::Primary && m.selector.name() == selector)
        else {
            continue;
        };

        let dropped = substitute_provision(provided, use_, protocol, class);
        check_signature_override_compatibility(
            class,
            &dropped,
            own_method,
            protocol,
            hierarchy,
            diagnostics,
        );
    }
}

/// Compares `own_method` (the class body's method) against `dropped` (the
/// substituted provision it replaced), warning on any parameter or return
/// type that isn't compatible — [`TypeChecker::is_type_compatible`]'s
/// nominal-chain rule, the same one `check_override_param_compatibility`
/// uses for a method overriding an inherited one (ADR 0127 §8).
fn check_signature_override_compatibility(
    class: &ClassDefinition,
    dropped: &MethodDefinition,
    own_method: &MethodDefinition,
    protocol: &ProtocolDefinition,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let selector = own_method.selector.name();

    for (i, (own_param, dropped_param)) in own_method
        .parameters
        .iter()
        .zip(dropped.parameters.iter())
        .enumerate()
    {
        let (Some(own_ty), Some(dropped_ty)) =
            (&own_param.type_annotation, &dropped_param.type_annotation)
        else {
            continue;
        };
        let own_name = own_ty.type_name();
        let dropped_name = dropped_ty.type_name();
        if !TypeChecker::is_type_compatible(&own_name, &dropped_name, hierarchy) {
            diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Parameter {} of `{selector}` in {} has type {own_name}, incompatible \
                         with {}'s {dropped_name}",
                        i + 1,
                        class.name.name,
                        protocol.name.name
                    ),
                    own_method.span,
                )
                .with_hint(format!(
                    "{} declares parameter type {dropped_name}",
                    protocol.name.name
                )),
            );
        }
    }

    if let (Some(own_rt), Some(dropped_rt)) = (&own_method.return_type, &dropped.return_type) {
        let own_name = own_rt.type_name();
        let dropped_name = dropped_rt.type_name();
        if !TypeChecker::is_type_compatible(&own_name, &dropped_name, hierarchy) {
            diagnostics.push(
                Diagnostic::warning(
                    format!(
                        "Return type of `{selector}` in {} is {own_name}, incompatible with {}'s \
                         {dropped_name}",
                        class.name.name, protocol.name.name
                    ),
                    own_method.span,
                )
                .with_hint(format!(
                    "{} declares return type {dropped_name}",
                    protocol.name.name
                )),
            );
        }
    }
}

/// §3a: every surviving provision that replaces an inherited method must be
/// acknowledged with `overriding:` on its `uses:` line. The staleness check
/// for an `overriding:` entry that no longer replaces anything lives in
/// [`check_excluding_and_overriding_names`] instead, alongside the
/// "does not provide" name check it would otherwise duplicate for an invalid
/// entry.
fn check_overriding_acknowledgement(
    class: &ClassDefinition,
    class_info: &ClassInfo,
    hierarchy: &ClassHierarchy,
    is_open_world: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(ref superclass) = class_info.superclass else {
        return; // No superclass — nothing to inherit, nothing to override.
    };

    // Every provision that actually survived flattening into this class,
    // keyed by selector — `MethodInfo::origin` is `Some` only for a method
    // `expand_class` spliced in (never for a class-body method or a
    // synthesised accessor, both `None`, per that field's own doc). Sorted
    // by selector before iterating: `class_info.methods` order is already
    // deterministic (`expand_class` splices provisions in selector-sorted
    // order), but collecting through a `HashMap` first would randomise it
    // again, and this loop pushes diagnostics — the same "no `RandomState`
    // in emitted order" reasoning `expand_class`'s own splice-order comment
    // gives.
    let mut surviving_provisions: Vec<(EcoString, EcoString)> = class_info
        .methods
        .iter()
        .filter_map(|m| m.origin.as_ref().map(|p| (m.selector.clone(), p.clone())))
        .collect();
    surviving_provisions.sort_by(|(a, _), (b, _)| a.cmp(b));

    for (selector, protocol_name) in &surviving_provisions {
        let Some(inherited) = hierarchy.find_method(superclass, selector) else {
            continue; // Nothing inherited at this selector — not an override.
        };

        // Same-origin exemption: the inherited method came from the same
        // protocol, un-customised by the superclass (§3a).
        if inherited.origin.as_ref() == Some(protocol_name) {
            continue;
        }

        // A sealed inherited method is already an error regardless of
        // `overriding:` — enforced unconditionally by
        // `ClassHierarchy::add_module_classes`'s existing sealed-override
        // check, which runs over every class-body method (including a
        // spliced-in provision, since flattening happens before the
        // hierarchy is built). Adding a second §3a-flavoured error here
        // would just duplicate that diagnostic.
        if inherited.is_sealed {
            continue;
        }

        // The two-selector allowlist: cosmetic `Object`/`Value` defaults a
        // class is expected to replace.
        if OVERRIDE_ALLOWLIST_SELECTORS.contains(&selector.as_str())
            && OVERRIDE_ALLOWLIST_ROOTS.contains(&inherited.defined_in.as_str())
        {
            continue;
        }

        let acknowledged = class.uses.iter().any(|u| {
            u.protocol.name == *protocol_name && u.overriding.iter().any(|id| id.name == *selector)
        });
        if acknowledged {
            continue;
        }

        let use_span = class
            .uses
            .iter()
            .find(|u| u.protocol.name == *protocol_name)
            .map_or(class.span, |u| u.span);
        let message = format!(
            "{protocol_name} provides `{selector}`, which {} would otherwise inherit from {}",
            class.name.name, inherited.defined_in
        );
        let hint = format!(
            "to use {protocol_name}'s version, write\n  uses: {protocol_name} overriding: #(#{selector})\n\
             to keep {}'s version, write\n  uses: {protocol_name} excluding: #(#{selector})",
            inherited.defined_in
        );
        diagnostics.push(if is_open_world {
            Diagnostic::hint(message, use_span).with_hint(hint)
        } else {
            Diagnostic::error(message, use_span).with_hint(hint)
        });
    }
}

#[cfg(test)]
mod tests;
