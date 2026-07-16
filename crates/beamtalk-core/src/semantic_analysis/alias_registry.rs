// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type alias registry (ADR 0108 Phases 2–3, BT-2895 + BT-2896).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Mirrors [`ProtocolRegistry`](crate::semantic_analysis::protocol_registry::ProtocolRegistry)'s
//! shape: a name → metadata map built once per compile from `Module::type_aliases`,
//! consulted by [`resolve_type_annotation`](crate::semantic_analysis::type_checker::resolve_type_annotation)
//! at annotation-resolution time (ADR 0108 Semantics: `subst` → alias table →
//! nominal class).
//!
//! **Batch registration order** (ADR 0108 Semantics/Implementation):
//! classes, then protocols (today's order), then aliases last — so
//! [`register_module`](AliasRegistry::register_module) assumes the
//! [`ClassHierarchy`] and [`ProtocolRegistry`] it is given are already fully
//! built for the current module. An alias colliding with either a class or a
//! protocol always has a fully-formed registry to check against, giving
//! bidirectional collision detection *within a single batch compile* for
//! free: since the whole module is parsed up front, a class or protocol that
//! textually follows a `type` declaration in source is still registered
//! first in *processing* order, so the alias side always sees it. (A live
//! REPL/hot-reload session can arrive in the opposite order — a `type Foo`
//! declared in one turn, then a live `class Foo` redefinition in a later
//! turn — which is out of scope here; see ADR 0108 Semantics and BT-2899.)
//!
//! **Cycle detection + topological sort** (ADR 0108 "No recursion",
//! BT-2896): [`register_module`](AliasRegistry::register_module) and
//! [`redefine_alias`](AliasRegistry::redefine_alias) both run a single DFS
//! ([`find_cycles`]) over the alias dependency graph — edges are RHS
//! references to other registered alias names — that detects reference
//! cycles (including self-reference) and produces a topological resolution
//! order as its post-order, in one walk. A batch-time cycle is a non-fatal
//! diagnostic (the alias still registers); a live redefinition that would
//! introduce a cycle is rejected outright, leaving the alias table
//! unchanged.
//!
//! **References:**
//! - `docs/ADR/0108-named-union-type-aliases.md` — Semantics (Namespace,
//!   Single-letter names, No recursion), Implementation
//! - `docs/ADR/0068-parametric-types-and-protocols.md` — the namespace and
//!   `TypeAnnotation` machinery this extends

use std::collections::HashMap;

use ecow::EcoString;

use crate::ast::{Module, TypeAliasDefinition, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::semantic_analysis::type_checker::is_generic_type_param;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};

/// Information about a type alias in the registry.
///
/// **DDD Context:** Semantic Analysis — Value Object
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AliasInfo {
    /// The alias name (e.g., `RestartStrategy`).
    pub name: EcoString,
    /// The declared right-hand-side annotation — stored unresolved (ADR
    /// 0108: eager expansion happens at *reference* resolution time, not at
    /// registration time, so a chain of aliases referencing each other
    /// still resolves through the same `resolve_type_annotation` recursion
    /// used for every other annotation).
    pub annotation: TypeAnnotation,
    /// Whether this alias is package-private (ADR 0071 `internal` modifier,
    /// ADR 0108 Phase 5, BT-2898): `internal type Foo = ...`.
    ///
    /// An internal alias declared in a *different* package than the current
    /// compilation is never seeded into the consumer's alias table at all
    /// ([`AliasRegistry::add_pre_loaded`] excludes it at the seeding
    /// boundary) — so by the time an `AliasInfo` with `is_internal: true` is
    /// visible in *any* registry, it is guaranteed to belong to the current
    /// package. Leakage checks therefore don't need to re-derive "current
    /// package" from `package` themselves the way
    /// [`crate::semantic_analysis::class_hierarchy::ClassInfo`]'s checks do
    /// — but `add_pre_loaded` itself still needs `package` to tell a
    /// same-package cross-file alias (must stay visible — ADR 0108: internal
    /// aliases are usable in `internal` signatures *within their declaring
    /// package*, not just their declaring file) apart from a dependency's
    /// internal alias (must never be seeded).
    pub is_internal: bool,
    /// Package that declares this alias (ADR 0070/0071), mirroring
    /// [`crate::semantic_analysis::class_hierarchy::ClassInfo::package`].
    /// `None` for `extract_alias_infos`'s raw output — populated by the
    /// caller (build pipeline) before merging into `pre_loaded_aliases`, the
    /// same way `ClassInfo::package` is populated downstream of
    /// `ClassHierarchy::extract_class_infos`.
    pub package: Option<EcoString>,
    /// Source span of the `type Name = ...` declaration (for diagnostics).
    pub span: Span,
}

impl AliasInfo {
    /// Builds an [`AliasInfo`] from a parsed [`TypeAliasDefinition`].
    ///
    /// `pub` (ADR 0108 Phase 8, BT-2902): the REPL's cross-turn alias
    /// persistence re-parses each previously-declared `type Name = ...` line
    /// standalone (the session has no live BEAM artifact to recover an
    /// alias from, unlike a class — see `AliasRegistry::add_pre_loaded`) and
    /// needs this constructor to turn the result into an `AliasInfo` without
    /// going through a full `Module`/`register_module` batch.
    #[must_use]
    pub fn from_definition(def: &TypeAliasDefinition) -> Self {
        Self {
            name: def.name.name.clone(),
            annotation: def.annotation.clone(),
            is_internal: def.is_internal,
            package: None,
            span: def.span,
        }
    }
}

/// Registry of type alias declarations for compile-time annotation
/// resolution (ADR 0108 Phase 2).
///
/// **DDD Context:** Semantic Analysis — Domain Service
///
/// Maps alias names to the [`TypeAnnotation`] they name. Built during
/// semantic analysis alongside the [`ClassHierarchy`] and [`ProtocolRegistry`]
/// — see module docs for the required batch registration order.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AliasRegistry {
    aliases: HashMap<EcoString, AliasInfo>,
    /// Canonicalised cycle chains (see [`canonical_cycle_key`]) already
    /// reported by [`register_module`](Self::register_module) — BT-2896:
    /// that method re-scans the *whole* accumulated `aliases` map on every
    /// call (see its doc), so a package-wide caller seeding this registry
    /// across several `register_module` calls (one per file) would
    /// otherwise see the same still-unresolved cycle re-reported on every
    /// subsequent call. Never *consulted* by
    /// [`redefine_alias`](Self::redefine_alias) — a live redefinition's
    /// diagnostics are always a fresh, one-off judgement of that single
    /// trial, never deduplicated against history — but a *successful*
    /// redefinition clears this set entirely: it just proved (by committing)
    /// that the new table is cycle-free, so any key left over from before
    /// describes a cycle that no longer exists and must not silently
    /// suppress a later `register_module` call that reintroduces the same
    /// cycle from scratch.
    reported_cycles: std::collections::HashSet<Vec<EcoString>>,
}

impl AliasRegistry {
    /// Creates an empty alias registry.
    #[must_use]
    pub fn new() -> Self {
        Self {
            aliases: HashMap::new(),
            reported_cycles: std::collections::HashSet::new(),
        }
    }

    /// Registers type aliases from a parsed module.
    ///
    /// Must be called *after* `hierarchy` and `protocol_registry` are fully
    /// populated for the current module (ADR 0108: classes → protocols →
    /// aliases) — see module docs.
    ///
    /// Returns diagnostics for:
    /// - Namespace collisions (alias name matches an existing class or
    ///   protocol name — ADR 0108 Semantics: aliases share the single
    ///   class/protocol namespace)
    /// - Duplicate alias definitions
    /// - Unbound single-letter type variables on the RHS (ADR 0108: a bare
    ///   unbound single letter on an alias RHS, e.g. `type Timeout = Integer
    ///   | T`, is a declaration error rather than a phantom class named `T`
    ///   — checked via its own pass, since a top-level `type` declaration
    ///   has no enclosing method and `subst` is always empty there)
    ///
    /// A colliding or duplicate name is skipped entirely (not inserted), so
    /// later resolution cannot pick it up — mirroring
    /// [`ProtocolRegistry::register_module`]'s skip-on-collision behaviour.
    /// An unbound-type-variable error is non-fatal: the alias still
    /// registers so the rest of the declaration (and any following
    /// declarations) still get checked.
    pub fn register_module(
        &mut self,
        module: &Module,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();

        for alias_def in &module.type_aliases {
            let name = &alias_def.name.name;

            // Namespace collision: alias name matches a class or protocol name.
            if let Some(diag) = namespace_collision_diagnostic(
                name,
                alias_def.name.span,
                hierarchy,
                protocol_registry,
            ) {
                diagnostics.push(diag);
                continue;
            }

            // Duplicate alias definition.
            if self.aliases.contains_key(name) {
                diagnostics.push(
                    Diagnostic::error(
                        format!("Duplicate type alias definition: `{name}`"),
                        alias_def.name.span,
                    )
                    .with_hint(
                        "Remove the duplicate definition — each type alias can only be \
                         defined once",
                    )
                    .with_category(DiagnosticCategory::Type),
                );
                continue;
            }

            // ADR 0108 Semantics / Implementation: unbound single-letter
            // type-variable check on the RHS, its own validation pass run
            // alongside (not as a byproduct of) `resolve_type_annotation`.
            check_unbound_type_vars(&alias_def.annotation, hierarchy, name, &mut diagnostics);

            self.aliases
                .insert(name.clone(), AliasInfo::from_definition(alias_def));
        }

        // ADR 0108 "No recursion" / BT-2896: cycle detection + topological
        // sort, package-wide. Runs over the *whole* accumulated `self.aliases`
        // (not just the aliases just registered from this one module) so that
        // calling `register_module` more than once against the same registry
        // — the shape a future package-wide caller (BT-2898) would use to
        // seed aliases from multiple files — still catches a cycle spanning
        // names declared in different calls. Non-fatal, mirroring the
        // unbound-type-variable check above: a cyclic alias still registers
        // (its `TypeAnnotation` is stored either way), relying on
        // `resolve_type_annotation`'s existing defensive `expanding` guard
        // (BT-2895) to keep *expansion* safe if something resolves through it
        // anyway — rejecting cyclic aliases outright would just cascade into
        // spurious "unknown type" diagnostics at every reference site instead
        // of the one precise cycle diagnostic here.
        //
        // Deduplicated against `self.reported_cycles` (see its doc): a cycle
        // that doesn't change between two `register_module` calls on the
        // same registry is reported only once, not once per call.
        //
        // The topological order (second element) has no consumer here — see
        // `find_cycles`'s doc for why: it exists to satisfy ADR 0108's
        // explicit "the same DFS ... produces this topological order for
        // free" acceptance criterion (proof that the walk *is* a valid
        // topological sort, pinned by
        // `find_cycles_post_order_is_a_valid_topological_order`), not
        // because anything downstream needs a precomputed resolution
        // order — `resolve_type_annotation` in `type_resolver.rs` resolves
        // aliases eagerly and lazily (pulled on reference, not pushed in
        // dependency order), with its own memo cache making a precomputed
        // order redundant for that purpose.
        let (cycles, _resolution_order) = find_cycles(&self.aliases);
        for (cycle, cycle_span) in cycles {
            if self.reported_cycles.insert(canonical_cycle_key(&cycle)) {
                diagnostics.push(cycle_diagnostic(&cycle, cycle_span));
            }
        }

        diagnostics
    }

    /// (Re)defines a single alias in a live/REPL session (ADR 0108 "No
    /// recursion", BT-2896).
    ///
    /// Unlike [`register_module`](Self::register_module), redefining an
    /// existing name is the whole point here (no "duplicate" check), but
    /// every declaration-time check — namespace collision, unbound
    /// type-variable, and (package-wide) cycle detection — re-runs against a
    /// *trial* copy of the alias table that includes the candidate
    /// redefinition. If that trial is clean, the redefinition commits, i.e.
    /// replaces the existing entry (or inserts a brand-new one, for a name
    /// not previously registered). If any diagnostic fires — a cycle the
    /// redefinition would introduce, or any other declaration-time error —
    /// nothing commits: `self.aliases` is left exactly as it was, so the
    /// previous binding (if any) stays in effect and no dependent annotation
    /// is ever left resolving against a missing or broken alias.
    ///
    /// This is the alias-table-consistency half of ADR 0108's hot-reload
    /// story. It does not, by itself, re-check other code that references
    /// the redefined name — that dependent-site re-check trigger is BT-2899,
    /// a separate, later issue building on this one.
    pub fn redefine_alias(
        &mut self,
        alias_def: &TypeAliasDefinition,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
    ) -> Vec<Diagnostic> {
        let name = &alias_def.name.name;
        let mut diagnostics = Vec::new();

        if let Some(diag) =
            namespace_collision_diagnostic(name, alias_def.name.span, hierarchy, protocol_registry)
        {
            diagnostics.push(diag);
        }

        check_unbound_type_vars(&alias_def.annotation, hierarchy, name, &mut diagnostics);

        let mut candidate = self.aliases.clone();
        candidate.insert(name.clone(), AliasInfo::from_definition(alias_def));
        // Always a fresh judgement of this one trial — never deduplicated
        // against `self.reported_cycles` (see its doc): a live redefinition
        // must be re-flagged every time it (re)introduces a cycle, even if
        // that exact cycle was already reported once during batch
        // registration.
        let (cycles, _resolution_order) = find_cycles(&candidate);
        for (cycle, cycle_span) in cycles {
            diagnostics.push(cycle_diagnostic(&cycle, cycle_span));
        }

        // All-or-nothing: any declaration-time error (namespace collision,
        // unbound type variable, or an introduced cycle) rejects the whole
        // redefinition — the alias table stays exactly as it was.
        if diagnostics.is_empty() {
            self.aliases = candidate;
            // A successful commit means `find_cycles(&candidate)` just
            // returned zero cycles above — i.e. the new table is cycle-free
            // by construction. `self.reported_cycles` may still hold keys
            // for cycles that existed in the *old* table (this redefinition
            // could be exactly what broke one); none of them describe the
            // committed state any more, so clearing here keeps
            // `register_module`'s cross-call dedup (see `reported_cycles`'s
            // doc) from staying stale and silently swallowing a genuinely
            // new occurrence of the same cycle in a later call.
            self.reported_cycles.clear();
        }

        diagnostics
    }

    /// Extract `AliasInfo` entries from a parsed module without registering them.
    ///
    /// BT-2898: Mirrors `ProtocolRegistry::extract_protocol_infos` /
    /// `ClassHierarchy::extract_class_infos` — used to collect alias metadata
    /// from a package's compiled sources ahead of compiling a downstream
    /// module that references those alias names (ADR 0108 Semantics:
    /// "Aliases are exported, like classes and protocols").
    ///
    /// Includes `internal` aliases — the seeding-boundary exclusion happens
    /// in [`Self::add_pre_loaded`], not here, so this function stays a
    /// faithful, unfiltered snapshot of the module's declarations (useful
    /// for same-package callers that *do* want to see internal aliases,
    /// e.g. a future same-package System Browser query).
    #[must_use]
    pub fn extract_alias_infos(module: &Module) -> Vec<AliasInfo> {
        module
            .type_aliases
            .iter()
            .map(AliasInfo::from_definition)
            .collect()
    }

    /// Seed the registry with aliases pre-compiled from other source files or
    /// packages, or carried over from earlier turns of the same REPL session.
    ///
    /// BT-2898: Mirrors `ProtocolRegistry::add_pre_loaded` (BT-2006). Skips
    /// entries whose names are already registered (current-module
    /// definitions win) and reports diagnostics for namespace collisions —
    /// a pre-loaded alias name that matches a class, protocol, or another
    /// already-seeded alias (e.g. two dependencies each exporting `type Id
    /// = ...`; ADR 0108 Semantics: "Cross-package collisions ... are
    /// diagnosed at seeding time").
    ///
    /// Also used for REPL cross-turn continuity (ADR 0108 Phase 8, BT-2902):
    /// callers pass previously-declared `type Name = ...` lines re-parsed
    /// standalone each turn (aliases erase to nothing at runtime, so unlike
    /// `pre_loaded_classes` there is no live BEAM artifact to recover them
    /// from). Those entries were already validated once when first declared;
    /// the collision checks below are harmless in that context since a
    /// caller is expected to have already filtered out any name the current
    /// turn itself redeclares (current turn wins — see
    /// `AnalysisContext::pre_loaded_aliases`), and `current_package` is
    /// `None` for a REPL/script session, which — per the seeding-boundary
    /// rule below — makes every carried-over `internal` alias visible again
    /// (there is no package boundary to enforce in an open-world REPL).
    ///
    /// **Seeding-boundary exclusion** (ADR 0108 Semantics / Implementation):
    /// an `internal` alias declared in a *different* package than
    /// `current_package` is *never* seeded — it is filtered out before the
    /// collision check even runs, so it is entirely absent from the
    /// consumer's alias table (and thus from every subsequent error path,
    /// resolution, or browse query), not merely hidden by a query-time
    /// filter layered on top. An internal alias from the *same* package
    /// (e.g. another file in a same-package multi-file compilation) is still
    /// seeded — ADR 0108: an internal alias is "usable in `internal`
    /// signatures within its declaring package", not just its declaring
    /// file. An entry whose `package` is unknown (`None`, e.g. a caller that
    /// hasn't populated it yet) is conservatively treated as foreign and
    /// excluded, unless `current_package` is also `None` (REPL/script
    /// open-world context, where there is no package boundary to enforce at
    /// all — mirrors [`crate::semantic_analysis::class_hierarchy::ClassHierarchy::add_from_beam_meta`]'s
    /// unconditional accept in that context).
    pub fn add_pre_loaded(
        &mut self,
        aliases: Vec<AliasInfo>,
        hierarchy: &ClassHierarchy,
        protocol_registry: &ProtocolRegistry,
        current_package: Option<&str>,
    ) -> Vec<Diagnostic> {
        let mut diagnostics = Vec::new();
        for info in aliases {
            // Seeding-boundary exclusion: an internal alias is seeded only
            // when it belongs to the package currently being compiled (or
            // there is no package context at all, e.g. REPL/script). An
            // internal alias from any other package — including one whose
            // `package` hasn't been populated (`None`) while `current_package`
            // is `Some` — is never seeded.
            if info.is_internal
                && current_package.is_some()
                && info.package.as_deref() != current_package
            {
                continue;
            }

            if hierarchy.has_class(&info.name) {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "Pre-loaded type alias `{}` collides with class of the same name — \
                             type aliases share the class/protocol namespace",
                            info.name
                        ),
                        info.span,
                    )
                    .with_hint(format!(
                        "Rename the type alias in its defining file to avoid conflicting with class `{}`",
                        info.name
                    ))
                    .with_category(DiagnosticCategory::Type),
                );
                continue;
            }

            if protocol_registry.has_protocol(&info.name) {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "Pre-loaded type alias `{}` collides with protocol of the same name — \
                             type aliases share the class/protocol namespace",
                            info.name
                        ),
                        info.span,
                    )
                    .with_hint(format!(
                        "Rename the type alias in its defining file to avoid conflicting with protocol `{}`",
                        info.name
                    ))
                    .with_category(DiagnosticCategory::Type),
                );
                continue;
            }

            if self.aliases.contains_key(&info.name) {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "Pre-loaded type alias `{}` collides with another type alias of the same name",
                            info.name
                        ),
                        info.span,
                    )
                    .with_hint(format!(
                        "Rename one of the `{}` type aliases — cross-package alias name collisions \
                         always have a zero-cost workaround (write the expansion, or re-declare a \
                         local alias under a different name)",
                        info.name
                    ))
                    .with_category(DiagnosticCategory::Type),
                );
                continue;
            }

            self.aliases.entry(info.name.clone()).or_insert(info);
        }
        diagnostics
    }

    /// Looks up an alias by name.
    #[must_use]
    pub fn get(&self, name: &str) -> Option<&AliasInfo> {
        self.aliases.get(name)
    }

    /// Checks if an alias exists in the registry.
    #[must_use]
    pub fn has_alias(&self, name: &str) -> bool {
        self.aliases.contains_key(name)
    }

    /// Returns an iterator over all alias names in the registry.
    pub fn alias_names(&self) -> impl Iterator<Item = &EcoString> {
        self.aliases.keys()
    }

    /// Returns an iterator over all aliases in the registry.
    pub fn aliases(&self) -> impl Iterator<Item = (&EcoString, &AliasInfo)> {
        self.aliases.iter()
    }

    /// Register an alias directly for testing purposes.
    ///
    /// Bypasses the module registration path — useful for unit tests (and
    /// [`resolve_type_annotation`](crate::semantic_analysis::type_checker::resolve_type_annotation)'s
    /// own tests) that need an alias table without constructing a full
    /// `Module` AST.
    #[cfg(test)]
    pub fn register_test_alias(&mut self, info: AliasInfo) {
        self.aliases.insert(info.name.clone(), info);
    }
}

/// Walks a type annotation's structure, reporting "unbound type parameter"
/// diagnostics for bare single-letter identifiers not covered by a class in
/// `hierarchy` (ADR 0068's reserved-for-type-parameters convention).
///
/// Checks the class hierarchy *first* — a class literally named `T` is
/// legal and resolves as a nominal class exactly as
/// [`resolve_type_annotation`](crate::semantic_analysis::type_checker::resolve_type_annotation)
/// would — only emitting the diagnostic when the name is not found there.
/// Mirrors `infer_method_local_params`'s existing
/// `!hierarchy.has_class(name)` guard (ADR 0108 Semantics).
///
/// Only [`TypeAnnotation::Simple`] positions are checked — a bare
/// single-letter identifier standing alone in type position is exactly
/// ADR 0068's implicit-type-parameter shape. A [`TypeAnnotation::Generic`]
/// base name (e.g. the `Result` in `Result(T, E)`) is always a concrete type
/// constructor, never a bare parameter reference, so it is not checked; its
/// `parameters` are walked recursively like every other nested position.
fn check_unbound_type_vars(
    ann: &TypeAnnotation,
    hierarchy: &ClassHierarchy,
    alias_name: &EcoString,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match ann {
        TypeAnnotation::Simple(type_id) => {
            let name = &type_id.name;
            if is_generic_type_param(name) && !hierarchy.has_class(name) {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "unbound type parameter `{name}` in type alias `{alias_name}`; \
                             parametric aliases (`type Name(T) = ...`) are not yet supported"
                        ),
                        type_id.span,
                    )
                    .with_category(DiagnosticCategory::Type),
                );
            }
        }
        TypeAnnotation::Generic { parameters, .. } => {
            for param in parameters {
                check_unbound_type_vars(param, hierarchy, alias_name, diagnostics);
            }
        }
        TypeAnnotation::Union { types, .. } => {
            for member in types {
                check_unbound_type_vars(member, hierarchy, alias_name, diagnostics);
            }
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            check_unbound_type_vars(inner, hierarchy, alias_name, diagnostics);
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            check_unbound_type_vars(base, hierarchy, alias_name, diagnostics);
            check_unbound_type_vars(excluded, hierarchy, alias_name, diagnostics);
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            check_unbound_type_vars(left, hierarchy, alias_name, diagnostics);
            check_unbound_type_vars(right, hierarchy, alias_name, diagnostics);
        }
        // SelfType / SelfClass / ClassOf / Singleton have no nested
        // identifier position that could be a bare type parameter.
        TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. }
        | TypeAnnotation::ClassOf { .. }
        | TypeAnnotation::Singleton { .. } => {}
    }
}

/// Namespace collision check shared by [`AliasRegistry::register_module`]
/// and [`AliasRegistry::redefine_alias`]: an alias name colliding with an
/// already-registered class or protocol name (ADR 0108 Semantics — aliases
/// share the class/protocol namespace).
fn namespace_collision_diagnostic(
    name: &EcoString,
    span: Span,
    hierarchy: &ClassHierarchy,
    protocol_registry: &ProtocolRegistry,
) -> Option<Diagnostic> {
    if hierarchy.has_class(name) {
        return Some(
            Diagnostic::error(
                format!(
                    "`{name}` is already defined as a class — \
                     type aliases share the class/protocol namespace"
                ),
                span,
            )
            .with_hint(format!(
                "Rename the type alias to avoid conflicting with class `{name}`"
            ))
            .with_category(DiagnosticCategory::Type),
        );
    }
    if protocol_registry.has_protocol(name) {
        return Some(
            Diagnostic::error(
                format!(
                    "`{name}` is already defined as a protocol — \
                     type aliases share the class/protocol namespace"
                ),
                span,
            )
            .with_hint(format!(
                "Rename the type alias to avoid conflicting with protocol `{name}`"
            ))
            .with_category(DiagnosticCategory::Type),
        );
    }
    None
}

/// DFS visitation state for [`find_cycles`] — standard white/grey/black
/// cycle detection, collapsed to two states since a `Done` (black) node
/// never needs revisiting or distinguishing from "never seen" once its
/// subtree is fully explored.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VisitState {
    /// On the current DFS path — an edge back to a `Visiting` node is a
    /// cycle.
    Visiting,
    /// Fully explored; safe to skip without re-walking its subtree.
    Done,
}

/// Walks the alias dependency graph — edges are RHS references to *other
/// registered alias names* (nominal class names are not edges; see
/// [`collect_alias_references`]) — detecting reference cycles and producing
/// a topological resolution order as the DFS post-order, in one walk (ADR
/// 0108 "No recursion": "the same DFS the cycle check already needs …
/// produces this topological order for free as a side effect via its
/// post-order").
///
/// Operates package-wide over whatever `aliases` map it is given — the
/// caller decides scope (the whole registry for [`AliasRegistry::
/// register_module`], or a trial candidate map for [`AliasRegistry::
/// redefine_alias`]) — so a cycle spanning aliases declared across separate
/// `register_module` calls (e.g. one call per file in a package) is still
/// caught, and a legal forward reference across those same calls (`type B =
/// A | #z` registered before `type A = #x | #y`) is never mistaken for one:
/// this only walks *declared* references, it never tries to eagerly resolve
/// them the way a naïve single sequential pass would.
///
/// Iteration starts from names in sorted order, and `path`/`order` are
/// plain `Vec`s rather than a `HashSet`, so the result — which cycle is
/// found first, and the exact post-order — is deterministic across runs
/// regardless of `HashMap` iteration order.
///
/// Returns each detected cycle as a raw `(chain, span)` pair — `chain`'s
/// first and last elements are equal (see [`visit_alias`]), and `span` is
/// where the diagnostic should point — rather than a pre-built
/// [`Diagnostic`], so callers can dedupe or otherwise post-process before
/// formatting (see [`AliasRegistry::register_module`]'s use of
/// [`canonical_cycle_key`]). Use [`cycle_diagnostic`] to format one.
fn find_cycles(
    aliases: &HashMap<EcoString, AliasInfo>,
) -> (Vec<(Vec<EcoString>, Span)>, Vec<EcoString>) {
    let mut state: HashMap<EcoString, VisitState> = HashMap::new();
    let mut path: Vec<EcoString> = Vec::new();
    let mut order: Vec<EcoString> = Vec::new();
    let mut cycles: Vec<(Vec<EcoString>, Span)> = Vec::new();

    let mut names: Vec<EcoString> = aliases.keys().cloned().collect();
    names.sort();

    for name in names {
        if !state.contains_key(&name) {
            visit_alias(
                &name,
                aliases,
                &mut state,
                &mut path,
                &mut order,
                &mut cycles,
            );
        }
    }
    (cycles, order)
}

/// The recursive DFS step behind [`find_cycles`]. See that function's doc
/// for the overall algorithm and its guarantees.
fn visit_alias(
    name: &EcoString,
    aliases: &HashMap<EcoString, AliasInfo>,
    state: &mut HashMap<EcoString, VisitState>,
    path: &mut Vec<EcoString>,
    order: &mut Vec<EcoString>,
    cycles: &mut Vec<(Vec<EcoString>, Span)>,
) {
    state.insert(name.clone(), VisitState::Visiting);
    path.push(name.clone());

    if let Some(info) = aliases.get(name) {
        let mut deps = Vec::new();
        collect_alias_references(&info.annotation, aliases, &mut deps);
        // Dedupe: a name referenced more than once in one RHS (`type A = A
        // | A`, or `type A = B | B` where `B` closes a cycle back to `A`) is
        // one graph edge, not two — `collect_alias_references` does not
        // dedupe (it is a straightforward structural walk), so a name
        // repeated in the RHS would otherwise be visited/checked twice here,
        // and a repeated *back-edge* would report the same cycle twice.
        let mut seen_deps: std::collections::HashSet<EcoString> = std::collections::HashSet::new();
        deps.retain(|dep| seen_deps.insert(dep.clone()));
        for dep in deps {
            match state.get(&dep) {
                None => visit_alias(&dep, aliases, state, path, order, cycles),
                Some(VisitState::Visiting) => {
                    // Back-edge to an ancestor still on the current path —
                    // a cycle. The chain from that ancestor's first
                    // occurrence through to here (plus repeating the
                    // ancestor once more) is exactly the `A → B → A` shape
                    // ADR 0108's Error examples specify. `position` is
                    // guaranteed to find `dep` (a `Visiting` node is always
                    // still on `path` — it was pushed when marked `Visiting`
                    // and is only popped after being marked `Done`) but
                    // falls back to the start of the path rather than
                    // panicking if that invariant is ever violated by a
                    // future refactor.
                    let start_idx = path.iter().position(|n| *n == dep).unwrap_or(0);
                    let mut cycle: Vec<EcoString> = path[start_idx..].to_vec();
                    cycle.push(dep.clone());
                    let span = aliases.get(&cycle[0]).map_or(Span::new(0, 0), |i| i.span);
                    cycles.push((cycle, span));
                }
                // Already fully explored (possibly via a different path) —
                // no cycle through this edge, and no need to re-walk it.
                Some(VisitState::Done) => {}
            }
        }
    }

    state.insert(name.clone(), VisitState::Done);
    path.pop();
    order.push(name.clone());
}

/// Canonicalises a cycle chain (as returned by [`find_cycles`], first and
/// last elements equal) to a rotation-independent key: the chain's "core"
/// (its members without the repeated closing name), rotated to start at its
/// lexicographically smallest member. The same directed cycle discovered
/// from two different entry points (e.g. `register_module` seeing `Ab`
/// before `Bc` in one call, then `Bc` before `Ab` in another) canonicalises
/// to the same key, which is what makes deduplication in
/// [`AliasRegistry::reported_cycles`](AliasRegistry) sound.
fn canonical_cycle_key(cycle: &[EcoString]) -> Vec<EcoString> {
    let core = &cycle[..cycle.len().saturating_sub(1)];
    let min_idx = core
        .iter()
        .enumerate()
        .min_by_key(|(_, name)| name.as_str())
        .map_or(0, |(idx, _)| idx);
    core[min_idx..]
        .iter()
        .chain(core[..min_idx].iter())
        .cloned()
        .collect()
}

/// Collects the names, among `ann`'s referenced identifiers, that are
/// registered alias names in `aliases` — i.e. the dependency-graph edges
/// out of the alias whose RHS is `ann`. A `Simple`/`Generic` name that is
/// *not* in `aliases` is an ordinary nominal class reference, not a graph
/// edge — existence of that class is checked elsewhere, not here.
fn collect_alias_references(
    ann: &TypeAnnotation,
    aliases: &HashMap<EcoString, AliasInfo>,
    out: &mut Vec<EcoString>,
) {
    match ann {
        TypeAnnotation::Simple(type_id) => {
            if aliases.contains_key(&type_id.name) {
                out.push(type_id.name.clone());
            }
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            if aliases.contains_key(&base.name) {
                out.push(base.name.clone());
            }
            for param in parameters {
                collect_alias_references(param, aliases, out);
            }
        }
        TypeAnnotation::Union { types, .. } => {
            for member in types {
                collect_alias_references(member, aliases, out);
            }
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            collect_alias_references(inner, aliases, out);
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            collect_alias_references(base, aliases, out);
            collect_alias_references(excluded, aliases, out);
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            collect_alias_references(left, aliases, out);
            collect_alias_references(right, aliases, out);
        }
        // SelfType / SelfClass / ClassOf / Singleton have no nested
        // identifier position that could reference another alias.
        TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. }
        | TypeAnnotation::ClassOf { .. }
        | TypeAnnotation::Singleton { .. } => {}
    }
}

/// Builds the "type alias cycle: `A` → `B` → `A`" diagnostic ADR 0108's
/// Error examples specify, given the full cycle chain (first and last
/// elements equal — see [`visit_alias`]).
fn cycle_diagnostic(cycle: &[EcoString], span: Span) -> Diagnostic {
    let chain = cycle
        .iter()
        .map(|n| format!("`{n}`"))
        .collect::<Vec<_>>()
        .join(" \u{2192} "); // "→"
    let mut message = format!("type alias cycle: {chain}");
    if cycle.len() == 2 && cycle[0] == cycle[1] {
        message.push_str(" (self-reference)");
    }
    Diagnostic::error(message, span)
        .with_hint(
            "Break the cycle by removing or redirecting one of the alias references — \
             recursive aliases are not supported (ADR 0108)",
        )
        .with_category(DiagnosticCategory::Type)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Identifier;

    fn span() -> Span {
        Span::new(0, 1)
    }

    fn ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: span(),
        }
    }

    fn alias_def(name: &str, annotation: TypeAnnotation) -> TypeAliasDefinition {
        internal_alias_def(name, annotation, false)
    }

    fn internal_alias_def(
        name: &str,
        annotation: TypeAnnotation,
        is_internal: bool,
    ) -> TypeAliasDefinition {
        TypeAliasDefinition {
            name: ident(name),
            annotation,
            is_internal,
            comments: crate::ast::CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }
    }

    fn singleton_union(names: &[&str]) -> TypeAnnotation {
        TypeAnnotation::Union {
            types: names
                .iter()
                .map(|n| TypeAnnotation::Singleton {
                    name: (*n).into(),
                    span: span(),
                })
                .collect(),
            span: span(),
        }
    }

    fn empty_module() -> Module {
        Module::new(vec![], span())
    }

    // ---- Basic registration ----

    #[test]
    fn register_simple_alias() {
        let module = Module {
            type_aliases: vec![alias_def(
                "RestartStrategy",
                singleton_union(&["temporary", "transient", "permanent"]),
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        assert!(registry.has_alias("RestartStrategy"));
        let info = registry.get("RestartStrategy").unwrap();
        assert_eq!(info.name.as_str(), "RestartStrategy");
    }

    #[test]
    fn register_multiple_aliases() {
        let module = Module {
            type_aliases: vec![
                alias_def("TimeoutMs", TypeAnnotation::Simple(ident("Integer"))),
                alias_def("JsonKey", singleton_union(&["a", "b"])),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        assert!(registry.has_alias("TimeoutMs"));
        assert!(registry.has_alias("JsonKey"));
    }

    // ---- Namespace collision: alias vs. class / protocol (bidirectional) ----

    #[test]
    fn namespace_collision_alias_vs_builtin_class() {
        // `Integer` is a built-in class — an alias of the same name must be
        // rejected and must not shadow the class in resolution.
        let module = Module {
            type_aliases: vec![alias_def(
                "Integer",
                TypeAnnotation::Simple(ident("String")),
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("already defined as a class"));
        assert!(!registry.has_alias("Integer"));
    }

    #[test]
    fn namespace_collision_alias_vs_protocol() {
        // A protocol `Printable` already registered — an alias of the same
        // name must be rejected. This is the "new plumbing" direction the
        // pre-existing `protocol_registry.rs:296-334` check (class vs.
        // protocol only) cannot see.
        let printable = crate::ast::ProtocolDefinition {
            name: ident("Printable"),
            type_params: vec![],
            extending: None,
            method_signatures: vec![],
            class_method_signatures: vec![],
            comments: crate::ast::CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        };
        let proto_module = Module {
            protocols: vec![printable],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut protocol_registry = ProtocolRegistry::new();
        let proto_diags = protocol_registry.register_module(&proto_module, &hierarchy);
        assert!(proto_diags.is_empty());

        let module = Module {
            type_aliases: vec![alias_def(
                "Printable",
                TypeAnnotation::Simple(ident("String")),
            )],
            ..empty_module()
        };
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("already defined as a protocol"));
        assert!(!registry.has_alias("Printable"));
    }

    #[test]
    fn namespace_collision_is_order_independent_within_a_module() {
        // ADR 0108: batch registration order (classes → protocols →
        // aliases) means a class/protocol that *textually* follows a `type`
        // declaration in the same module still collides — the whole module
        // is parsed up front, so processing order (not textual order)
        // determines what the alias side sees. This demonstrates the
        // "vice versa" bidirectionality the AC calls for within a batch
        // compile: it doesn't matter that a user wrote `type Foo = ...`
        // above `Object subclass: Foo` in the source file.
        let module = Module {
            classes: vec![crate::ast::ClassDefinition::new(
                ident("Foo"),
                ident("Object"),
                vec![],
                vec![],
                span(),
            )],
            type_aliases: vec![alias_def("Foo", TypeAnnotation::Simple(ident("String")))],
            ..empty_module()
        };
        let (hierarchy, hierarchy_diags) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        assert!(hierarchy_diags.is_empty());
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 1, "expected collision, got: {diags:?}");
        assert!(diags[0].message.contains("already defined as a class"));
        assert!(!registry.has_alias("Foo"));
    }

    #[test]
    fn duplicate_alias_definition() {
        let module = Module {
            type_aliases: vec![
                alias_def("TimeoutMs", TypeAnnotation::Simple(ident("Integer"))),
                alias_def("TimeoutMs", TypeAnnotation::Simple(ident("String"))),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Duplicate type alias"));
        // First definition wins.
        assert_eq!(
            registry.get("TimeoutMs").unwrap().annotation,
            TypeAnnotation::Simple(ident("Integer"))
        );
    }

    // ---- Unbound type-variable check on RHS ----

    #[test]
    fn unbound_single_letter_on_rhs_errors() {
        // `type Timeout = Integer | T` — ADR 0108 Error examples.
        let module = Module {
            type_aliases: vec![alias_def(
                "Timeout",
                TypeAnnotation::Union {
                    types: vec![
                        TypeAnnotation::Simple(ident("Integer")),
                        TypeAnnotation::Simple(ident("T")),
                    ],
                    span: span(),
                },
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 1, "expected one diagnostic, got: {diags:?}");
        assert!(diags[0].message.contains("unbound type parameter `T`"));
        assert!(diags[0].message.contains("Timeout"));
        // Non-fatal: the alias still registers.
        assert!(registry.has_alias("Timeout"));
    }

    #[test]
    fn single_letter_matching_real_class_is_not_unbound() {
        // A class literally named `T` is legal and must resolve as a
        // nominal class, not trigger the unbound-type-parameter error
        // (ADR 0108 Semantics — mirrors `infer_method_local_params`'s
        // `!hierarchy.has_class(name)` guard).
        let module = Module {
            classes: vec![crate::ast::ClassDefinition::new(
                ident("T"),
                ident("Object"),
                vec![],
                vec![],
                span(),
            )],
            type_aliases: vec![alias_def("Wrapper", TypeAnnotation::Simple(ident("T")))],
            ..empty_module()
        };
        let (hierarchy, _) = ClassHierarchy::build(&module);
        let hierarchy = hierarchy.unwrap();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        assert!(registry.has_alias("Wrapper"));
    }

    #[test]
    fn unbound_type_var_nested_in_intersection_and_difference() {
        // `type Weird = (A & String) \ B` — both nested positions must be
        // walked, not just the top-level annotation shape.
        let module = Module {
            type_aliases: vec![alias_def(
                "Weird",
                TypeAnnotation::difference(
                    TypeAnnotation::intersection(
                        TypeAnnotation::Simple(ident("A")),
                        TypeAnnotation::Simple(ident("String")),
                        span(),
                    ),
                    TypeAnnotation::Simple(ident("B")),
                    span(),
                ),
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 2, "expected A and B unbound, got: {diags:?}");
        assert!(diags.iter().any(|d| d.message.contains('A')));
        assert!(diags.iter().any(|d| d.message.contains('B')));
    }

    #[test]
    fn multi_letter_unknown_rhs_name_is_not_flagged_here() {
        // Multi-letter unknown names are unaffected by this check (ADR 0108)
        // — they fall through to ordinary `resolve_type_annotation` at
        // reference-resolution time, unrelated to this registration-time
        // pass.
        let module = Module {
            type_aliases: vec![alias_def(
                "Wrapper",
                TypeAnnotation::Simple(ident("TotallyUnknownClass")),
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    }

    // ---- Cycle detection + topological sort (ADR 0108 "No recursion", BT-2896) ----

    #[test]
    fn self_reference_alias_is_a_cycle() {
        // `type Loop = Loop | Integer` — a self-reference is a cycle of
        // length one. (Multi-letter name: ADR 0108 reserves bare single
        // uppercase letters for generic type parameters.)
        let module = Module {
            type_aliases: vec![alias_def(
                "Loop",
                TypeAnnotation::Union {
                    types: vec![
                        TypeAnnotation::Simple(ident("Loop")),
                        TypeAnnotation::Simple(ident("Integer")),
                    ],
                    span: span(),
                },
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 1, "expected one cycle diagnostic: {diags:?}");
        assert!(diags[0].message.contains("type alias cycle"));
        assert!(diags[0].message.contains("Loop"));
        assert!(diags[0].message.contains("self-reference"));
        // Non-fatal: the alias still registers (see `register_module` doc) so
        // a slipped-through reference to it does not become a spurious
        // "unknown type" error on top of the cycle diagnostic.
        assert!(registry.has_alias("Loop"));
    }

    #[test]
    fn two_alias_cycle_is_detected() {
        // ADR 0108 Error examples, verbatim:
        //   type Ab = Bc | Integer
        //   type Bc = Ab | Symbol
        //   // Error: type alias cycle: `Ab` → `Bc` → `Ab`
        let module = Module {
            type_aliases: vec![
                alias_def(
                    "Ab",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Bc")),
                            TypeAnnotation::Simple(ident("Integer")),
                        ],
                        span: span(),
                    },
                ),
                alias_def(
                    "Bc",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Ab")),
                            TypeAnnotation::Simple(ident("Symbol")),
                        ],
                        span: span(),
                    },
                ),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 1, "expected one cycle diagnostic: {diags:?}");
        assert!(
            diags[0]
                .message
                .contains("type alias cycle: `Ab` \u{2192} `Bc` \u{2192} `Ab`"),
            "unexpected message: {}",
            diags[0].message
        );
        // Both aliases still register — see `self_reference_alias_is_a_cycle`.
        assert!(registry.has_alias("Ab"));
        assert!(registry.has_alias("Bc"));
    }

    #[test]
    fn three_alias_cycle_is_detected() {
        // type Xa = Yb, type Yb = Zc, type Zc = Xa | Integer
        let module = Module {
            type_aliases: vec![
                alias_def("Xa", TypeAnnotation::Simple(ident("Yb"))),
                alias_def("Yb", TypeAnnotation::Simple(ident("Zc"))),
                alias_def(
                    "Zc",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Xa")),
                            TypeAnnotation::Simple(ident("Integer")),
                        ],
                        span: span(),
                    },
                ),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 1, "expected one cycle diagnostic: {diags:?}");
        assert!(
            diags[0]
                .message
                .contains("type alias cycle: `Xa` \u{2192} `Yb` \u{2192} `Zc` \u{2192} `Xa`"),
            "unexpected message: {}",
            diags[0].message
        );
    }

    #[test]
    fn non_cyclic_chain_produces_no_cycle_diagnostic() {
        // type Pa = Integer, type Qb = Pa, type Rc = Qb — a legal acyclic
        // chain must not be flagged.
        let module = Module {
            type_aliases: vec![
                alias_def("Pa", TypeAnnotation::Simple(ident("Integer"))),
                alias_def("Qb", TypeAnnotation::Simple(ident("Pa"))),
                alias_def("Rc", TypeAnnotation::Simple(ident("Qb"))),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    }

    #[test]
    fn find_cycles_post_order_is_a_valid_topological_order() {
        // ADR 0108 "No recursion": "the same DFS the cycle check already
        // needs ... produces this topological order for free as a side
        // effect via its post-order" — asserted directly against the raw
        // algorithm here, not just indirectly via `register_module`'s
        // pass/fail outcome. type Pa = Integer, type Qb = Pa, type Rc = Qb:
        // `Pa` (no dependencies) must resolve before `Qb`, which must
        // resolve before `Rc`.
        let mut aliases = HashMap::new();
        aliases.insert(
            EcoString::from("Pa"),
            AliasInfo {
                name: "Pa".into(),
                annotation: TypeAnnotation::Simple(ident("Integer")),
                is_internal: false,
                package: None,
                span: span(),
            },
        );
        aliases.insert(
            EcoString::from("Qb"),
            AliasInfo {
                name: "Qb".into(),
                annotation: TypeAnnotation::Simple(ident("Pa")),
                is_internal: false,
                package: None,
                span: span(),
            },
        );
        aliases.insert(
            EcoString::from("Rc"),
            AliasInfo {
                name: "Rc".into(),
                annotation: TypeAnnotation::Simple(ident("Qb")),
                is_internal: false,
                package: None,
                span: span(),
            },
        );

        let (cycles, order) = find_cycles(&aliases);
        assert!(cycles.is_empty(), "unexpected cycles: {cycles:?}");
        assert_eq!(
            order.len(),
            3,
            "expected all three names in the order: {order:?}"
        );
        let position = |n: &str| order.iter().position(|x| x == n).unwrap();
        assert!(
            position("Pa") < position("Qb"),
            "Pa must resolve before Qb: {order:?}"
        );
        assert!(
            position("Qb") < position("Rc"),
            "Qb must resolve before Rc: {order:?}"
        );
    }

    #[test]
    fn repeated_register_module_calls_report_the_same_cycle_only_once() {
        // BT-2896: `register_module` re-scans the *whole* accumulated
        // registry on every call (see its doc) so a package-wide caller
        // seeding aliases across several files — one `register_module` call
        // per file — still catches a cross-file cycle. But that means a
        // cycle already reported by an earlier call must not be re-reported
        // by a later call that merely adds unrelated aliases; `Ab`/`Bc`'s
        // cycle must be flagged exactly once across both calls below.
        let cyclic_file = Module {
            type_aliases: vec![
                alias_def(
                    "Ab",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Bc")),
                            TypeAnnotation::Simple(ident("Integer")),
                        ],
                        span: span(),
                    },
                ),
                alias_def(
                    "Bc",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Ab")),
                            TypeAnnotation::Simple(ident("Symbol")),
                        ],
                        span: span(),
                    },
                ),
            ],
            ..empty_module()
        };
        let unrelated_file = Module {
            type_aliases: vec![alias_def(
                "Unrelated",
                TypeAnnotation::Simple(ident("Integer")),
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();

        let diags1 = registry.register_module(&cyclic_file, &hierarchy, &protocol_registry);
        assert_eq!(diags1.len(), 1, "expected one cycle diagnostic: {diags1:?}");

        let diags2 = registry.register_module(&unrelated_file, &hierarchy, &protocol_registry);
        assert!(
            diags2.is_empty(),
            "the already-reported Ab/Bc cycle must not resurface: {diags2:?}"
        );
        assert!(registry.has_alias("Unrelated"));
    }

    #[test]
    fn multi_file_forward_reference_resolves_without_cycle() {
        // ADR 0108 "No recursion" / BT-2896: `type B = A | #z` and `type A =
        // #x | #y` declared in the same package but *different files* — `B`
        // registered (via its own `register_module` call, simulating file 1)
        // before `A` is even known (file 2's `register_module` call hasn't
        // happened yet). A naive single sequential pass would spuriously
        // report "unknown type `A`" here; the package-wide DFS must not,
        // because it only walks *declared* references and defers judgement
        // until the whole graph (both calls) is in.
        let file1 = Module {
            type_aliases: vec![alias_def(
                "Gb",
                TypeAnnotation::Union {
                    types: vec![
                        TypeAnnotation::Simple(ident("Fa")),
                        TypeAnnotation::Singleton {
                            name: "z".into(),
                            span: span(),
                        },
                    ],
                    span: span(),
                },
            )],
            ..empty_module()
        };
        let file2 = Module {
            type_aliases: vec![alias_def("Fa", singleton_union(&["x", "y"]))],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();

        let diags1 = registry.register_module(&file1, &hierarchy, &protocol_registry);
        assert!(diags1.is_empty(), "unexpected diagnostics: {diags1:?}");
        let diags2 = registry.register_module(&file2, &hierarchy, &protocol_registry);
        assert!(diags2.is_empty(), "unexpected diagnostics: {diags2:?}");

        assert!(registry.has_alias("Fa"));
        assert!(registry.has_alias("Gb"));

        // `Gb` resolves all the way through to `Fa`'s expansion, exactly as
        // it would if both were declared in one file.
        let resolved = crate::semantic_analysis::type_checker::resolve_type_annotation(
            &TypeAnnotation::Simple(ident("Gb")),
            &HashMap::new(),
            None,
            Some(&registry),
        );
        let crate::semantic_analysis::type_checker::InferredType::Union { members, .. } = resolved
        else {
            panic!("expected Union, got {resolved:?}");
        };
        assert_eq!(members.len(), 3, "expected #x | #y | #z, got {members:?}");
    }

    // ---- Live redefinition (ADR 0108 "No recursion", BT-2896) ----

    #[test]
    fn clean_redefinition_commits() {
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "Timeout".into(),
            annotation: TypeAnnotation::Simple(ident("Integer")),
            is_internal: false,
            package: None,
            span: span(),
        });

        let diags = registry.redefine_alias(
            &alias_def("Timeout", TypeAnnotation::Simple(ident("Float"))),
            &hierarchy,
            &protocol_registry,
        );

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        assert_eq!(
            registry.get("Timeout").unwrap().annotation,
            TypeAnnotation::Simple(ident("Float"))
        );
    }

    #[test]
    fn live_redefinition_introducing_cycle_is_rejected() {
        // `type Ha = Integer`, `type Ib = Ha` — both legal at declaration
        // time. Live-redefining `Ha` to `type Ha = Ib` introduces a cycle the
        // one-shot declaration check already cleared (ADR 0108 "No
        // recursion") — it must be caught, not silently accepted.
        let module = Module {
            type_aliases: vec![
                alias_def("Ha", TypeAnnotation::Simple(ident("Integer"))),
                alias_def("Ib", TypeAnnotation::Simple(ident("Ha"))),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");

        let redefine_diags = registry.redefine_alias(
            &alias_def("Ha", TypeAnnotation::Simple(ident("Ib"))),
            &hierarchy,
            &protocol_registry,
        );

        assert_eq!(
            redefine_diags.len(),
            1,
            "expected one cycle diagnostic: {redefine_diags:?}"
        );
        assert!(redefine_diags[0].message.contains("type alias cycle"));
    }

    #[test]
    fn rejected_redefinition_preserves_old_binding() {
        // A rejected redefinition — whatever the reason — must leave the
        // alias table completely unchanged, so a failed live edit can never
        // strand a dependent annotation resolving against a missing alias
        // (ADR 0108 "No recursion").
        let module = Module {
            type_aliases: vec![
                alias_def("Ha", TypeAnnotation::Simple(ident("Integer"))),
                alias_def("Ib", TypeAnnotation::Simple(ident("Ha"))),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        registry.register_module(&module, &hierarchy, &protocol_registry);

        // Cycle-triggering redefinition — must be rejected.
        let diags = registry.redefine_alias(
            &alias_def("Ha", TypeAnnotation::Simple(ident("Ib"))),
            &hierarchy,
            &protocol_registry,
        );
        assert!(!diags.is_empty());

        // `Ha`'s original binding is untouched...
        assert_eq!(
            registry.get("Ha").unwrap().annotation,
            TypeAnnotation::Simple(ident("Integer"))
        );
        // ...so `Ib`, which depends on `Ha`, still resolves cleanly instead
        // of pointing at a missing or broken alias.
        let resolved = crate::semantic_analysis::type_checker::resolve_type_annotation(
            &TypeAnnotation::Simple(ident("Ib")),
            &HashMap::new(),
            None,
            Some(&registry),
        );
        assert_eq!(
            resolved,
            crate::semantic_analysis::type_checker::InferredType::known("Integer")
        );
    }

    #[test]
    fn redefinition_colliding_with_class_is_rejected_and_preserves_old_binding() {
        // A non-cycle declaration-time error (namespace collision) must also
        // roll back — "any other declaration-time error" per ADR 0108.
        let class_module = Module {
            classes: vec![crate::ast::ClassDefinition::new(
                ident("Foo"),
                ident("Object"),
                vec![],
                vec![],
                span(),
            )],
            ..empty_module()
        };
        let (hierarchy, _) = ClassHierarchy::build(&class_module);
        let hierarchy = hierarchy.unwrap();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "Timeout".into(),
            annotation: TypeAnnotation::Simple(ident("Integer")),
            is_internal: false,
            package: None,
            span: span(),
        });

        let diags = registry.redefine_alias(
            &alias_def("Timeout", TypeAnnotation::Simple(ident("Foo"))),
            &hierarchy,
            &protocol_registry,
        );
        // This redefinition doesn't rename `Timeout` itself, so the
        // collision check (which only guards the *name being defined*, not
        // its RHS) doesn't fire here; assert the redefinition of `Timeout`
        // to reference `Foo` succeeds, then separately verify a redefinition
        // that collides on the name itself rolls back.
        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");

        let collision_diags = registry.redefine_alias(
            &alias_def("Foo", TypeAnnotation::Simple(ident("String"))),
            &hierarchy,
            &protocol_registry,
        );
        assert_eq!(collision_diags.len(), 1);
        assert!(
            collision_diags[0]
                .message
                .contains("already defined as a class")
        );
        assert!(!registry.has_alias("Foo"));
    }

    #[test]
    fn doubled_self_reference_reports_exactly_one_cycle_diagnostic() {
        // `type Loop = Loop | Loop` — `Loop` appears twice on the RHS, which
        // must still surface as *one* cycle, not two: a name referenced more
        // than once in a single RHS is one graph edge, not one per textual
        // occurrence. Checked through both `register_module` and
        // `redefine_alias`, since the two paths dedupe cycle diagnostics via
        // different mechanisms (cross-call `reported_cycles` vs. nothing —
        // both rely on `visit_alias` deduping the *edges* it walks).
        let module = Module {
            type_aliases: vec![alias_def(
                "Loop",
                TypeAnnotation::Union {
                    types: vec![
                        TypeAnnotation::Simple(ident("Loop")),
                        TypeAnnotation::Simple(ident("Loop")),
                    ],
                    span: span(),
                },
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);
        assert_eq!(
            diags.len(),
            1,
            "register_module: expected exactly one cycle diagnostic: {diags:?}"
        );

        // Redefining a fresh registry's `Loop` with the same doubled
        // self-reference must also report exactly one diagnostic.
        let mut fresh = AliasRegistry::new();
        let redefine_diags = fresh.redefine_alias(
            &alias_def(
                "Loop",
                TypeAnnotation::Union {
                    types: vec![
                        TypeAnnotation::Simple(ident("Loop")),
                        TypeAnnotation::Simple(ident("Loop")),
                    ],
                    span: span(),
                },
            ),
            &hierarchy,
            &protocol_registry,
        );
        assert_eq!(
            redefine_diags.len(),
            1,
            "redefine_alias: expected exactly one cycle diagnostic: {redefine_diags:?}"
        );
    }

    #[test]
    fn diamond_dependency_graph_has_no_false_cycle() {
        // type Top = Left | Right, type Left = Bottom, type Right = Bottom,
        // type Bottom = Integer — `Bottom` is reachable from `Top` via two
        // paths but is not part of a cycle; the DFS's `Done`-skip must
        // recognise the second path as already-resolved, not a back-edge.
        let module = Module {
            type_aliases: vec![
                alias_def(
                    "Top",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Left")),
                            TypeAnnotation::Simple(ident("Right")),
                        ],
                        span: span(),
                    },
                ),
                alias_def("Left", TypeAnnotation::Simple(ident("Bottom"))),
                alias_def("Right", TypeAnnotation::Simple(ident("Bottom"))),
                alias_def("Bottom", TypeAnnotation::Simple(ident("Integer"))),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
    }

    #[test]
    fn two_disjoint_cycles_in_one_module_are_both_reported() {
        // Two unrelated 2-cycles declared in the same module: `Ab`/`Bc` and
        // `Xa`/`Yb`. Both must be reported — one problem must not mask the
        // other.
        let module = Module {
            type_aliases: vec![
                alias_def(
                    "Ab",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Bc")),
                            TypeAnnotation::Simple(ident("Integer")),
                        ],
                        span: span(),
                    },
                ),
                alias_def(
                    "Bc",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Ab")),
                            TypeAnnotation::Simple(ident("Symbol")),
                        ],
                        span: span(),
                    },
                ),
                alias_def(
                    "Xa",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Yb")),
                            TypeAnnotation::Simple(ident("Integer")),
                        ],
                        span: span(),
                    },
                ),
                alias_def(
                    "Yb",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Xa")),
                            TypeAnnotation::Simple(ident("Symbol")),
                        ],
                        span: span(),
                    },
                ),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(
            diags.len(),
            2,
            "expected both disjoint cycles reported: {diags:?}"
        );
        assert!(diags.iter().any(|d| d.message.contains("`Ab`")));
        assert!(diags.iter().any(|d| d.message.contains("`Xa`")));
    }

    #[test]
    fn cycle_through_generic_base_name_is_detected() {
        // type Wrap = Container(Integer), type Container = Wrap — a cycle
        // formed through a `Generic` annotation's *base* name (not a bare
        // `Simple` reference) must still be caught.
        let module = Module {
            type_aliases: vec![
                alias_def(
                    "Wrap",
                    TypeAnnotation::Generic {
                        base: ident("Container"),
                        parameters: vec![TypeAnnotation::Simple(ident("Integer"))],
                        span: span(),
                    },
                ),
                alias_def("Container", TypeAnnotation::Simple(ident("Wrap"))),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert_eq!(diags.len(), 1, "expected one cycle diagnostic: {diags:?}");
        assert!(diags[0].message.contains("type alias cycle"));
    }

    #[test]
    fn redefine_alias_defines_a_brand_new_name() {
        // `redefine_alias` is an upsert: a name with no prior binding is
        // simply defined, not just "redefined".
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        assert!(!registry.has_alias("Fresh"));

        let diags = registry.redefine_alias(
            &alias_def("Fresh", TypeAnnotation::Simple(ident("Integer"))),
            &hierarchy,
            &protocol_registry,
        );

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        assert_eq!(
            registry.get("Fresh").unwrap().annotation,
            TypeAnnotation::Simple(ident("Integer"))
        );
    }

    #[test]
    fn redefine_alias_with_identical_annotation_is_a_clean_no_op() {
        // Resubmitting the exact same annotation must commit cleanly, not
        // be mistaken for a self-cycle or rejected for any other reason.
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        registry.register_test_alias(AliasInfo {
            name: "Timeout".into(),
            annotation: TypeAnnotation::Simple(ident("Integer")),
            is_internal: false,
            package: None,
            span: span(),
        });

        let diags = registry.redefine_alias(
            &alias_def("Timeout", TypeAnnotation::Simple(ident("Integer"))),
            &hierarchy,
            &protocol_registry,
        );

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        assert_eq!(
            registry.get("Timeout").unwrap().annotation,
            TypeAnnotation::Simple(ident("Integer"))
        );
    }

    #[test]
    fn successful_redefinition_clears_stale_reported_cycles() {
        // BT-2896 adversarial finding: `register_module` remembers a
        // reported cycle's canonical key in `self.reported_cycles` so a
        // later `register_module` call doesn't re-report the same
        // still-unresolved cycle (see
        // `repeated_register_module_calls_report_the_same_cycle_only_once`).
        // But once a live `redefine_alias` call *breaks* that cycle, the
        // committed table is cycle-free by construction (the commit only
        // happens because `find_cycles` on the candidate found none) — so
        // nothing should be left in `reported_cycles` afterwards. A leftover
        // key would wrongly suppress a genuinely new occurrence of the same
        // cycle if it were reintroduced later (e.g. a subsequent file in a
        // package-wide compile).
        let module = Module {
            type_aliases: vec![
                alias_def(
                    "Ab",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Bc")),
                            TypeAnnotation::Simple(ident("Integer")),
                        ],
                        span: span(),
                    },
                ),
                alias_def(
                    "Bc",
                    TypeAnnotation::Union {
                        types: vec![
                            TypeAnnotation::Simple(ident("Ab")),
                            TypeAnnotation::Simple(ident("Symbol")),
                        ],
                        span: span(),
                    },
                ),
            ],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags1 = registry.register_module(&module, &hierarchy, &protocol_registry);
        assert_eq!(diags1.len(), 1, "expected the initial cycle: {diags1:?}");
        assert_eq!(
            registry.reported_cycles.len(),
            1,
            "the cycle should be remembered for cross-call dedup"
        );

        // Break the cycle live: `Bc` no longer references `Ab`.
        let break_diags = registry.redefine_alias(
            &alias_def("Bc", TypeAnnotation::Simple(ident("Symbol"))),
            &hierarchy,
            &protocol_registry,
        );
        assert!(
            break_diags.is_empty(),
            "unexpected diagnostics breaking the cycle: {break_diags:?}"
        );
        assert!(
            registry.reported_cycles.is_empty(),
            "a successful redefinition must clear stale cycle memory: {:?}",
            registry.reported_cycles
        );
    }

    // ---- `internal` modifier parsing (ADR 0071, BT-2898) ----

    #[test]
    fn internal_type_alias_definition_parses_as_internal() {
        let module = Module {
            type_aliases: vec![internal_alias_def(
                "ParserState",
                TypeAnnotation::Simple(ident("Integer")),
                true,
            )],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();
        let diags = registry.register_module(&module, &hierarchy, &protocol_registry);

        assert!(diags.is_empty(), "unexpected diagnostics: {diags:?}");
        let info = registry.get("ParserState").unwrap();
        assert!(
            info.is_internal,
            "expected the alias to register as internal"
        );
    }

    // ---- BT-2898: extract_alias_infos (pattern to mirror:
    //      ProtocolRegistry::extract_protocol_infos) ----

    #[test]
    fn extract_alias_infos_returns_all_declarations() {
        let module = Module {
            type_aliases: vec![
                alias_def("Public", TypeAnnotation::Simple(ident("String"))),
                internal_alias_def("Internal", TypeAnnotation::Simple(ident("Integer")), true),
            ],
            ..empty_module()
        };

        let infos = AliasRegistry::extract_alias_infos(&module);
        assert_eq!(infos.len(), 2);
        assert!(
            infos
                .iter()
                .any(|i| i.name.as_str() == "Public" && !i.is_internal)
        );
        assert!(
            infos
                .iter()
                .any(|i| i.name.as_str() == "Internal" && i.is_internal)
        );
    }

    // ---- BT-2898: add_pre_loaded (pattern to mirror:
    //      ProtocolRegistry::add_pre_loaded, BT-2006) ----

    fn alias_info(name: &str, annotation: TypeAnnotation, is_internal: bool) -> AliasInfo {
        alias_info_with_package(name, annotation, is_internal, None)
    }

    fn alias_info_with_package(
        name: &str,
        annotation: TypeAnnotation,
        is_internal: bool,
        package: Option<&str>,
    ) -> AliasInfo {
        AliasInfo {
            name: name.into(),
            annotation,
            is_internal,
            package: package.map(EcoString::from),
            span: span(),
        }
    }

    #[test]
    fn add_pre_loaded_accepts_non_colliding_alias() {
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();

        let alias = alias_info("Timeout", TypeAnnotation::Simple(ident("Integer")), false);
        let diags = registry.add_pre_loaded(vec![alias], &hierarchy, &protocol_registry, None);

        assert!(diags.is_empty());
        assert!(registry.has_alias("Timeout"));
    }

    #[test]
    fn add_pre_loaded_reports_collision_with_class() {
        // `Integer` is a built-in class — a pre-loaded alias of the same
        // name must be rejected and must not shadow the class.
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();

        let alias = alias_info("Integer", TypeAnnotation::Simple(ident("String")), false);
        let diags = registry.add_pre_loaded(vec![alias], &hierarchy, &protocol_registry, None);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("collides with class"));
        assert!(!registry.has_alias("Integer"));
    }

    #[test]
    fn add_pre_loaded_reports_collision_with_protocol() {
        let printable = crate::ast::ProtocolDefinition {
            name: ident("Printable"),
            type_params: vec![],
            extending: None,
            method_signatures: vec![],
            class_method_signatures: vec![],
            comments: crate::ast::CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        };
        let proto_module = Module {
            protocols: vec![printable],
            ..empty_module()
        };
        let hierarchy = ClassHierarchy::with_builtins();
        let mut protocol_registry = ProtocolRegistry::new();
        let proto_diags = protocol_registry.register_module(&proto_module, &hierarchy);
        assert!(proto_diags.is_empty());

        let mut registry = AliasRegistry::new();
        let alias = alias_info("Printable", TypeAnnotation::Simple(ident("String")), false);
        let diags = registry.add_pre_loaded(vec![alias], &hierarchy, &protocol_registry, None);

        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("collides with protocol"));
        assert!(!registry.has_alias("Printable"));
    }

    #[test]
    fn add_pre_loaded_reports_cross_package_alias_collision() {
        // Two dependencies each exporting `type Id = ...` — ADR 0108
        // Semantics: "Cross-package collisions ... are diagnosed at seeding
        // time." First entry wins; the second is reported and dropped.
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();

        let from_pkg_a = alias_info("Id", TypeAnnotation::Simple(ident("String")), false);
        let from_pkg_b = alias_info("Id", TypeAnnotation::Simple(ident("Integer")), false);
        let diags = registry.add_pre_loaded(
            vec![from_pkg_a, from_pkg_b],
            &hierarchy,
            &protocol_registry,
            None,
        );

        assert_eq!(
            diags.len(),
            1,
            "expected exactly one collision, got: {diags:?}"
        );
        assert!(
            diags[0]
                .message
                .contains("collides with another type alias")
        );
        // First entry (pkg_a's) wins.
        assert_eq!(
            registry.get("Id").unwrap().annotation,
            TypeAnnotation::Simple(ident("String"))
        );
    }

    #[test]
    fn add_pre_loaded_never_seeds_internal_alias_from_different_package() {
        // ADR 0108 Semantics: seeding-boundary exclusion — an `internal`
        // alias from a *different* package is never seeded into the
        // consumer's alias table at all, not merely hidden behind a
        // query-time filter.
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();

        let internal = alias_info_with_package(
            "ParserState",
            TypeAnnotation::Simple(ident("Integer")),
            true,
            Some("other_pkg"),
        );
        let diags =
            registry.add_pre_loaded(vec![internal], &hierarchy, &protocol_registry, Some("app"));

        assert!(
            diags.is_empty(),
            "an internal alias must not produce a collision diagnostic"
        );
        assert!(
            !registry.has_alias("ParserState"),
            "a different-package internal alias must never be visible in the consumer's table"
        );
    }

    #[test]
    fn add_pre_loaded_seeds_internal_alias_from_same_package() {
        // ADR 0108: `internal` scopes an alias to its *declaring package*,
        // not just its declaring file — another file in the same package
        // must still see it.
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();

        let internal = alias_info_with_package(
            "ParserState",
            TypeAnnotation::Simple(ident("Integer")),
            true,
            Some("json"),
        );
        let diags =
            registry.add_pre_loaded(vec![internal], &hierarchy, &protocol_registry, Some("json"));

        assert!(diags.is_empty());
        assert!(
            registry.has_alias("ParserState"),
            "a same-package internal alias from another file must remain visible"
        );
    }

    #[test]
    fn add_pre_loaded_never_seeds_internal_alias_with_unknown_package() {
        // An internal entry whose `package` hasn't been populated (`None`)
        // is conservatively treated as foreign when `current_package` is
        // `Some` — it must not be seeded just because we can't prove it's
        // foreign.
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();

        let internal = alias_info(
            "ParserState",
            TypeAnnotation::Simple(ident("Integer")),
            true,
        );
        let diags =
            registry.add_pre_loaded(vec![internal], &hierarchy, &protocol_registry, Some("app"));

        assert!(diags.is_empty());
        assert!(!registry.has_alias("ParserState"));
    }

    #[test]
    fn add_pre_loaded_seeds_internal_alias_when_no_package_context() {
        // REPL/script open-world context (`current_package: None`) — there
        // is no package boundary to enforce at all, mirroring
        // `ClassHierarchy::add_from_beam_meta`'s unconditional accept.
        let hierarchy = ClassHierarchy::with_builtins();
        let protocol_registry = ProtocolRegistry::new();
        let mut registry = AliasRegistry::new();

        let internal = alias_info(
            "ParserState",
            TypeAnnotation::Simple(ident("Integer")),
            true,
        );
        let diags = registry.add_pre_loaded(vec![internal], &hierarchy, &protocol_registry, None);

        assert!(diags.is_empty());
        assert!(registry.has_alias("ParserState"));
    }
}
