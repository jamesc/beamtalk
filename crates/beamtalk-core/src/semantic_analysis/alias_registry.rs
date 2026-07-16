// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type alias registry (ADR 0108 Phase 2, BT-2895).
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
//! **References:**
//! - `docs/ADR/0108-named-union-type-aliases.md` — Semantics (Namespace,
//!   Single-letter names), Implementation
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
    fn from_definition(def: &TypeAliasDefinition) -> Self {
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
}

impl AliasRegistry {
    /// Creates an empty alias registry.
    #[must_use]
    pub fn new() -> Self {
        Self {
            aliases: HashMap::new(),
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

            // Namespace collision: alias name matches a class name.
            if hierarchy.has_class(name) {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "`{name}` is already defined as a class — \
                             type aliases share the class/protocol namespace"
                        ),
                        alias_def.name.span,
                    )
                    .with_hint(format!(
                        "Rename the type alias to avoid conflicting with class `{name}`"
                    ))
                    .with_category(DiagnosticCategory::Type),
                );
                continue;
            }

            // Namespace collision: alias name matches a protocol name.
            if protocol_registry.has_protocol(name) {
                diagnostics.push(
                    Diagnostic::error(
                        format!(
                            "`{name}` is already defined as a protocol — \
                             type aliases share the class/protocol namespace"
                        ),
                        alias_def.name.span,
                    )
                    .with_hint(format!(
                        "Rename the type alias to avoid conflicting with protocol `{name}`"
                    ))
                    .with_category(DiagnosticCategory::Type),
                );
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
    /// packages.
    ///
    /// BT-2898: Mirrors `ProtocolRegistry::add_pre_loaded` (BT-2006). Skips
    /// entries whose names are already registered (current-module
    /// definitions win) and reports diagnostics for namespace collisions —
    /// a pre-loaded alias name that matches a class, protocol, or another
    /// already-seeded alias (e.g. two dependencies each exporting `type Id
    /// = ...`; ADR 0108 Semantics: "Cross-package collisions ... are
    /// diagnosed at seeding time").
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
