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

    /// Seeds the registry with already-validated aliases from outside the
    /// current module (ADR 0108 Phase 8, BT-2902).
    ///
    /// Unlike [`register_module`](Self::register_module), this performs no
    /// collision or unbound-type-variable checks — callers are expected to
    /// pass entries that were already validated once (e.g. a REPL session's
    /// previously-declared `type Name = ...` lines, re-parsed each turn
    /// since aliases erase to nothing at runtime and have no live BEAM
    /// artifact to recover cross-file class metadata from). A caller that
    /// wants the current module's own declarations to win a name collision
    /// (live redefinition) should filter `aliases` before calling this —
    /// see `AnalysisContext::pre_loaded_aliases`.
    pub fn add_pre_loaded(&mut self, aliases: Vec<AliasInfo>) {
        for info in aliases {
            self.aliases.insert(info.name.clone(), info);
        }
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
        TypeAliasDefinition {
            name: ident(name),
            annotation,
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
}
