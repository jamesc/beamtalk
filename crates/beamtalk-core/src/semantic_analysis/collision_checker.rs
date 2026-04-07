// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Cross-package class collision detection (ADR 0070 Phase 3).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Detects when two dependencies export the same class name and the
//! depending package references that name without a package qualifier.
//! Also enforces stdlib class name reservation for dependencies.
//!
//! Detection is **lazy**: collisions are only reported at the use site,
//! not eagerly when dependencies are loaded. If two packages both export
//! `Parser` but the depending code never references `Parser`, no error
//! is emitted.
//!
//! **References:**
//! - `docs/ADR/0070-package-namespaces-and-dependencies.md` Section 3

use std::collections::HashMap;

use crate::ast::{Expression, Module};
use crate::ast_walker;
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::source_analysis::{Diagnostic, DiagnosticCategory, Span};

/// A record of which package exports a given class name.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassExport {
    /// The package that exports this class.
    pub package: String,
    /// The compiled BEAM module name (e.g. `bt@json@parser`).
    pub module_name: String,
    /// Whether this export comes from a direct dependency (`true`) or a
    /// transitive dependency (`false`).
    pub is_direct: bool,
    /// For transitive dependencies, the chain of direct dependencies through
    /// which this package is reached. E.g., if `my_app` depends on `json`
    /// which depends on `utils`, then for `utils` this would be `["json"]`.
    /// Empty for direct dependencies.
    pub via_chain: Vec<String>,
}

/// Registry of class names exported by dependencies, indexed by class name.
///
/// When a class name maps to more than one entry, it is ambiguous and must
/// be referenced with a package qualifier (`json@Parser`).
#[derive(Debug, Clone, Default)]
pub struct DependencyRegistry {
    /// Maps unqualified class names to their exporting packages.
    /// A class with 2+ entries is ambiguous.
    exports: HashMap<String, Vec<ClassExport>>,
}

impl DependencyRegistry {
    /// Creates a new empty registry.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Returns true if the registry has any entries.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.exports.is_empty()
    }

    /// Registers a class exported by a direct dependency.
    pub fn register(&mut self, class_name: &str, package: &str, module_name: &str) {
        self.register_with_transitivity(class_name, package, module_name, true, Vec::new());
    }

    /// Registers a class exported by a dependency with transitivity metadata.
    ///
    /// `is_direct` indicates whether this is a direct dependency (`true`) or
    /// transitive (`false`). `via_chain` lists the intermediate packages for
    /// transitive dependencies (e.g., `["json"]` if reached through `json`).
    pub fn register_with_transitivity(
        &mut self,
        class_name: &str,
        package: &str,
        module_name: &str,
        is_direct: bool,
        via_chain: Vec<String>,
    ) {
        self.exports
            .entry(class_name.to_string())
            .or_default()
            .push(ClassExport {
                package: package.to_string(),
                module_name: module_name.to_string(),
                is_direct,
                via_chain,
            });
    }

    /// Returns the packages that export the given class name, if any.
    #[must_use]
    pub fn lookup(&self, class_name: &str) -> Option<&[ClassExport]> {
        self.exports.get(class_name).map(Vec::as_slice)
    }

    /// Returns true if the given class name is ambiguous (exported by 2+ packages).
    #[must_use]
    pub fn is_ambiguous(&self, class_name: &str) -> bool {
        self.exports
            .get(class_name)
            .is_some_and(|exports| exports.len() > 1)
    }

    /// Returns all class names that are exported by dependencies.
    #[must_use]
    pub fn all_class_names(&self) -> Vec<&str> {
        self.exports.keys().map(String::as_str).collect()
    }
}

/// Build a `DependencyRegistry` from per-dependency class module indexes.
///
/// Each entry in `dep_indexes` is `(package_name, class_module_index)` where
/// `class_module_index` maps class names to their compiled BEAM module names.
///
/// All dependencies are treated as direct (no transitive metadata).
/// Use [`build_dependency_registry_with_graph`] for transitive dep tracking.
#[must_use]
#[allow(clippy::implicit_hasher)]
pub fn build_dependency_registry(
    dep_indexes: &[(String, HashMap<String, String>)],
) -> DependencyRegistry {
    let mut registry = DependencyRegistry::new();
    for (package, index) in dep_indexes {
        for (class_name, module_name) in index {
            registry.register(class_name, package, module_name);
        }
    }
    registry
}

/// Information about a resolved dependency, including transitivity metadata.
#[derive(Debug, Clone)]
pub struct DepInfo {
    /// The package name.
    pub name: String,
    /// Maps class names to compiled BEAM module names.
    pub class_module_index: HashMap<String, String>,
    /// Whether this is a direct dependency of the root package.
    pub is_direct: bool,
    /// For transitive deps, the chain of packages through which this dep is reached.
    /// E.g., `["json"]` if the root depends on `json` which depends on this package.
    pub via_chain: Vec<String>,
}

/// Build a `DependencyRegistry` from dependency info that includes
/// transitivity metadata (ADR 0070 Phase 3).
///
/// This variant tracks whether each class export comes from a direct or
/// transitive dependency, enabling the W0302 warning for transitive dep usage.
#[must_use]
pub fn build_dependency_registry_with_graph(dep_infos: &[DepInfo]) -> DependencyRegistry {
    let mut registry = DependencyRegistry::new();
    for dep in dep_infos {
        for (class_name, module_name) in &dep.class_module_index {
            registry.register_with_transitivity(
                class_name,
                &dep.name,
                module_name,
                dep.is_direct,
                dep.via_chain.clone(),
            );
        }
    }
    registry
}

/// Check for stdlib reservation violations from dependencies.
///
/// Emits an error for each dependency class whose name matches a reserved
/// stdlib class name (e.g., `Integer`, `String`, `Actor`). This is checked
/// eagerly — unlike collision detection, stdlib reservation violations are
/// always reported, even if the name is never referenced.
///
/// Returns diagnostics with a synthetic span (0, 0) since the offending
/// class definition is in a dependency, not in the current compilation unit.
pub fn check_stdlib_reservation(registry: &DependencyRegistry, diagnostics: &mut Vec<Diagnostic>) {
    for class_name in registry.all_class_names() {
        if ClassHierarchy::is_runtime_protected_class(class_name) {
            if let Some(exports) = registry.lookup(class_name) {
                for export in exports {
                    diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "Dependency '{}' exports class `{}` which conflicts with a \
                                 stdlib class name. Stdlib class names are reserved.",
                                export.package, class_name
                            ),
                            Span::new(0, 0),
                        )
                        .with_hint(format!(
                            "The dependency must rename its `{class_name}` class. \
                             Stdlib names like `{class_name}` cannot be shadowed by dependencies."
                        )),
                    );
                }
            }
        }
    }
}

/// Check for ambiguous class references in a module.
///
/// Walks the AST looking for unqualified `ClassReference` expressions. When
/// the referenced class name is ambiguous (exported by 2+ packages), emits
/// an error suggesting qualified names.
///
/// References that already have a package qualifier (`json@Parser`) are not
/// checked — they are unambiguous by definition.
///
/// Also checks superclass references in class definitions.
pub fn check_collision_at_use_sites(
    module: &Module,
    registry: &DependencyRegistry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if registry.is_empty() {
        return;
    }

    // Check superclass references in class definitions
    for class in &module.classes {
        if let Some(ref superclass) = class.superclass {
            // Only check unqualified superclass references
            if class.superclass_package.is_none() {
                check_ambiguous_reference(
                    superclass.name.as_str(),
                    superclass.span,
                    registry,
                    diagnostics,
                );
            }
        }
    }

    // Check standalone method definition targets (Tonel-style `Class >> method`)
    for standalone in &module.method_definitions {
        if standalone.package.is_none() {
            check_ambiguous_reference(
                standalone.class_name.name.as_str(),
                standalone.class_name.span,
                registry,
                diagnostics,
            );
        }
    }

    // Walk all expressions looking for unqualified ClassReference nodes
    ast_walker::walk_module(module, &mut |expr| {
        if let Expression::ClassReference {
            name,
            package: None,
            span,
            ..
        } = expr
        {
            check_ambiguous_reference(name.name.as_str(), *span, registry, diagnostics);
        }
    });
}

/// Emit an error if the given class name is ambiguous in the dependency registry.
fn check_ambiguous_reference(
    class_name: &str,
    span: Span,
    registry: &DependencyRegistry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(exports) = registry.lookup(class_name) {
        if exports.len() > 1 {
            let qualified_suggestions: Vec<String> = exports
                .iter()
                .map(|e| format!("{}@{}", e.package, class_name))
                .collect();

            let note_lines: Vec<String> = exports
                .iter()
                .map(|e| {
                    format!(
                        "'{}' is defined in package '{}' ({}@{})",
                        class_name, e.package, e.package, class_name
                    )
                })
                .collect();

            diagnostics.push(
                Diagnostic::error(
                    format!("Class name '{class_name}' is exported by multiple dependencies"),
                    span,
                )
                .with_hint(format!(
                    "{}\nUse qualified name: {}",
                    note_lines.join("\n"),
                    qualified_suggestions.join(" or ")
                )),
            );
        }
    }
}

/// Check for usage of classes from transitive (non-direct) dependencies.
///
/// When a class reference resolves to a package that is only a transitive
/// dependency (not declared in the root package's `[dependencies]`), this
/// emits a warning W0302 (or an error when `strict_deps` is `true`).
///
/// The warning message includes the "via" chain showing how the transitive
/// dependency is reached. For example:
///
/// ```text
/// warning[W0302]: Class 'StringUtils' is from transitive dependency 'utils' (via 'json')
///   = help: add 'utils' to [dependencies] in beamtalk.toml to make this explicit
/// ```
///
/// **References:** ADR 0070 Section 2
pub fn check_transitive_dep_usage(
    module: &Module,
    registry: &DependencyRegistry,
    strict_deps: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if registry.is_empty() {
        return;
    }

    // Check superclass references
    for class in &module.classes {
        if let Some(ref superclass) = class.superclass {
            if class.superclass_package.is_none() {
                check_transitive_reference(
                    superclass.name.as_str(),
                    superclass.span,
                    registry,
                    strict_deps,
                    diagnostics,
                );
            }
        }
    }

    // Check standalone method definition targets
    for standalone in &module.method_definitions {
        if standalone.package.is_none() {
            check_transitive_reference(
                standalone.class_name.name.as_str(),
                standalone.class_name.span,
                registry,
                strict_deps,
                diagnostics,
            );
        }
    }

    // Walk all expressions looking for unqualified ClassReference nodes
    ast_walker::walk_module(module, &mut |expr| {
        if let Expression::ClassReference {
            name,
            package: None,
            span,
            ..
        } = expr
        {
            check_transitive_reference(
                name.name.as_str(),
                *span,
                registry,
                strict_deps,
                diagnostics,
            );
        }
    });
}

/// Emit a warning (or error in strict mode) if the given class name comes
/// from a transitive dependency.
fn check_transitive_reference(
    class_name: &str,
    span: Span,
    registry: &DependencyRegistry,
    strict_deps: bool,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if let Some(exports) = registry.lookup(class_name) {
        // Only check single-package exports (ambiguous names are handled by
        // check_collision_at_use_sites and would produce a confusing double error)
        if exports.len() == 1 {
            let export = &exports[0];
            if !export.is_direct {
                let via_desc = if export.via_chain.is_empty() {
                    String::new()
                } else {
                    format!(" (via '{}')", export.via_chain.join("' -> '"))
                };

                let message = format!(
                    "Class '{}' is from transitive dependency '{}'{}",
                    class_name, export.package, via_desc
                );

                let diag = if strict_deps {
                    Diagnostic::error(message, span)
                } else {
                    Diagnostic::warning(message, span).with_category(DiagnosticCategory::Type)
                };

                diagnostics.push(diag.with_hint(format!(
                    "Add '{}' to [dependencies] in beamtalk.toml to make this explicit",
                    export.package
                )));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::Severity;

    fn parse_module(src: &str) -> Module {
        let tokens = crate::source_analysis::lex_with_eof(src);
        let (module, diagnostics) = crate::source_analysis::parse(tokens);
        let parse_errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(
            parse_errors.is_empty(),
            "Test fixture failed to parse cleanly: {parse_errors:?}"
        );
        module
    }

    fn make_registry(entries: &[(&str, &str, &str)]) -> DependencyRegistry {
        let mut registry = DependencyRegistry::new();
        for (class, pkg, module) in entries {
            registry.register(class, pkg, module);
        }
        registry
    }

    // ── DependencyRegistry unit tests ─────────────────────────────────────

    #[test]
    fn empty_registry_has_no_ambiguity() {
        let registry = DependencyRegistry::new();
        assert!(!registry.is_ambiguous("Parser"));
        assert!(registry.is_empty());
    }

    #[test]
    fn single_export_is_not_ambiguous() {
        let registry = make_registry(&[("Parser", "json", "bt@json@parser")]);
        assert!(!registry.is_ambiguous("Parser"));
    }

    #[test]
    fn two_exports_same_name_is_ambiguous() {
        let registry = make_registry(&[
            ("Parser", "json", "bt@json@parser"),
            ("Parser", "xml", "bt@xml@parser"),
        ]);
        assert!(registry.is_ambiguous("Parser"));
    }

    #[test]
    fn different_names_are_not_ambiguous() {
        let registry = make_registry(&[
            ("JsonParser", "json", "bt@json@json_parser"),
            ("XmlParser", "xml", "bt@xml@xml_parser"),
        ]);
        assert!(!registry.is_ambiguous("JsonParser"));
        assert!(!registry.is_ambiguous("XmlParser"));
    }

    // ── build_dependency_registry ─────────────────────────────────────────

    #[test]
    fn build_registry_from_dep_indexes() {
        let deps = vec![
            (
                "json".to_string(),
                HashMap::from([("Parser".to_string(), "bt@json@parser".to_string())]),
            ),
            (
                "xml".to_string(),
                HashMap::from([("Parser".to_string(), "bt@xml@parser".to_string())]),
            ),
        ];

        let registry = build_dependency_registry(&deps);
        assert!(registry.is_ambiguous("Parser"));

        let exports = registry.lookup("Parser").unwrap();
        assert_eq!(exports.len(), 2);
    }

    // ── Stdlib reservation checks ─────────────────────────────────────────

    #[test]
    fn stdlib_reservation_error_for_protected_name() {
        let registry = make_registry(&[("Integer", "math_ext", "bt@math_ext@integer")]);
        let mut diagnostics = Vec::new();
        check_stdlib_reservation(&registry, &mut diagnostics);
        assert_eq!(diagnostics.len(), 1);
        assert!(diagnostics[0].message.contains("Integer"));
        assert!(diagnostics[0].message.contains("math_ext"));
        assert_eq!(diagnostics[0].severity, Severity::Error);
    }

    #[test]
    fn stdlib_reservation_no_error_for_user_name() {
        let registry = make_registry(&[("MyParser", "json", "bt@json@my_parser")]);
        let mut diagnostics = Vec::new();
        check_stdlib_reservation(&registry, &mut diagnostics);
        assert!(diagnostics.is_empty());
    }

    #[test]
    fn stdlib_reservation_multiple_deps_same_protected_name() {
        let registry = make_registry(&[
            ("String", "pkg_a", "bt@pkg_a@string"),
            ("String", "pkg_b", "bt@pkg_b@string"),
        ]);
        let mut diagnostics = Vec::new();
        check_stdlib_reservation(&registry, &mut diagnostics);
        // Each offending export gets its own error
        assert_eq!(diagnostics.len(), 2);
    }

    // ── Collision detection at use sites ───────────────────────────────────

    #[test]
    fn collision_error_for_ambiguous_class_reference() {
        let registry = make_registry(&[
            ("Parser", "json", "bt@json@parser"),
            ("Parser", "xml", "bt@xml@parser"),
        ]);

        // Source uses unqualified Parser
        let module = parse_module("Parser new");
        let mut diagnostics = Vec::new();
        check_collision_at_use_sites(&module, &registry, &mut diagnostics);

        assert_eq!(diagnostics.len(), 1);
        assert!(
            diagnostics[0]
                .message
                .contains("exported by multiple dependencies")
        );
        assert_eq!(diagnostics[0].severity, Severity::Error);
        // Hint should suggest qualified names
        let hint = diagnostics[0].hint.as_deref().unwrap_or("");
        assert!(
            hint.contains("json@Parser"),
            "hint should suggest json@Parser: {hint}"
        );
        assert!(
            hint.contains("xml@Parser"),
            "hint should suggest xml@Parser: {hint}"
        );
    }

    #[test]
    fn no_collision_when_name_not_referenced() {
        let registry = make_registry(&[
            ("Parser", "json", "bt@json@parser"),
            ("Parser", "xml", "bt@xml@parser"),
        ]);

        // Source does not use Parser at all
        let module = parse_module("42 + 1");
        let mut diagnostics = Vec::new();
        check_collision_at_use_sites(&module, &registry, &mut diagnostics);

        assert!(
            diagnostics.is_empty(),
            "No error when ambiguous name is never referenced"
        );
    }

    #[test]
    fn no_collision_for_single_export() {
        let registry = make_registry(&[("Helper", "utils", "bt@utils@helper")]);

        let module = parse_module("Helper new");
        let mut diagnostics = Vec::new();
        check_collision_at_use_sites(&module, &registry, &mut diagnostics);

        assert!(
            diagnostics.is_empty(),
            "No collision when class is exported by only one package"
        );
    }

    #[test]
    fn collision_in_superclass_reference() {
        let registry = make_registry(&[
            ("Base", "pkg_a", "bt@pkg_a@base"),
            ("Base", "pkg_b", "bt@pkg_b@base"),
        ]);

        let module = parse_module("Base subclass: Derived\n  value => 1");
        let mut diagnostics = Vec::new();
        check_collision_at_use_sites(&module, &registry, &mut diagnostics);

        // Should detect collision in superclass reference (exactly once)
        assert_eq!(
            diagnostics.len(),
            1,
            "Expected exactly 1 collision diagnostic for superclass, got {}: {diagnostics:?}",
            diagnostics.len()
        );
        assert!(
            diagnostics[0]
                .message
                .contains("exported by multiple dependencies")
        );
    }

    #[test]
    fn no_collision_for_qualified_reference() {
        let registry = make_registry(&[
            ("Parser", "json", "bt@json@parser"),
            ("Parser", "xml", "bt@xml@parser"),
        ]);

        // Source uses qualified json@Parser — no collision
        let module = parse_module("json@Parser new");
        let mut diagnostics = Vec::new();
        check_collision_at_use_sites(&module, &registry, &mut diagnostics);

        assert!(
            diagnostics.is_empty(),
            "No collision for qualified class reference: {diagnostics:?}"
        );
    }

    #[test]
    fn collision_in_standalone_method_target() {
        let registry = make_registry(&[
            ("Parser", "json", "bt@json@parser"),
            ("Parser", "xml", "bt@xml@parser"),
        ]);

        // Standalone method on ambiguous class name
        let module = parse_module("Parser >> customParse: input => input");
        let mut diagnostics = Vec::new();
        check_collision_at_use_sites(&module, &registry, &mut diagnostics);

        assert_eq!(
            diagnostics.len(),
            1,
            "Should detect collision in standalone method target: {diagnostics:?}"
        );
        assert!(
            diagnostics[0]
                .message
                .contains("exported by multiple dependencies")
        );
    }

    #[test]
    fn no_collision_in_qualified_standalone_method() {
        let registry = make_registry(&[
            ("Parser", "json", "bt@json@parser"),
            ("Parser", "xml", "bt@xml@parser"),
        ]);

        // Qualified standalone method — no collision
        let module = parse_module("json@Parser >> customParse: input => input");
        let mut diagnostics = Vec::new();
        check_collision_at_use_sites(&module, &registry, &mut diagnostics);

        assert!(
            diagnostics.is_empty(),
            "No collision for qualified standalone method: {diagnostics:?}"
        );
    }

    #[test]
    fn collision_with_empty_registry_is_noop() {
        let registry = DependencyRegistry::new();
        let module = parse_module("Parser new");
        let mut diagnostics = Vec::new();
        check_collision_at_use_sites(&module, &registry, &mut diagnostics);
        assert!(diagnostics.is_empty());
    }

    // ── Transitive dependency detection (BT-1654) ───────────────────────────

    /// Helper to create a registry with transitivity metadata.
    fn make_transitive_registry(
        entries: &[(&str, &str, &str, bool, Vec<String>)],
    ) -> DependencyRegistry {
        let mut registry = DependencyRegistry::new();
        for (class, pkg, module, is_direct, via) in entries {
            registry.register_with_transitivity(class, pkg, module, *is_direct, via.clone());
        }
        registry
    }

    #[test]
    fn transitive_dep_emits_warning() {
        let registry = make_transitive_registry(&[(
            "StringUtils",
            "utils",
            "bt@utils@string_utils",
            false,
            vec!["json".to_string()],
        )]);

        let module = parse_module("StringUtils new");
        let mut diagnostics = Vec::new();
        check_transitive_dep_usage(&module, &registry, false, &mut diagnostics);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(diagnostics[0].severity, Severity::Warning);
        assert!(
            diagnostics[0].message.contains("StringUtils"),
            "Message should mention class name: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0].message.contains("utils"),
            "Message should mention package name: {}",
            diagnostics[0].message
        );
        assert!(
            diagnostics[0].message.contains("via 'json'"),
            "Message should include via chain: {}",
            diagnostics[0].message
        );
        let hint = diagnostics[0].hint.as_deref().unwrap_or("");
        assert!(
            hint.contains("Add 'utils' to [dependencies]"),
            "Hint should suggest adding dep: {hint}"
        );
    }

    #[test]
    fn strict_deps_promotes_warning_to_error() {
        let registry = make_transitive_registry(&[(
            "StringUtils",
            "utils",
            "bt@utils@string_utils",
            false,
            vec!["json".to_string()],
        )]);

        let module = parse_module("StringUtils new");
        let mut diagnostics = Vec::new();
        check_transitive_dep_usage(&module, &registry, true, &mut diagnostics);

        assert_eq!(diagnostics.len(), 1);
        assert_eq!(
            diagnostics[0].severity,
            Severity::Error,
            "strict-deps should produce an error, not a warning"
        );
        assert!(diagnostics[0].message.contains("StringUtils"));
    }

    #[test]
    fn direct_dep_no_transitive_warning() {
        let registry = make_transitive_registry(&[(
            "Helper",
            "utils",
            "bt@utils@helper",
            true, // direct dep
            Vec::new(),
        )]);

        let module = parse_module("Helper new");
        let mut diagnostics = Vec::new();
        check_transitive_dep_usage(&module, &registry, false, &mut diagnostics);

        assert!(
            diagnostics.is_empty(),
            "Direct dependency should not trigger transitive warning: {diagnostics:?}"
        );
    }

    #[test]
    fn transitive_dep_no_warning_when_not_referenced() {
        let registry = make_transitive_registry(&[(
            "StringUtils",
            "utils",
            "bt@utils@string_utils",
            false,
            vec!["json".to_string()],
        )]);

        let module = parse_module("42 + 1");
        let mut diagnostics = Vec::new();
        check_transitive_dep_usage(&module, &registry, false, &mut diagnostics);

        assert!(
            diagnostics.is_empty(),
            "No warning when transitive class is not referenced"
        );
    }

    #[test]
    fn transitive_dep_in_superclass_emits_warning() {
        let registry = make_transitive_registry(&[(
            "Base",
            "core_lib",
            "bt@core_lib@base",
            false,
            vec!["framework".to_string()],
        )]);

        let module = parse_module("Base subclass: MyClass\n  value => 1");
        let mut diagnostics = Vec::new();
        check_transitive_dep_usage(&module, &registry, false, &mut diagnostics);

        assert_eq!(
            diagnostics.len(),
            1,
            "Transitive dep in superclass should warn: {diagnostics:?}"
        );
        assert_eq!(diagnostics[0].severity, Severity::Warning);
    }

    #[test]
    fn build_registry_with_graph_tracks_transitivity() {
        let dep_infos = vec![
            DepInfo {
                name: "json".to_string(),
                class_module_index: HashMap::from([(
                    "Parser".to_string(),
                    "bt@json@parser".to_string(),
                )]),
                is_direct: true,
                via_chain: Vec::new(),
            },
            DepInfo {
                name: "utils".to_string(),
                class_module_index: HashMap::from([(
                    "StringUtils".to_string(),
                    "bt@utils@string_utils".to_string(),
                )]),
                is_direct: false,
                via_chain: vec!["json".to_string()],
            },
        ];

        let registry = build_dependency_registry_with_graph(&dep_infos);

        // Direct dep
        let parser_exports = registry.lookup("Parser").unwrap();
        assert_eq!(parser_exports.len(), 1);
        assert!(parser_exports[0].is_direct);
        assert!(parser_exports[0].via_chain.is_empty());

        // Transitive dep
        let utils_exports = registry.lookup("StringUtils").unwrap();
        assert_eq!(utils_exports.len(), 1);
        assert!(!utils_exports[0].is_direct);
        assert_eq!(utils_exports[0].via_chain, vec!["json".to_string()]);
    }
}
