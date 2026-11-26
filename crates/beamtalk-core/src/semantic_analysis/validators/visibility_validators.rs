// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Visibility enforcement (ADR 0071, Phases 2–3).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validates two rules:
//!
//! - **E0401 (cross-package internal reference):** Code in package A must not
//!   reference an internal class from package B. Checked at all reference
//!   positions: superclass, type annotations, `isKindOf:` argument, and
//!   extension-method target.
//!
//! - **E0402 (leaked visibility):** A public class must not expose an internal
//!   class in its public signature (parameter types, return types, state type
//!   annotations). Internal-on-internal is fine.
//!
//! ADR 0108 Phase 5 extends E0402 to `internal type` aliases in two
//! ways: a public class/protocol signature directly naming an internal alias
//! is a leak (mirrors the internal-class rule exactly), and a *public* alias
//! whose expansion transitively reaches an internal class or alias is a leak
//! at its own declaration — see [`check_alias_leaked_visibility`].

use crate::ast::{Expression, MethodDefinition, Module, TypeAnnotation};
use crate::ast_walker::walk_expression;
use crate::semantic_analysis::alias_registry::AliasRegistry;
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::semantic_analysis::protocol_registry::ProtocolRegistry;
use crate::source_analysis::DiagnosticCategory;
use crate::source_analysis::{Diagnostic, MethodSide, Span};
use ecow::EcoString;
use std::collections::{HashMap, HashSet};

/// Checks all class-level visibility rules for the module.
///
/// `current_package` is the package being compiled. When `None` (REPL / script),
/// no visibility checks are run — there is no cross-package context.
pub fn check_class_visibility(
    module: &Module,
    hierarchy: &ClassHierarchy,
    alias_registry: &AliasRegistry,
    current_package: Option<&str>,
    provision_packages: &HashMap<EcoString, EcoString>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(current_pkg) = current_package else {
        return;
    };
    // Provisions flattened in from a protocol in another package are checked
    // against *that* package by `check_provision_visibility`, not here.
    let is_foreign_provision = |class: &crate::ast::ClassDefinition, method: &MethodDefinition| {
        foreign_provision_package(class, method, hierarchy, provision_packages, current_pkg)
            .is_some()
    };

    for class in &module.classes {
        // E0401: superclass reference
        if let Some(ref superclass) = class.superclass {
            check_cross_package_ref(
                &superclass.name,
                superclass.span,
                current_pkg,
                hierarchy,
                diagnostics,
            );
        }

        // E0402: leaked visibility — only applies to public classes
        if !class.is_internal {
            check_leaked_visibility_class(
                class,
                alias_registry,
                current_pkg,
                hierarchy,
                diagnostics,
            );
        }

        // E0401: type annotations in methods (both instance and class-side)
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            if is_foreign_provision(class, method) {
                continue;
            }
            // Parameter types
            for param in &method.parameters {
                if let Some(ref ty) = param.type_annotation {
                    check_type_annotation_cross_package(ty, current_pkg, hierarchy, diagnostics);
                }
            }
            // Return type
            if let Some(ref ty) = method.return_type {
                check_type_annotation_cross_package(ty, current_pkg, hierarchy, diagnostics);
            }
        }

        // E0401: state field type annotations
        for state in &class.state {
            if let Some(ref ty) = state.type_annotation {
                check_type_annotation_cross_package(ty, current_pkg, hierarchy, diagnostics);
            }
        }
    }

    // E0401: class references in expressions (e.g. `ParserState new`, `x isKindOf: ParserState`)
    for stmt in &module.expressions {
        check_expression_cross_package(&stmt.expression, current_pkg, hierarchy, diagnostics);
    }
    for class in &module.classes {
        for method in class.methods.iter().chain(class.class_methods.iter()) {
            if is_foreign_provision(class, method) {
                continue;
            }
            for stmt in &method.body {
                check_expression_cross_package(
                    &stmt.expression,
                    current_pkg,
                    hierarchy,
                    diagnostics,
                );
            }
        }
        // State default values
        for state in &class.state {
            if let Some(ref default) = state.default_value {
                check_expression_cross_package(default, current_pkg, hierarchy, diagnostics);
            }
        }
    }

    // E0401: extension method targets
    for method_def in &module.method_definitions {
        // Check if the target class is internal and from another package
        check_cross_package_ref(
            &method_def.class_name.name,
            method_def.class_name.span,
            current_pkg,
            hierarchy,
            diagnostics,
        );

        // E0401: type annotations in extension method signatures
        for param in &method_def.method.parameters {
            if let Some(ref ty) = param.type_annotation {
                check_type_annotation_cross_package(ty, current_pkg, hierarchy, diagnostics);
            }
        }
        if let Some(ref ty) = method_def.method.return_type {
            check_type_annotation_cross_package(ty, current_pkg, hierarchy, diagnostics);
        }

        // Also check expressions in extension method bodies
        for stmt in &method_def.method.body {
            check_expression_cross_package(&stmt.expression, current_pkg, hierarchy, diagnostics);
        }

        check_standalone_signature_leaked(
            method_def,
            current_pkg,
            hierarchy,
            alias_registry,
            diagnostics,
        );
    }
}

/// E0402 for a standalone `Foo >> sel` method (BT-4923): the extension is a
/// method of its target class, so an internal class in its signature leaks
/// exactly as it would inline. All methods of an internal class are internal,
/// and an unknown target is not ours to judge.
fn check_standalone_signature_leaked(
    method_def: &crate::ast::StandaloneMethodDefinition,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    alias_registry: &AliasRegistry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if hierarchy
        .get_class(&method_def.class_name.name)
        .is_some_and(|info| !info.is_internal)
    {
        check_method_signature_leaked(
            &method_def.method,
            &method_def.class_name.name,
            current_pkg,
            hierarchy,
            alias_registry,
            diagnostics,
        );
    }
}

/// The package a flattened provision was declared in, when it differs from
/// `current_pkg` and is known (ADR 0127 §3, "Name resolution").
fn foreign_provision_package<'a>(
    class: &crate::ast::ClassDefinition,
    method: &MethodDefinition,
    hierarchy: &ClassHierarchy,
    provision_packages: &'a HashMap<EcoString, EcoString>,
    current_pkg: &str,
) -> Option<&'a EcoString> {
    if method.is_class_method {
        return None;
    }
    let selector = method.selector.name();
    let origin = hierarchy
        .get_class(&class.name.name)?
        .methods
        .iter()
        .find(|m| m.selector == selector)?
        .origin
        .as_ref()?;
    provision_packages
        .get(origin)
        .filter(|pkg| pkg.as_str() != current_pkg)
}

/// E0401 for flattened provisions: their references are checked against the
/// *protocol's* package (ADR 0127 §3, "Name resolution"), so a provision may
/// use its own package's `internal` classes but not another package's. The
/// diagnostic lies in the protocol's file and is attributed to it (once
/// merged across users, [`crate::source_analysis::merge_provision_diagnostics`]).
///
/// `provision_packages` maps a protocol name to its declaring package.
pub fn check_provision_visibility(
    module: &Module,
    hierarchy: &ClassHierarchy,
    current_package: Option<&str>,
    provision_packages: &HashMap<EcoString, EcoString>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(current_pkg) = current_package else {
        return;
    };
    for class in &module.classes {
        for method in &class.methods {
            let Some(protocol_pkg) = foreign_provision_package(
                class,
                method,
                hierarchy,
                provision_packages,
                current_pkg,
            ) else {
                continue;
            };
            let Some(protocol) = hierarchy
                .get_class(&class.name.name)
                .and_then(|c| {
                    c.methods
                        .iter()
                        .find(|m| m.selector == method.selector.name())
                })
                .and_then(|m| m.origin.clone())
            else {
                continue;
            };
            let mut found = Vec::new();
            for param in &method.parameters {
                if let Some(ref ty) = param.type_annotation {
                    check_type_annotation_cross_package(ty, protocol_pkg, hierarchy, &mut found);
                }
            }
            if let Some(ref ty) = method.return_type {
                check_type_annotation_cross_package(ty, protocol_pkg, hierarchy, &mut found);
            }
            for stmt in &method.body {
                check_expression_cross_package(
                    &stmt.expression,
                    protocol_pkg,
                    hierarchy,
                    &mut found,
                );
            }
            for mut diagnostic in found {
                diagnostic.set_provision_origin(protocol.clone(), class.name.name.clone());
                diagnostics.push(diagnostic);
            }
        }
    }
}

/// Recursively checks expressions for cross-package internal class references (E0401).
///
/// Walks all `ClassReference` nodes in the expression tree. This catches
/// `ParserState new`, `x isKindOf: ParserState`, and similar patterns where
/// an internal class name appears in an expression.
fn check_expression_cross_package(
    expr: &Expression,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_expression(expr, &mut |e| {
        if let Expression::ClassReference { name, package, .. } = e {
            check_qualified_cross_package_ref(
                &name.name,
                package.as_ref().map(|p| p.name.as_str()),
                name.span,
                current_pkg,
                hierarchy,
                diagnostics,
            );
        }
    });
}

/// Checks if a class name reference crosses a package boundary to an internal class.
///
/// Emits E0401 if the referenced class is internal and belongs to a different package.
fn check_cross_package_ref(
    class_name: &ecow::EcoString,
    span: Span,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    check_qualified_cross_package_ref(class_name, None, span, current_pkg, hierarchy, diagnostics);
}

/// As [`check_cross_package_ref`], for a reference that may carry a package
/// qualifier (`json@Parser`). A qualified reference naming a class other than
/// the one the hierarchy holds under the bare name is not that class — there
/// is nothing to enforce against (ADR 0127 §3, "Name resolution").
fn check_qualified_cross_package_ref(
    class_name: &ecow::EcoString,
    qualifier: Option<&str>,
    span: Span,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if hierarchy.qualified_ref_names_other_class(class_name, qualifier) {
        return;
    }
    let Some(info) = hierarchy.get_class(class_name) else {
        return; // Unknown class — other passes handle this
    };
    if !info.is_internal {
        return;
    }
    // Same package is fine
    if let Some(ref pkg) = info.package {
        if pkg.as_str() == current_pkg {
            return;
        }
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "Class '{class_name}' is internal to package '{pkg}' \
                     and cannot be referenced from '{current_pkg}'"
                ),
                span,
            )
            .with_hint(format!(
                "'{class_name}' is declared 'internal' in package '{pkg}'"
            ))
            .with_category(DiagnosticCategory::Visibility),
        );
    }
    // info.package == None: builtins or REPL classes — no package boundary to enforce
}

/// Recursively checks a type annotation for cross-package internal class references (E0401).
fn check_type_annotation_cross_package(
    ty: &TypeAnnotation,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match ty {
        TypeAnnotation::Simple(id) => {
            check_cross_package_ref(&id.name, id.span, current_pkg, hierarchy, diagnostics);
        }
        TypeAnnotation::Union { types, .. } => {
            for t in types {
                check_type_annotation_cross_package(t, current_pkg, hierarchy, diagnostics);
            }
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            check_cross_package_ref(&base.name, base.span, current_pkg, hierarchy, diagnostics);
            for p in parameters {
                check_type_annotation_cross_package(p, current_pkg, hierarchy, diagnostics);
            }
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            check_type_annotation_cross_package(base, current_pkg, hierarchy, diagnostics);
            check_type_annotation_cross_package(excluded, current_pkg, hierarchy, diagnostics);
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            check_type_annotation_cross_package(left, current_pkg, hierarchy, diagnostics);
            check_type_annotation_cross_package(right, current_pkg, hierarchy, diagnostics);
        }
        TypeAnnotation::Shape { fields, .. } => {
            for f in fields {
                check_type_annotation_cross_package(&f.value, current_pkg, hierarchy, diagnostics);
            }
        }
        TypeAnnotation::ClassOf { class_name, .. } => {
            check_cross_package_ref(
                &class_name.name,
                class_name.span,
                current_pkg,
                hierarchy,
                diagnostics,
            );
        }
        // Singleton, SelfType, and SelfClass don't reference classes
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. } => {}
    }
}

/// Checks E0402: leaked visibility — an internal class appearing in a public class's
/// public signature (parameter types, return types, state type annotations).
fn check_leaked_visibility_class(
    class: &crate::ast::ClassDefinition,
    alias_registry: &AliasRegistry,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let class_name = &class.name.name;

    // Check state field type annotations
    for state in &class.state {
        if let Some(ref ty) = state.type_annotation {
            check_type_annotation_leaked(
                ty,
                class_name,
                None,
                current_pkg,
                hierarchy,
                alias_registry,
                diagnostics,
            );
        }
    }

    // Check method signatures (only non-internal methods on a public class)
    for method in class.methods.iter().chain(class.class_methods.iter()) {
        check_method_signature_leaked(
            method,
            class_name,
            current_pkg,
            hierarchy,
            alias_registry,
            diagnostics,
        );
    }
}

/// E0402 for one method's signature: an internal class of the current package
/// in a parameter or return type of a non-`internal` method. Shared by the
/// inline methods of a public class and by standalone `Foo >> sel` methods
/// (BT-4923), so both forms report the same message.
fn check_method_signature_leaked(
    method: &MethodDefinition,
    class_name: &EcoString,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    alias_registry: &AliasRegistry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if method.is_internal {
        // Internal method on public class — its signature can reference
        // internal classes from the same package without leaking.
        return;
    }

    let selector = method.selector.name();

    // Parameter types
    for param in &method.parameters {
        if let Some(ref ty) = param.type_annotation {
            check_type_annotation_leaked(
                ty,
                class_name,
                Some(&selector),
                current_pkg,
                hierarchy,
                alias_registry,
                diagnostics,
            );
        }
    }

    // Return type
    if let Some(ref ty) = method.return_type {
        check_type_annotation_leaked(
            ty,
            class_name,
            Some(&selector),
            current_pkg,
            hierarchy,
            alias_registry,
            diagnostics,
        );
    }
}

/// Recursively checks a type annotation for leaked internal classes (E0402).
///
/// An internal class from the *same* package appearing in the public signature
/// of a public class is a leaked-visibility error.
#[allow(clippy::too_many_lines)] // ADR 0102 added an `Intersection` recursion arm
fn check_type_annotation_leaked(
    ty: &TypeAnnotation,
    class_name: &ecow::EcoString,
    method_selector: Option<&ecow::EcoString>,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    alias_registry: &AliasRegistry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match ty {
        TypeAnnotation::Simple(id) => {
            check_leaked_ref(
                &id.name,
                id.span,
                class_name,
                method_selector,
                current_pkg,
                hierarchy,
                alias_registry,
                diagnostics,
            );
        }
        TypeAnnotation::Union { types, .. } => {
            for t in types {
                check_type_annotation_leaked(
                    t,
                    class_name,
                    method_selector,
                    current_pkg,
                    hierarchy,
                    alias_registry,
                    diagnostics,
                );
            }
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            check_leaked_ref(
                &base.name,
                base.span,
                class_name,
                method_selector,
                current_pkg,
                hierarchy,
                alias_registry,
                diagnostics,
            );
            for p in parameters {
                check_type_annotation_leaked(
                    p,
                    class_name,
                    method_selector,
                    current_pkg,
                    hierarchy,
                    alias_registry,
                    diagnostics,
                );
            }
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            check_type_annotation_leaked(
                base,
                class_name,
                method_selector,
                current_pkg,
                hierarchy,
                alias_registry,
                diagnostics,
            );
            check_type_annotation_leaked(
                excluded,
                class_name,
                method_selector,
                current_pkg,
                hierarchy,
                alias_registry,
                diagnostics,
            );
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            check_type_annotation_leaked(
                left,
                class_name,
                method_selector,
                current_pkg,
                hierarchy,
                alias_registry,
                diagnostics,
            );
            check_type_annotation_leaked(
                right,
                class_name,
                method_selector,
                current_pkg,
                hierarchy,
                alias_registry,
                diagnostics,
            );
        }
        TypeAnnotation::Shape { fields, .. } => {
            for f in fields {
                check_type_annotation_leaked(
                    &f.value,
                    class_name,
                    method_selector,
                    current_pkg,
                    hierarchy,
                    alias_registry,
                    diagnostics,
                );
            }
        }
        TypeAnnotation::ClassOf {
            class_name: cls, ..
        } => {
            check_leaked_ref(
                &cls.name,
                cls.span,
                class_name,
                method_selector,
                current_pkg,
                hierarchy,
                alias_registry,
                diagnostics,
            );
        }
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. } => {}
    }
}

/// Checks if a type name in a public signature references an internal class
/// or internal type alias, which is a leaked-visibility error (E0402).
///
/// An internal *alias* reference is always a leak when found here —
/// unlike internal classes, an internal alias is never seeded across a
/// package boundary at all ([`AliasRegistry::add_pre_loaded`]'s
/// seeding-boundary exclusion), so a name that resolves via `alias_registry`
/// with `is_internal: true` is guaranteed to be scoped to the current
/// compilation; no same-package comparison is needed (contrast with the
/// class case below, which must additionally check `info.package ==
/// current_pkg` since internal classes *are* passed across the boundary via
/// `pre_loaded_classes`/`add_from_beam_meta`, with E0401 handling the
/// cross-package case separately).
#[allow(clippy::too_many_arguments)] // `alias_registry` plus the rest; each param is load-bearing context, not bundleable without obscuring the leaf check
fn check_leaked_ref(
    type_name: &ecow::EcoString,
    span: Span,
    class_name: &ecow::EcoString,
    method_selector: Option<&ecow::EcoString>,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    alias_registry: &AliasRegistry,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let location = if let Some(sel) = method_selector {
        format!("{class_name} >> {sel}")
    } else {
        format!("{class_name}")
    };

    if let Some(alias_info) = alias_registry.get(type_name) {
        if alias_info.is_internal {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Internal type alias '{type_name}' appears in public signature of '{location}'"
                    ),
                    span,
                )
                .with_hint(format!(
                    "'{type_name}' is declared 'internal' — make it public, or change the type"
                ))
                .with_category(DiagnosticCategory::Visibility),
            );
        }
        // Alias names are disjoint from class names (namespace collision
        // checks in `AliasRegistry::register_module`/`add_pre_loaded`
        // prevent a name from being both), so there is no class lookup to
        // fall through to here.
        return;
    }

    let Some(info) = hierarchy.get_class(type_name) else {
        return; // Unknown type — other passes handle this
    };
    if !info.is_internal {
        return;
    }
    // Only flag if the internal class is from the same package.
    // Cross-package references are caught by E0401 separately.
    if let Some(ref pkg) = info.package {
        if pkg.as_str() != current_pkg {
            return; // E0401 handles this case
        }
    } else {
        return; // No package — builtins or REPL
    }

    diagnostics.push(
        Diagnostic::error(
            format!("Internal class '{type_name}' appears in public signature of '{location}'"),
            span,
        )
        .with_hint(format!(
            "'{type_name}' is declared 'internal' — make it public, or change the type"
        ))
        .with_category(DiagnosticCategory::Visibility),
    );
}

/// E0402 (ADR 0108 Semantics): a *public* type alias whose expanded
/// annotation transitively reaches an internal class or internal alias is a
/// leaked-visibility error — even though the internal name never appears
/// directly in any *consumer's* signature, `Pub`'s exported expansion still
/// exposes it.
///
/// `public type Pub = InternalAlias | String` never writes `InternalAlias`
/// in a public *signature* (that direct-reference case is
/// [`check_leaked_ref`]'s job, run against usage sites), but `Pub`'s
/// expansion contains whatever `InternalAlias` expands to — so this check
/// walks the alias's own right-hand side at *declaration* time, following
/// alias chains (multi-hop) with cycle detection, mirroring how the same
/// expansion is already computed eagerly by
/// [`resolve_type_annotation`](crate::semantic_analysis::type_checker::resolve_type_annotation).
///
/// `internal` aliases are skipped entirely — an internal alias's RHS may
/// reference anything visible in its own package without leaking (mirrors
/// `check_leaked_visibility_class`'s `!class.is_internal` gate). Protocols
/// are not checked here: they do not currently have an `is_internal` flag
/// (see [`check_leaked_method_visibility`]'s note), so there is nothing to
/// leak from that direction yet.
pub fn check_alias_leaked_visibility(
    module: &Module,
    hierarchy: &ClassHierarchy,
    alias_registry: &AliasRegistry,
    current_package: Option<&str>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(_current_pkg) = current_package else {
        return;
    };

    for alias_def in &module.type_aliases {
        if alias_def.is_internal {
            continue;
        }
        let mut visited = HashSet::new();
        visited.insert(alias_def.name.name.clone());
        check_type_annotation_alias_leak(
            &alias_def.annotation,
            &alias_def.name.name,
            hierarchy,
            alias_registry,
            &mut visited,
            diagnostics,
        );
    }
}

/// Recursively walks a public alias's expanded annotation for E0402 alias
/// leakage. Structurally identical recursion arms to
/// [`check_type_annotation_leaked`], but delegates to
/// [`check_alias_leak_ref`] and threads a `visited` set for cycle safety
/// across alias chains.
#[allow(clippy::too_many_lines)] // one arm per TypeAnnotation variant, mirroring check_type_annotation_leaked; irreducible
fn check_type_annotation_alias_leak(
    ty: &TypeAnnotation,
    alias_name: &ecow::EcoString,
    hierarchy: &ClassHierarchy,
    alias_registry: &AliasRegistry,
    visited: &mut HashSet<EcoString>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match ty {
        TypeAnnotation::Simple(id) => {
            check_alias_leak_ref(
                &id.name,
                id.span,
                alias_name,
                hierarchy,
                alias_registry,
                visited,
                diagnostics,
            );
        }
        TypeAnnotation::Union { types, .. } => {
            for t in types {
                check_type_annotation_alias_leak(
                    t,
                    alias_name,
                    hierarchy,
                    alias_registry,
                    visited,
                    diagnostics,
                );
            }
        }
        TypeAnnotation::Generic {
            base, parameters, ..
        } => {
            check_alias_leak_ref(
                &base.name,
                base.span,
                alias_name,
                hierarchy,
                alias_registry,
                visited,
                diagnostics,
            );
            for p in parameters {
                check_type_annotation_alias_leak(
                    p,
                    alias_name,
                    hierarchy,
                    alias_registry,
                    visited,
                    diagnostics,
                );
            }
        }
        TypeAnnotation::Difference { base, excluded, .. } => {
            check_type_annotation_alias_leak(
                base,
                alias_name,
                hierarchy,
                alias_registry,
                visited,
                diagnostics,
            );
            check_type_annotation_alias_leak(
                excluded,
                alias_name,
                hierarchy,
                alias_registry,
                visited,
                diagnostics,
            );
        }
        TypeAnnotation::Intersection { left, right, .. } => {
            check_type_annotation_alias_leak(
                left,
                alias_name,
                hierarchy,
                alias_registry,
                visited,
                diagnostics,
            );
            check_type_annotation_alias_leak(
                right,
                alias_name,
                hierarchy,
                alias_registry,
                visited,
                diagnostics,
            );
        }
        TypeAnnotation::Shape { fields, .. } => {
            for f in fields {
                check_type_annotation_alias_leak(
                    &f.value,
                    alias_name,
                    hierarchy,
                    alias_registry,
                    visited,
                    diagnostics,
                );
            }
        }
        TypeAnnotation::ClassOf {
            class_name: cls, ..
        } => {
            check_alias_leak_ref(
                &cls.name,
                cls.span,
                alias_name,
                hierarchy,
                alias_registry,
                visited,
                diagnostics,
            );
        }
        TypeAnnotation::Singleton { .. }
        | TypeAnnotation::SelfType { .. }
        | TypeAnnotation::SelfClass { .. } => {}
    }
}

/// Leaf check for [`check_type_annotation_alias_leak`]: if `type_name`
/// resolves to an internal class or internal alias, the enclosing public
/// alias `alias_name` leaks it. If `type_name` resolves to a (public) alias,
/// recurses into its own expansion — `visited` guards against a reference
/// cycle slipping through (full cycle *detection* at declaration time is
/// a separate concern, out of scope here; this guard only prevents this
/// checker from hanging on one).
///
/// Unlike [`check_leaked_ref`], this does **not** compare `info.package` to
/// a "current package" — there is no such thing here: a *public* alias's
/// exported expansion must be resolvable by every consumer, so an internal
/// class reachable through it is a leak regardless of which package
/// declared that class (contrast [`check_leaked_ref`]'s usage-site check,
/// where an internal class from a *different* package is E0401's job, not
/// E0402's, and a same-package internal class is fine to reference from an
/// internal signature). A public alias has no "internal signature" escape
/// hatch — it is public by definition — so every internal class or alias
/// its expansion reaches is a leak, full stop.
fn check_alias_leak_ref(
    type_name: &EcoString,
    span: Span,
    alias_name: &EcoString,
    hierarchy: &ClassHierarchy,
    alias_registry: &AliasRegistry,
    visited: &mut HashSet<EcoString>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Dedup at the top (not just around the alias-chasing recursion below):
    // a diamond-shaped alias graph (`Pub = A | B`, `A = C`, `B = C`) would
    // otherwise reach `C` twice and emit the same leak diagnostic twice.
    // Once a name has been checked for this top-level alias, every further
    // reference to it — whether reached again as a class or as an alias —
    // is a no-op.
    if !visited.insert(type_name.clone()) {
        return;
    }

    if let Some(alias_info) = alias_registry.get(type_name) {
        if alias_info.is_internal {
            diagnostics.push(
                Diagnostic::error(
                    format!(
                        "Internal type alias '{type_name}' appears in the expansion of \
                         public type alias '{alias_name}'"
                    ),
                    span,
                )
                .with_hint(format!(
                    "'{type_name}' is declared 'internal' — make it public, or change '{alias_name}'"
                ))
                .with_category(DiagnosticCategory::Visibility),
            );
            // Don't recurse into the internal alias's own expansion — it would
            // double-report the same leak (e.g. a further internal class inside
            // it) as a second, noisier diagnostic on the same `alias_name`
            // declaration. `check_alias_leaked_visibility` iterates every
            // top-level alias independently, so `type_name` itself still gets
            // checked as its own declaration if/when it's public.
            return;
        }
        check_type_annotation_alias_leak(
            &alias_info.annotation,
            alias_name,
            hierarchy,
            alias_registry,
            visited,
            diagnostics,
        );
        return;
    }

    if let Some(info) = hierarchy.get_class(type_name)
        && info.is_internal
    {
        diagnostics.push(
            Diagnostic::error(
                format!(
                    "Internal class '{type_name}' appears in the expansion of \
                         public type alias '{alias_name}'"
                ),
                span,
            )
            .with_hint(format!(
                "'{type_name}' is declared 'internal' — make it public, or change '{alias_name}'"
            ))
            .with_category(DiagnosticCategory::Visibility),
        );
    }
    // Protocols have no `is_internal` flag yet — nothing to check.
}

/// Selector and span of each `internal` method on one side of a class.
type InternalMethods = Vec<(EcoString, Span)>;

/// E0402: Emit error when an internal method satisfies a public protocol.
///
/// If a class implements a selector required by a public protocol but declares
/// the method `internal`, that's a leaked visibility error — the protocol
/// promises the method is part of the public API, but the method is hidden.
///
/// Note: protocols do not currently have an `is_internal` flag, so all
/// protocols are treated as public. When protocol visibility is added,
/// internal methods satisfying internal protocols should be allowed.
pub fn check_leaked_method_visibility(
    module: &Module,
    hierarchy: &ClassHierarchy,
    protocol_registry: &ProtocolRegistry,
    current_package: Option<&str>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Only enforce when a package context is available
    let Some(_pkg) = current_package else {
        return;
    };

    // Internal methods per owning class, inline and standalone together
    // (BT-4923), in order of first appearance.
    let mut by_class: Vec<(&str, InternalMethods, InternalMethods)> = Vec::new();
    for (class_name, side, method) in module.methods_with_owner() {
        if !method.is_internal {
            continue;
        }
        let idx = by_class
            .iter()
            .position(|(n, _, _)| *n == class_name)
            .unwrap_or_else(|| {
                by_class.push((class_name, Vec::new(), Vec::new()));
                by_class.len() - 1
            });
        let entry = (method.selector.name(), method.span);
        match side {
            MethodSide::Instance => by_class[idx].1.push(entry),
            MethodSide::Class => by_class[idx].2.push(entry),
        }
    }

    if by_class.is_empty() {
        return;
    }
    // Sorted by name so the errors come out in a stable order: the registry
    // is a `HashMap` (BT-5184).
    let mut protocols: Vec<_> = protocol_registry
        .protocols()
        .filter(|(_, info)| {
            !crate::semantic_analysis::stdlib_protocols::is_seeded_stdlib_info(info)
        })
        .collect();
    protocols.sort_unstable_by(|a, b| a.0.cmp(b.0));

    for (class_name, internal_instance, internal_class) in &by_class {
        // Check each protocol the class might conform to
        // (Protocols don't have is_internal yet, so all protocols are treated as public)
        for (_proto_name, proto_info) in &protocols {
            // Use the protocol registry's conformance check so we stay consistent
            // with DNU-bypass, cross-file-parent, and inherited method handling.
            if protocol_registry
                .check_conformance(class_name, &proto_info.name, hierarchy)
                .is_err()
            {
                continue; // Class doesn't conform — no leaked visibility
            }

            let proto_name = &proto_info.name;

            // Check instance methods
            for req in &proto_info.methods {
                if let Some((_sel, span)) = internal_instance
                    .iter()
                    .find(|(sel, _)| *sel == req.selector)
                {
                    let selector = &req.selector;
                    let message: EcoString = format!(
                        "Internal method '{selector}' on '{class_name}' satisfies public protocol '{proto_name}' — make the method public"
                    ).into();
                    diagnostics.push(
                        Diagnostic::error(message, *span)
                            .with_hint(format!(
                                "'{selector}' is required by protocol '{proto_name}' but declared 'internal'"
                            ))
                            .with_category(DiagnosticCategory::Visibility),
                    );
                }
            }

            // Check class methods
            for req in &proto_info.class_methods {
                if let Some((_sel, span)) =
                    internal_class.iter().find(|(sel, _)| *sel == req.selector)
                {
                    let selector = &req.selector;
                    let message: EcoString = format!(
                        "Internal class method '{selector}' on '{class_name}' satisfies public protocol '{proto_name}' — make the method public"
                    ).into();
                    diagnostics.push(
                        Diagnostic::error(message, *span)
                            .with_hint(format!(
                                "'{selector}' is required by protocol '{proto_name}' but declared 'internal'"
                            ))
                            .with_category(DiagnosticCategory::Visibility),
                    );
                }
            }
        }
    }
}

/// W0401: Warn when a subclass defines a selector that shadows an
/// internal method on the superclass.
///
/// This is a potential footgun — the subclass author may not know the superclass
/// has an internal helper with that name, leading to accidental shadowing.
pub fn check_internal_method_shadow(
    module: &Module,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    for class in &module.classes {
        let class_name = &class.name.name;

        // Get the superclass name
        let Some(superclass) = class.superclass.as_ref() else {
            continue;
        };
        let superclass_name = &superclass.name;

        // Skip if the superclass is not in the hierarchy
        if !hierarchy.has_class(superclass_name) {
            continue;
        }

        // Check each instance method
        for method in &class.methods {
            check_shadow_for_method(
                class_name,
                superclass_name,
                &method.selector.name(),
                method.span,
                hierarchy,
                diagnostics,
            );
        }

        // Check each class method
        for method in &class.class_methods {
            check_shadow_for_method_class_side(
                class_name,
                superclass_name,
                &method.selector.name(),
                method.span,
                hierarchy,
                diagnostics,
            );
        }
    }

    // Standalone `Sub >> helper` / `Sub class >> helper` (BT-4923). The class
    // may be declared in another file, so its superclass comes from the
    // hierarchy rather than from `module.classes`.
    for standalone in &module.method_definitions {
        let class_name = &standalone.class_name.name;
        let Some(superclass_name) = hierarchy
            .get_class(class_name)
            .and_then(|info| info.superclass.as_ref())
        else {
            continue;
        };
        if !hierarchy.has_class(superclass_name) {
            continue;
        }
        let method = &standalone.method;
        if standalone.is_class_method {
            check_shadow_for_method_class_side(
                class_name,
                superclass_name,
                &method.selector.name(),
                method.span,
                hierarchy,
                diagnostics,
            );
        } else {
            check_shadow_for_method(
                class_name,
                superclass_name,
                &method.selector.name(),
                method.span,
                hierarchy,
                diagnostics,
            );
        }
    }
}

/// Helper: check if an instance method shadows a superclass internal method.
fn check_shadow_for_method(
    class_name: &EcoString,
    superclass_name: &EcoString,
    selector: &str,
    span: crate::source_analysis::Span,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    // Look up the method on the superclass (not on our class — we want the inherited version)
    let Some(super_method) = hierarchy.find_method(superclass_name, selector) else {
        return;
    };

    if !super_method.is_internal {
        return;
    }

    let defined_in = &super_method.defined_in;
    let message: EcoString = format!(
        "Method '{selector}' on '{class_name}' shadows internal method from '{defined_in}'"
    )
    .into();
    diagnostics.push(
        Diagnostic::warning(message, span)
            .with_hint(format!(
                "'{defined_in}' has an internal method '{selector}' — shadowing may cause unexpected behavior"
            ))
            .with_category(DiagnosticCategory::Visibility),
    );
}

/// Helper: check if a class method shadows a superclass internal class method.
fn check_shadow_for_method_class_side(
    class_name: &EcoString,
    superclass_name: &EcoString,
    selector: &str,
    span: crate::source_analysis::Span,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(super_method) = hierarchy.find_class_method(superclass_name, selector) else {
        return;
    };

    if !super_method.is_internal {
        return;
    }

    let defined_in = &super_method.defined_in;
    let message: EcoString = format!(
        "Class method '{selector}' on '{class_name}' shadows internal class method from '{defined_in}'"
    )
    .into();
    diagnostics.push(
        Diagnostic::warning(message, span)
            .with_hint(format!(
                "'{defined_in}' has an internal class method '{selector}' — shadowing may cause unexpected behavior"
            ))
            .with_category(DiagnosticCategory::Visibility),
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    use crate::source_analysis::Severity;
    use crate::test_helpers::test_support::parse_bt;
    use ecow::EcoString;
    use std::collections::HashMap;

    fn build_hierarchy_with_internal_class(
        module: &Module,
        internal_class: &str,
        internal_pkg: &str,
    ) -> ClassHierarchy {
        let (mut h, _) = ClassHierarchy::build(module);
        // Add an internal class from another package
        let info = ClassInfo {
            is_internal: true,
            package: Some(EcoString::from(internal_pkg)),
            ..ClassInfo::test(internal_class, Some("Object"))
        };
        h.add_from_beam_meta(vec![info]);
        h
    }

    // --- E0401: Cross-package internal class reference ---

    #[test]
    fn e0401_cross_package_superclass_reference() {
        let module = parse_bt("ParserState subclass: MyParser\n  parse => 42");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("my_app"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("is internal to package")
                && d.message.contains("ParserState")
                && d.message.contains("json")
                && d.message.contains("my_app")),
            "Expected error for cross-package superclass ref, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_same_package_no_error() {
        let module = parse_bt(
            "internal Object subclass: ParserState\n  reset => nil\n\nParserState subclass: ExtendedState\n  parse => 42",
        );
        let (mut h, _) = ClassHierarchy::build(&module);
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("json"),
            &HashMap::new(),
            &mut diags,
        );

        let errors: Vec<_> = diags
            .iter()
            .filter(|d| {
                d.severity == Severity::Error && d.message.contains("is internal to package")
            })
            .collect();
        assert!(
            errors.is_empty(),
            "Expected no error for same-package ref, got: {errors:?}"
        );
    }

    #[test]
    fn e0401_public_class_cross_package_no_error() {
        let module = parse_bt("Object subclass: MyParser\n  parse => 42");
        let (mut h, _) = ClassHierarchy::build(&module);
        // Add a public class from another package
        let info = ClassInfo {
            package: Some(EcoString::from("json")),
            ..ClassInfo::test("Parser", Some("Object"))
        };
        h.add_from_beam_meta(vec![info]);
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("my_app"),
            &HashMap::new(),
            &mut diags,
        );

        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(
            errors.is_empty(),
            "Expected no errors for public class cross-package ref, got: {errors:?}"
        );
    }

    #[test]
    fn e0401_type_annotation_cross_package() {
        let module = parse_bt("Object subclass: Foo\n  process: input :: ParserState => 42");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("my_app"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("is internal to package")
                && d.message.contains("ParserState")),
            "Expected error for type annotation ref, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_return_type_cross_package() {
        let module = parse_bt("Object subclass: Foo\n  getState -> ParserState => nil");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("my_app"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("is internal to package")
                && d.message.contains("ParserState")),
            "Expected error for return type ref, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_no_check_without_current_package() {
        let module = parse_bt("ParserState subclass: MyParser\n  parse => 42");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            None,
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.is_empty(),
            "Expected no errors when current_package is None, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_state_type_annotation_cross_package() {
        let module = parse_bt("Object subclass: Foo\n  state: buf :: ParserState = nil");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("my_app"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("is internal to package")
                && d.message.contains("ParserState")),
            "Expected error for state type annotation ref, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_class_reference_in_expression() {
        // A class reference to an internal class in a method body expression
        let module = parse_bt("Object subclass: Foo\n  bar => ParserState new");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("my_app"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("is internal to package")
                && d.message.contains("ParserState")),
            "Expected error for class reference in expression, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_class_reference_in_top_level_expression() {
        // A class reference at the top level
        let module = parse_bt("ParserState new");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("my_app"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("is internal to package")
                && d.message.contains("ParserState")),
            "Expected error for top-level class reference, got: {diags:?}"
        );
    }

    // --- E0402: Leaked visibility ---

    #[test]
    fn e0402_internal_class_in_public_return_type() {
        // A public class with a public method returning an internal class from same package
        let module = parse_bt(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  tokenize: input :: String -> TokenBuffer => nil",
        );
        let (mut h, _) = ClassHierarchy::build(&module);
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("json"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("public signature")
                || d.message.contains("satisfies public protocol")
                    && d.message.contains("TokenBuffer")
                    && d.message.contains("Parser >> tokenize:")),
            "Expected leaked visibility error for leaked return type, got: {diags:?}"
        );
    }

    #[test]
    fn e0402_internal_class_in_public_param_type() {
        let module = parse_bt(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  process: buf :: TokenBuffer => nil",
        );
        let (mut h, _) = ClassHierarchy::build(&module);
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("json"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("public signature")
                || d.message.contains("satisfies public protocol")
                    && d.message.contains("TokenBuffer")
                    && d.message.contains("Parser >> process:")),
            "Expected leaked visibility error for leaked param type, got: {diags:?}"
        );
    }

    #[test]
    fn e0402_internal_class_in_state_type_annotation() {
        let module = parse_bt(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  state: buf :: TokenBuffer = nil",
        );
        let (mut h, _) = ClassHierarchy::build(&module);
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("json"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("public signature")
                || d.message.contains("satisfies public protocol")
                    && d.message.contains("TokenBuffer")
                    && d.message.contains("Parser")),
            "Expected leaked visibility error for leaked state type, got: {diags:?}"
        );
    }

    #[test]
    fn e0402_no_error_internal_class_on_internal_class() {
        // Internal class using internal class in signature — no leak
        let module = parse_bt(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             internal Object subclass: InternalParser\n  tokenize: input :: String -> TokenBuffer => nil",
        );
        let (mut h, _) = ClassHierarchy::build(&module);
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("json"),
            &HashMap::new(),
            &mut diags,
        );

        let leaked: Vec<_> = diags
            .iter()
            .filter(|d| {
                d.message.contains("public signature")
                    || d.message.contains("satisfies public protocol")
            })
            .collect();
        assert!(
            leaked.is_empty(),
            "Expected no leaked visibility error for internal-on-internal, got: {leaked:?}"
        );
    }

    #[test]
    fn e0402_no_error_internal_method_on_public_class() {
        // Internal method on a public class can reference internal classes
        let module = parse_bt(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  internal tokenize: input :: String -> TokenBuffer => nil",
        );
        let (mut h, _) = ClassHierarchy::build(&module);
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            Some("json"),
            &HashMap::new(),
            &mut diags,
        );

        let leaked: Vec<_> = diags
            .iter()
            .filter(|d| {
                d.message.contains("public signature")
                    || d.message.contains("satisfies public protocol")
            })
            .collect();
        assert!(
            leaked.is_empty(),
            "Expected no leaked visibility error for internal method on public class, got: {leaked:?}"
        );
    }

    #[test]
    fn e0402_no_check_without_current_package() {
        let module = parse_bt(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  tokenize: input :: String -> TokenBuffer => nil",
        );
        let (mut h, _) = ClassHierarchy::build(&module);
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &AliasRegistry::new(),
            None,
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.is_empty(),
            "Expected no errors when current_package is None, got: {diags:?}"
        );
    }

    // --- ADR 0108 Phase 5: internal type alias leakage ---

    /// Builds a `ClassHierarchy` and `AliasRegistry` from the module's own
    /// declarations, package-stamped for `pkg` — the same-package
    /// counterpart of `build_hierarchy_with_internal_class` for tests that
    /// exercise a module-local `internal type` alongside a public class.
    fn build_hierarchy_and_aliases(module: &Module, pkg: &str) -> (ClassHierarchy, AliasRegistry) {
        let (mut h, _) = ClassHierarchy::build(module);
        h.stamp_package(pkg);
        let protocol_registry = ProtocolRegistry::new();
        let mut alias_registry = AliasRegistry::new();
        let diags = alias_registry.register_module(module, &h, &protocol_registry);
        assert!(
            diags.is_empty(),
            "unexpected alias registration diagnostics: {diags:?}"
        );
        (h, alias_registry)
    }

    #[test]
    fn e0402_public_signature_directly_referencing_internal_alias() {
        // A public method returning an internal alias directly — mirrors
        // ADR 0071's existing rule for internal classes.
        let module = parse_bt(
            "internal type ParserState = Integer\n\n\
             Object subclass: Parser\n  tokenize: input :: String -> ParserState => nil",
        );
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &alias_registry,
            Some("json"),
            &HashMap::new(),
            &mut diags,
        );

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("Internal type alias 'ParserState'")
                && d.message.contains("Parser >> tokenize:")),
            "Expected leaked visibility error for internal alias in public return type, got: {diags:?}"
        );
    }

    #[test]
    fn e0402_no_error_internal_method_referencing_internal_alias() {
        // Internal-on-internal is fine — an internal method on a public
        // class can reference an internal alias without leaking it.
        let module = parse_bt(
            "internal type ParserState = Integer\n\n\
             Object subclass: Parser\n  internal tokenize: input :: String -> ParserState => nil",
        );
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_class_visibility(
            &module,
            &h,
            &alias_registry,
            Some("json"),
            &HashMap::new(),
            &mut diags,
        );

        let leaked: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("Internal type alias"))
            .collect();
        assert!(
            leaked.is_empty(),
            "Expected no leaked visibility error for internal method referencing internal alias, got: {leaked:?}"
        );
    }

    #[test]
    fn alias_leak_public_alias_directly_exposes_internal_alias() {
        // `public type Pub = InternalAlias | String` — direct one-hop case.
        let module = parse_bt("internal type Priv = Integer\ntype Pub = Priv | String");
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, Some("json"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("Internal type alias 'Priv'")
                && d.message.contains("public type alias 'Pub'")),
            "Expected alias-leak error, got: {diags:?}"
        );
    }

    #[test]
    fn alias_leak_multi_hop_through_intermediate_public_alias() {
        // `type Pub = Mid`, `type Mid = Priv` (internal) — the leak must be
        // found by following the chain, not just Pub's own written RHS.
        let module = parse_bt("internal type Priv = Integer\ntype Mid = Priv\ntype Pub = Mid");
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, Some("json"), &mut diags);

        let pub_leaks: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("public type alias 'Pub'"))
            .collect();
        assert!(
            pub_leaks.iter().any(|d| d.message.contains("Priv")),
            "Expected multi-hop alias-leak error naming 'Priv' reachable through 'Mid', got: {diags:?}"
        );
    }

    #[test]
    fn alias_leak_diamond_shaped_expansion_reports_once() {
        // `type Pub = Aa | Bb`, `type Aa = Priv`, `type Bb = Priv` (internal)
        // — both branches of the diamond reach `Priv`; the leak must be
        // reported exactly once, not once per path.
        let module = parse_bt(
            "internal type Priv = Integer\ntype Aa = Priv\ntype Bb = Priv\ntype Pub = Aa | Bb",
        );
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, Some("json"), &mut diags);

        let priv_leaks: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("public type alias 'Pub'") && d.message.contains("Priv"))
            .collect();
        assert_eq!(
            priv_leaks.len(),
            1,
            "expected exactly one leak diagnostic for 'Priv' reached via both 'A' and 'B', got: {diags:?}"
        );
    }

    #[test]
    fn alias_leak_chained_internal_aliases_reports_only_the_first_hop() {
        // `type Pub = InternalMid`, `internal type InternalMid = InternalClass`
        // (also internal) — reviewer-flagged: once `InternalMid` itself is
        // reported as the leak, recursing further into its own expansion to
        // separately flag `InternalClass` is redundant noise (both diagnostics
        // are "true", but the user only needs to fix the first boundary).
        let module = parse_bt(
            "internal Object subclass: InternalClass\n  reset => nil\n\n\
             internal type InternalMid = InternalClass\n\
             type Pub = InternalMid",
        );
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, Some("json"), &mut diags);

        let pub_leaks: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("public type alias 'Pub'"))
            .collect();
        assert_eq!(
            pub_leaks.len(),
            1,
            "expected exactly one leak diagnostic on 'Pub' (the 'InternalMid' boundary), \
             not a second one for 'InternalClass' inside it, got: {diags:?}"
        );
        assert!(pub_leaks[0].message.contains("InternalMid"));
    }

    #[test]
    fn alias_leak_chain_of_public_aliases_cascades_one_diagnostic_per_alias() {
        // `internal type Priv = Integer`, `type Mid = Priv` (public), `type
        // High = Mid` (public) — reviewer-flagged mirror of the internal-chain
        // case above: `check_alias_leaked_visibility` checks every top-level
        // *public* alias independently (its own fresh `visited` set), so a
        // chain of N public aliases reaching the same internal root produces
        // N diagnostics, one per alias, all naming 'Priv'. This is accepted
        // behavior (fixing the innermost boundary, 'Mid', makes both
        // diagnostics disappear) — pinned explicitly so the count doesn't
        // silently change in either direction.
        let module = parse_bt("internal type Priv = Integer\ntype Mid = Priv\ntype High = Mid");
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, Some("json"), &mut diags);

        let priv_leaks: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("Priv"))
            .collect();
        assert_eq!(
            priv_leaks.len(),
            2,
            "expected one cascading diagnostic each for 'Mid' and 'High' (both reach \
             'Priv' independently), got: {diags:?}"
        );
        assert!(
            priv_leaks
                .iter()
                .any(|d| d.message.contains("public type alias 'Mid'"))
        );
        assert!(
            priv_leaks
                .iter()
                .any(|d| d.message.contains("public type alias 'High'"))
        );
    }

    #[test]
    fn alias_leak_through_generic_type_argument() {
        // `type Pub = List(Priv)` — the leak-check must recurse into a
        // Generic annotation's type arguments, not just its base name.
        let module = parse_bt("internal type Priv = Integer\ntype Pub = List(Priv)");
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, Some("json"), &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Internal type alias 'Priv'")
                    && d.message.contains("public type alias 'Pub'")),
            "Expected alias-leak error through a Generic type argument, got: {diags:?}"
        );
    }

    #[test]
    fn alias_leak_through_intersection_and_difference() {
        // `type Pub = (Priv & String) \ Symbol` — both the Intersection and
        // Difference recursion arms must be walked.
        let module = parse_bt("internal type Priv = Integer\ntype Pub = (Priv & String) \\ Symbol");
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, Some("json"), &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Internal type alias 'Priv'")
                    && d.message.contains("public type alias 'Pub'")),
            "Expected alias-leak error through Intersection/Difference, got: {diags:?}"
        );
    }

    #[test]
    fn alias_leak_public_alias_reaching_internal_class() {
        // `type Pub = ParserState` where `ParserState` is an internal class
        // — the leakage check also walks through to internal classes, not
        // just internal aliases.
        let module = parse_bt("type Pub = ParserState");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let protocol_registry = ProtocolRegistry::new();
        let mut alias_registry = AliasRegistry::new();
        let diags = alias_registry.register_module(&module, &h, &protocol_registry);
        assert!(diags.is_empty());

        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, Some("json"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("Internal class 'ParserState'")
                && d.message.contains("public type alias 'Pub'")),
            "Expected alias-leak error naming the internal class, got: {diags:?}"
        );
    }

    #[test]
    fn alias_leak_internal_alias_is_not_checked_itself() {
        // Internal-on-internal is fine: an internal alias's own RHS may
        // reference anything visible in its own package without leaking.
        let module = parse_bt("internal type Priv = ParserState");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let protocol_registry = ProtocolRegistry::new();
        let mut alias_registry = AliasRegistry::new();
        let diags = alias_registry.register_module(&module, &h, &protocol_registry);
        assert!(diags.is_empty());

        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, Some("json"), &mut diags);

        assert!(
            diags.is_empty(),
            "An internal alias's own RHS must not be leak-checked, got: {diags:?}"
        );
    }

    #[test]
    fn alias_leak_no_check_without_current_package() {
        let module = parse_bt("internal type Priv = Integer\ntype Pub = Priv | String");
        let (h, alias_registry) = build_hierarchy_and_aliases(&module, "json");
        let mut diags = Vec::new();
        check_alias_leaked_visibility(&module, &h, &alias_registry, None, &mut diags);

        assert!(
            diags.is_empty(),
            "Expected no errors when current_package is None, got: {diags:?}"
        );
    }

    // Additional test imports
    use crate::ast::{
        ClassDefinition, ClassModifiers, CommentAttachment, ExpressionStatement, Identifier,
        MessageSelector, MethodDefinition,
    };
    use crate::semantic_analysis::class_hierarchy::MethodInfo;
    use crate::source_analysis::Span as Sp;

    fn bt1702_span() -> Sp {
        Sp::new(0, 1)
    }

    fn bt1702_ident(name: &str) -> Identifier {
        Identifier {
            name: name.into(),
            span: bt1702_span(),
        }
    }

    fn make_method(selector_name: &str, is_internal: bool) -> MethodDefinition {
        MethodDefinition {
            body: vec![ExpressionStatement::bare(crate::ast::Expression::Literal(
                crate::ast::Literal::Integer(42),
                bt1702_span(),
            ))],
            is_internal,
            ..MethodDefinition::test_at(selector_name, bt1702_span())
        }
    }

    fn make_class_with_methods(
        name: &str,
        superclass: &str,
        methods: Vec<MethodDefinition>,
    ) -> ClassDefinition {
        ClassDefinition::with_modifiers(
            bt1702_ident(name),
            Some(bt1702_ident(superclass)),
            ClassModifiers::default(),
            vec![],
            methods,
            bt1702_span(),
        )
    }

    fn make_module_with_classes(classes: Vec<ClassDefinition>) -> Module {
        let mut module = Module::new(vec![], bt1702_span());
        module.classes = classes;
        module
    }

    // ── W0401: Shadow warning tests ──

    #[test]
    fn w0401_shadow_internal_superclass_method() {
        // Parent has internal method `helper`, Child redefines `helper`
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![ClassInfo {
            package: Some("lib".into()),
            methods: vec![MethodInfo {
                selector: "helper".into(),
                arity: 0,
                defined_in: "Parent".into(),
                is_sealed: false,
                is_internal: true,
                spawns_block: false,
                return_type: None,
                param_types: vec![],
                doc: None,

                origin: None,
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            }],
            ..ClassInfo::test("Parent", Some("Object"))
        }]);

        let module = make_module_with_classes(vec![make_class_with_methods(
            "Child",
            "Parent",
            vec![make_method("helper", false)],
        )]);

        let mut diagnostics = Vec::new();
        check_internal_method_shadow(&module, &hierarchy, &mut diagnostics);

        let w0401: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("shadows internal"))
            .collect();
        assert_eq!(w0401.len(), 1, "Expected shadow warning, got: {w0401:?}");
        assert!(w0401[0].message.contains("helper"));
        assert!(w0401[0].message.contains("Parent"));
    }

    #[test]
    fn w0401_no_warning_for_public_superclass_method() {
        // Parent has a public method `helper` — Child overriding it is fine (no warning)
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![ClassInfo {
            package: Some("lib".into()),
            methods: vec![MethodInfo {
                selector: "helper".into(),
                arity: 0,
                defined_in: "Parent".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![],
                doc: None,

                origin: None,
                primitive: None,
                constant_value: None,
                answers_receiver: false,
            }],
            ..ClassInfo::test("Parent", Some("Object"))
        }]);

        let module = make_module_with_classes(vec![make_class_with_methods(
            "Child",
            "Parent",
            vec![make_method("helper", false)],
        )]);

        let mut diagnostics = Vec::new();
        check_internal_method_shadow(&module, &hierarchy, &mut diagnostics);

        let w0401: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message.contains("shadows internal"))
            .collect();
        assert!(
            w0401.is_empty(),
            "No shadow warning for public method override, got: {w0401:?}"
        );
    }

    #[test]
    fn w0401_no_warning_for_new_selector() {
        // Child defines a new method that doesn't exist on superclass — no warning
        let mut hierarchy = ClassHierarchy::with_builtins();
        hierarchy.add_from_beam_meta(vec![ClassInfo {
            package: Some("lib".into()),
            ..ClassInfo::test("Parent", Some("Object"))
        }]);

        let module = make_module_with_classes(vec![make_class_with_methods(
            "Child",
            "Parent",
            vec![make_method("newMethod", false)],
        )]);

        let mut diagnostics = Vec::new();
        check_internal_method_shadow(&module, &hierarchy, &mut diagnostics);

        assert!(
            diagnostics.is_empty(),
            "New method should not trigger W0401, got: {diagnostics:?}"
        );
    }

    // ── E0402: Leaked visibility tests ──

    #[test]
    fn e0402_internal_method_satisfying_public_protocol() {
        // Register the class in the hierarchy (needed for resolves_selector)
        let module = make_module_with_classes(vec![make_class_with_methods(
            "MyPrinter",
            "Object",
            vec![make_method("asString", true)], // internal!
        )]);
        let (h, _) = ClassHierarchy::build(&module);
        let hierarchy = h;

        // Create a protocol registry with a Printable protocol requiring `asString`
        let mut registry = ProtocolRegistry::new();
        // Manually insert a protocol (the registry doesn't have a public insert method,
        // so we use register_module with a module containing a protocol definition)
        let proto_module = {
            use crate::ast::{ProtocolDefinition, ProtocolMethodSignature};
            let mut m = Module::new(vec![], bt1702_span());
            m.protocols = vec![ProtocolDefinition {
                name: bt1702_ident("Printable"),
                type_params: vec![],
                extending: None,
                method_signatures: vec![ProtocolMethodSignature {
                    selector: MessageSelector::Unary("asString".into()),
                    parameters: vec![],
                    return_type: None,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: bt1702_span(),
                }],
                class_method_signatures: vec![],
                provided_methods: vec![],
                package: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: bt1702_span(),
            }];
            m
        };
        let _reg_diags = registry.register_module(&proto_module, &hierarchy);

        let mut diagnostics = Vec::new();
        check_leaked_method_visibility(
            &module,
            &hierarchy,
            &registry,
            Some("my_pkg"),
            &mut diagnostics,
        );

        let e0402: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                d.message.contains("public signature")
                    || d.message.contains("satisfies public protocol")
            })
            .collect();
        assert_eq!(
            e0402.len(),
            1,
            "Expected leaked visibility error for internal method satisfying public protocol, got: {e0402:?}"
        );
        assert!(e0402[0].message.contains("asString"));
        assert!(e0402[0].message.contains("Printable"));
    }

    #[test]
    fn e0402_public_method_satisfying_protocol_is_fine() {
        let module = make_module_with_classes(vec![make_class_with_methods(
            "MyPrinter",
            "Object",
            vec![make_method("asString", false)], // public
        )]);
        let (h, _) = ClassHierarchy::build(&module);
        let hierarchy = h;

        let mut registry = ProtocolRegistry::new();
        let proto_module = {
            use crate::ast::{ProtocolDefinition, ProtocolMethodSignature};
            let mut m = Module::new(vec![], bt1702_span());
            m.protocols = vec![ProtocolDefinition {
                name: bt1702_ident("Printable"),
                type_params: vec![],
                extending: None,
                method_signatures: vec![ProtocolMethodSignature {
                    selector: MessageSelector::Unary("asString".into()),
                    parameters: vec![],
                    return_type: None,
                    comments: CommentAttachment::default(),
                    doc_comment: None,
                    span: bt1702_span(),
                }],
                class_method_signatures: vec![],
                provided_methods: vec![],
                package: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: bt1702_span(),
            }];
            m
        };
        let _reg_diags = registry.register_module(&proto_module, &hierarchy);

        let mut diagnostics = Vec::new();
        check_leaked_method_visibility(
            &module,
            &hierarchy,
            &registry,
            Some("my_pkg"),
            &mut diagnostics,
        );

        let e0402: Vec<_> = diagnostics
            .iter()
            .filter(|d| {
                d.message.contains("public signature")
                    || d.message.contains("satisfies public protocol")
            })
            .collect();
        assert!(
            e0402.is_empty(),
            "Public method should not trigger leaked visibility error, got: {e0402:?}"
        );
    }

    /// BT-5184: the protocol registry is a `HashMap`, so walking it unsorted
    /// emitted one class's E0402 errors in a different order on each run.
    /// Every fresh registry gets a fresh hash seed, so rebuilding it per run
    /// is the "different seeds" check.
    #[test]
    fn e0402_errors_are_in_protocol_name_order_on_every_run() {
        use std::fmt::Write as _;
        let names = [
            "Golf", "Alpha", "Echo", "Charlie", "Foxtrot", "Delta", "Bravo",
        ];
        let mut source = String::new();
        for name in names {
            let sel = name.to_lowercase();
            let _ = write!(source, "Protocol define: {name}\n  {sel} -> Integer\n\n");
        }
        source.push_str("Object subclass: Leaky\n");
        for name in names {
            let _ = writeln!(source, "  internal {} -> Integer => 1", name.to_lowercase());
        }
        let run = || {
            let (module, hierarchy) =
                crate::semantic_analysis::test_helpers::parse_and_build(&source);
            let mut registry = ProtocolRegistry::new();
            let _ = registry.register_module(&module, &hierarchy);
            let mut diagnostics = Vec::new();
            check_leaked_method_visibility(
                &module,
                &hierarchy,
                &registry,
                Some("my_pkg"),
                &mut diagnostics,
            );
            diagnostics
                .iter()
                .map(|d| d.message.to_string())
                .collect::<Vec<_>>()
        };
        let first = run();
        assert_eq!(first.len(), names.len(), "{first:?}");
        let mut sorted_names = names;
        sorted_names.sort_unstable();
        for (message, name) in first.iter().zip(sorted_names) {
            assert!(
                message.contains(&format!("protocol '{name}'")),
                "out of order: {first:?}"
            );
        }
        for _ in 0..20 {
            assert_eq!(run(), first);
        }
    }
}
