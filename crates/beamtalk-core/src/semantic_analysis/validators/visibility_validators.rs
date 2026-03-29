// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-level visibility enforcement (ADR 0071, Phase 2).
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

use crate::ast::{Expression, Module, TypeAnnotation};
use crate::semantic_analysis::class_hierarchy::ClassHierarchy;
use crate::source_analysis::{Diagnostic, Span};

/// Checks all class-level visibility rules for the module.
///
/// `current_package` is the package being compiled. When `None` (REPL / script),
/// no visibility checks are run — there is no cross-package context.
pub fn check_class_visibility(
    module: &Module,
    hierarchy: &ClassHierarchy,
    current_package: Option<&str>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let Some(current_pkg) = current_package else {
        return;
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
            check_leaked_visibility_class(class, current_pkg, hierarchy, diagnostics);
        }

        // E0401: type annotations in methods (both instance and class-side)
        for method in class.methods.iter().chain(class.class_methods.iter()) {
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
        // Extension methods targeting a class from another package —
        // check if that class is internal.
        if let Some(ref pkg) = method_def.package {
            // The target class is explicitly from another package
            let class_name = &method_def.class_name.name;
            if let Some(info) = hierarchy.get_class(class_name) {
                if info.is_internal {
                    let target_pkg = info.package.as_deref().unwrap_or_else(|| pkg.name.as_str());
                    diagnostics.push(
                        Diagnostic::error(
                            format!(
                                "error[E0401]: Class '{class_name}' is internal to package '{target_pkg}' \
                                 and cannot be referenced from '{current_pkg}'"
                            ),
                            method_def.class_name.span,
                        )
                        .with_hint(format!(
                            "'{class_name}' is declared 'internal' in package '{target_pkg}'"
                        )),
                    );
                }
            }
        }

        // Also check expressions in extension method bodies
        for stmt in &method_def.method.body {
            check_expression_cross_package(&stmt.expression, current_pkg, hierarchy, diagnostics);
        }
    }
}

/// Recursively checks expressions for cross-package internal class references (E0401).
///
/// Walks all `ClassReference` nodes in the expression tree. This catches
/// `ParserState new`, `x isKindOf: ParserState`, and similar patterns where
/// an internal class name appears in an expression.
#[allow(clippy::too_many_lines)] // one arm per Expression variant; irreducible
fn check_expression_cross_package(
    expr: &Expression,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::ClassReference { name, .. } => {
            check_cross_package_ref(&name.name, name.span, current_pkg, hierarchy, diagnostics);
        }

        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            check_expression_cross_package(receiver, current_pkg, hierarchy, diagnostics);
            for arg in arguments {
                check_expression_cross_package(arg, current_pkg, hierarchy, diagnostics);
            }
        }

        Expression::Assignment { target, value, .. } => {
            check_expression_cross_package(target, current_pkg, hierarchy, diagnostics);
            check_expression_cross_package(value, current_pkg, hierarchy, diagnostics);
        }

        Expression::Block(block) => {
            for stmt in &block.body {
                check_expression_cross_package(
                    &stmt.expression,
                    current_pkg,
                    hierarchy,
                    diagnostics,
                );
            }
        }

        Expression::Return { value, .. } | Expression::DestructureAssignment { value, .. } => {
            check_expression_cross_package(value, current_pkg, hierarchy, diagnostics);
        }

        Expression::Parenthesized { expression, .. } => {
            check_expression_cross_package(expression, current_pkg, hierarchy, diagnostics);
        }

        Expression::FieldAccess { receiver, .. } => {
            check_expression_cross_package(receiver, current_pkg, hierarchy, diagnostics);
        }

        Expression::Cascade {
            receiver, messages, ..
        } => {
            check_expression_cross_package(receiver, current_pkg, hierarchy, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    check_expression_cross_package(arg, current_pkg, hierarchy, diagnostics);
                }
            }
        }

        Expression::Match { value, arms, .. } => {
            check_expression_cross_package(value, current_pkg, hierarchy, diagnostics);
            for arm in arms {
                check_expression_cross_package(&arm.body, current_pkg, hierarchy, diagnostics);
                if let Some(ref guard) = arm.guard {
                    check_expression_cross_package(guard, current_pkg, hierarchy, diagnostics);
                }
            }
        }

        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                check_expression_cross_package(&pair.key, current_pkg, hierarchy, diagnostics);
                check_expression_cross_package(&pair.value, current_pkg, hierarchy, diagnostics);
            }
        }

        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                check_expression_cross_package(elem, current_pkg, hierarchy, diagnostics);
            }
            if let Some(t) = tail {
                check_expression_cross_package(t, current_pkg, hierarchy, diagnostics);
            }
        }

        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                check_expression_cross_package(elem, current_pkg, hierarchy, diagnostics);
            }
        }

        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let crate::ast::StringSegment::Interpolation(inner) = segment {
                    check_expression_cross_package(inner, current_pkg, hierarchy, diagnostics);
                }
            }
        }

        // No class references in these expression types
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
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
                    "error[E0401]: Class '{class_name}' is internal to package '{pkg}' \
                     and cannot be referenced from '{current_pkg}'"
                ),
                span,
            )
            .with_hint(format!(
                "'{class_name}' is declared 'internal' in package '{pkg}'"
            )),
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
        TypeAnnotation::FalseOr { inner, .. } => {
            check_type_annotation_cross_package(inner, current_pkg, hierarchy, diagnostics);
        }
        // Singleton and SelfType don't reference classes
        TypeAnnotation::Singleton { .. } | TypeAnnotation::SelfType { .. } => {}
    }
}

/// Checks E0402: leaked visibility — an internal class appearing in a public class's
/// public signature (parameter types, return types, state type annotations).
fn check_leaked_visibility_class(
    class: &crate::ast::ClassDefinition,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
    let class_name = &class.name.name;

    // Check state field type annotations
    for state in &class.state {
        if let Some(ref ty) = state.type_annotation {
            check_type_annotation_leaked(ty, class_name, None, current_pkg, hierarchy, diagnostics);
        }
    }

    // Check method signatures (only non-internal methods on a public class)
    for method in class.methods.iter().chain(class.class_methods.iter()) {
        if method.is_internal {
            // Internal method on public class — its signature can reference
            // internal classes from the same package without leaking.
            continue;
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
                diagnostics,
            );
        }
    }
}

/// Recursively checks a type annotation for leaked internal classes (E0402).
///
/// An internal class from the *same* package appearing in the public signature
/// of a public class is a leaked-visibility error.
fn check_type_annotation_leaked(
    ty: &TypeAnnotation,
    class_name: &ecow::EcoString,
    method_selector: Option<&ecow::EcoString>,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
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
                diagnostics,
            );
            for p in parameters {
                check_type_annotation_leaked(
                    p,
                    class_name,
                    method_selector,
                    current_pkg,
                    hierarchy,
                    diagnostics,
                );
            }
        }
        TypeAnnotation::FalseOr { inner, .. } => {
            check_type_annotation_leaked(
                inner,
                class_name,
                method_selector,
                current_pkg,
                hierarchy,
                diagnostics,
            );
        }
        TypeAnnotation::Singleton { .. } | TypeAnnotation::SelfType { .. } => {}
    }
}

/// Checks if a type name in a public signature references an internal class
/// from the same package, which is a leaked-visibility error (E0402).
fn check_leaked_ref(
    type_name: &ecow::EcoString,
    span: Span,
    class_name: &ecow::EcoString,
    method_selector: Option<&ecow::EcoString>,
    current_pkg: &str,
    hierarchy: &ClassHierarchy,
    diagnostics: &mut Vec<Diagnostic>,
) {
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

    let location = if let Some(sel) = method_selector {
        format!("{class_name} >> {sel}")
    } else {
        format!("{class_name}")
    };

    diagnostics.push(
        Diagnostic::error(
            format!(
                "error[E0402]: Internal class '{type_name}' appears in public signature of '{location}'"
            ),
            span,
        )
        .with_hint(format!(
            "'{type_name}' is declared 'internal' — make it public, or change the type"
        )),
    );
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    use crate::source_analysis::{Severity, lex_with_eof, parse};
    use ecow::EcoString;
    use std::collections::HashMap;

    fn parse_module(src: &str) -> Module {
        let tokens = lex_with_eof(src);
        let (module, _diags) = parse(tokens);
        module
    }

    fn build_hierarchy_with_internal_class(
        module: &Module,
        internal_class: &str,
        internal_pkg: &str,
    ) -> ClassHierarchy {
        let (Ok(mut h), _) = ClassHierarchy::build(module) else {
            panic!("build should succeed");
        };
        // Add an internal class from another package
        let info = ClassInfo {
            name: EcoString::from(internal_class),
            superclass: Some(EcoString::from("Object")),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: true,
            package: Some(EcoString::from(internal_pkg)),
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        };
        h.add_from_beam_meta(vec![info]);
        h
    }

    // --- E0401: Cross-package internal class reference ---

    #[test]
    fn e0401_cross_package_superclass_reference() {
        let module = parse_module("ParserState subclass: MyParser\n  parse => 42");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("my_app"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("E0401")
                && d.message.contains("ParserState")
                && d.message.contains("json")
                && d.message.contains("my_app")),
            "Expected E0401 for cross-package superclass ref, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_same_package_no_error() {
        let module = parse_module(
            "internal Object subclass: ParserState\n  reset => nil\n\nParserState subclass: ExtendedState\n  parse => 42",
        );
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("json"), &mut diags);

        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error && d.message.contains("E0401"))
            .collect();
        assert!(
            errors.is_empty(),
            "Expected no E0401 for same-package ref, got: {errors:?}"
        );
    }

    #[test]
    fn e0401_public_class_cross_package_no_error() {
        let module = parse_module("Object subclass: MyParser\n  parse => 42");
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        // Add a public class from another package
        let info = ClassInfo {
            name: EcoString::from("Parser"),
            superclass: Some(EcoString::from("Object")),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: Some(EcoString::from("json")),
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        };
        h.add_from_beam_meta(vec![info]);
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("my_app"), &mut diags);

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
        let module = parse_module("Object subclass: Foo\n  process: input :: ParserState => 42");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("my_app"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("E0401")
                && d.message.contains("ParserState")),
            "Expected E0401 for type annotation ref, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_return_type_cross_package() {
        let module = parse_module("Object subclass: Foo\n  getState -> ParserState => nil");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("my_app"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("E0401")
                && d.message.contains("ParserState")),
            "Expected E0401 for return type ref, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_no_check_without_current_package() {
        let module = parse_module("ParserState subclass: MyParser\n  parse => 42");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, None, &mut diags);

        assert!(
            diags.is_empty(),
            "Expected no errors when current_package is None, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_state_type_annotation_cross_package() {
        let module = parse_module("Object subclass: Foo\n  state: buf :: ParserState = nil");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("my_app"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("E0401")
                && d.message.contains("ParserState")),
            "Expected E0401 for state type annotation ref, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_class_reference_in_expression() {
        // A class reference to an internal class in a method body expression
        let module = parse_module("Object subclass: Foo\n  bar => ParserState new");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("my_app"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("E0401")
                && d.message.contains("ParserState")),
            "Expected E0401 for class reference in expression, got: {diags:?}"
        );
    }

    #[test]
    fn e0401_class_reference_in_top_level_expression() {
        // A class reference at the top level
        let module = parse_module("ParserState new");
        let h = build_hierarchy_with_internal_class(&module, "ParserState", "json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("my_app"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("E0401")
                && d.message.contains("ParserState")),
            "Expected E0401 for top-level class reference, got: {diags:?}"
        );
    }

    // --- E0402: Leaked visibility ---

    #[test]
    fn e0402_internal_class_in_public_return_type() {
        // A public class with a public method returning an internal class from same package
        let module = parse_module(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  tokenize: input :: String -> TokenBuffer => nil",
        );
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("json"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("E0402")
                && d.message.contains("TokenBuffer")
                && d.message.contains("Parser >> tokenize:")),
            "Expected E0402 for leaked return type, got: {diags:?}"
        );
    }

    #[test]
    fn e0402_internal_class_in_public_param_type() {
        let module = parse_module(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  process: buf :: TokenBuffer => nil",
        );
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("json"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("E0402")
                && d.message.contains("TokenBuffer")
                && d.message.contains("Parser >> process:")),
            "Expected E0402 for leaked param type, got: {diags:?}"
        );
    }

    #[test]
    fn e0402_internal_class_in_state_type_annotation() {
        let module = parse_module(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  state: buf :: TokenBuffer = nil",
        );
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("json"), &mut diags);

        assert!(
            diags.iter().any(|d| d.severity == Severity::Error
                && d.message.contains("E0402")
                && d.message.contains("TokenBuffer")
                && d.message.contains("Parser")),
            "Expected E0402 for leaked state type, got: {diags:?}"
        );
    }

    #[test]
    fn e0402_no_error_internal_class_on_internal_class() {
        // Internal class using internal class in signature — no leak
        let module = parse_module(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             internal Object subclass: InternalParser\n  tokenize: input :: String -> TokenBuffer => nil",
        );
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("json"), &mut diags);

        let leaked: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("E0402"))
            .collect();
        assert!(
            leaked.is_empty(),
            "Expected no E0402 for internal-on-internal, got: {leaked:?}"
        );
    }

    #[test]
    fn e0402_no_error_internal_method_on_public_class() {
        // Internal method on a public class can reference internal classes
        let module = parse_module(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  internal tokenize: input :: String -> TokenBuffer => nil",
        );
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, Some("json"), &mut diags);

        let leaked: Vec<_> = diags
            .iter()
            .filter(|d| d.message.contains("E0402"))
            .collect();
        assert!(
            leaked.is_empty(),
            "Expected no E0402 for internal method on public class, got: {leaked:?}"
        );
    }

    #[test]
    fn e0402_no_check_without_current_package() {
        let module = parse_module(
            "internal Object subclass: TokenBuffer\n  data => nil\n\n\
             Object subclass: Parser\n  tokenize: input :: String -> TokenBuffer => nil",
        );
        let (Ok(mut h), _) = ClassHierarchy::build(&module) else {
            panic!("build should succeed");
        };
        h.stamp_package("json");
        let mut diags = Vec::new();
        check_class_visibility(&module, &h, None, &mut diags);

        assert!(
            diags.is_empty(),
            "Expected no errors when current_package is None, got: {diags:?}"
        );
    }
}
