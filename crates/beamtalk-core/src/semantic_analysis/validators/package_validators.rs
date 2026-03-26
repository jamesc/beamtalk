// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Package qualifier validation (ADR 0070, Phase 2).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validates that package qualifiers in `ClassReference` nodes and
//! `StandaloneMethodDefinition` targets reference declared dependencies.

use crate::ast::{Expression, Module};
use crate::source_analysis::Diagnostic;
use std::collections::HashSet;

/// Validates all package qualifiers in the module against known dependencies.
///
/// Emits an error for each package qualifier that doesn't match any declared
/// dependency. The `known_packages` set should contain the package names from
/// the `[dependencies]` section of `beamtalk.toml`.
///
/// If `known_packages` is empty, validation is skipped — this handles the
/// common case where no package system is in use (REPL, single-file scripts).
pub fn check_package_qualifiers(
    module: &Module,
    known_packages: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    if known_packages.is_empty() {
        return;
    }

    // Check class references in top-level expressions
    for stmt in &module.expressions {
        check_expression_packages(&stmt.expression, known_packages, diagnostics);
    }

    // Check class references within class definitions
    for class in &module.classes {
        // Check superclass if it has a package qualifier
        if let Some(ref pkg) = class.superclass_package {
            if !known_packages.contains(pkg.name.as_str()) {
                let superclass_name = class.superclass.as_ref().map_or("?", |s| s.name.as_str());
                diagnostics.push(Diagnostic::error(
                    format!(
                        "Unknown package '{}' in superclass reference '{}@{}'",
                        pkg.name, pkg.name, superclass_name
                    ),
                    pkg.span,
                ));
            }
        }

        // Check expressions in method bodies
        for method in &class.methods {
            for stmt in &method.body {
                check_expression_packages(&stmt.expression, known_packages, diagnostics);
            }
        }

        for method in &class.class_methods {
            for stmt in &method.body {
                check_expression_packages(&stmt.expression, known_packages, diagnostics);
            }
        }

        // Check state variable default values
        for state in &class.state {
            if let Some(ref default) = state.default_value {
                check_expression_packages(default, known_packages, diagnostics);
            }
        }
    }

    // Check standalone method definitions
    for method_def in &module.method_definitions {
        if let Some(ref pkg) = method_def.package {
            if !known_packages.contains(pkg.name.as_str()) {
                diagnostics.push(Diagnostic::error(
                    format!(
                        "Unknown package '{}' in extension target '{}@{}'",
                        pkg.name, pkg.name, method_def.class_name.name
                    ),
                    pkg.span,
                ));
            }
        }

        for stmt in &method_def.method.body {
            check_expression_packages(&stmt.expression, known_packages, diagnostics);
        }
    }
}

/// Recursively checks expressions for package-qualified class references.
#[allow(clippy::too_many_lines)] // one arm per Expression variant; irreducible
fn check_expression_packages(
    expr: &Expression,
    known_packages: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    match expr {
        Expression::ClassReference {
            name,
            package: Some(pkg),
            ..
        } => {
            if !known_packages.contains(pkg.name.as_str()) {
                diagnostics.push(Diagnostic::error(
                    format!(
                        "Unknown package '{}' in qualified reference '{}@{}'",
                        pkg.name, pkg.name, name.name
                    ),
                    pkg.span,
                ));
            }
        }

        Expression::MessageSend {
            receiver,
            arguments,
            ..
        } => {
            check_expression_packages(receiver, known_packages, diagnostics);
            for arg in arguments {
                check_expression_packages(arg, known_packages, diagnostics);
            }
        }

        Expression::Assignment { target, value, .. } => {
            check_expression_packages(target, known_packages, diagnostics);
            check_expression_packages(value, known_packages, diagnostics);
        }

        Expression::Block(block) => {
            for stmt in &block.body {
                check_expression_packages(&stmt.expression, known_packages, diagnostics);
            }
        }

        Expression::Return { value, .. } => {
            check_expression_packages(value, known_packages, diagnostics);
        }

        Expression::Parenthesized { expression, .. } => {
            check_expression_packages(expression, known_packages, diagnostics);
        }

        Expression::FieldAccess { receiver, .. } => {
            check_expression_packages(receiver, known_packages, diagnostics);
        }

        Expression::Cascade {
            receiver, messages, ..
        } => {
            check_expression_packages(receiver, known_packages, diagnostics);
            for msg in messages {
                for arg in &msg.arguments {
                    check_expression_packages(arg, known_packages, diagnostics);
                }
            }
        }

        Expression::Match { value, arms, .. } => {
            check_expression_packages(value, known_packages, diagnostics);
            for arm in arms {
                check_expression_packages(&arm.body, known_packages, diagnostics);
                if let Some(ref guard) = arm.guard {
                    check_expression_packages(guard, known_packages, diagnostics);
                }
                // Check patterns for qualified class references
                check_pattern_packages(&arm.pattern, known_packages, diagnostics);
            }
        }

        Expression::MapLiteral { pairs, .. } => {
            for pair in pairs {
                check_expression_packages(&pair.key, known_packages, diagnostics);
                check_expression_packages(&pair.value, known_packages, diagnostics);
            }
        }

        Expression::ListLiteral { elements, tail, .. } => {
            for elem in elements {
                check_expression_packages(elem, known_packages, diagnostics);
            }
            if let Some(t) = tail {
                check_expression_packages(t, known_packages, diagnostics);
            }
        }

        Expression::ArrayLiteral { elements, .. } => {
            for elem in elements {
                check_expression_packages(elem, known_packages, diagnostics);
            }
        }

        Expression::StringInterpolation { segments, .. } => {
            for segment in segments {
                if let crate::ast::StringSegment::Interpolation(inner) = segment {
                    check_expression_packages(inner, known_packages, diagnostics);
                }
            }
        }

        Expression::DestructureAssignment { pattern, value, .. } => {
            check_expression_packages(value, known_packages, diagnostics);
            check_pattern_packages(pattern, known_packages, diagnostics);
        }

        // No package qualifiers in these expression types
        Expression::Literal(..)
        | Expression::Identifier(..)
        | Expression::ClassReference { package: None, .. }
        | Expression::Super(..)
        | Expression::Primitive { .. }
        | Expression::ExpectDirective { .. }
        | Expression::Spread { .. }
        | Expression::Error { .. } => {}
    }
}

/// Checks patterns for package-qualified class references (used in pattern matching).
///
/// Currently a no-op: `Pattern` does not have a `ClassReference` variant with a
/// package qualifier. The `Constructor` pattern has a `class: Identifier` but no
/// package field yet. This function will be needed when package-qualified constructor
/// patterns are added (e.g. `json@Result ok: v`).
#[allow(unused_variables)] // parameters are infrastructure for future pattern types
fn check_pattern_packages(
    _pattern: &crate::ast::Pattern,
    _known_packages: &HashSet<String>,
    _diagnostics: &mut Vec<Diagnostic>,
) {
    // No-op: Pattern types don't currently carry package qualifiers.
    // When Constructor patterns support `package@Class ok: v` syntax,
    // this function will recursively validate qualifiers.
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::{Severity, lex_with_eof, parse};

    fn parse_module(src: &str) -> Module {
        let tokens = lex_with_eof(src);
        let (module, _diags) = parse(tokens);
        module
    }

    #[test]
    fn known_package_qualifier_no_error() {
        let module = parse_module("json@Parser parse: input");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(errors.is_empty(), "Expected no errors, got: {errors:?}");
    }

    #[test]
    fn unknown_package_qualifier_produces_error() {
        let module = parse_module("xml@Parser parse: input");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error, got: {diags:?}"
        );
    }

    #[test]
    fn empty_known_packages_skips_validation() {
        let module = parse_module("xml@Parser parse: input");
        let known = HashSet::new();
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags.is_empty(),
            "Expected no errors when known_packages is empty, got: {diags:?}"
        );
    }

    #[test]
    fn unqualified_class_reference_no_error() {
        let module = parse_module("Counter spawn");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags.is_empty(),
            "Expected no errors for unqualified ref, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_method_body_checked() {
        let module = parse_module("Object subclass: Foo\n  bar => xml@Parser parse: \"test\"");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in method body, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_standalone_method_target() {
        let module = parse_module("xml@Parser >> lenient => 42");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in extension target, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_standalone_method_target_known() {
        let module = parse_module("json@Parser >> lenient => 42");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(
            errors.is_empty(),
            "Expected no errors for known package, got: {errors:?}"
        );
    }

    #[test]
    fn multiple_unknown_packages_multiple_errors() {
        let module = parse_module("a := xml@Parser parse: \"x\".\nb := yaml@Loader load: \"y\"");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert_eq!(
            errors.len(),
            2,
            "Expected 2 errors for 2 unknown packages, got: {errors:?}"
        );
    }

    #[test]
    fn qualified_in_block_body() {
        let module = parse_module("[:x | xml@Parser parse: x]");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in block body, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_superclass_unknown_package() {
        let module = parse_module("xml@Parser subclass: LenientParser\n  parse => 42");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")
                    && d.message.contains("superclass")),
            "Expected unknown package error in superclass, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_superclass_known_package() {
        let module = parse_module("json@Parser subclass: LenientParser\n  parse => 42");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        let errors: Vec<_> = diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(
            errors.is_empty(),
            "Expected no errors for known superclass package, got: {errors:?}"
        );
    }
}
