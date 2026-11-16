// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Package qualifier validation (ADR 0070, Phase 2).
//!
//! **DDD Context:** Semantic Analysis
//!
//! Validates that package qualifiers in `ClassReference` nodes and
//! `StandaloneMethodDefinition` targets, `uses: pkg@Proto` lines and the
//! bodies of a protocol's provided methods name a known package: a declared
//! dependency, the current package or the stdlib.

use crate::ast::{Expression, Identifier, Module};
use crate::ast_walker::walk_expression;
use crate::semantic_analysis::trait_expansion::missing_protocol_diagnostic;
use crate::source_analysis::Diagnostic;
use std::collections::HashSet;

/// The one "Unknown package" error every qualifier site reports (ADR 0070):
/// `message` says where the qualifier was written, the span and the hint are
/// the same for all of them.
fn unknown_package_diagnostic(pkg: &Identifier, message: String) -> Diagnostic {
    Diagnostic::error(message, pkg.span).with_hint(format!(
        "Add '{}' to [dependencies] in beamtalk.toml",
        pkg.name
    ))
}

/// Validates all package qualifiers in the module against known dependencies.
///
/// `module` must be the module as written, not its flattened form: a
/// `uses:`d provision is spliced into each using class, and would be checked
/// again there (and, when the protocol is in another file, reported against
/// the wrong file).
///
/// Emits an error for each package qualifier that doesn't match a known
/// package. Build the `known_packages` set with
/// [`known_qualifier_packages`](crate::semantic_analysis::known_qualifier_packages):
/// the `[dependencies]` of `beamtalk.toml`, the package itself and the stdlib.
///
/// An empty `known_packages` skips validation. That is only a guard: the set
/// from `known_qualifier_packages` always holds `stdlib`, and callers with no
/// package at all (single-file scripts, a manifest-less directory) pass no
/// set, so the check never runs for them.
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
        if let Some(ref pkg) = class.superclass_package
            && !known_packages.contains(pkg.name.as_str())
        {
            let superclass_name = class.superclass.as_ref().map_or("?", |s| s.name.as_str());
            diagnostics.push(unknown_package_diagnostic(
                pkg,
                format!(
                    "Unknown package '{}' in superclass reference '{}@{}'",
                    pkg.name, pkg.name, superclass_name
                ),
            ));
        }

        // `uses: pkg@Proto` names a package too (BT-4929).
        for use_ in &class.uses {
            if let Some(pkg) = &use_.package
                && !known_packages.contains(pkg.name.as_str())
            {
                // The flattener reports an unresolvable `uses:` as "no source
                // available" (or "unknown protocol"). For a package nobody
                // declared that is the same mistake, so the more precise
                // "Unknown package" replaces it instead of adding to it.
                // Built by the flattener's own constructor, so the two cannot
                // drift apart.
                let flattener_error = missing_protocol_diagnostic(use_);
                diagnostics.retain(|d| *d != flattener_error);
                diagnostics.push(unknown_package_diagnostic(
                    pkg,
                    format!(
                        "Unknown package '{}' in protocol reference '{}@{}'",
                        pkg.name, pkg.name, use_.protocol.name
                    ),
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

    // Provided-method bodies in a protocol's own file (BT-4929). `module` is
    // the module as written (see `analyse_full`'s Phase 7), so a body is seen
    // here once; the copies the flattener splices into each using class's
    // `methods` are not part of it.
    for protocol in &module.protocols {
        for method in &protocol.provided_methods {
            for stmt in &method.body {
                check_expression_packages(&stmt.expression, known_packages, diagnostics);
            }
        }
    }

    // Check standalone method definitions
    for method_def in &module.method_definitions {
        if let Some(ref pkg) = method_def.package
            && !known_packages.contains(pkg.name.as_str())
        {
            diagnostics.push(unknown_package_diagnostic(
                pkg,
                format!(
                    "Unknown package '{}' in extension target '{}@{}'",
                    pkg.name, pkg.name, method_def.class_name.name
                ),
            ));
        }

        for stmt in &method_def.method.body {
            check_expression_packages(&stmt.expression, known_packages, diagnostics);
        }
    }
}

/// Checks `expr` and everything under it for package-qualified class
/// references (walked with the shared [`walk_expression`]).
fn check_expression_packages(
    expr: &Expression,
    known_packages: &HashSet<String>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    walk_expression(expr, &mut |e| {
        let Expression::ClassReference {
            name,
            package: Some(pkg),
            ..
        } = e
        else {
            return;
        };
        if !known_packages.contains(pkg.name.as_str()) {
            diagnostics.push(unknown_package_diagnostic(
                pkg,
                format!(
                    "Unknown package '{}' in qualified reference '{}@{}'",
                    pkg.name, pkg.name, name.name
                ),
            ));
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source_analysis::Severity;
    use crate::test_helpers::test_support::parse_bt;

    #[test]
    fn known_package_qualifier_no_error() {
        let module = parse_bt("json@Parser parse: input");
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
        let module = parse_bt("xml@Parser parse: input");
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
        let module = parse_bt("xml@Parser parse: input");
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
        let module = parse_bt("Counter spawn");
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
        let module = parse_bt("Object subclass: Foo\n  bar => xml@Parser parse: \"test\"");
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
        let module = parse_bt("xml@Parser >> lenient => 42");
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
        let module = parse_bt("json@Parser >> lenient => 42");
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
        let module = parse_bt("a := xml@Parser parse: \"x\".\nb := yaml@Loader load: \"y\"");
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
        let module = parse_bt("[:x | xml@Parser parse: x]");
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
        let module = parse_bt("xml@Parser subclass: LenientParser\n  parse => 42");
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
        let module = parse_bt("json@Parser subclass: LenientParser\n  parse => 42");
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

    #[test]
    fn qualified_in_class_side_method_body() {
        let module =
            parse_bt("Object subclass: Foo\n  class bar => xml@Parser parse: 1\n  baz => 42");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in class-side method body, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_state_default_value() {
        let module = parse_bt("Object subclass: Foo\n  state: x = xml@Parser default\n  bar => 42");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in state default value, got: {diags:?}"
        );
    }

    #[test]
    fn unqualified_standalone_method_target_body_still_checked() {
        // `Foo` has no package qualifier, so `method_def.package` is `None` —
        // only the body should be checked, and no "extension target" error
        // should be raised for the (absent) target qualifier.
        let module = parse_bt("Foo >> bar => xml@Parser parse: 1");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")
                    && d.message.contains("qualified reference")),
            "Expected unknown package error from body, got: {diags:?}"
        );
        assert!(
            !diags.iter().any(|d| d.message.contains("extension target")),
            "Did not expect an extension-target error for an unqualified target, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_return_expression() {
        let module = parse_bt(
            "Object subclass: Foo\n  bar =>\n    true ifTrue: [^xml@Parser parse: 1]\n    2",
        );
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in return expression, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_parenthesized_expression() {
        let module = parse_bt("(xml@Parser parse: 1)");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in parenthesized expression, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_field_access_receiver() {
        let module = parse_bt("xml@Parser.field");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in field access receiver, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_cascade_receiver_and_argument() {
        let module = parse_bt("xml@Parser new; bar: (yaml@Loader load: 1)");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error for cascade receiver, got: {diags:?}"
        );
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'yaml'")),
            "Expected unknown package error for cascade message argument, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_match_arm_body_and_guard() {
        let module =
            parse_bt("x match: [n when: [xml@Parser parse: n] -> yaml@Loader load: n; _ -> 0]");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in match guard, got: {diags:?}"
        );
        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'yaml'")),
            "Expected unknown package error in match arm body, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_map_literal_value() {
        let module = parse_bt("#{#a => xml@Parser parse: 1}");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in map literal value, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_list_literal_element_and_tail() {
        let module = parse_bt("#(1, xml@Parser parse: 1 | rest)");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in list literal element, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_array_literal_element() {
        let module = parse_bt("#[1, xml@Parser parse: 1]");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in array literal element, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_string_interpolation() {
        let module = parse_bt("\"result: {xml@Parser parse: 1}\"");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in string interpolation, got: {diags:?}"
        );
    }

    #[test]
    fn qualified_in_destructure_assignment_value() {
        let module = parse_bt("#(a, b) := xml@Parser parse: 1");
        let mut known = HashSet::new();
        known.insert("json".to_string());
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);

        assert!(
            diags
                .iter()
                .any(|d| d.message.contains("Unknown package 'xml'")),
            "Expected unknown package error in destructure assignment value, got: {diags:?}"
        );
    }

    #[test]
    fn known_qualifier_packages_has_dependencies_current_package_and_stdlib() {
        let known =
            crate::semantic_analysis::known_qualifier_packages(Some("app"), ["json", "http"]);
        let expected: HashSet<String> = ["json", "http", "app", "stdlib"]
            .into_iter()
            .map(String::from)
            .collect();
        assert_eq!(known, expected);

        // No package: still the stdlib, so the set is never empty.
        let known = crate::semantic_analysis::known_qualifier_packages(None, std::iter::empty());
        assert_eq!(known, HashSet::from(["stdlib".to_string()]));
    }

    fn unknown_package_errors(source: &str, known: &[&str]) -> Vec<String> {
        let module = parse_bt(source);
        let known: HashSet<String> = known.iter().map(|k| (*k).to_string()).collect();
        let mut diags = Vec::new();
        check_package_qualifiers(&module, &known, &mut diags);
        diags
            .into_iter()
            .filter(|d| d.severity == Severity::Error)
            .map(|d| d.message.to_string())
            .collect()
    }

    /// BT-4929: `uses: pkg@Proto` names a package, so an undeclared one is
    /// an unknown package like any other qualifier.
    #[test]
    fn uses_unknown_package_qualifier_produces_error() {
        let errors = unknown_package_errors(
            "Object subclass: Widget\n  uses: nosuchpkg@Foo\n  name -> String => \"w\"\n",
            &["json"],
        );
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(
            errors[0].contains("Unknown package 'nosuchpkg'")
                && errors[0].contains("nosuchpkg@Foo"),
            "{errors:?}"
        );
    }

    #[test]
    fn uses_known_package_qualifier_no_error() {
        let errors = unknown_package_errors(
            "Object subclass: Widget\n  uses: json@Foo\n  name -> String => \"w\"\n",
            &["json"],
        );
        assert!(errors.is_empty(), "{errors:?}");
    }

    #[test]
    fn uses_unqualified_protocol_no_error() {
        let errors = unknown_package_errors(
            "Object subclass: Widget\n  uses: Foo\n  name -> String => \"w\"\n",
            &["json"],
        );
        assert!(errors.is_empty(), "{errors:?}");
    }

    /// Each `uses:` line is checked, not just the first.
    #[test]
    fn uses_every_line_is_checked() {
        let errors = unknown_package_errors(
            "Object subclass: Widget\n  uses: aaa@Foo\n  uses: json@Bar\n  uses: bbb@Baz\n  name -> String => \"w\"\n",
            &["json"],
        );
        assert_eq!(errors.len(), 2, "{errors:?}");
    }

    /// BT-4929: a protocol's provided-method bodies live in
    /// `module.protocols`, which the walk did not visit.
    #[test]
    fn protocol_provided_method_body_unknown_package_produces_error() {
        let errors = unknown_package_errors(
            "Protocol define: Greets\n  name -> String\n\n  greet -> Object => nosuchpkg@Widget new\n",
            &["json"],
        );
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(
            errors[0].contains("Unknown package 'nosuchpkg'"),
            "{errors:?}"
        );
    }

    #[test]
    fn protocol_provided_method_body_known_package_no_error() {
        let errors = unknown_package_errors(
            "Protocol define: Greets\n  name -> String\n\n  greet -> Object => json@Widget new\n",
            &["json"],
        );
        assert!(errors.is_empty(), "{errors:?}");
    }

    /// BT-4929: the flattener already fails an unresolvable `uses: pkg@Proto`
    /// ("no source available …"). For an unknown `pkg` that is the same
    /// mistake as "Unknown package", so only the latter is reported.
    #[test]
    fn uses_unknown_package_supersedes_the_flatteners_no_source_error() {
        let module = parse_bt(
            "Object subclass: Widget\n  uses: nosuchpkg@Foo\n  uses: json@Bar\n  name -> String => \"w\"\n",
        );
        let known: HashSet<String> = HashSet::from(["json".to_string()]);
        let use_of = |pkg: &str| {
            module.classes[0]
                .uses
                .iter()
                .find(|u| u.package.as_ref().is_some_and(|p| p.name == pkg))
                .unwrap()
        };
        let mut diags = vec![
            missing_protocol_diagnostic(use_of("nosuchpkg")),
            missing_protocol_diagnostic(use_of("json")),
        ];
        check_package_qualifiers(&module, &known, &mut diags);

        let messages: Vec<_> = diags.iter().map(|d| d.message.to_string()).collect();
        assert_eq!(messages.len(), 2, "{messages:?}");
        assert!(
            messages
                .iter()
                .any(|m| m.contains("Unknown package 'nosuchpkg'")),
            "{messages:?}"
        );
        // A known package's missing source is a different problem: kept.
        assert!(
            messages
                .iter()
                .any(|m| m.contains("no source available") && m.contains("json@Bar")),
            "{messages:?}"
        );
        assert!(
            !messages
                .iter()
                .any(|m| m.contains("no source available") && m.contains("nosuchpkg")),
            "{messages:?}"
        );
    }
}
