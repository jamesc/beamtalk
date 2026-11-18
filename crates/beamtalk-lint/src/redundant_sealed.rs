// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lint: flag `sealed` on a method of a `sealed` class (BT-5008).
//!
//! A sealed class cannot be subclassed, so none of its methods can be
//! overridden: writing `sealed` on one of them adds no protection. Every
//! method of a sealed class counts as sealed already
//! (`ClassDefinition::is_method_sealed`), so removing the keyword changes
//! nothing a user or the runtime can observe.
//!
//! ```text
//! // Bad — Point is sealed, so `x` cannot be overridden anyway
//! sealed Value subclass: Point
//!   sealed x => ...
//!
//! // Good
//! sealed Value subclass: Point
//!   x => ...
//!
//! // Fine — Shape is open, so `sealed` is what stops a subclass overriding `area`
//! Object subclass: Shape
//!   sealed area => ...
//! ```
//!
//! Only classes that seal their methods are checked
//! (`ClassDefinition::seals_its_methods`: `sealed` and not `abstract`), the same
//! predicate the derived per-method flag uses. `abstract sealed` classes such as
//! `Boolean` have stdlib-internal subclasses (`True`/`False`), so their
//! method-level `sealed` is meaningful and is left alone.
//! Covers instance-side and class-side methods, including the
//! `class sealed` / `class internal sealed` spellings, and inline and
//! standalone (`Foo >> bar`) definitions.

use crate::{LintContext, LintPass};
use beamtalk_core::ast::{MethodDefinition, Module};
use beamtalk_core::source_analysis::Diagnostic;

/// Lint pass that flags `sealed` on a method of a sealed class.
pub(crate) struct RedundantSealedPass;

impl LintPass for RedundantSealedPass {
    fn check(&self, module: &Module, _ctx: &LintContext<'_>, diagnostics: &mut Vec<Diagnostic>) {
        for class in &module.classes {
            if !class.seals_its_methods() {
                continue;
            }
            for method in class.methods.iter().chain(&class.class_methods) {
                check_method(&class.name.name, method, diagnostics);
            }
        }
        // Standalone `Foo >> bar` definitions on a sealed class declared in
        // the same module.
        for standalone in &module.method_definitions {
            if module
                .classes
                .iter()
                .any(|c| c.seals_its_methods() && c.name.name == standalone.class_name.name)
            {
                check_method(&standalone.class_name.name, &standalone.method, diagnostics);
            }
        }
    }
}

fn check_method(class_name: &str, method: &MethodDefinition, diagnostics: &mut Vec<Diagnostic>) {
    if method.is_sealed {
        diagnostics.push(
            Diagnostic::lint(
                format!(
                    "redundant `sealed`: `{class_name}` is sealed, so its methods cannot be overridden"
                ),
                method.span,
            )
            .with_hint("Remove `sealed` from this method — every method of a sealed class is sealed"),
        );
    }
}

#[cfg(test)]
mod tests {
    use crate::run_lint_passes;
    use beamtalk_core::source_analysis::{DiagnosticCategory, Severity, lex_with_eof, parse};

    fn lint(source: &str) -> Vec<beamtalk_core::source_analysis::Diagnostic> {
        let tokens = lex_with_eof(source);
        let (module, parse_diags) = parse(tokens);
        assert!(
            parse_diags.is_empty(),
            "Parse failed for lint fixture: {parse_diags:?}"
        );
        run_lint_passes(&module)
            .into_iter()
            .filter(|d| d.message.contains("redundant `sealed`"))
            .collect()
    }

    #[test]
    fn fires_on_instance_method_of_sealed_class() {
        let diags = lint("sealed Object subclass: Foo\n  sealed bar => 1\n  baz => 2");
        assert_eq!(diags.len(), 1, "got: {diags:?}");
        assert_eq!(diags[0].severity, Severity::Lint);
        assert_eq!(diags[0].category, Some(DiagnosticCategory::Lint));
        assert!(diags[0].message.contains("`Foo` is sealed"));
    }

    #[test]
    fn fires_on_class_side_methods_of_sealed_class() {
        let diags = lint(
            "sealed Object subclass: Foo\n  class sealed make => 1\n  class internal sealed make2 => 2\n  class plain => 3",
        );
        assert_eq!(diags.len(), 2, "got: {diags:?}");
    }

    #[test]
    fn does_not_fire_on_open_class() {
        let diags = lint("Object subclass: Foo\n  sealed bar => 1\n  class sealed make => 2");
        assert!(diags.is_empty(), "got: {diags:?}");
    }

    #[test]
    fn does_not_fire_on_abstract_class() {
        let diags =
            lint("abstract Object subclass: Foo\n  sealed bar => 1\n  class sealed make => 2");
        assert!(diags.is_empty(), "got: {diags:?}");
    }

    #[test]
    fn does_not_fire_on_abstract_sealed_class() {
        // `abstract sealed` classes (e.g. `Boolean`) have stdlib-internal
        // subclasses, so the lint stays out of them.
        let diags = lint(
            "abstract sealed Object subclass: Foo\n  sealed bar => 1\n  class sealed make => 2",
        );
        assert!(diags.is_empty(), "got: {diags:?}");
    }

    #[test]
    fn suppressed_by_expect_lint() {
        let src = "sealed Object subclass: Foo\n  @expect lint \"kept for the test\"\n  sealed bar => 1\n";
        let (module, parse_diags) = parse(lex_with_eof(src));
        assert!(parse_diags.is_empty(), "{parse_diags:?}");
        let mut diags = Vec::new();
        crate::run_pass(&super::RedundantSealedPass, &module, &mut diags);
        assert_eq!(diags.len(), 1, "fires before @expect is applied");
        beamtalk_core::compilation::diagnostics_policy::apply_expect_directives(
            &module, &mut diags,
        );
        assert!(
            diags.iter().all(|d| d.severity != Severity::Lint),
            "@expect lint should suppress, got: {diags:?}"
        );
    }

    #[test]
    fn does_not_fire_without_keyword() {
        let diags = lint("sealed Object subclass: Foo\n  bar => 1\n  class make => 2");
        assert!(diags.is_empty(), "got: {diags:?}");
    }
}
