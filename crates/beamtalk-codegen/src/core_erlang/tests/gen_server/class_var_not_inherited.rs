// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3762 (ADR 0013 §1, ADR 0130): class variables are per class, not
//! inherited. A subclass class method that reads or writes a class variable
//! declared only on an ancestor does not compile, in all three spellings of a
//! class method (compiled class, class-side extension, `ClassBuilder`
//! `classMethods:`), and the `class-state-abroad` lint agrees: it treats only
//! the class's own declarations as class variables, so it never reasons about
//! an access that codegen rejects.

use super::*;
use beamtalk_core::semantic_analysis::class_hierarchy::ClassHierarchy;
use beamtalk_core::source_analysis::DiagnosticCategory;

const BASE: &str = "Object subclass: Base\n  classState: n = 0\n\n";

use beamtalk_core::test_helpers::test_support::parse_source as parse_module;

/// The class-state-abroad warnings the lint reports for `src`.
fn abroad_warnings(src: &str) -> usize {
    beamtalk_core::semantic_analysis::analyse(&parse_module(src))
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::ClassStateAbroad))
        .count()
}

fn assert_per_class_error(err: &str, what: &str) {
    assert!(
        err.contains("per class") && err.contains("classState: n"),
        "{what}: should say class variables are per class and suggest `classState: n`. Got: {err}"
    );
    assert!(
        !err.contains("cannot access instance field"),
        "{what}: must not be the instance-field error. Got: {err}"
    );
}

/// Subclass class-method bodies touching the ancestor's `n`: an inherited
/// read, an inherited write, and an escaping block (the lint's concern).
const ACCESSES: [(&str, &str); 3] = [
    ("read", "self.n"),
    ("write", "self.n := 1"),
    ("escaping read", "[self.n]"),
];

/// Compiles the `Sub` class alone (one class per module, as in a real build)
/// against a hierarchy that already holds `Base`.
fn compile_sub(sub_src: &str) -> Result<String> {
    generate_module(
        &parse_module(sub_src),
        CodegenOptions::new("sub")
            .with_class_hierarchy(ClassHierarchy::extract_class_infos(&parse_module(BASE))),
    )
}

#[test]
fn compiled_subclass_rejects_inherited_class_variable_and_lint_agrees() {
    for (what, body) in ACCESSES {
        let sub = format!("Base subclass: Sub\n\n  class touch => {body}\n");
        let err = compile_sub(&sub).expect_err(&format!(
            "compiled {what} of an inherited class variable must fail"
        ));
        assert_per_class_error(&format!("{err}"), &format!("compiled {what}"));
        assert_eq!(
            abroad_warnings(&format!("{BASE}{sub}")),
            0,
            "compiled {what}: the lint must not treat an inherited variable as class state"
        );

        // Redeclared: codegen accepts.
        let ok = format!("Base subclass: Sub\n  classState: n = 0\n\n  class touch => {body}\n");
        compile_sub(&ok).unwrap_or_else(|e| panic!("redeclared {what} must compile: {e}"));
    }
    // ... and the lint now sees the class's own variable.
    let escaping =
        format!("{BASE}Base subclass: Sub\n  classState: n = 0\n\n  class touch => [self.n]\n");
    assert_eq!(
        abroad_warnings(&escaping),
        1,
        "the lint flags a redeclared (own) variable in an escaping block"
    );
}

#[test]
fn class_builder_class_methods_reject_inherited_class_variable() {
    for (what, body) in ACCESSES {
        // `n` belongs to the superclass `Base`, not to this builder's
        // `classVars:`, so the fun has no class variable `n`.
        // A builder class is not in the static class hierarchy, so lowering has
        // no declared superclass to consult: it gets the generic "cannot
        // access instance field" error, not the per-class message the
        // compiled and extension spellings get.
        let src = format!(
            "Object classBuilder name: #SubB; superclass: Base; \
             classMethods: #{{ #touch => [:self | {body}] }}; register"
        );
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let expr = module
            .expressions
            .into_iter()
            .next()
            .expect("one expression")
            .expression;
        let mut generator = CoreErlangGenerator::new("test");
        let err = generator
            .expression_doc(&expr)
            .expect_err(&format!(
                "builder {what} of an inherited class variable must fail"
            ))
            .to_string();
        assert_generic_builder_error(&err, &format!("builder {what}"));
    }
}

fn assert_generic_builder_error(err: &str, what: &str) {
    assert!(
        err.contains("instance field 'n' in a class method"),
        "{what}: should be the generic instance-field error. Got: {err}"
    );
    assert!(
        !err.contains("per class"),
        "{what}: must not be the per-class message. Got: {err}"
    );
}
