// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Destructure-assignment binding and extension method checking.

use super::common::*;

// ---- Destructure assignment TypeEnv binding tests ----

fn destructure_assign(pattern: Pattern, value: Expression) -> Expression {
    Expression::DestructureAssignment {
        pattern,
        value: Box::new(value),
        span: span(),
    }
}

fn array_pattern(names: &[&str]) -> Pattern {
    Pattern::Array {
        elements: names.iter().map(|n| Pattern::Variable(ident(n))).collect(),
        list_syntax: false,
        rest: None,
        span: span(),
    }
}

fn list_pattern(names: &[&str]) -> Pattern {
    Pattern::Array {
        elements: names.iter().map(|n| Pattern::Variable(ident(n))).collect(),
        list_syntax: true,
        rest: None,
        span: span(),
    }
}

#[test]
fn test_tuple_destructure_binds_vars_in_env() {
    // {x, y} := someValue
    // x foo   <- x is Dynamic (local), not a field; no DNU warning expected
    //
    // Without the fix, x is unbound and the lookup falls back to self-field lookup.
    // If a class field named `x` were Integer, `x foo` would warn. With the fix,
    // x is bound as Dynamic and no warning is produced.
    let state = vec![StateDeclaration::with_type_and_default(
        ident("x"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method(
        "doWork",
        vec![
            // {x, _} := someValue  — shadows field `x` with local
            destructure_assign(
                Pattern::Tuple {
                    elements: vec![Pattern::Variable(ident("x")), Pattern::Wildcard(span())],
                    span: span(),
                },
                int_lit(0),
            ),
            // x foo — x is Dynamic (local), should NOT produce a DNU warning
            msg_send(var("x"), MessageSelector::Unary("foo".into()), vec![]),
        ],
    );
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let checker = check(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "destructured local `x` should shadow field and be Dynamic (no DNU): {dnu_warnings:?}"
    );
}

#[test]
fn test_array_destructure_binds_vars_in_env() {
    // #[first, second] := someValue
    // first + 1 — first is Dynamic, no warnings expected
    let method = make_method(
        "doWork",
        vec![
            destructure_assign(array_pattern(&["first", "second"]), int_lit(0)),
            msg_send(
                var("first"),
                MessageSelector::Binary("+".into()),
                vec![int_lit(1)],
            ),
        ],
    );
    let class = make_class_with_methods("Thing", vec![method]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let checker = check(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "array-destructured vars should be Dynamic — no warnings: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_list_destructure_binds_vars_in_env() {
    // #(first, second) := someValue  (list syntax)
    // first + 1 — first is Dynamic, no warnings expected
    // Verifies list_syntax: true takes the same type-checking path as list_syntax: false.
    let method = make_method(
        "doWork",
        vec![
            destructure_assign(list_pattern(&["first", "second"]), int_lit(0)),
            msg_send(
                var("first"),
                MessageSelector::Binary("+".into()),
                vec![int_lit(1)],
            ),
        ],
    );
    let class = make_class_with_methods("Thing", vec![method]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let checker = check(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "list-destructured vars should be Dynamic — no warnings: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_bind_pattern_vars_skips_wildcard() {
    // Directly verify that bind_pattern_vars does NOT insert `_` into TypeEnv
    // while still binding the named variable `y`.
    let pattern = Pattern::Tuple {
        elements: vec![Pattern::Wildcard(span()), Pattern::Variable(ident("y"))],
        span: span(),
    };
    let mut env = TypeEnv::new();
    TypeChecker::bind_pattern_vars(&pattern, &mut env);
    assert!(
        env.get_local("_").is_none(),
        "`_` must not be bound by bind_pattern_vars"
    );
    assert_eq!(
        env.get_local("y"),
        Some(InferredType::Dynamic(DynamicReason::Unknown)),
        "`y` must be bound as Dynamic"
    );
}

/// Cross-file Value sub-subclass `self new:` should NOT produce DNU warning.
///
/// Simulates the build command's flow: Child is in the current file,
/// Base is injected from another file via pre-loaded classes.
#[test]
fn cross_file_value_sub_subclass_no_dnu_for_new() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;

    // Parse a module containing only Child (extends Base, has a class method using self new:)
    let source = r"
Base subclass: Child
  field: y = 0
  class make: val => self new: #{#y => val}
";
    let module = parse_ok(source);

    // Simulate Base from another file: Value subclass: Base (field: x = 0)
    let base_info = ClassInfo {
        is_value: true,
        state: vec![eco_string("x")],
        ..ClassInfo::test("Base", Some("Value"))
    };

    // Use the full analysis pipeline (same as the build command)
    let result = crate::semantic_analysis::analyse_full(
        &module,
        crate::semantic_analysis::AnalysisContext::default()
            .with_pre_loaded_classes(vec![base_info]),
    );
    let dnu_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "self new: in class method should not produce DNU for Value sub-subclass, got: {dnu_warnings:?}"
    );
}
