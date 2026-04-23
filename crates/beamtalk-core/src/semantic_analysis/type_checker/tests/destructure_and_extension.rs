// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Destructure-assignment binding and extension method checking (BT-1518).

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
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
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
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "array-destructured vars should be Dynamic — no warnings: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_list_destructure_binds_vars_in_env() {
    // #(first, second) := someValue  (list syntax, BT-1279)
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
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
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

// --- Extension method type checking tests (BT-1518) ---

#[test]
fn extension_method_no_dnu_warning() {
    // `"hello" shout` where `String >> shout` is defined as an extension.
    // Should NOT produce "does not understand" warning.
    let hierarchy = hierarchy_with_extension("String", "shout", 0, vec![], Some("String".into()));
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Unary("shout".into()),
        vec![],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Extension method 'shout' should be recognised, got: {dnu:?}"
    );
}

#[test]
fn extension_method_return_type_propagates() {
    // `"hello" shout` returns String (from extension annotation).
    // Sending `size` on the result should not warn.
    let hierarchy = hierarchy_with_extension("String", "shout", 0, vec![], Some("String".into()));
    let module = make_module(vec![msg_send(
        msg_send(
            str_lit("hello"),
            MessageSelector::Unary("shout".into()),
            vec![],
        ),
        MessageSelector::Unary("size".into()),
        vec![],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Chained send on extension return type should resolve, got: {dnu:?}"
    );
}

#[test]
fn extension_method_unannotated_returns_dynamic() {
    // `"hello" shout` with no return type annotation returns Dynamic.
    // Sending any message on the result should not warn (Dynamic = no checking).
    let hierarchy = hierarchy_with_extension(
        "String",
        "shout",
        0,
        vec![],
        None, // unannotated → Dynamic
    );
    let module = make_module(vec![msg_send(
        msg_send(
            str_lit("hello"),
            MessageSelector::Unary("shout".into()),
            vec![],
        ),
        MessageSelector::Unary("nonexistentMethod".into()),
        vec![],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Unannotated extension returns Dynamic — no false type errors, got: {dnu:?}"
    );
}

#[test]
fn missing_extension_still_warns() {
    // `"hello" nonExistent` — no extension defined, should still warn.
    let hierarchy = ClassHierarchy::with_builtins();
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Unary("nonExistent".into()),
        vec![],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu.len(),
        1,
        "Missing extension should still produce DNU warning"
    );
}

#[test]
fn extension_method_class_side_no_warning() {
    // `String fromJson: "..."` where `String class >> fromJson:` is an extension.
    use crate::compilation::extension_index::{
        ExtensionIndex, ExtensionKey, ExtensionLocation, ExtensionTypeInfo, MethodSide,
    };
    use std::path::PathBuf;

    let mut h = ClassHierarchy::with_builtins();
    let mut index = ExtensionIndex::new();
    let key = ExtensionKey {
        class_name: "String".into(),
        side: MethodSide::Class,
        selector: "fromJson:".into(),
    };
    index.entries_mut().insert(
        key,
        vec![ExtensionLocation {
            file_path: PathBuf::from("test.bt"),
            span: span(),
            type_info: ExtensionTypeInfo {
                arity: 1,
                param_types: vec![Some("String".into())],
                return_type: Some("String".into()),
            },
        }],
    );
    h.register_extensions(&index);

    let module = make_module(vec![msg_send(
        class_ref("String"),
        MessageSelector::Keyword(vec![KeywordPart::new("fromJson:", span())]),
        vec![str_lit("{}")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &h);
    let dnu: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu.is_empty(),
        "Class-side extension should be recognised, got: {dnu:?}"
    );
}

#[test]
fn extension_method_argument_type_checking() {
    // `42 addString: "hello"` where `Integer >> addString: s :: String -> Integer`
    // Sending with wrong arg type should warn.
    let hierarchy = hierarchy_with_extension(
        "Integer",
        "addString:",
        1,
        vec![Some("String".into())],
        Some("Integer".into()),
    );
    // Correct arg type — no warning
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Keyword(vec![KeywordPart::new("addString:", span())]),
        vec![str_lit("hello")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        arg_warnings.is_empty(),
        "Correct argument type should not warn, got: {arg_warnings:?}"
    );

    // Wrong arg type — should warn
    let module2 = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Keyword(vec![KeywordPart::new("addString:", span())]),
        vec![int_lit(99)],
    )]);
    let mut checker2 = TypeChecker::new();
    checker2.check_module(&module2, &hierarchy);
    let arg_warnings2: Vec<_> = checker2
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects String"))
        .collect();
    assert_eq!(
        arg_warnings2.len(),
        1,
        "Wrong argument type to extension should warn"
    );
}

/// BT-1559: Cross-file Value sub-subclass `self new:` should NOT produce DNU warning.
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
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    // Simulate Base from another file: Value subclass: Base (field: x = 0)
    let base_info = ClassInfo {
        name: eco_string("Base"),
        superclass: Some(eco_string("Value")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![eco_string("x")],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    // Use the full analysis pipeline (same as the build command)
    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![base_info],
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
