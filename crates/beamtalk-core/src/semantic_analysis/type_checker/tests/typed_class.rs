// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Typed class enforcement and @expect suppression (BT-587).

use super::common::*;

// ---- Typed class tests (BT-587) ----

#[test]
fn test_typed_class_warns_on_missing_param_annotation() {
    // typed class with untyped parameter
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Actor")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::new(ident("amount"))], // no type annotation
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing type annotation for parameter"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "should warn about untyped param `amount`"
    );
    assert!(warnings[0].message.contains("amount"));
}

#[test]
fn test_typed_class_warns_on_missing_return_type() {
    // typed class with method missing return type
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Actor")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Unary("increment".into()),
            vec![],
            vec![bare(int_lit(1))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing return type"))
        .collect();
    assert_eq!(warnings.len(), 1, "should warn about missing return type");
}

#[test]
fn test_typed_class_no_warning_when_fully_annotated() {
    // typed class with fully annotated method
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Actor")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Integer")),
            )],
            vec![bare(int_lit(0))],
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let typed_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing"))
        .collect();
    assert!(
        typed_warnings.is_empty(),
        "fully annotated method should not warn, got: {typed_warnings:?}"
    );
}

// ── BT-1856: @expect suppression on declarations ───────────────────────────

#[test]
fn test_expect_type_suppresses_typed_method_warnings() {
    // BT-1883: typed class with untyped method that has @expect type —
    // type checker emits the diagnostic, apply_expect_directives suppresses it,
    // and no stale @expect warning is produced.
    let mut method = MethodDefinition::new(
        MessageSelector::Unary("first".into()),
        vec![],
        vec![bare(int_lit(42))],
        span(),
    );
    method.expect = Some((ExpectCategory::Type, None, span()));

    let class_def = ClassDefinition::with_modifiers(
        ident("MyTyped"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![method],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        diags.is_empty(),
        "@expect type should suppress missing annotation warnings with no stale warning, got: {diags:?}"
    );
}

#[test]
fn test_expect_all_suppresses_typed_method_warnings() {
    // BT-1883: @expect all should also suppress typed class warnings
    // without producing a stale @expect warning.
    let mut method = MethodDefinition::new(
        MessageSelector::Unary("first".into()),
        vec![],
        vec![bare(int_lit(42))],
        span(),
    );
    method.expect = Some((ExpectCategory::All, None, span()));

    let class_def = ClassDefinition::with_modifiers(
        ident("MyTyped"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![method],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        diags.is_empty(),
        "@expect all should suppress missing annotation warnings with no stale warning, got: {diags:?}"
    );
}

#[test]
fn test_typed_no_default_state_no_warning() {
    // BT-1947: A type annotation replaces the need for a default value.
    // `state: count :: Integer` (no default) should produce no warnings.
    let typed_no_default = StateDeclaration::with_type(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        span(),
    );

    let typed_with_default = StateDeclaration::with_type_and_default(
        ident("name"),
        TypeAnnotation::simple("String", span()),
        str_lit("default"),
        span(),
    );

    let class_def = ClassDefinition::with_modifiers(
        ident("MyTyped"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![typed_no_default, typed_with_default],
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let uninitialized: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("uninitialized"))
        .collect();
    assert!(
        uninitialized.is_empty(),
        "Typed state with type annotation should not warn about uninitialized (BT-1947), got: {uninitialized:?}"
    );
}

#[test]
fn test_non_typed_class_no_warnings() {
    // non-typed class with untyped method — no warnings expected
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Actor")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::new(ident("amount"))],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let typed_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing"))
        .collect();
    assert!(
        typed_warnings.is_empty(),
        "non-typed class should not warn about annotations"
    );
}

#[test]
fn test_typed_class_skips_primitive_methods() {
    // typed class with @primitive method — no warnings
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictInteger"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Binary("+".into()),
            vec![ParameterDefinition::new(ident("other"))],
            vec![bare(Expression::Primitive {
                name: "+".into(),
                is_quoted: true,
                is_intrinsic: false,
                span: span(),
            })],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let typed_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Missing"))
        .collect();
    assert!(
        typed_warnings.is_empty(),
        "primitive methods should be skipped in typed classes"
    );
}

#[test]
fn test_as_type_assertion_infers_correct_type() {
    // x asType: Integer  should infer x as Integer
    let module = make_module(vec![msg_send(
        var("x"),
        MessageSelector::Keyword(vec![KeywordPart::new("asType:", span())]),
        vec![class_ref("Integer")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // The send itself shouldn't warn (asType: is special)
    assert!(
        checker.diagnostics().is_empty(),
        "asType: should not produce warnings, got: {:?}",
        checker.diagnostics()
    );
}
