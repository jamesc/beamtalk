// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! State field assignment and typed state declarations (BT-672, BT-1831, BT-1913).

use super::common::*;

// --- State field assignment type checking tests (BT-672) ---

#[test]
fn test_empty_body_with_return_type_no_crash() {
    // Empty method body with return type annotation — should not crash
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("count".into()),
            vec![],
            vec![], // empty body
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // Dynamic body type — no return type warning (can't infer from empty body)
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "empty body should not produce return type mismatch"
    );
}

#[test]
fn test_integer_plus_string_exactly_one_warning() {
    // 42 + "hello" — should produce exactly 1 warning (binary operand check),
    // not 2 (no duplicate from check_argument_types)
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "should produce exactly 1 warning (no duplicate), got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_field_assign_typed_mismatch_warns() {
    // state: count: Integer = 0; self.count := "bad" → warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method("badMethod", vec![field_assign("count", str_lit("bad"))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 type mismatch warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(warnings[0].message.contains("count"));
    assert!(warnings[0].message.contains("Integer"));
    assert!(warnings[0].message.contains("String"));
}

#[test]
fn test_field_assign_typed_match_no_warn() {
    // state: count: Integer = 0; self.count := 42 → no warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method("goodMethod", vec![field_assign("count", int_lit(42))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "No warnings expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_return_type_subtype_compatible() {
    // Method declares -> Number, body returns Integer (subtype) — no warning
    let class_def = ClassDefinition::with_modifiers(
        ident("Calculator"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("compute".into()),
            vec![],
            vec![bare(int_lit(42))], // Integer is subtype of Number
            TypeAnnotation::Simple(ident("Number")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "Integer return should be compatible with Number declaration"
    );
}

#[test]
fn test_field_assign_untyped_no_warn() {
    // state: value = 0; self.value := "anything" → no warning
    let state = vec![StateDeclaration::with_default(
        ident("value"),
        int_lit(0),
        span(),
    )];
    let method = make_method(
        "setAnything",
        vec![field_assign("value", str_lit("anything"))],
    );
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Untyped fields should not produce warnings, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_early_return_uses_return_type_not_trailing() {
    // Method with early return: ^ 42 followed by unreachable "oops"
    // Return type checking should use the return expression type (Integer),
    // not the unreachable trailing expression (String)
    let class_def = ClassDefinition::with_modifiers(
        ident("Calculator"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("compute".into()),
            vec![],
            vec![
                bare(Expression::Return {
                    value: Box::new(int_lit(42)),
                    span: span(),
                }),
                bare(str_lit("unreachable")), // would be String, bare(but never reached)
            ],
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "early return of Integer should match declared Integer, not unreachable String"
    );
}

#[test]
fn test_field_assign_subtype_no_warn() {
    // Integer is assignable to Number (Integer's superclass chain includes Number)
    let state = vec![StateDeclaration::with_type(
        ident("value"),
        TypeAnnotation::simple("Number", span()),
        span(),
    )];
    let method = make_method("setNumber", vec![field_assign("value", int_lit(42))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_mismatches: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert!(
        type_mismatches.is_empty(),
        "Integer should be assignable to Number, got: {type_mismatches:?}"
    );
}

#[test]
fn test_state_default_value_mismatch_warns() {
    // state: count: Integer = "bad" → warning at class definition time
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        str_lit("bad"),
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 type mismatch for default value, got: {:?}",
        checker.diagnostics()
    );
    assert!(warnings[0].message.contains("count"));
}

#[test]
fn test_state_default_value_match_no_warn() {
    // state: count: Integer = 0 → no warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Default value matching declared type should not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_dynamic_expression_no_warn() {
    // Assigning a dynamic expression (unknown variable) to a typed field → no assignment warning
    let state = vec![StateDeclaration::with_type(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        span(),
    )];
    let method = make_method("setUnknown", vec![field_assign("count", var("unknownVar"))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_mismatches: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert!(
        type_mismatches.is_empty(),
        "Dynamic expressions should never produce type mismatch warnings, got: {type_mismatches:?}"
    );
}

#[test]
fn test_union_type_annotation_no_false_positive() {
    // state: value: Integer | String = 42 → no warning (union types skip check)
    let state = vec![StateDeclaration::with_type_and_default(
        ident("value"),
        TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("String", span()),
            ],
            span(),
        ),
        int_lit(42),
        span(),
    )];
    let method = make_method("setValue", vec![field_assign("value", str_lit("hello"))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Union type annotations should not produce false positives, got: {:?}",
        checker.diagnostics()
    );
}

// --- Typed state field declarations (BT-1831, BT-1947) ---

#[test]
fn test_typed_state_no_default_no_warn() {
    // BT-1947: Mixed-default class: name :: String (no default) + count :: Integer = 0 (has default)
    // Type annotation replaces the need for a default — no uninitialized warning.
    let state = vec![
        StateDeclaration::with_type(
            ident("name"),
            TypeAnnotation::simple("String", span()),
            span(),
        ),
        StateDeclaration::with_type_and_default(
            ident("count"),
            TypeAnnotation::simple("Integer", span()),
            int_lit(0),
            span(),
        ),
    ];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("uninitialized"))
        .collect();
    assert!(
        warnings.is_empty(),
        "BT-1947: Typed state without default should not warn, got: {warnings:?}"
    );
}

#[test]
fn test_all_factory_fields_no_warn() {
    // All-factory class: all typed fields have no default → no warnings.
    // Sibling-default heuristic: if no typed field has a default, the class
    // is factory-constructed (spawnWith:/new:) — suppress warnings.
    let state = vec![
        StateDeclaration::with_type(
            ident("name"),
            TypeAnnotation::simple("String", span()),
            span(),
        ),
        StateDeclaration::with_type(
            ident("config"),
            TypeAnnotation::simple("Dictionary", span()),
            span(),
        ),
    ];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("uninitialized"))
        .collect();
    assert!(
        warnings.is_empty(),
        "All-factory class should not warn about uninitialized fields, got: {warnings:?}"
    );
}

#[test]
fn test_nilable_typed_state_no_warn() {
    // state: name :: String | Nil → no warning (nil is valid)
    let state = vec![StateDeclaration::with_type(
        ident("name"),
        TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("Nil", span()),
            ],
            span(),
        ),
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
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
        "Nilable typed state should not warn about uninitialized, got: {uninitialized:?}"
    );
}

#[test]
fn test_typed_state_with_default_no_warn() {
    // state: name :: String = "" → no warning (has default)
    let state = vec![StateDeclaration::with_type_and_default(
        ident("name"),
        TypeAnnotation::simple("String", span()),
        str_lit(""),
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
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
        "Typed state with default should not warn about uninitialized, got: {uninitialized:?}"
    );
}

#[test]
fn test_untyped_state_no_default_no_warn() {
    // state: name (no type, no default) → no warning
    let state = vec![StateDeclaration::new(ident("name"), span())];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
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
        "Untyped state should not warn about uninitialized, got: {uninitialized:?}"
    );
}

// --- BT-1913: Missing state field annotations in typed classes ---

#[test]
fn test_typed_class_warns_on_missing_state_annotation() {
    // typed class with state field lacking type annotation
    let state = vec![StateDeclaration::new(ident("count"), span())];
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        state,
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "should warn about untyped state field `count`"
    );
    assert!(warnings[0].message.contains("count"));
    assert!(warnings[0].message.contains("StrictCounter"));
}

#[test]
fn test_typed_class_no_warning_when_state_annotated() {
    // typed class with state field that has a type annotation
    let state = vec![StateDeclaration::with_type(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        span(),
    )];
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        state,
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert!(
        warnings.is_empty(),
        "annotated state field should not warn, got: {warnings:?}"
    );
}

#[test]
fn test_inherited_typed_class_warns_on_missing_state_annotation() {
    // A subclass of a typed class should also get the warning
    let parent = ClassDefinition::with_modifiers(
        ident("TypedBase"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![],
        span(),
    );
    let child = ClassDefinition::with_modifiers(
        ident("Child"),
        Some(ident("TypedBase")),
        ClassModifiers::default(),
        vec![StateDeclaration::new(ident("name"), span())],
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "inherited typed class should warn about untyped state field `name`"
    );
    assert!(warnings[0].message.contains("name"));
    assert!(warnings[0].message.contains("Child"));
}

#[test]
fn test_expect_type_suppresses_typed_state_warning() {
    // BT-1913: @expect type on a state field should suppress the warning
    let mut state_decl = StateDeclaration::new(ident("count"), span());
    state_decl.expect = Some((ExpectCategory::Type, None, span()));

    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![state_decl],
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // The type checker should emit the warning...
    let raw_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert_eq!(
        raw_warnings.len(),
        1,
        "type checker should emit the warning"
    );

    // ...but apply_expect_directives should suppress it
    let mut diagnostics = checker.diagnostics().to_vec();
    crate::queries::diagnostic_provider::apply_expect_directives(&module, &mut diagnostics);
    let remaining: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert!(
        remaining.is_empty(),
        "@expect type should suppress the state field warning, got: {remaining:?}"
    );
    // No stale @expect warning
    let stale: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message.contains("stale"))
        .collect();
    assert!(
        stale.is_empty(),
        "should not produce stale @expect warning, got: {stale:?}"
    );
}

#[test]
fn test_untyped_class_no_state_annotation_warning() {
    // Non-typed class should NOT warn about missing state annotations
    let state = vec![StateDeclaration::new(ident("count"), span())];
    let class_def = ClassDefinition::with_modifiers(
        ident("SimpleCounter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        state,
        vec![],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message
                .contains("Missing type annotation for state field")
        })
        .collect();
    assert!(
        warnings.is_empty(),
        "non-typed class should not warn about state annotations, got: {warnings:?}"
    );
}
