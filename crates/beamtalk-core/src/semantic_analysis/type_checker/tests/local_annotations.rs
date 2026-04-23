// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Local variable type annotations and method-call union returns (BT-2012, BT-2017).

use super::common::*;

// --- Local variable type annotation tests (BT-2012) ---

#[test]
fn annotated_assignment_uses_declared_type() {
    // `x :: Integer := expr` should bind x as Integer, not Dynamic
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let hierarchy = ClassHierarchy::with_builtins();

    let expr = Expression::Assignment {
        target: Box::new(var("x")),
        value: Box::new(Expression::Identifier(Identifier::new("someVar", span()))),
        type_annotation: Some(TypeAnnotation::simple("Integer", span())),
        span: span(),
    };
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);
    assert_eq!(ty, InferredType::known("Integer"));
    assert_eq!(env.get_local("x"), Some(InferredType::known("Integer")));
}

#[test]
fn annotated_assignment_dynamic_rhs_no_warning() {
    // Dynamic RHS (the primary use case) should be accepted silently
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let hierarchy = ClassHierarchy::with_builtins();

    // someVar is unknown → Dynamic
    let expr = Expression::Assignment {
        target: Box::new(var("x")),
        value: Box::new(Expression::Identifier(Identifier::new("someVar", span()))),
        type_annotation: Some(TypeAnnotation::simple("String", span())),
        span: span(),
    };
    checker.infer_expr(&expr, &hierarchy, &mut env, false);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Dynamic RHS should not produce type mismatch, got: {type_warnings:?}"
    );
}

#[test]
fn annotated_assignment_narrowing_from_object_no_warning() {
    // `x :: Dictionary := <Object>` is the primary type-erasure use case:
    // callees like `Binary deserialize:` return Object, and the annotation is
    // the user's explicit assertion that the runtime type is more specific.
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let hierarchy = ClassHierarchy::with_builtins();

    // Seed someVar with inferred type Object in the env
    env.set_local("someVar", InferredType::known("Object"));

    let expr = Expression::Assignment {
        target: Box::new(var("x")),
        value: Box::new(Expression::Identifier(Identifier::new("someVar", span()))),
        type_annotation: Some(TypeAnnotation::simple("Dictionary", span())),
        span: span(),
    };
    checker.infer_expr(&expr, &hierarchy, &mut env, false);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Narrowing from Object to Dictionary should not warn, got: {type_warnings:?}"
    );
}

#[test]
fn annotated_assignment_narrowing_to_parametric_type_no_warning() {
    // `dict :: Dictionary(Symbol, String) := <Object>` — narrowing to a
    // parametric type should also be silently accepted.
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let hierarchy = ClassHierarchy::with_builtins();

    env.set_local("someVar", InferredType::known("Object"));

    let dict_ann = TypeAnnotation::Generic {
        base: Identifier::new("Dictionary", span()),
        parameters: vec![
            TypeAnnotation::simple("Symbol", span()),
            TypeAnnotation::simple("String", span()),
        ],
        span: span(),
    };
    let expr = Expression::Assignment {
        target: Box::new(var("dict")),
        value: Box::new(Expression::Identifier(Identifier::new("someVar", span()))),
        type_annotation: Some(dict_ann),
        span: span(),
    };
    checker.infer_expr(&expr, &hierarchy, &mut env, false);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Narrowing Object to Dictionary(Symbol, String) should not warn, got: {type_warnings:?}"
    );
}

#[test]
fn annotated_assignment_mismatch_warns() {
    // Known incompatible type should emit a warning
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let hierarchy = ClassHierarchy::with_builtins();

    // RHS is Integer literal, declared as String → should warn
    let expr = Expression::Assignment {
        target: Box::new(var("x")),
        value: Box::new(int_lit(42)),
        type_annotation: Some(TypeAnnotation::simple("String", span())),
        span: span(),
    };
    checker.infer_expr(&expr, &hierarchy, &mut env, false);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert!(
        !type_warnings.is_empty(),
        "Integer assigned to String annotation should produce a type mismatch warning"
    );
}

#[test]
fn annotated_assignment_compatible_type_no_warning() {
    // Compatible subtype should not warn
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    let hierarchy = ClassHierarchy::with_builtins();

    // RHS is Integer literal, declared as Number (Integer's superclass) → no warning
    let expr = Expression::Assignment {
        target: Box::new(var("x")),
        value: Box::new(int_lit(42)),
        type_annotation: Some(TypeAnnotation::simple("Number", span())),
        span: span(),
    };
    checker.infer_expr(&expr, &hierarchy, &mut env, false);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Type mismatch"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Integer assigned to Number should not warn, got: {type_warnings:?}"
    );
}

// ── BT-2016: isNil narrowing propagation into keyword args / typed assignment ──

/// BT-2016: After `ms isNil ifTrue: [^nil]`, the narrowed type of `ms`
/// should be `Integer` (not `Integer | Nil`), so passing it to a class-side
/// keyword method that expects `Integer` must not produce a type mismatch.
#[test]
fn narrowing_isnil_propagates_to_keyword_arg_e2e() {
    let source = r"
Object subclass: Receiver
  class process: ms :: Integer => ms

Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [^nil]
    Receiver process: ms
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    // Filter by DiagnosticCategory::Type so argument-type warnings
    // (formatted as "Argument ... expects ... got ...") are caught too,
    // not only assignment-style "Type mismatch" messages.
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Narrowed Integer should not warn as keyword arg, got: {type_warnings:?}"
    );
}

/// BT-2016: After `ms isNil ifTrue: [^nil]`, assigning the narrowed value
/// to a typed local `local :: Integer := ms` must not produce a type mismatch.
#[test]
fn narrowing_isnil_propagates_to_typed_assignment_rhs_e2e() {
    let source = r"
Object subclass: Caller
  run: ms :: Integer | Nil =>
    ms isNil ifTrue: [^nil]
    local :: Integer := ms
    local
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Narrowed Integer in typed assignment should not warn, got: {type_warnings:?}"
    );
}

/// BT-2016: After `ms isNil ifTrue: [^nil]`, passing the narrowed value to
/// an instance-side keyword method that expects `Integer` must not warn.
#[test]
fn narrowing_isnil_propagates_to_instance_send_arg_e2e() {
    // `Value subclass:` so the receiver can be `new`d — Object subclasses are
    // not instantiable. The test focuses on the narrowing propagation into
    // `r process: ms`, not on instantiation semantics.
    let source = r"
Value subclass: Receiver
  process: ms :: Integer => ms

Object subclass: Caller
  run: ms :: Integer | Nil =>
    r := Receiver new
    ms isNil ifTrue: [^nil]
    r process: ms
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");

    let result = crate::semantic_analysis::analyse_with_options_and_classes(
        &module,
        &crate::CompilerOptions::default(),
        vec![],
    );
    let type_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::Type))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Narrowed Integer in instance send arg should not warn, got: {type_warnings:?}"
    );
}

/// BT-2016: `non_nil_type` must strip `Known("Nil")` from unions, not just
/// `Known("UndefinedObject")`. This ensures the defense-in-depth layer works
/// even if a `"Nil"` member slips past `resolve_type_keyword`.
#[test]
fn non_nil_type_strips_capital_nil_from_union() {
    // Union containing "Nil" (capital, as parser might produce from type annotations)
    let union_with_nil = InferredType::simple_union(&["Integer", "Nil"]);
    let narrowed = TypeChecker::non_nil_type(&union_with_nil);
    assert_eq!(
        narrowed,
        InferredType::known("Integer"),
        "non_nil_type should strip Nil from union"
    );
}

/// BT-2016: `non_nil_type` must strip both `Known("UndefinedObject")` and
/// `Known("Nil")` when both appear in the same union.
#[test]
fn non_nil_type_strips_both_nil_variants() {
    let union = InferredType::Union {
        members: vec![
            InferredType::known("Integer"),
            InferredType::known("UndefinedObject"),
            InferredType::known("Nil"),
        ],
        provenance: TypeProvenance::Inferred(span()),
    };
    let narrowed = TypeChecker::non_nil_type(&union);
    assert_eq!(
        narrowed,
        InferredType::known("Integer"),
        "non_nil_type should strip both UndefinedObject and Nil"
    );
}

// ---- BT-2017: Method-call-bound locals must resolve union return types ----

/// BT-2017: When a method returns `Integer | Nil`, the return type must be
/// resolved as a proper `Union([Integer, UndefinedObject])`, not as a flat
/// `Known("Integer | Nil")`. Without this fix, locals bound from such method
/// calls get a malformed type that causes false binary-operand warnings
/// (e.g. `>=` on Integer expects a numeric argument, got Integer | Nil).
#[test]
fn method_call_union_return_no_false_binary_warning() {
    // Build two classes:
    //   Object subclass: Cfg
    //     v -> Integer | Nil => nil
    //
    //   typed Object subclass: Consumer
    //     check: cfg :: Cfg -> Boolean =>
    //       local := cfg v
    //       5 >= local      // should NOT warn about "Integer | Nil"
    //       true

    // --- Cfg class: has method `v` returning `Integer | Nil` ---
    let cfg_class = ClassDefinition::with_modifiers(
        ident("Cfg"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("v".into()),
            vec![],
            vec![bare(var("nil"))],
            TypeAnnotation::Union {
                types: vec![
                    TypeAnnotation::Simple(ident("Integer")),
                    TypeAnnotation::Simple(ident("Nil")),
                ],
                span: span(),
            },
            span(),
        )],
        span(),
    );

    // --- Consumer class (typed): calls cfg v, assigns to local, uses in >= ---
    let consumer_class = ClassDefinition::with_modifiers(
        ident("Consumer"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("check:", span())]),
            vec![ParameterDefinition::with_type(
                ident("cfg"),
                TypeAnnotation::Simple(ident("Cfg")),
            )],
            vec![
                // local := cfg v
                bare(assign(
                    "local",
                    msg_send(var("cfg"), MessageSelector::Unary("v".into()), vec![]),
                )),
                // 5 >= local
                bare(msg_send(
                    int_lit(5),
                    MessageSelector::Binary(">=".into()),
                    vec![var("local")],
                )),
                // true
                bare(var("true")),
            ],
            TypeAnnotation::Simple(ident("Boolean")),
            span(),
        )],
        span(),
    );

    let module = make_module_with_classes(vec![], vec![cfg_class, consumer_class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let binary_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a numeric argument"))
        .collect();
    assert!(
        binary_warnings.is_empty(),
        "BT-2017: method-call-bound local with union return type should not produce \
         false binary operand warning, got: {binary_warnings:?}"
    );
}

/// BT-2017: Verify that `resolve_type_name_string` correctly splits
/// "Integer | Nil" into a proper Union type (not Known("Integer | Nil")).
#[test]
fn resolve_type_name_string_splits_union() {
    let ty = TypeChecker::resolve_type_name_string(&"Integer | Nil".into());
    match &ty {
        InferredType::Union { members, .. } => {
            assert_eq!(
                members.len(),
                2,
                "Expected 2 union members for 'Integer | Nil', got {members:?}"
            );
            assert!(
                members.contains(&InferredType::known("Integer")),
                "Union should contain Integer"
            );
            assert!(
                members.contains(&InferredType::known("UndefinedObject")),
                "Union should contain UndefinedObject (resolved from Nil)"
            );
        }
        other => panic!("Expected Union type for 'Integer | Nil', got: {other:?}"),
    }
}

/// BT-2017: Parameter-bound locals (case a) should continue to work
/// without binary operand warnings — regression guard.
#[test]
fn param_passthrough_local_no_false_binary_warning() {
    // typed Object subclass: Test
    //   check: x :: Integer | Nil -> Boolean =>
    //     local := x
    //     5 >= local
    //     true
    let class_def = ClassDefinition::with_modifiers(
        ident("Test"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Keyword(vec![KeywordPart::new("check:", span())]),
            vec![ParameterDefinition::with_type(
                ident("x"),
                TypeAnnotation::Union {
                    types: vec![
                        TypeAnnotation::Simple(ident("Integer")),
                        TypeAnnotation::Simple(ident("Nil")),
                    ],
                    span: span(),
                },
            )],
            vec![
                bare(assign("local", var("x"))),
                bare(msg_send(
                    int_lit(5),
                    MessageSelector::Binary(">=".into()),
                    vec![var("local")],
                )),
                bare(var("true")),
            ],
            TypeAnnotation::Simple(ident("Boolean")),
            span(),
        )],
        span(),
    );

    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let binary_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a numeric argument"))
        .collect();
    assert!(
        binary_warnings.is_empty(),
        "BT-2017: method-call-bound local with union return type should not produce \
         false binary operand warning, got: {binary_warnings:?}"
    );
}
