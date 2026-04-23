// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Argument and return type checking, @expect suppression (BT-671).

use super::common::*;

// ---- BT-671: Argument and return type checking tests ----

#[test]
fn test_integer_plus_string_warns_operand_type() {
    // 42 + "hello" — Integer + expects numeric arg, String is wrong type
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        !checker.diagnostics().is_empty(),
        "42 + 'hello' should produce a type warning"
    );
}

#[test]
fn test_integer_plus_integer_no_arg_type_warning() {
    // 3 + 4 — valid, no warnings (acceptance criteria)
    let module = make_module(vec![msg_send(
        int_lit(3),
        MessageSelector::Binary("+".into()),
        vec![int_lit(4)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "3 + 4 should not produce warnings, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_integer_subtype_of_number_no_warning() {
    // Integer is a subtype of Number via superclass chain
    // Integer + Integer should work since param declares Number
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Integer should be valid for Number param"
    );
}

#[test]
fn test_dynamic_args_never_warn() {
    // unknownVar + 42 — receiver is Dynamic, no warning
    let module = make_module(vec![msg_send(
        var("unknownVar"),
        MessageSelector::Binary("+".into()),
        vec![var("alsoUnknown")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    assert!(
        checker.diagnostics().is_empty(),
        "Dynamic arguments should never produce warnings"
    );
}

#[test]
fn test_return_type_mismatch_warns() {
    // getBalance -> Integer => 'oops' warns (acceptance criteria)
    let class_def = ClassDefinition::with_modifiers(
        ident("Account"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![bare(str_lit("oops"))], // Returns String, declared Integer
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
    assert_eq!(
        return_warnings.len(),
        1,
        "should warn about return type mismatch"
    );
    assert!(return_warnings[0].message.contains("Integer"));
    assert!(return_warnings[0].message.contains("String"));
}

#[test]
fn test_return_type_match_no_warning() {
    // getBalance -> Integer => 42  — correct return type
    let class_def = ClassDefinition::with_modifiers(
        ident("Account"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![bare(int_lit(42))],
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
        "matching return type should not warn"
    );
}

#[test]
fn test_return_type_skip_primitive_methods() {
    // @primitive method — should not check return type
    let class_def = ClassDefinition::with_modifiers(
        ident("MyInt"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("value".into()),
            vec![],
            vec![bare(Expression::Primitive {
                name: "value".into(),
                is_quoted: true,
                is_intrinsic: false,
                span: span(),
            })],
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
        "primitive methods should skip return type check"
    );
}

#[test]
fn test_return_type_no_annotation_no_warning() {
    // Method without return type annotation — no warning
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Unary("count".into()),
            vec![],
            vec![bare(str_lit("oops"))],
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
        "method without return type should not warn about return type"
    );
}
#[test]
fn test_expect_dnu_suppresses_dnu_warning() {
    // @expect dnu before a DNU-producing send suppresses the hint.
    // Real-world use: `self species withAll: result` in Collection.bt.
    // Spans must be real for apply_expect_directives to match by span.
    let hierarchy = ClassHierarchy::with_builtins();

    // Without @expect: should warn
    let module_bare = parse_source("42 unknownSelector");
    let diags_without = run_with_expect(&module_bare, &hierarchy);
    assert!(
        !diags_without.is_empty(),
        "42 unknownSelector should produce a DNU hint without @expect"
    );

    // With @expect dnu: should be suppressed
    let module_with = parse_source("@expect dnu\n42 unknownSelector");
    let diags_with = run_with_expect(&module_with, &hierarchy);
    assert!(
        diags_with.is_empty(),
        "@expect dnu should suppress DNU hint, got: {diags_with:?}"
    );
}

#[test]
fn test_expect_type_suppresses_type_warning() {
    // @expect type before a type-mismatch expression suppresses the warning.
    let hierarchy = ClassHierarchy::with_builtins();

    // Without @expect: should warn
    let module_bare = parse_source("1 + \"hello\"");
    let diags_without = run_with_expect(&module_bare, &hierarchy);
    assert!(
        !diags_without.is_empty(),
        "1 + \"hello\" should produce a type warning without @expect"
    );

    // With @expect type: should be suppressed
    let module_with = parse_source("@expect type\n1 + \"hello\"");
    let diags_with = run_with_expect(&module_with, &hierarchy);
    assert!(
        diags_with.is_empty(),
        "@expect type should suppress type warning, got: {diags_with:?}"
    );
}

#[test]
fn test_expect_type_suppresses_dnu_hint() {
    // BT-1273: @expect type also suppresses method-not-found (DNU) hints,
    // not just type-mismatch warnings.
    let hierarchy = ClassHierarchy::with_builtins();

    // Without @expect: calling unknownMethod on Integer produces a DNU hint
    let module_bare = parse_source("42 unknownMethod");
    let diags_without = run_with_expect(&module_bare, &hierarchy);
    assert!(
        !diags_without.is_empty(),
        "42 unknownMethod should produce a DNU hint without @expect"
    );

    // With @expect type: should also suppress the DNU hint
    let module_with = parse_source("@expect type\n42 unknownMethod");
    let diags_with = run_with_expect(&module_with, &hierarchy);
    assert!(
        diags_with.is_empty(),
        "@expect type should suppress DNU hint, got: {diags_with:?}"
    );
}

#[test]
fn test_expect_type_stale_when_no_dnu_or_type_diagnostic() {
    // BT-1273: @expect type is stale when the following expression has
    // neither a type warning nor a DNU hint.
    let hierarchy = ClassHierarchy::with_builtins();

    // 42 alone produces no diagnostic — @expect type should be stale
    let module = parse_source("@expect type\n42");
    let diags = run_with_expect(&module, &hierarchy);
    let has_stale = diags.iter().any(|d| d.message.contains("stale @expect"));
    assert!(
        has_stale,
        "@expect type on `42` (no diagnostic) must emit stale warning, got: {diags:?}"
    );
}

#[test]
fn test_expect_does_not_suppress_next_next_expression() {
    // @expect dnu only suppresses the immediately following expression.
    // Here @expect applies to `42` (no DNU) → stale warning is emitted,
    // and `42 unknownSelector` still produces its own DNU hint.
    let module = parse_source("@expect dnu\n42\n42 unknownSelector");
    let hierarchy = ClassHierarchy::with_builtins();
    let diags = run_with_expect(&module, &hierarchy);
    assert!(
        !diags.is_empty(),
        "@expect dnu on `42` must not suppress DNU on the following expression"
    );
}

#[test]
fn test_as_type_suppresses_subsequent_warnings() {
    // (x asType: Integer) + "hello" — x is asserted Integer, should warn about String arg
    let module = make_module(vec![msg_send(
        msg_send(
            var("x"),
            MessageSelector::Keyword(vec![KeywordPart::new("asType:", span())]),
            vec![class_ref("Integer")],
        ),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // After asType:, x is Known(Integer), so + "hello" should warn
    assert!(
        !checker.diagnostics().is_empty(),
        "asType: Integer + String should produce a type warning"
    );
}

#[test]
fn test_keyword_arg_type_mismatch_warns() {
    // Counter with typed parameter: deposit: amount: Integer
    // Sending deposit: "hello" should warn
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Integer")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![
            // c := Counter new
            assign(
                "c",
                msg_send(
                    class_ref("Counter"),
                    MessageSelector::Unary("new".into()),
                    vec![],
                ),
            ),
            // c deposit: "hello"
            msg_send(
                var("c"),
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![str_lit("hello")],
            ),
        ],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects Integer"))
        .collect();
    assert_eq!(
        arg_warnings.len(),
        1,
        "should warn about String argument where Integer expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_keyword_arg_type_match_no_warning() {
    // Counter deposit: 42 — correct type
    let class_def = ClassDefinition::with_modifiers(
        ident("Counter"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Integer")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![
            assign(
                "c",
                msg_send(
                    class_ref("Counter"),
                    MessageSelector::Unary("new".into()),
                    vec![],
                ),
            ),
            msg_send(
                var("c"),
                MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
                vec![int_lit(42)],
            ),
        ],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        arg_warnings.is_empty(),
        "correct arg type should not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_parametric_block_arg_type_mismatch_warns() {
    // BT-2002: A non-Block argument to a `Block(T, R)` parameter should still
    // produce a type warning. Previously the hierarchy lookup on the raw
    // annotation string (e.g. "Block(Integer, R)") missed the base class and
    // the conservative "unknown → compatible" escape hatch suppressed the
    // warning.
    let class_def = ClassDefinition::with_modifiers(
        ident("Repro"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("run:", span())]),
            vec![ParameterDefinition::with_type(
                ident("block"),
                TypeAnnotation::Generic {
                    base: ident("Block"),
                    parameters: vec![
                        TypeAnnotation::Simple(ident("Integer")),
                        TypeAnnotation::Simple(ident("R")),
                    ],
                    span: span(),
                },
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![
            assign(
                "r",
                msg_send(
                    class_ref("Repro"),
                    MessageSelector::Unary("new".into()),
                    vec![],
                ),
            ),
            msg_send(
                var("r"),
                MessageSelector::Keyword(vec![KeywordPart::new("run:", span())]),
                vec![str_lit("not a block")],
            ),
        ],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("expects Block(Integer, R)") && d.message.contains("got String")
        })
        .collect();
    assert_eq!(
        arg_warnings.len(),
        1,
        "should warn when String is passed to a Block(Integer, R) parameter, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_parametric_block_arg_type_match_no_warning() {
    // BT-2002: A Block argument to a `Block(T, R)` parameter should not warn.
    let class_def = ClassDefinition::with_modifiers(
        ident("Repro"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("run:", span())]),
            vec![ParameterDefinition::with_type(
                ident("block"),
                TypeAnnotation::Generic {
                    base: ident("Block"),
                    parameters: vec![
                        TypeAnnotation::Simple(ident("Integer")),
                        TypeAnnotation::Simple(ident("R")),
                    ],
                    span: span(),
                },
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![
            assign(
                "r",
                msg_send(
                    class_ref("Repro"),
                    MessageSelector::Unary("new".into()),
                    vec![],
                ),
            ),
            msg_send(
                var("r"),
                MessageSelector::Keyword(vec![KeywordPart::new("run:", span())]),
                vec![block_expr(vec![int_lit(0)])],
            ),
        ],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let arg_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects Block"))
        .collect();
    assert!(
        arg_warnings.is_empty(),
        "a Block arg for a Block(Integer, R) param should not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_override_incompatible_param_type_warns() {
    // Parent: deposit: amount: Number
    // Child: deposit: amount: String (incompatible)
    let parent = ClassDefinition::with_modifiers(
        ident("Base"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Number")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let child = ClassDefinition::with_modifiers(
        ident("Derived"),
        Some(ident("Base")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("String")),
            )],
            vec![bare(str_lit("ok"))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let override_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("incompatible with parent"))
        .collect();
    assert_eq!(
        override_warnings.len(),
        1,
        "should warn about incompatible override param type"
    );
}

#[test]
fn test_override_compatible_param_type_no_warning() {
    // Parent: deposit: amount: Number
    // Child: deposit: amount: Integer (compatible — Integer is subclass of Number)
    let parent = ClassDefinition::with_modifiers(
        ident("Base"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Number")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let child = ClassDefinition::with_modifiers(
        ident("Derived"),
        Some(ident("Base")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Integer")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let override_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("incompatible with parent"))
        .collect();
    assert!(
        override_warnings.is_empty(),
        "compatible override should not warn"
    );
}

#[test]
fn test_override_parametric_incompatible_param_type_warns() {
    // BT-2002: Child overrides parent with a parameterized type annotation
    // (`Array(Integer)`) that is not compatible with the parent's type
    // (`Number`). Previously `is_type_compatible` only normalized the
    // expected side, so `hierarchy.has_class("Array(Integer)")` on the
    // actual side hit the conservative "unknown → compatible" escape hatch
    // and the override warning was silently suppressed.
    let parent = ClassDefinition::with_modifiers(
        ident("Base"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Simple(ident("Number")),
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let child = ClassDefinition::with_modifiers(
        ident("Derived"),
        Some(ident("Base")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
            vec![ParameterDefinition::with_type(
                ident("amount"),
                TypeAnnotation::Generic {
                    base: ident("Array"),
                    parameters: vec![TypeAnnotation::Simple(ident("Integer"))],
                    span: span(),
                },
            )],
            vec![bare(int_lit(0))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let override_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("incompatible with parent"))
        .collect();
    assert_eq!(
        override_warnings.len(),
        1,
        "should warn about incompatible parametric override param type, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn test_all_type_warnings_are_severity_warning() {
    // Verify all diagnostics from type checking are Severity::Warning
    let class_def = ClassDefinition::with_modifiers(
        ident("Account"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("getBalance".into()),
            vec![],
            vec![bare(str_lit("oops"))],
            TypeAnnotation::Simple(ident("Integer")),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(
        vec![msg_send(
            int_lit(42),
            MessageSelector::Binary("+".into()),
            vec![str_lit("hello")],
        )],
        vec![class_def],
    );
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    for diag in checker.diagnostics() {
        assert_eq!(
            diag.severity,
            crate::source_analysis::Severity::Warning,
            "all type checking diagnostics should be warnings, not errors: {}",
            diag.message
        );
    }
}
