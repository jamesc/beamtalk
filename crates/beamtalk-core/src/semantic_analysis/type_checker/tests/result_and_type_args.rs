// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Result isOk/isError narrowing, type-args validation, flatten/partition, coverage (BT-1859, BT-1882).

use super::super::*;
use super::common::*;

// ---- BT-1859: Result isOk/isError narrowing tests ----

#[test]
fn test_detect_narrowing_is_ok_pattern() {
    let expr = is_ok("r");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect isOk narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable, EnvKey::local("r"));
    assert!(info.is_result_ok_check);
    assert!(!info.is_result_error_check);
    assert!(!info.is_nil_check);
}

#[test]
fn test_detect_narrowing_is_error_pattern() {
    let expr = is_error("r");
    let info = TypeChecker::detect_narrowing(&expr);
    assert!(info.is_some(), "Should detect isError narrowing");
    let info = info.unwrap();
    assert_eq!(info.variable, EnvKey::local("r"));
    assert!(!info.is_result_ok_check);
    assert!(info.is_result_error_check);
    assert!(!info.is_nil_check);
}

#[test]
fn test_result_isok_narrowing_true_branch() {
    // r :: Result(String, Error)
    // r isOk ifTrue: [r value]
    // Inside the block, `r` should still be Result(String, Error) so
    // `value` resolves to String via generic substitution (no DNU).
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            // Use Result as param type (the type checker will resolve it)
            vec![("r", Some("Result"))],
            vec![if_true(
                is_ok("r"),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("value".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "r value inside isOk ifTrue: should not produce DNU, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_isok_narrowing_false_branch() {
    // r :: Result(String, Error)
    // r isOk ifFalse: [r error]
    // Inside the false block, `r` is the error variant, so `error` is valid.
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("r", Some("Result"))],
            vec![if_false(
                is_ok("r"),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("error".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "r error inside isOk ifFalse: should not produce DNU, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_isok_narrowing_both_branches() {
    // r :: Result(String, Error)
    // r isOk ifTrue: [r value] ifFalse: [r error]
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("r", Some("Result"))],
            vec![if_true_if_false(
                is_ok("r"),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("value".into()),
                    vec![],
                )]),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("error".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "r value/error inside isOk ifTrue:ifFalse: should not produce DNU, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_iserror_narrowing_true_branch() {
    // r :: Result(String, Error)
    // r isError ifTrue: [r error]
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("r", Some("Result"))],
            vec![if_true(
                is_error("r"),
                block_expr(vec![msg_send(
                    var("r"),
                    MessageSelector::Unary("error".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        warnings.is_empty(),
        "r error inside isError ifTrue: should not produce DNU, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_narrowing_non_result_no_narrowing() {
    // x :: Integer
    // x isOk ifTrue: [x unknownThing]
    // Integer does not understand isOk, so we should get a DNU warning for isOk
    // and also for unknownThing. The narrowing should NOT apply because x is not a Result.
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("x", Some("Integer"))],
            vec![if_true(
                is_ok("x"),
                block_expr(vec![msg_send(
                    var("x"),
                    MessageSelector::Unary("unknownThing".into()),
                    vec![],
                )]),
            )],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    // Should have DNU for 'isOk' on Integer and 'unknownThing' on Integer
    assert!(
        !warnings.is_empty(),
        "Non-Result receiver should still produce DNU warnings, got: {:?}",
        warnings.iter().map(|d| &d.message).collect::<Vec<_>>()
    );
}

#[test]
fn test_result_narrowing_does_not_leak() {
    // r :: Result(String, Error)
    // r isOk ifTrue: [r value]
    // r unknownMethod  // should warn — narrowing scoped to block
    let hierarchy = ClassHierarchy::with_builtins();
    let class = {
        let process_method = make_keyword_method(
            &["process:"],
            vec![("r", Some("Result"))],
            vec![
                if_true(
                    is_ok("r"),
                    block_expr(vec![msg_send(
                        var("r"),
                        MessageSelector::Unary("value".into()),
                        vec![],
                    )]),
                ),
                msg_send(
                    var("r"),
                    MessageSelector::Unary("unknownMethod".into()),
                    vec![],
                ),
            ],
        );
        ClassDefinition::new(
            ident("TestClass"),
            ident("Object"),
            vec![],
            vec![process_method],
            span(),
        )
    };
    let module = make_module_with_classes(vec![], vec![class]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("does not understand") && d.message.contains("unknownMethod")
        })
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Narrowing should not leak outside the block, expected 1 DNU for unknownMethod"
    );
}

// BT-1861: Warn on type args for classes with no type params
#[test]
fn type_args_for_non_generic_class_warns() {
    // Integer has no type params — `:: Integer(String)` should warn
    let state = vec![StateDeclaration::with_type(
        ident("count"),
        TypeAnnotation::Generic {
            base: ident("Integer"),
            parameters: vec![TypeAnnotation::Simple(ident("String"))],
            span: span(),
        },
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 'no type parameters' warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        warnings[0].message.contains("Integer"),
        "Warning should mention the class name, got: {}",
        warnings[0].message
    );
}

#[test]
fn type_args_for_generic_class_no_false_positive() {
    // GenericDict has type params (K, V) — `:: GenericDict(Symbol, Integer)` should NOT warn
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![
        ClassInfo {
            name: "GenericDict".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec!["K".into(), "V".into()],
            type_param_bounds: vec![None, None],
            superclass_type_args: vec![],
        },
        ClassInfo {
            name: "Counter".into(),
            superclass: Some("Object".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: false,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        },
    ]);

    let state = vec![StateDeclaration::with_type(
        ident("refs"),
        TypeAnnotation::Generic {
            base: ident("GenericDict"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Symbol")),
                TypeAnnotation::Simple(ident("Integer")),
            ],
            span: span(),
        },
        span(),
    )];
    let class = counter_class_with_typed_state(vec![], state);
    let module = make_module_with_classes(vec![], vec![class]);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert!(
        warnings.is_empty(),
        "GenericDict has type params — should not warn, got: {warnings:?}",
    );
}

#[test]
fn type_args_for_non_generic_class_in_method_param_warns() {
    // Method parameter annotated as `:: Boolean(Integer)` — Boolean has no type params
    let method = MethodDefinition {
        selector: MessageSelector::Unary("doSomething".into()),
        parameters: vec![ParameterDefinition {
            name: ident("flag"),
            type_annotation: Some(TypeAnnotation::Generic {
                base: ident("Boolean"),
                parameters: vec![TypeAnnotation::Simple(ident("Integer"))],
                span: span(),
            }),
        }],
        body: vec![],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let class = counter_class_with_typed_state(vec![method], vec![]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 'no type parameters' warning for method param, got: {:?}",
        checker.diagnostics()
    );
    assert!(warnings[0].message.contains("Boolean"));
}

#[test]
fn type_args_for_non_generic_class_in_return_type_warns() {
    // Return type annotated as `:: String(Integer)` — String has no type params
    let method = MethodDefinition::with_return_type(
        MessageSelector::Unary("name".into()),
        vec![],
        vec![],
        TypeAnnotation::Generic {
            base: ident("String"),
            parameters: vec![TypeAnnotation::Simple(ident("Integer"))],
            span: span(),
        },
        span(),
    );
    let class = counter_class_with_typed_state(vec![method], vec![]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "Expected 1 'no type parameters' warning for return type, got: {:?}",
        checker.diagnostics()
    );
    assert!(warnings[0].message.contains("String"));
}

#[test]
fn type_args_for_block_no_false_positive() {
    // Block uses type args as documentation convention (e.g., `Block(E, Boolean)`)
    // This should NOT trigger the "no type parameters" warning
    let method = MethodDefinition {
        selector: MessageSelector::Unary("doSomething".into()),
        parameters: vec![ParameterDefinition {
            name: ident("block"),
            type_annotation: Some(TypeAnnotation::Generic {
                base: ident("Block"),
                parameters: vec![
                    TypeAnnotation::Simple(ident("E")),
                    TypeAnnotation::Simple(ident("Boolean")),
                ],
                span: span(),
            }),
        }],
        body: vec![],
        kind: MethodKind::Primary,
        return_type: None,
        is_sealed: false,
        is_internal: false,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };
    let class = counter_class_with_typed_state(vec![method], vec![]);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds_in_module(&module, &hierarchy, &ProtocolRegistry::new());
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("no type parameters"))
        .collect();
    assert!(
        warnings.is_empty(),
        "Block type args are a documentation convention — should not warn, got: {warnings:?}",
    );
}

/// BT-1872: When no non-nil union members understand the selector, the DNU
/// diagnostic should be a warning (definite runtime failure).
#[test]
fn union_dnu_all_missing_emits_warning() {
    // Neither Integer nor Float understands `size`.
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("size".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "x",
        InferredType::Union {
            members: vec![InferredType::known("Integer"), InferredType::known("Float")],
            provenance: super::super::TypeProvenance::Inferred(span()),
        },
    );

    let _ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("not understand"))
        .collect();
    assert_eq!(
        dnu_diags.len(),
        1,
        "Expected exactly one DNU diagnostic, got {dnu_diags:?}"
    );
    assert_eq!(
        dnu_diags[0].severity,
        crate::source_analysis::Severity::Warning,
        "DNU when no members respond should be Warning severity"
    );
}

/// BT-1872: When some but not all non-nil union members understand the
/// selector, the DNU diagnostic should remain a hint.
#[test]
fn union_dnu_partial_missing_emits_hint() {
    // String understands `size`, Integer does not.
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("size".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["String", "Integer"]));

    let _ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("not understand"))
        .collect();
    assert_eq!(
        dnu_diags.len(),
        1,
        "Expected exactly one DNU diagnostic, got {dnu_diags:?}"
    );
    assert_eq!(
        dnu_diags[0].severity,
        crate::source_analysis::Severity::Hint,
        "DNU when some members respond should be Hint severity"
    );
}

// ---- BT-1882: flatten and partition: type annotations ----

/// BT-1882: List(List(Integer)) flatten returns List (not List(List(Integer))).
#[test]
fn generic_list_flatten_returns_unparameterized_list() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Variable typed as List(List(Integer)) — a nested list
    env.set_local(
        "nested",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::Known {
                class_name: eco_string("List"),
                type_args: vec![InferredType::known("Integer")],
                provenance: TypeProvenance::Inferred(span()),
            }],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("nested"),
            MessageSelector::Unary("flatten".into()),
            vec![],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("List"),
        "List(List(Integer)) flatten should return List, got: {result:?}"
    );
}

/// BT-1882: List(Integer) partition: returns Dictionary (not List(List(Integer))).
#[test]
fn generic_list_partition_returns_dictionary() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set_local(
        "nums",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let block = Expression::Block(Block::new(
        vec![crate::ast::BlockParameter::new("x", span())],
        vec![bare(msg_send(
            var("x"),
            MessageSelector::Unary("isEven".into()),
            vec![],
        ))],
        span(),
    ));

    let result = checker.infer_expr(
        &msg_send(
            var("nums"),
            MessageSelector::Keyword(vec![KeywordPart::new("partition:", span())]),
            vec![block],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Dictionary"),
        "List(Integer) partition: should return Dictionary, got: {result:?}"
    );
}

// ── CoverageReport tests (BT-1915) ─────────────────────────────────

#[test]
fn coverage_report_from_module_counts_typed_vs_dynamic() {
    let source = r"
Object subclass: Counter
  state: count :: Integer = 0
  increment => self.count := self.count + 1
  value => self.count
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _diags) = crate::source_analysis::parse(tokens);

    let hierarchy = ClassHierarchy::with_builtins();
    let type_map = infer_types(&module, &hierarchy);

    let report = CoverageReport::from_module(&module, &type_map, "test.bt", false);

    assert_eq!(report.classes.len(), 1, "should have one class");
    assert_eq!(report.classes[0].name.as_str(), "Counter");
    assert!(report.total_expressions > 0, "should have some expressions");
    assert!(
        report.typed_expressions <= report.total_expressions,
        "typed <= total"
    );
    assert!(
        report.dynamic_entries.is_empty(),
        "no detail entries without detail mode"
    );
}

#[test]
fn coverage_report_detail_mode_collects_dynamic_entries() {
    let source = r"
Object subclass: Greeter
  greet: name => name hello
";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _diags) = crate::source_analysis::parse(tokens);

    let hierarchy = ClassHierarchy::with_builtins();
    let type_map = infer_types(&module, &hierarchy);

    let report = CoverageReport::from_module(&module, &type_map, "greeter.bt", true);

    assert_eq!(report.classes.len(), 1);
    // With detail mode, any Dynamic entries should be collected
    // (name parameter is untyped -> Dynamic)
    let dynamic_count = report.total_expressions - report.typed_expressions;
    assert_eq!(
        report.dynamic_entries.len(),
        dynamic_count,
        "detail mode should collect all dynamic entries"
    );
}

#[test]
fn coverage_report_empty_module() {
    let module = make_module(vec![]);
    let hierarchy = ClassHierarchy::with_builtins();
    let type_map = infer_types(&module, &hierarchy);

    let report = CoverageReport::from_module(&module, &type_map, "empty.bt", false);

    assert!(report.classes.is_empty());
    assert_eq!(report.total_expressions, 0);
    assert_eq!(report.typed_expressions, 0);
    // 100% coverage for empty module (nothing to cover)
    assert!((report.coverage_percent() - 100.0).abs() < f64::EPSILON);
}

#[test]
fn coverage_report_merge() {
    let mut report_a = CoverageReport {
        classes: vec![ClassCoverage {
            name: "A".into(),
            file: "a.bt".to_string(),
            total: 10,
            typed: 8,
        }],
        dynamic_entries: vec![],
        total_expressions: 10,
        typed_expressions: 8,
    };
    let report_b = CoverageReport {
        classes: vec![ClassCoverage {
            name: "B".into(),
            file: "b.bt".to_string(),
            total: 5,
            typed: 5,
        }],
        dynamic_entries: vec![],
        total_expressions: 5,
        typed_expressions: 5,
    };

    report_a.merge(report_b);

    assert_eq!(report_a.classes.len(), 2);
    assert_eq!(report_a.total_expressions, 15);
    assert_eq!(report_a.typed_expressions, 13);
}

#[test]
fn coverage_percent_calculation() {
    let coverage = ClassCoverage {
        name: "Test".into(),
        file: "test.bt".to_string(),
        total: 100,
        typed: 75,
    };
    assert!((coverage.coverage_percent() - 75.0).abs() < f64::EPSILON);

    let empty = ClassCoverage {
        name: "Empty".into(),
        file: "empty.bt".to_string(),
        total: 0,
        typed: 0,
    };
    assert!((empty.coverage_percent() - 100.0).abs() < f64::EPSILON);
}
