// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Dynamic inference warnings in typed classes and block-param type propagation (BT-1914).

use super::common::*;

// ---- BT-1914: Dynamic inference warnings in typed classes ----

#[test]
fn test_dynamic_inference_warning_in_typed_class() {
    // BT-1914: unannotated parameter in typed class should produce a warning
    // when referenced in the method body.
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("process:", span())]),
            vec![ParameterDefinition::new(ident("handler"))], // no type annotation
            // Body references `handler` — this is an identifier expression
            // that infers as Dynamic(UnannotatedParam)
            vec![bare(Expression::Identifier(ident("handler")))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        !dynamic_warnings.is_empty(),
        "typed class should warn about Dynamic inference from unannotated param"
    );
    assert!(
        dynamic_warnings[0].message.contains("StrictCounter"),
        "warning should mention the class name: {:?}",
        dynamic_warnings[0].message
    );
    assert!(
        dynamic_warnings[0]
            .message
            .contains("unannotated parameter"),
        "warning should include the DynamicReason description: {:?}",
        dynamic_warnings[0].message
    );
    assert_eq!(
        dynamic_warnings[0].category,
        Some(DiagnosticCategory::Type),
        "warning should have Type category"
    );
}

#[test]
fn test_no_dynamic_inference_warning_in_untyped_class() {
    // BT-1914: non-typed class should NOT warn about Dynamic inference
    let class_def = ClassDefinition::with_modifiers(
        ident("SimpleCounter"),
        Some(ident("Object")),
        ClassModifiers::default(), // NOT typed
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Keyword(vec![KeywordPart::new("process:", span())]),
            vec![ParameterDefinition::new(ident("handler"))],
            vec![bare(Expression::Identifier(ident("handler")))],
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "non-typed class should NOT warn about Dynamic inference, got: {dynamic_warnings:?}"
    );
}

#[test]
fn test_no_dynamic_inference_warning_for_known_types() {
    // BT-1914: expressions with known types in typed class should NOT warn
    let class_def = ClassDefinition::with_modifiers(
        ident("StrictCounter"),
        Some(ident("Object")),
        ClassModifiers {
            is_typed: true,
            ..Default::default()
        },
        vec![],
        vec![MethodDefinition::new(
            MessageSelector::Unary("getValue".into()),
            vec![],
            vec![bare(int_lit(42))], // Integer literal — known type
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "known type should NOT produce Dynamic inference warning, got: {dynamic_warnings:?}"
    );
}

#[test]
fn test_expect_type_suppresses_dynamic_inference_warning() {
    // BT-1914: @expect type should suppress Dynamic inference warnings.
    // Uses parse_source for real spans so apply_expect_directives can match.
    let source =
        "typed Object subclass: Processor\n  process: handler =>\n    @expect type\n    handler";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "@expect type should suppress Dynamic inference warning, got: {dynamic_warnings:?}"
    );
    // No stale @expect warning
    let stale: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("stale"))
        .collect();
    assert!(
        stale.is_empty(),
        "should not produce stale @expect warning, got: {stale:?}"
    );
}

#[test]
fn test_expect_type_stale_when_no_dynamic_warning() {
    // BT-1914: @expect type on a fully-typed expression in a typed class
    // should produce a stale @expect warning.
    let source = "typed Object subclass: Processor\n  getValue =>\n    @expect type\n    42";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let stale: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("stale"))
        .collect();
    assert!(
        !stale.is_empty(),
        "@expect type on `42` in typed class should produce stale warning, got diags: {diags:?}"
    );
}

#[test]
fn dynamic_reason_descriptions() {
    assert_eq!(
        DynamicReason::UnannotatedParam.description(),
        Some("unannotated parameter")
    );
    assert_eq!(
        DynamicReason::DynamicReceiver.description(),
        Some("dynamic receiver")
    );
    assert_eq!(DynamicReason::UntypedFfi.description(), Some("untyped FFI"));
    assert_eq!(
        DynamicReason::DynamicSpec.description(),
        Some("FFI spec is Dynamic")
    );
    assert_eq!(DynamicReason::Unknown.description(), None);
}

// ---- block param type propagation from method signatures ----

#[test]
fn block_params_typed_from_sort_on_parameterized_list() {
    // List(String)>>sort: declares Block(E, E, Boolean) — block params should get String.
    let source = "typed Object subclass: T\n  field: items :: List(String)\n  m -> List(String) => self.items sort: [:a :b | a < b]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "sort: block params should be typed from List(String), got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_typed_from_any_satisfy_on_parameterized_list() {
    // List(Integer)>>anySatisfy: declares Block(E, Boolean) — block param should get Integer.
    let source = "typed Object subclass: T\n  field: nums :: List(Integer)\n  m -> Boolean => self.nums anySatisfy: [:n | n > 0]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "anySatisfy: block param should be typed from List(Integer), got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_typed_from_inject_into() {
    // List(Integer)>>inject:into: declares [A, Block(A, E, A)] — acc gets type from first arg.
    let source = "typed Object subclass: T\n  field: nums :: List(Integer)\n  m -> Integer => self.nums inject: 0 into: [:acc :n | acc + n]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "inject:into: block params should be typed, got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_typed_from_collect_on_parameterized_array() {
    // Array(Integer)>>collect: declares Block(E, R) — block param should get Integer.
    let source = "typed Object subclass: T\n  field: nums :: Array(Integer)\n  m -> Array => self.nums collect: [:n | n + 1]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "collect: block param should be typed from Array(Integer), got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_remain_dynamic_for_unparameterized_list() {
    // List>>sort: with unparameterized List — E can't be resolved, params stay Dynamic.
    let source = "typed Object subclass: T\n  field: items :: List\n  m -> List => self.items sort: [:a :b | a < b]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        !dynamic_warnings.is_empty(),
        "unparameterized List should still produce Dynamic warnings for block params"
    );
}

#[test]
fn resolve_type_name_string_parametric() {
    // List(String) should parse to Known("List") with type_args [Known("String")]
    let result = TypeChecker::resolve_type_name_string(&"List(String)".into());
    match &result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "List");
            assert_eq!(type_args.len(), 1);
            assert_eq!(type_args[0], InferredType::known("String"));
        }
        other => panic!("Expected Known with type_args, got {other:?}"),
    }
}

#[test]
fn resolve_type_name_string_nested_parametric() {
    // Result(List(Integer), String) should parse correctly
    let result = TypeChecker::resolve_type_name_string(&"Result(List(Integer), String)".into());
    match &result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Result");
            assert_eq!(type_args.len(), 2);
            // First arg: List(Integer)
            match &type_args[0] {
                InferredType::Known {
                    class_name,
                    type_args: inner,
                    ..
                } => {
                    assert_eq!(class_name.as_str(), "List");
                    assert_eq!(inner.len(), 1);
                    assert_eq!(inner[0], InferredType::known("Integer"));
                }
                other => panic!("Expected Known(List(Integer)), got {other:?}"),
            }
            assert_eq!(type_args[1], InferredType::known("String"));
        }
        other => panic!("Expected Known with type_args, got {other:?}"),
    }
}

#[test]
fn block_params_fewer_than_signature_no_crash() {
    // List(String)>>sort: expects Block(E, E, Boolean) — 2 block params.
    // User passes a 1-param block. Extra resolved types are silently dropped.
    let source = "typed Object subclass: T\n  field: items :: List(String)\n  m -> List(String) => self.items sort: [:a | a]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // Should not crash, and the one param should be typed (no Dynamic warning for it)
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "single block param should still be typed from List(String), got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_more_than_signature_extra_stays_dynamic() {
    // List(Integer)>>anySatisfy: expects Block(E, Boolean) — 1 block param.
    // User passes a 2-param block. Extra param stays Dynamic.
    let source = "typed Object subclass: T\n  field: nums :: List(Integer)\n  m -> Boolean => self.nums anySatisfy: [:n :extra | n > 0]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    // The first param (n) is used and should still be typed from List(Integer),
    // so no Dynamic inference warning should be emitted for this block.
    let dynamic_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "used block param should still be typed when extra params are present, got: {dynamic_warnings:?}"
    );
}

#[test]
fn block_params_typed_via_method_parameter_annotation() {
    // Method parameter `items :: List(Dictionary)` — sort: block params should get Dictionary.
    let source = "typed Object subclass: T\n  m: items :: List(Dictionary) -> List(Dictionary) => items sort: [:a :b | (a at: \"x\" ifAbsent: [0]) < (b at: \"x\" ifAbsent: [0])]";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "block params should be typed from method param List(Dictionary), got: {dynamic_warnings:?}"
    );
}

#[test]
fn split_union_respecting_parens_simple() {
    let result = TypeChecker::split_union_respecting_parens("String | nil");
    assert_eq!(result, vec!["String", "nil"]);
}

#[test]
fn split_union_respecting_parens_inside_parametric() {
    // Pipe inside parens should NOT cause a split
    let result = TypeChecker::split_union_respecting_parens("Result(String | Integer, Error)");
    assert_eq!(result, vec!["Result(String | Integer, Error)"]);
}

#[test]
fn split_union_respecting_parens_mixed() {
    // Top-level union with parametric member
    let result = TypeChecker::split_union_respecting_parens("List(String) | nil");
    assert_eq!(result, vec!["List(String)", "nil"]);
}

#[test]
fn block_params_typed_in_cascade_sends() {
    // Cascade sends should also propagate block param types.
    let source = "typed Object subclass: T\n  field: nums :: List(Integer)\n  m -> List(Integer) =>\n    self.nums\n      sort: [:a :b | a < b];\n      yourself";
    let module = parse_source(source);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let diags = run_with_expect(&module, &hierarchy);
    let dynamic_warnings: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("expression inferred as Dynamic"))
        .collect();
    assert!(
        dynamic_warnings.is_empty(),
        "cascade sort: block params should be typed from List(Integer), got: {dynamic_warnings:?}"
    );
}

// ── BT-1945: Never bottom type ──────────────────────────────────────────────

#[test]
fn never_union_identity() {
    // T | Never = T
    let result = InferredType::union_of(&[InferredType::known("Integer"), InferredType::Never]);
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn never_union_identity_reversed() {
    // Never | T = T
    let result = InferredType::union_of(&[InferredType::Never, InferredType::known("String")]);
    assert_eq!(result, InferredType::known("String"));
}

#[test]
fn never_union_multiple() {
    // T | Never | U = T | U
    let result = InferredType::union_of(&[
        InferredType::known("Integer"),
        InferredType::Never,
        InferredType::known("String"),
    ]);
    assert_eq!(result, InferredType::simple_union(&["Integer", "String"]));
}

#[test]
fn never_union_all_never() {
    // Never | Never = Never
    let result = InferredType::union_of(&[InferredType::Never, InferredType::Never]);
    assert_eq!(result, InferredType::Never);
}

#[test]
fn never_display_name() {
    assert_eq!(InferredType::Never.display_name(), Some("Never".into()));
}

#[test]
fn never_equality() {
    assert_eq!(InferredType::Never, InferredType::Never);
    assert_ne!(InferredType::Never, InferredType::known("Never"));
    assert_ne!(
        InferredType::Never,
        InferredType::Dynamic(DynamicReason::Unknown)
    );
}

#[test]
fn never_as_known_is_none() {
    assert!(InferredType::Never.as_known().is_none());
}

#[test]
fn resolve_type_annotation_never() {
    let ann = TypeAnnotation::Simple(ident("Never"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::Never);
}
