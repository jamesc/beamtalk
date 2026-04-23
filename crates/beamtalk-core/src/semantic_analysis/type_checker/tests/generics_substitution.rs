// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generic substitution and constructor type inference (BT-1570, BT-1571).

use super::super::*;
use super::common::*;

// ---- BT-1570: Generic substitution tests ----

/// BT-1570: Substitution map built from `GenResult(Integer, IOError)`.
/// `unwrap` returns `T` → `Integer`.
#[test]
fn generic_substitution_unwrap_returns_concrete_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "r",
        InferredType::Known {
            class_name: eco_string("GenResult"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("IOError"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("unwrap".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "unwrap on GenResult(Integer, IOError) should return Integer, got: {result_ty:?}"
    );
}

/// BT-1570: `error` returns `E` → `IOError`.
#[test]
fn generic_substitution_error_returns_concrete_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "r",
        InferredType::Known {
            class_name: eco_string("GenResult"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("IOError"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("error".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("IOError"),
        "error on GenResult(Integer, IOError) should return IOError, got: {result_ty:?}"
    );
}

/// BT-1570: Non-generic return type (`isOk` -> `Boolean`) is unaffected.
#[test]
fn generic_substitution_non_generic_return_unchanged() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "r",
        InferredType::Known {
            class_name: eco_string("GenResult"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("IOError"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("isOk".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Boolean"),
        "isOk on GenResult should return Boolean regardless of type args"
    );
}

/// BT-1570: No `type_args` on receiver falls back to unsubstituted return.
#[test]
fn generic_no_type_args_returns_unsubstituted() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    // Set r to bare GenResult (no type_args) — unparameterized
    env.set_local("r", InferredType::known("GenResult"));

    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("unwrap".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    // Without type_args, the return type "T" is returned as-is (known("T")).
    // It won't produce false-positive warnings because "T" is unknown to
    // the hierarchy, so all type compatibility checks return true.
    assert!(
        result_ty.as_known().map(EcoString::as_str) != Some("Integer"),
        "unwrap on bare GenResult (no type_args) should NOT return Integer"
    );
}

/// BT-1570: `set_param_types` handles generic annotations.
#[test]
fn set_param_types_resolves_generic_annotation() {
    // Parameter annotated as :: Result(Integer, Error) should be Known("Result")
    // with type_args
    let params = vec![ParameterDefinition {
        name: ident("r"),
        type_annotation: Some(TypeAnnotation::Generic {
            base: ident("Result"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Simple(ident("Error")),
            ],
            span: span(),
        }),
    }];

    let mut env = TypeEnv::new();
    TypeChecker::set_param_types(&mut env, &params);

    let r_type = env.get_local("r").expect("r should be in env");
    match r_type {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Result");
            assert_eq!(type_args.len(), 2);
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("Integer")
            );
            assert_eq!(
                type_args[1].as_known().map(EcoString::as_str),
                Some("Error")
            );
        }
        other => panic!("Expected Known type with type_args, got: {other:?}"),
    }
}

/// BT-1570: Generic return type check extracts base type for compatibility.
#[test]
fn check_return_type_handles_generic_declared_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    // Method declares -> GenResult(Integer, Error), body returns GenResult
    let method = MethodDefinition {
        selector: MessageSelector::Unary("compute".into()),
        parameters: vec![],
        body: vec![bare(int_lit(42))], // placeholder body
        return_type: Some(TypeAnnotation::Generic {
            base: ident("GenResult"),
            parameters: vec![
                TypeAnnotation::Simple(ident("Integer")),
                TypeAnnotation::Simple(ident("Error")),
            ],
            span: span(),
        }),
        kind: MethodKind::Primary,
        is_sealed: false,
        is_internal: false,
        span: span(),
        doc_comment: None,
        expect: None,
        comments: CommentAttachment::default(),
    };

    let body_type = InferredType::known("GenResult");
    let mut checker = TypeChecker::new();
    checker.check_return_type(&method, &body_type, &eco_string("MyClass"), &hierarchy);

    // Should NOT warn: GenResult is compatible with declared GenResult(Integer, Error)
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("return type"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "GenResult body should be compatible with GenResult(Integer, Error) return type, got: {type_warnings:?}"
    );
}

/// BT-1570: `is_assignable_to` handles generic declared types structurally.
#[test]
fn is_assignable_to_generic_declared_type() {
    let hierarchy = ClassHierarchy::with_builtins();

    // Result is assignable to Result(Integer, Error) — base type matches
    assert!(
        TypeChecker::is_assignable_to(
            &eco_string("Result"),
            &eco_string("Result(Integer, Error)"),
            &hierarchy,
        ),
        "Result should be assignable to Result(Integer, Error)"
    );

    // Integer is NOT assignable to Result(Integer, Error) — base types differ
    assert!(
        !TypeChecker::is_assignable_to(
            &eco_string("Integer"),
            &eco_string("Result(Integer, Error)"),
            &hierarchy,
        ),
        "Integer should NOT be assignable to Result(Integer, Error)"
    );
}

// ---- BT-1571: Constructor type inference tests ----

/// BT-1571: `GenResult ok: 42` infers T = Integer → GenResult(Integer, Dynamic).
#[test]
fn constructor_inference_ok_infers_t_from_integer() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // GenResult ok: 42
    let result_ty = checker.infer_expr(
        &msg_send(
            class_ref("GenResult"),
            MessageSelector::Keyword(vec![KeywordPart::new("ok:", span())]),
            vec![int_lit(42)],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    match &result_ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(
                class_name.as_str(),
                "GenResult",
                "Constructor should return GenResult"
            );
            assert_eq!(type_args.len(), 2, "Should have 2 type args (T, E)");
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("Integer"),
                "T should be inferred as Integer from argument"
            );
            assert!(
                matches!(type_args[1], InferredType::Dynamic(DynamicReason::Unknown)),
                "E should be Dynamic (not inferrable from ok:), got: {:?}",
                type_args[1]
            );
        }
        other => panic!("Expected Known type with type_args, got: {other:?}"),
    }
}

/// BT-1571: `GenResult error: #not_found` infers E = Symbol → GenResult(Dynamic, Symbol).
#[test]
fn constructor_inference_error_infers_e_from_symbol() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // GenResult error: #not_found
    let result_ty = checker.infer_expr(
        &msg_send(
            class_ref("GenResult"),
            MessageSelector::Keyword(vec![KeywordPart::new("error:", span())]),
            vec![Expression::Literal(
                Literal::Symbol("not_found".into()),
                span(),
            )],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    match &result_ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "GenResult");
            assert_eq!(type_args.len(), 2, "Should have 2 type args (T, E)");
            assert!(
                matches!(type_args[0], InferredType::Dynamic(DynamicReason::Unknown)),
                "T should be Dynamic (not inferrable from error:), got: {:?}",
                type_args[0]
            );
            assert_eq!(
                type_args[1].as_known().map(EcoString::as_str),
                Some("#not_found"),
                "E should be inferred as #not_found from symbol literal argument"
            );
        }
        other => panic!("Expected Known type with type_args, got: {other:?}"),
    }
}

/// BT-1571: Constructor inference on non-generic class returns plain Known.
#[test]
fn constructor_inference_non_generic_class_no_type_args() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Array new — Array is generic Array(E), so constructor with no args
    // produces Dynamic type_args since E cannot be inferred.
    let result_ty = checker.infer_expr(
        &msg_send(
            class_ref("Array"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    match &result_ty {
        InferredType::Known { class_name, .. } => {
            assert_eq!(class_name.as_str(), "Array");
        }
        other => panic!("Expected Known type, got: {other:?}"),
    }
}

/// BT-1571: Inferred type from constructor flows into subsequent instance sends.
/// `(GenResult ok: 42) unwrap` should return Integer via substitution.
#[test]
fn constructor_inference_flows_to_instance_sends() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // r := GenResult ok: 42
    let assign_expr = Expression::Assignment {
        target: Box::new(Expression::Identifier(ident("r"))),
        value: Box::new(msg_send(
            class_ref("GenResult"),
            MessageSelector::Keyword(vec![KeywordPart::new("ok:", span())]),
            vec![int_lit(42)],
        )),
        type_annotation: None,
        span: span(),
    };
    checker.infer_expr(&assign_expr, &hierarchy, &mut env, false);

    // r unwrap — should return Integer via T substitution
    let unwrap_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("unwrap".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        unwrap_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "unwrap on (GenResult ok: 42) should return Integer via constructor inference"
    );
}

/// BT-1571: Constructor inference for error: flows into error accessor.
/// `(GenResult error: #not_found) error` should return Symbol.
#[test]
fn constructor_inference_error_flows_to_error_accessor() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // r := GenResult error: #not_found
    let assign_expr = Expression::Assignment {
        target: Box::new(Expression::Identifier(ident("r"))),
        value: Box::new(msg_send(
            class_ref("GenResult"),
            MessageSelector::Keyword(vec![KeywordPart::new("error:", span())]),
            vec![Expression::Literal(
                Literal::Symbol("not_found".into()),
                span(),
            )],
        )),
        type_annotation: None,
        span: span(),
    };
    checker.infer_expr(&assign_expr, &hierarchy, &mut env, false);

    // r error — should return Symbol via E substitution
    let error_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("error".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        error_ty.as_known().map(EcoString::as_str),
        Some("#not_found"),
        "error on (GenResult error: #not_found) should return #not_found via constructor inference"
    );
}
