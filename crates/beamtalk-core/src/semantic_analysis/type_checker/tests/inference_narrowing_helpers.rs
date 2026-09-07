// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `detect_narrowing`, `non_nil_type`, `extract_variable_name`, and
//! `block_has_return` inference helpers.

use super::common::*;
use crate::semantic_analysis::type_checker::narrowing::extract::extract_variable_name;
use crate::semantic_analysis::type_checker::narrowing::visitors::block_has_return;
use crate::semantic_analysis::type_checker::narrowing::{ClassTestKind, NarrowingInfo};

// ---- detect_narrowing ----

#[test]
fn detect_narrowing_is_nil() {
    // x isNil
    let expr = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Unary("isNil".into()),
        arguments: vec![],
        is_cast: false,
        span: span(),
    };
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect isNil");
    assert_eq!(info.variable, EnvKey::local("x"));
    assert_eq!(info.true_type, InferredType::known("UndefinedObject"));
    assert!(info.is_nil_check);
    assert!(info.responded_selector.is_none());
}

#[test]
fn detect_narrowing_is_kind_of() {
    // x isKindOf: Integer
    let expr = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "isKindOf:".into(),
            span: span(),
        }]),
        arguments: vec![class_ref("Integer")],
        is_cast: false,
        span: span(),
    };
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect isKindOf:");
    assert_eq!(info.variable, EnvKey::local("x"));
    // ADR 0102 §2 group 2: `detect` leaves `true_type` provisional and
    // records the tested class in `class_test`; `refine_class_narrowing`
    // resolves the actual narrowed type later.
    assert_eq!(
        info.true_type,
        InferredType::Dynamic(DynamicReason::Unknown)
    );
    let class_test = info.class_test.expect("should record class_test");
    assert_eq!(class_test.class_name, "Integer");
    assert_eq!(class_test.kind, ClassTestKind::KindOf);
    assert!(!info.is_nil_check);
}

#[test]
fn detect_narrowing_class_identity_eq() {
    // x class =:= Float
    let class_send = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Unary("class".into()),
        arguments: vec![],
        is_cast: false,
        span: span(),
    };
    let expr = Expression::MessageSend {
        receiver: Box::new(class_send),
        selector: MessageSelector::Binary("=:=".into()),
        arguments: vec![class_ref("Float")],
        is_cast: false,
        span: span(),
    };
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect class =:=");
    assert_eq!(info.variable, EnvKey::local("x"));
    assert_eq!(
        info.true_type,
        InferredType::Dynamic(DynamicReason::Unknown)
    );
    let class_test = info.class_test.expect("should record class_test");
    assert_eq!(class_test.class_name, "Float");
    assert_eq!(class_test.kind, ClassTestKind::Exact);
}

#[test]
fn detect_narrowing_responds_to() {
    // x respondsTo: #doSomething
    let expr = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "respondsTo:".into(),
            span: span(),
        }]),
        arguments: vec![Expression::Literal(
            Literal::Symbol("doSomething".into()),
            span(),
        )],
        is_cast: false,
        span: span(),
    };
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect respondsTo:");
    assert_eq!(info.variable, EnvKey::local("x"));
    assert_eq!(
        info.true_type,
        InferredType::Dynamic(DynamicReason::Unknown)
    );
    assert!(!info.is_nil_check);
    assert_eq!(info.responded_selector.as_deref(), Some("doSomething"));
}

#[test]
fn detect_narrowing_responds_to_non_symbol() {
    // x respondsTo: someVar (not a symbol literal — should not match)
    let expr = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "respondsTo:".into(),
            span: span(),
        }]),
        arguments: vec![var("someVar")],
        is_cast: false,
        span: span(),
    };
    assert!(TypeChecker::detect_narrowing(&expr).is_none());
}

#[test]
fn detect_narrowing_no_match() {
    // x size (not a type-testing pattern)
    let expr = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Unary("size".into()),
        arguments: vec![],
        is_cast: false,
        span: span(),
    };
    assert!(TypeChecker::detect_narrowing(&expr).is_none());
}

#[test]
fn detect_narrowing_parenthesized_class_eq() {
    // (x class) =:= Integer
    let class_send = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Unary("class".into()),
        arguments: vec![],
        is_cast: false,
        span: span(),
    };
    let parens = Expression::Parenthesized {
        expression: Box::new(class_send),
        span: span(),
    };
    let expr = Expression::MessageSend {
        receiver: Box::new(parens),
        selector: MessageSelector::Binary("=:=".into()),
        arguments: vec![class_ref("Integer")],
        is_cast: false,
        span: span(),
    };
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect (x class) =:= Type");
    assert_eq!(info.variable, EnvKey::local("x"));
    assert_eq!(
        info.true_type,
        InferredType::Dynamic(DynamicReason::Unknown)
    );
    let class_test = info.class_test.expect("should record class_test");
    assert_eq!(class_test.class_name, "Integer");
    assert_eq!(class_test.kind, ClassTestKind::Exact);
}

// ---- detect_narrowing: isOk / ok / isError (BT-1859) ----

#[test]
fn detect_narrowing_is_ok() {
    // x isOk
    let expr = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Unary("isOk".into()),
        arguments: vec![],
        is_cast: false,
        span: span(),
    };
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect isOk");
    assert_eq!(info.variable, EnvKey::local("x"));
    assert!(info.is_result_ok_check);
    assert!(!info.is_result_error_check);
    assert!(!info.is_nil_check);
}

#[test]
fn detect_narrowing_ok() {
    // x ok
    let expr = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Unary("ok".into()),
        arguments: vec![],
        is_cast: false,
        span: span(),
    };
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect ok");
    assert_eq!(info.variable, EnvKey::local("x"));
    assert!(info.is_result_ok_check);
    assert!(!info.is_result_error_check);
}

#[test]
fn detect_narrowing_is_error() {
    // x isError
    let expr = Expression::MessageSend {
        receiver: Box::new(var("x")),
        selector: MessageSelector::Unary("isError".into()),
        arguments: vec![],
        is_cast: false,
        span: span(),
    };
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect isError");
    assert_eq!(info.variable, EnvKey::local("x"));
    assert!(!info.is_result_ok_check);
    assert!(info.is_result_error_check);
    assert!(!info.is_nil_check);
}

#[test]
fn refine_result_narrowing_with_result_type() {
    // When the variable has type Result(String, Error), refine should set
    // true_type and false_type to the full Result type.
    let mut env = TypeEnv::new();
    let result_ty = InferredType::Known {
        class_name: "Result".into(),
        type_args: vec![InferredType::known("String"), InferredType::known("Error")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    env.set_local("r", result_ty.clone());

    let info = NarrowingInfo {
        variable: EnvKey::local("r"),
        true_type: InferredType::Dynamic(DynamicReason::Unknown),
        false_type: None,
        is_nil_check: false,
        is_result_ok_check: true,
        is_result_error_check: false,
        responded_selector: None,
        singleton_eq: None,
        class_test: None,
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let refined = TypeChecker::refine_result_narrowing(info, &env, &hierarchy, None);
    assert_eq!(refined.true_type, result_ty);
    assert_eq!(refined.false_type, Some(result_ty));
    assert!(refined.is_result_ok_check);
}

#[test]
fn refine_result_narrowing_non_result_disables() {
    // When the variable is not a Result, the result flags are cleared.
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::known("Integer"));

    let info = NarrowingInfo {
        variable: EnvKey::local("x"),
        true_type: InferredType::Dynamic(DynamicReason::Unknown),
        false_type: None,
        is_nil_check: false,
        is_result_ok_check: true,
        is_result_error_check: false,
        responded_selector: None,
        singleton_eq: None,
        class_test: None,
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let refined = TypeChecker::refine_result_narrowing(info, &env, &hierarchy, None);
    assert!(!refined.is_result_ok_check);
    assert!(!refined.is_result_error_check);
    assert!(refined.false_type.is_none());
    // true_type should be preserved as the current type, not Dynamic
    assert_eq!(refined.true_type, InferredType::known("Integer"));
}

// ---- non_nil_type ----

#[test]
fn non_nil_type_removes_nil_from_union() {
    let ty = InferredType::simple_union(&["String", "nil"]);
    let result = TypeChecker::non_nil_type(&ty);
    assert_eq!(result, InferredType::known("String"));
}

#[test]
fn non_nil_type_preserves_non_nil_union() {
    let ty = InferredType::simple_union(&["String", "Integer"]);
    let result = TypeChecker::non_nil_type(&ty);
    assert_eq!(result, ty);
}

#[test]
fn non_nil_type_all_nil_becomes_dynamic() {
    // union_of with single member returns the member itself, so build manually
    let ty = InferredType::Union {
        members: vec![InferredType::known("UndefinedObject")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let result = TypeChecker::non_nil_type(&ty);
    assert_eq!(result, InferredType::Dynamic(DynamicReason::Unknown));
}

#[test]
fn non_nil_type_known_type_unchanged() {
    let ty = InferredType::known("Integer");
    let result = TypeChecker::non_nil_type(&ty);
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn non_nil_type_dynamic_unchanged() {
    let result = TypeChecker::non_nil_type(&InferredType::Dynamic(DynamicReason::Unknown));
    assert_eq!(result, InferredType::Dynamic(DynamicReason::Unknown));
}

#[test]
fn non_nil_type_three_member_union() {
    let ty = InferredType::simple_union(&["String", "Integer", "nil"]);
    let result = TypeChecker::non_nil_type(&ty);
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert!(members.contains(&InferredType::known("String")));
            assert!(members.contains(&InferredType::known("Integer")));
        }
        other => panic!("Expected Union, got {other:?}"),
    }
}

// ---- extract_variable_name ----

#[test]
fn extract_variable_name_from_ident() {
    let expr = var("foo");
    assert_eq!(extract_variable_name(&expr), Some(EnvKey::local("foo")));
}

#[test]
fn extract_variable_name_from_parenthesized() {
    let expr = Expression::Parenthesized {
        expression: Box::new(var("bar")),
        span: span(),
    };
    assert_eq!(extract_variable_name(&expr), Some(EnvKey::local("bar")));
}

#[test]
fn extract_variable_name_from_non_ident() {
    let expr = int_lit(42);
    assert!(extract_variable_name(&expr).is_none());
}

#[test]
fn extract_variable_name_from_self_field() {
    // BT-2048 / BT-2062: self.supervisor → EnvKey::SelfField("supervisor")
    let expr = Expression::FieldAccess {
        receiver: Box::new(var("self")),
        field: ident("supervisor"),
        span: span(),
    };
    assert_eq!(
        extract_variable_name(&expr),
        Some(EnvKey::self_field("supervisor")),
    );
}

#[test]
fn extract_variable_name_from_non_self_field() {
    // other.field → None (only self.field is supported)
    let expr = Expression::FieldAccess {
        receiver: Box::new(var("other")),
        field: ident("value"),
        span: span(),
    };
    assert!(extract_variable_name(&expr).is_none());
}

// ---- detect_narrowing: self.field isNil (BT-2048) ----

#[test]
fn detect_narrowing_self_field_is_nil() {
    // self.supervisor isNil
    let field_access = Expression::FieldAccess {
        receiver: Box::new(var("self")),
        field: ident("supervisor"),
        span: span(),
    };
    let expr = Expression::MessageSend {
        receiver: Box::new(field_access),
        selector: MessageSelector::Unary("isNil".into()),
        arguments: vec![],
        is_cast: false,
        span: span(),
    };
    let info = TypeChecker::detect_narrowing(&expr).expect("should detect self.field isNil");
    assert_eq!(info.variable, EnvKey::self_field("supervisor"));
    assert_eq!(info.true_type, InferredType::known("UndefinedObject"));
    assert!(info.is_nil_check);
}

// ---- block_has_return ----

#[test]
fn block_has_return_true() {
    let block = Block::new(
        vec![],
        vec![ExpressionStatement::bare(Expression::Return {
            value: Box::new(int_lit(1)),
            span: span(),
        })],
        span(),
    );
    assert!(block_has_return(&block));
}

#[test]
fn block_has_return_false() {
    let block = Block::new(vec![], vec![ExpressionStatement::bare(int_lit(42))], span());
    assert!(!block_has_return(&block));
}

#[test]
fn block_has_return_empty() {
    let block = Block::new(vec![], vec![], span());
    assert!(!block_has_return(&block));
}
