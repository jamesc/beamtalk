// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Union type display, resolution, and receiver dispatch.

use super::super::*;
use super::common::*;

// ── Union type tests (BT-1572) ──────────────────────────────────────────

#[test]
fn union_of_simplifies_single_type() {
    let result = InferredType::union_of(&[
        InferredType::known("Integer"),
        InferredType::known("Integer"),
    ]);
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn union_of_preserves_different_types() {
    let result = InferredType::union_of(&[
        InferredType::known("String"),
        InferredType::known("UndefinedObject"),
    ]);
    assert_eq!(
        result,
        InferredType::simple_union(&["String", "UndefinedObject"])
    );
}

#[test]
fn union_of_returns_dynamic_if_any_dynamic() {
    let result = InferredType::union_of(&[
        InferredType::known("String"),
        InferredType::Dynamic(DynamicReason::Unknown),
    ]);
    assert_eq!(result, InferredType::Dynamic(DynamicReason::Unknown));
}

#[test]
fn union_display_name() {
    let ty = InferredType::simple_union(&["String", "UndefinedObject"]);
    assert_eq!(ty.display_name(), Some("String | UndefinedObject".into()));
}

#[test]
fn known_display_name() {
    let ty = InferredType::known("Integer");
    assert_eq!(ty.display_name(), Some("Integer".into()));
}

#[test]
fn dynamic_display_name_unknown() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::Unknown).display_name(),
        Some(ecow::EcoString::from("Dynamic"))
    );
}

#[test]
fn dynamic_display_name_unannotated_param() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::UnannotatedParam).display_name(),
        Some(ecow::EcoString::from("Dynamic (unannotated parameter)"))
    );
}

#[test]
fn dynamic_display_name_unannotated_return() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::UnannotatedReturn).display_name(),
        Some(ecow::EcoString::from("Dynamic (unannotated return)"))
    );
}

#[test]
fn dynamic_display_name_dynamic_receiver() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::DynamicReceiver).display_name(),
        Some(ecow::EcoString::from("Dynamic (dynamic receiver)"))
    );
}

#[test]
fn dynamic_display_name_ambiguous_control_flow() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::AmbiguousControlFlow).display_name(),
        Some(ecow::EcoString::from("Dynamic (ambiguous control flow)"))
    );
}

#[test]
fn dynamic_display_name_untyped_ffi() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::UntypedFfi).display_name(),
        Some(ecow::EcoString::from("Dynamic (untyped FFI)"))
    );
}

#[test]
fn dynamic_display_name_dynamic_spec() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::DynamicSpec).display_name(),
        Some(ecow::EcoString::from("Dynamic (FFI spec is Dynamic)"))
    );
}

#[test]
fn resolve_type_annotation_simple() {
    let ann = TypeAnnotation::Simple(ident("Integer"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn resolve_type_annotation_nil_keyword() {
    let ann = TypeAnnotation::Simple(ident("nil"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::known("UndefinedObject"));
}

#[test]
fn resolve_type_annotation_false_keyword() {
    let ann = TypeAnnotation::Simple(ident("false"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::known("False"));
}

#[test]
fn resolve_type_annotation_true_keyword() {
    let ann = TypeAnnotation::Simple(ident("true"));
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::known("True"));
}

#[test]
fn resolve_type_annotation_union() {
    let ann = TypeAnnotation::Union {
        types: vec![
            TypeAnnotation::Simple(ident("String")),
            TypeAnnotation::Simple(ident("nil")),
        ],
        span: span(),
    };
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(
        result,
        InferredType::simple_union(&["String", "UndefinedObject"])
    );
}

#[test]
fn resolve_type_annotation_false_or() {
    let ann = TypeAnnotation::FalseOr {
        inner: Box::new(TypeAnnotation::Simple(ident("Integer"))),
        span: span(),
    };
    let result = TypeChecker::resolve_type_annotation(&ann);
    assert_eq!(result, InferredType::simple_union(&["Integer", "False"]));
}

#[test]
fn resolve_type_name_string_simple() {
    let result = TypeChecker::resolve_type_name_string(&"Integer".into());
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn resolve_type_name_string_union() {
    let result = TypeChecker::resolve_type_name_string(&"String | nil".into());
    assert_eq!(
        result,
        InferredType::simple_union(&["String", "UndefinedObject"])
    );
}

/// BT-1572 + BT-1857: Message send on union receiver warns if non-Nil member lacks the selector.
#[test]
fn union_receiver_warns_when_non_nil_member_lacks_selector() {
    // String understands `trim` but Integer does not.
    // Sending `trim` to a `String | Integer` receiver should emit a DNU hint for Integer.
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("trim".into()),
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

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu_hints.len(),
        1,
        "Should warn that Integer does not understand 'trim'"
    );
    assert!(
        dnu_hints[0].message.contains("Integer"),
        "Warning should mention which member lacks the selector"
    );
}

/// BT-1857: Nil members in unions are skipped for method resolution.
/// String | Nil sending `size` should not warn — Nil is skipped.
#[test]
fn union_receiver_nil_skipped_no_warning() {
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
        InferredType::simple_union(&["String", "UndefinedObject"]),
    );

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "Should NOT warn when Nil is the only non-responding member (it's skipped)"
    );

    // BT-2017: Return type should be Integer from String.size.
    // UndefinedObject does NOT respond to `size`, so no nil widening
    // (BT-1857 only widens when UndefinedObject responds to the selector).
    assert_eq!(
        ty,
        InferredType::known("Integer"),
        "Return type should be Integer from String>>size (no nil widening \
         since UndefinedObject doesn't respond to size)"
    );
}

/// BT-1857: Nullable hint still appears when non-Nil member also lacks selector.
#[test]
fn union_receiver_nullable_hint_with_non_nil_missing() {
    // Float | Nil — Float doesn't understand `size`, and union has Nil.
    // Should warn about Float AND add nullable hint.
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
        InferredType::simple_union(&["Float", "UndefinedObject"]),
    );

    checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(dnu_hints.len(), 1, "Should warn about Float");
    assert!(
        dnu_hints[0].message.contains("Float"),
        "Warning should mention Float, not UndefinedObject"
    );
    // BT-1857: UndefinedObject should NOT appear as a non-responding member.
    // Extract the subject (text before "does not understand") and verify it
    // does not mention UndefinedObject.
    let subject = dnu_hints[0]
        .message
        .split("does not understand")
        .next()
        .unwrap_or("");
    assert!(
        !subject.contains("UndefinedObject"),
        "UndefinedObject should not be listed as a non-responding member, got subject: {subject:?}"
    );
}

/// BT-1572: No warning when all union members understand the selector.
#[test]
fn union_receiver_no_warning_when_all_understand() {
    // Both Integer and String understand `asString` (via Object hierarchy).
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("asString".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["Integer", "String"]));

    checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "No warning when all union members understand the selector"
    );
}

/// BT-1857: Dynamic member in union is handled conservatively (no warning).
#[test]
fn union_receiver_dynamic_member_no_warning() {
    // If a union member is Dynamic, we can't know what it responds to.
    // Should not warn — conservative handling.
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("fooBar".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    // Union with a Dynamic member — should fall through to Dynamic result
    env.set_local(
        "x",
        InferredType::Union {
            members: vec![
                InferredType::known("String"),
                InferredType::Dynamic(DynamicReason::Unknown),
            ],
            provenance: super::super::TypeProvenance::Inferred(span()),
        },
    );

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "Should not warn when Dynamic is in the union (conservative)"
    );
    // Dynamic poisons the union → result should be Dynamic
    assert!(
        matches!(ty, InferredType::Dynamic(_)),
        "Return type should be Dynamic when union contains Dynamic, got {ty:?}"
    );
}

/// BT-1857: Return type is union of return types from all responding members.
#[test]
fn union_receiver_return_type_is_union_of_member_returns() {
    // Integer.asString -> String, Array.asString -> String.
    // Both understand `asString` and return String, so result is String (not a union).
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("asString".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["Integer", "String"]));

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    // Both return String, so union_of collapses to Known(String)
    assert!(
        matches!(ty, InferredType::Known { ref class_name, .. } if class_name == "String"),
        "Return type should be String when all members return String, got {ty:?}"
    );
}

/// BT-1857: Nil-only union (`UndefinedObject` alone) — no members to check, returns Dynamic.
#[test]
fn union_receiver_nil_only_returns_dynamic() {
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
    // Degenerate case: union of just Nil
    env.set_local(
        "x",
        InferredType::Union {
            members: vec![InferredType::known("UndefinedObject")],
            provenance: super::super::TypeProvenance::Inferred(span()),
        },
    );

    let _ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "Nil-only union should not produce DNU warnings"
    );
}

/// BT-1871: Non-responding union members should not widen return type with Dynamic.
/// `(String | Integer).size` — Integer doesn't understand `size`, but String does
/// and returns Integer. Result should be Integer, not Integer | Dynamic.
#[test]
fn union_receiver_non_responding_member_does_not_widen_return_type() {
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

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    // String.size returns Integer. Integer doesn't understand `size`.
    // BT-1871: The return type should be just Integer, NOT Integer | Dynamic.
    assert!(
        matches!(ty, InferredType::Known { ref class_name, .. } if class_name == "Integer"),
        "Return type should be Integer (not widened with Dynamic), got {ty:?}"
    );

    // Should still warn about Integer not understanding `size`
    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu_hints.len(),
        1,
        "Should warn that Integer does not understand 'size'"
    );
}

/// BT-1871: When NO union members respond, return Dynamic as fallback.
#[test]
fn union_receiver_no_members_respond_returns_dynamic() {
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("nonExistentMethod".into()),
            vec![],
        ))],
        span(),
    );

    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["String", "Integer"]));

    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );

    // Neither String nor Integer understand `nonExistentMethod`.
    // With no return types collected, union_of returns Dynamic as fallback.
    assert!(
        matches!(ty, InferredType::Dynamic(DynamicReason::Unknown)),
        "Return type should be Dynamic when no members respond, got {ty:?}"
    );
}

/// BT-1572: Integer | False (`FalseOr`) parameter resolution.
#[test]
fn false_or_param_resolves_to_union() {
    // A method with parameter `x :: Integer | False` should infer x as Union.
    let method = MethodDefinition {
        selector: MessageSelector::Keyword(vec![KeywordPart::new("check:", span())]),
        parameters: vec![ParameterDefinition::with_type(
            ident("x"),
            TypeAnnotation::FalseOr {
                inner: Box::new(TypeAnnotation::Simple(ident("Integer"))),
                span: span(),
            },
        )],
        body: vec![bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("isNil".into()),
            vec![],
        ))],
        return_type: None,
        is_sealed: false,
        is_internal: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: span(),
    };

    let class = ClassDefinition {
        name: ident("MyClass"),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![method],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let module = Module {
        classes: vec![class],
        method_definitions: vec![],
        protocols: vec![],
        expressions: vec![],
        span: span(),
        file_leading_comments: vec![],
        file_trailing_comments: vec![],
    };

    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // No DNU for isNil — both Integer and False understand it (via Object)
    let dnu_hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_hints.is_empty(),
        "FalseOr resolved as union; no DNU for universal messages: {dnu_hints:?}"
    );
}

/// BT-1572: `is_assignable_to` with union declared type (via `type_name` string).
#[test]
fn is_assignable_to_union_string() {
    let hierarchy = ClassHierarchy::with_builtins();
    // "String | nil" declared type should accept "String"
    assert!(
        TypeChecker::is_assignable_to(&"String".into(), &"String | nil".into(), &hierarchy),
        "String should be assignable to String | nil"
    );
    // "Integer" should NOT be assignable to "String | nil"
    assert!(
        !TypeChecker::is_assignable_to(&"Integer".into(), &"String | nil".into(), &hierarchy),
        "Integer should not be assignable to String | nil"
    );
}

/// BT-1572: `is_assignable_to` with Integer | String union.
#[test]
fn is_assignable_to_integer_or_string() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(TypeChecker::is_assignable_to(
        &"Integer".into(),
        &"Integer | String".into(),
        &hierarchy
    ));
    assert!(TypeChecker::is_assignable_to(
        &"String".into(),
        &"Integer | String".into(),
        &hierarchy
    ));
    assert!(!TypeChecker::is_assignable_to(
        &"Float".into(),
        &"Integer | String".into(),
        &hierarchy
    ));
}
