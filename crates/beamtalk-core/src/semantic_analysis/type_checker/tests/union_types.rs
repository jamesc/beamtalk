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
    assert_eq!(ty.display_name(), "String | UndefinedObject");
}

#[test]
fn known_display_name() {
    let ty = InferredType::known("Integer");
    assert_eq!(ty.display_name(), "Integer");
}

#[test]
fn dynamic_display_name_unknown() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::Unknown).display_name(),
        "Dynamic"
    );
}

#[test]
fn dynamic_display_name_unannotated_param() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::UnannotatedParam).display_name(),
        "Dynamic (unannotated parameter)"
    );
}

#[test]
fn dynamic_display_name_unannotated_return() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::UnannotatedReturn).display_name(),
        "Dynamic (unannotated return)"
    );
}

#[test]
fn dynamic_display_name_dynamic_receiver() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::DynamicReceiver).display_name(),
        "Dynamic (dynamic receiver)"
    );
}

#[test]
fn dynamic_display_name_ambiguous_control_flow() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::AmbiguousControlFlow).display_name(),
        "Dynamic (ambiguous control flow)"
    );
}

#[test]
fn dynamic_display_name_untyped_ffi() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::UntypedFfi).display_name(),
        "Dynamic (untyped FFI)"
    );
}

#[test]
fn dynamic_display_name_dynamic_spec() {
    assert_eq!(
        InferredType::Dynamic(DynamicReason::DynamicSpec).display_name(),
        "Dynamic (FFI spec is Dynamic)"
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
        is_class_method: false,
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

// ── BT-2624: singleton members in unions ────────────────────────────────

/// Infer `x <selector>` (optionally with `arg`) where `x :: Integer | #infinity`,
/// returning the inferred type and the rendered diagnostics (`"Severity: msg"`).
fn infer_singleton_union_send(
    selector: MessageSelector,
    args: Vec<Expression>,
) -> (InferredType, Vec<String>) {
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            selector,
            args,
        ))],
        span(),
    );
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["Integer", "#infinity"]));
    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );
    let diags = checker
        .diagnostics()
        .iter()
        .map(|d| format!("{:?}: {}", d.severity, d.message))
        .collect();
    (ty, diags)
}

/// BT-2624 (item 2): the equality alias `=` on a union receiver is a universal
/// comparison — it returns Boolean without a spurious "does not understand '='".
#[test]
fn bt2624_union_equality_alias_returns_boolean_no_warning() {
    let (ty, diags) = infer_singleton_union_send(
        MessageSelector::Binary("=".into()),
        vec![Expression::Literal(
            Literal::Symbol("infinity".into()),
            span(),
        )],
    );
    assert_eq!(ty, InferredType::known("Boolean"));
    assert!(
        diags.iter().all(|d| !d.contains("understand")),
        "no DNU expected for `=` on a union, got: {diags:?}"
    );
}

/// BT-2624 (item 2/3): identity comparison `=:=` on a singleton-bearing union
/// returns Boolean, not Dynamic — the singleton member no longer poisons it.
#[test]
fn bt2624_union_identity_comparison_returns_boolean_not_dynamic() {
    let (ty, diags) = infer_singleton_union_send(
        MessageSelector::Binary("=:=".into()),
        vec![Expression::Literal(
            Literal::Symbol("infinity".into()),
            span(),
        )],
    );
    assert_eq!(ty, InferredType::known("Boolean"));
    assert!(diags.is_empty(), "no diagnostics expected, got: {diags:?}");
}

/// BT-2624 (item 3): a singleton member resolves its inherited `Symbol` methods,
/// so a method every member understands (`asString`) infers a concrete type
/// rather than `Dynamic`, with no warning.
#[test]
fn bt2624_union_singleton_resolves_inherited_symbol_method() {
    let (ty, diags) = infer_singleton_union_send(MessageSelector::Unary("asString".into()), vec![]);
    assert_eq!(ty, InferredType::known("String"));
    assert!(
        diags.iter().all(|d| !d.contains("understand")),
        "asString resolves on both members, got: {diags:?}"
    );
}

/// BT-2624 (item 3): when NO member understands the selector, the singleton is
/// resolved (via Symbol) too, so this is a definite failure — a Warning, not a
/// downgraded Hint. (Two members → the plural "do not understand" wording.)
#[test]
fn bt2624_union_singleton_genuine_nonresponder_warns() {
    let (_ty, diags) =
        infer_singleton_union_send(MessageSelector::Unary("frobnicate".into()), vec![]);
    let dnu: Vec<_> = diags.iter().filter(|d| d.contains("understand")).collect();
    assert_eq!(dnu.len(), 1, "expected one DNU diagnostic, got: {diags:?}");
    assert!(
        dnu[0].starts_with("Warning:"),
        "a definite non-responder should warn, not hint: {dnu:?}"
    );
    assert!(
        dnu[0].contains("Integer") && dnu[0].contains("#infinity"),
        "both members should be named: {dnu:?}"
    );
}

/// BT-2624 (item 3): when only the singleton fails to respond, the singleton is
/// surfaced by name in a Hint (previously it was silently masked as Dynamic).
#[test]
fn bt2624_union_singleton_partial_nonresponder_hints_singleton() {
    let (ty, diags) = infer_singleton_union_send(MessageSelector::Unary("abs".into()), vec![]);
    assert_eq!(ty, InferredType::known("Integer"));
    let dnu: Vec<_> = diags.iter().filter(|d| d.contains("understand")).collect();
    assert_eq!(dnu.len(), 1, "expected one DNU hint, got: {diags:?}");
    assert!(
        dnu[0].starts_with("Hint:") && dnu[0].contains("#infinity"),
        "Integer responds but #infinity does not — hint should name the singleton: {dnu:?}"
    );
}

/// BT-2624 (review follow-up): `#foo class` on a union resolves through
/// `Symbol`, so a singleton member's `Self class` return is `Symbol class`
/// (`Meta("Symbol")`) — not a phantom `Meta("#foo")`. A union of singletons
/// collapses to a single `Meta("Symbol")`.
#[test]
fn bt2624_union_singleton_class_resolves_to_symbol_metatype() {
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            MessageSelector::Unary("class".into()),
            vec![],
        ))],
        span(),
    );
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["#north", "#south"]));
    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(ty, InferredType::meta("Symbol"), "got: {ty:?}");
}

// ── BT-2647: non-union singleton receivers resolve against Symbol ─────────

/// Infer `x <selector>` where `x :: #infinity` (a *non-union* singleton),
/// returning the inferred type and rendered diagnostics.
fn infer_singleton_send(
    selector: MessageSelector,
    args: Vec<Expression>,
) -> (InferredType, Vec<String>) {
    let module = Module::new(
        vec![ExpressionStatement::bare(msg_send(
            Expression::Identifier(ident("x")),
            selector,
            args,
        ))],
        span(),
    );
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::known("#infinity"));
    let ty = checker.infer_expr(
        &module.expressions[0].expression,
        &hierarchy,
        &mut env,
        false,
    );
    let diags = checker
        .diagnostics()
        .iter()
        .map(|d| format!("{:?}: {}", d.severity, d.message))
        .collect();
    (ty, diags)
}

/// BT-2647: a non-union singleton (`#infinity`) is a subtype of `Symbol`, so a
/// method `Symbol` understands (`asString`) infers a concrete type — not the
/// `Dynamic` it fell through to before this fix — with no DNU warning.
#[test]
fn bt2647_singleton_resolves_inherited_symbol_method() {
    let (ty, diags) = infer_singleton_send(MessageSelector::Unary("asString".into()), vec![]);
    assert_eq!(ty, InferredType::known("String"));
    assert!(
        diags.iter().all(|d| !d.contains("understand")),
        "asString resolves on Symbol, got: {diags:?}"
    );
}

/// BT-2647: a selector `Symbol` does not understand on a non-union singleton now
/// produces a DNU diagnostic — previously the send fell through silently to
/// Dynamic. Instance-selector DNUs are emitted at Hint severity (matching every
/// other known-class instance send, via `emit_unknown_selector_warning`).
///
/// BT-2679: the message names the singleton the user wrote (`#infinity`), not
/// the `Symbol` it resolves selectors against. Resolution still routes through
/// `Symbol` — the positive path is covered by
/// `bt2647_singleton_resolves_inherited_symbol_method`.
#[test]
fn bt2647_singleton_genuine_nonresponder_hints() {
    let (_ty, diags) = infer_singleton_send(MessageSelector::Unary("frobnicate".into()), vec![]);
    let dnu: Vec<_> = diags.iter().filter(|d| d.contains("understand")).collect();
    assert_eq!(dnu.len(), 1, "expected one DNU diagnostic, got: {diags:?}");
    // `check_instance_selector` still resolves the selector (and any "did you
    // mean" suggestion) against `Symbol` — that routing is unchanged and proven
    // by `bt2647_singleton_resolves_inherited_symbol_method` (positive path) and
    // `bt2679_singleton_binary_send_bypasses_symbol_redirect` (the bypass pin).
    // BT-2679 changed only the *display* name: the message must name `#infinity`,
    // never the `Symbol` it resolved through.
    assert!(
        dnu[0].starts_with("Hint:") && dnu[0].contains("#infinity"),
        "singleton DNU should hint and name the singleton receiver: {dnu:?}"
    );
    assert!(
        !dnu[0].contains("Symbol"),
        "singleton DNU should name `#infinity`, not the `Symbol` it resolves via: {dnu:?}"
    );
}

/// BT-2647: `#infinity class` resolves through `Symbol`, so the metatype is
/// `Symbol class` (`Meta("Symbol")`), not a phantom `Meta("#infinity")`.
#[test]
fn bt2647_singleton_class_resolves_to_symbol_metatype() {
    let (ty, _diags) = infer_singleton_send(MessageSelector::Unary("class".into()), vec![]);
    assert_eq!(ty, InferredType::meta("Symbol"), "got: {ty:?}");
}

/// BT-2679: the `resolve_class` redirect deliberately excludes binary sends
/// (`!matches!(selector, MessageSelector::Binary(_))`), so a singleton receiver
/// in an (in)equality send is NOT routed through `Symbol`. This is load-bearing:
/// `Symbol` understands `=:=`/`=`, so routing through it would short-circuit the
/// result to `Boolean` and swallow the BT-2631 statically-decidable-comparison
/// hint. Co-located with the BT-2647 redirect it guards (previously covered only
/// indirectly by `bt2631_standalone_singleton_eq_union_on_right_emits_hint`).
#[test]
fn bt2679_singleton_binary_send_bypasses_symbol_redirect() {
    // `#infinity =:= x` where `x :: Integer | String` can never be true — the
    // union admits no singleton.
    let expr = msg_send(
        symbol_lit("infinity"),
        MessageSelector::Binary("=:=".into()),
        vec![var("x")],
    );
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("x", InferredType::simple_union(&["Integer", "String"]));
    let ty = checker.infer_expr(&expr, &hierarchy, &mut env, false);

    // Had the singleton routed through `Symbol`, `=:=` would resolve to Boolean.
    assert!(
        !matches!(&ty, InferredType::Known { class_name, .. } if class_name == "Boolean"),
        "binary send on a singleton must bypass the Symbol redirect, got: {ty:?}"
    );

    // The BT-2631 impossible-comparison hint must still fire.
    let hints: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("can never be true") && d.message.contains("#infinity"))
        .collect();
    assert_eq!(
        hints.len(),
        1,
        "expected the BT-2631 hint to fire, got: {:?}",
        checker.diagnostics()
    );
}
