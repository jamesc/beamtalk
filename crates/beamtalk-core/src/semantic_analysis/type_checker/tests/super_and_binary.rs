// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Super type inference and binary operand validation (BT-596).

use super::super::*;
use super::common::*;

// --- Super type inference tests (BT-596) ---

#[test]
fn test_super_infers_parent_class_type() {
    // class Child < Parent; method: reset => super reset
    // super should infer as Parent type, not Dynamic
    let parent = ClassDefinition {
        name: ident("Parent"),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("reset".into()),
            parameters: vec![],
            body: vec![bare(int_lit(0))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let super_span = Span::new(100, 105);
    let msg_span = Span::new(100, 115);
    let child = ClassDefinition {
        name: ident("Child"),
        superclass: Some(ident("Parent")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("reset".into()),
            parameters: vec![],
            body: vec![bare(Expression::MessageSend {
                receiver: Box::new(Expression::Super(super_span)),
                selector: MessageSelector::Unary("reset".into()),
                arguments: vec![],
                is_cast: false,
                span: msg_span,
            })],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let type_map = infer_types(&module, &hierarchy);

    // super should be inferred as Parent (not Dynamic)
    let ty = type_map.get(super_span);
    assert_eq!(
        ty,
        Some(&InferredType::known("Parent")),
        "super should infer as parent class type"
    );
}

#[test]
fn test_super_unknown_selector_warns() {
    // class Child < Parent; method: test => super nonExistent
    // Should warn because Parent doesn't have nonExistent
    let parent = ClassDefinition {
        name: ident("Parent"),
        superclass: Some(ident("Object")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("reset".into()),
            parameters: vec![],
            body: vec![bare(int_lit(0))],
            return_type: None,
            is_sealed: false,
            is_internal: false,
            kind: MethodKind::Primary,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let child = ClassDefinition {
        name: ident("Child"),
        superclass: Some(ident("Parent")),
        superclass_package: None,
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("test".into()),
            parameters: vec![],
            body: vec![bare(msg_send(
                Expression::Super(span()),
                MessageSelector::Unary("nonExistent".into()),
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
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: span(),
    };

    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "should warn about unknown super selector"
    );
    assert!(
        warnings[0].message.contains("Parent"),
        "warning should reference parent class"
    );
}

// --- Binary operand type validation tests (BT-596) ---

#[test]
fn test_integer_plus_string_warns() {
    // 42 + "hello" — type mismatch
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a numeric argument"))
        .collect();
    assert_eq!(type_warnings.len(), 1);
    assert!(type_warnings[0].message.contains("Integer"));
    assert!(type_warnings[0].message.contains("String"));
}

#[test]
fn test_integer_plus_integer_no_warning() {
    // 42 + 1 — valid
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a numeric"))
        .collect();
    assert!(type_warnings.is_empty());
}

#[test]
fn test_string_plus_integer_warns_unknown_selector() {
    // "hello" + 42 — String doesn't define +, so we get unknown selector (not operand type)
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Binary("+".into()),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // String doesn't define +, so we get "does not understand" (not operand type warning)
    let warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(warnings.len(), 1);
    assert!(warnings[0].message.contains("String"));
}

#[test]
fn test_string_concat_no_operand_warning() {
    // "hello" ++ " world" — valid String concatenation
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Binary("++".into()),
        vec![str_lit(" world")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a"))
        .collect();
    assert!(type_warnings.is_empty());
}

#[test]
fn test_string_concat_integer_warns() {
    // "hello" ++ 42 — wrong type for string concat
    let module = make_module(vec![msg_send(
        str_lit("hello"),
        MessageSelector::Binary("++".into()),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a String argument"))
        .collect();
    assert_eq!(type_warnings.len(), 1);
}

#[test]
fn test_dynamic_type_skips_operand_check() {
    // x + 42 — x is dynamic, no warning
    let module = make_module(vec![msg_send(
        var("x"),
        MessageSelector::Binary("+".into()),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a"))
        .collect();
    assert!(type_warnings.is_empty());
}

#[test]
fn test_comparison_integer_vs_string_warns() {
    // 42 < "hello" — type mismatch for comparison
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("<".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a numeric argument"))
        .collect();
    assert_eq!(type_warnings.len(), 1);
}

#[test]
fn test_binary_operand_warnings_are_warnings_not_errors() {
    // All operand type diagnostics should be warnings
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    for diag in checker.diagnostics() {
        if diag.message.contains("expects a") {
            assert_eq!(
                diag.severity,
                crate::source_analysis::Severity::Warning,
                "operand type diagnostics should be warnings"
            );
        }
    }
}
