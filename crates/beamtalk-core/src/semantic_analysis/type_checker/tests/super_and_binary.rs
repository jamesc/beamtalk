// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Super type inference and binary operand validation.

use super::super::*;
use super::common::*;

// --- Super type inference tests ---

#[test]
fn test_super_infers_parent_class_type() {
    // class Child < Parent; method: reset => super reset
    // super should infer as Parent type, not Dynamic
    let parent = ClassDefinition {
        methods: vec![MethodDefinition {
            body: vec![bare(int_lit(0))],
            ..MethodDefinition::test_at("reset", span())
        }],
        ..ClassDefinition::test_at("Parent", Some("Object"), ClassKind::Object, span())
    };

    let super_span = Span::new(100, 105);
    let msg_span = Span::new(100, 115);
    let child = ClassDefinition {
        methods: vec![MethodDefinition {
            body: vec![bare(Expression::MessageSend {
                receiver: Box::new(Expression::Super(super_span)),
                selector: MessageSelector::Unary("reset".into()),
                arguments: vec![],
                is_cast: false,
                span: msg_span,
            })],
            ..MethodDefinition::test_at("reset", span())
        }],
        ..ClassDefinition::test_at("Child", Some("Parent"), ClassKind::Object, span())
    };

    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let type_map = infer_types(&module, &hierarchy, None);

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
        methods: vec![MethodDefinition {
            body: vec![bare(int_lit(0))],
            ..MethodDefinition::test_at("reset", span())
        }],
        ..ClassDefinition::test_at("Parent", Some("Object"), ClassKind::Object, span())
    };

    let child = ClassDefinition {
        methods: vec![MethodDefinition {
            body: vec![bare(msg_send(
                Expression::Super(span()),
                MessageSelector::Unary("nonExistent".into()),
                vec![],
            ))],
            ..MethodDefinition::test_at("test", span())
        }],
        ..ClassDefinition::test_at("Child", Some("Parent"), ClassKind::Object, span())
    };

    let module = make_module_with_classes(vec![], vec![parent, child]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let checker = check(&module, &hierarchy);

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

// --- Binary operand type validation tests ---

#[test]
fn test_integer_plus_string_warns() {
    // 42 + "hello" — type mismatch
    let module = make_module(vec![msg_send(
        int_lit(42),
        MessageSelector::Binary("+".into()),
        vec![str_lit("hello")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let checker = check(&module, &hierarchy);

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
    let checker = check(&module, &hierarchy);

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
    let checker = check(&module, &hierarchy);

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
    let checker = check(&module, &hierarchy);

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
    let checker = check(&module, &hierarchy);

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
    let checker = check(&module, &hierarchy);

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
    let checker = check(&module, &hierarchy);

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
    let checker = check(&module, &hierarchy);

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

// --- BT-3928: comparison argument kind for Character/String/Symbol receivers ---

fn comparison_kind_warnings(receiver: Expression, arg: Expression) -> Vec<String> {
    let module = make_module(vec![msg_send(
        receiver,
        MessageSelector::Binary("<".into()),
        vec![arg],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let checker = check(&module, &hierarchy);
    checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a"))
        .map(|d| d.message.to_string())
        .collect()
}

#[test]
fn test_comparison_symbol_receiver_mismatched_arg_warns() {
    let w = comparison_kind_warnings(symbol_lit("a"), int_lit(5));
    assert_eq!(w.len(), 1, "{w:?}");
    assert!(w[0].contains("expects a Symbol argument"), "{w:?}");
}

#[test]
fn test_comparison_symbol_receiver_same_kind_silent() {
    assert!(comparison_kind_warnings(symbol_lit("a"), symbol_lit("b")).is_empty());
}

#[test]
fn test_comparison_symbol_receiver_untyped_arg_silent() {
    assert!(comparison_kind_warnings(symbol_lit("a"), var("x")).is_empty());
}

#[test]
fn test_comparison_character_receiver_mismatched_arg_warns() {
    let w = comparison_kind_warnings(char_lit('a'), int_lit(1));
    assert_eq!(w.len(), 1, "{w:?}");
    assert!(w[0].contains("expects a Character argument"), "{w:?}");
}

#[test]
fn test_comparison_character_receiver_same_kind_silent() {
    assert!(comparison_kind_warnings(char_lit('a'), char_lit('b')).is_empty());
}

#[test]
fn test_comparison_character_receiver_untyped_arg_silent() {
    assert!(comparison_kind_warnings(char_lit('a'), var("x")).is_empty());
}

#[test]
fn test_comparison_string_receiver_mismatched_arg_warns() {
    let w = comparison_kind_warnings(str_lit("x"), int_lit(1));
    assert_eq!(w.len(), 1, "{w:?}");
    assert!(w[0].contains("expects a String argument"), "{w:?}");
}

#[test]
fn test_comparison_string_receiver_same_kind_silent() {
    assert!(comparison_kind_warnings(str_lit("x"), str_lit("y")).is_empty());
}

#[test]
fn test_comparison_string_receiver_untyped_arg_silent() {
    assert!(comparison_kind_warnings(str_lit("x"), var("x")).is_empty());
}
