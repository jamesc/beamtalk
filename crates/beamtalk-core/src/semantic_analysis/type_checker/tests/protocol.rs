// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Behaviour protocol / class hierarchy fallback (BT-777).

use super::common::*;

// --- Behaviour protocol / Class hierarchy fallback tests (BT-777) ---

#[test]
fn test_behaviour_protocol_superclass_no_warning() {
    // Integer superclass — should NOT warn (superclass is a Behaviour instance method
    // resolved via the Class→Behaviour chain fallback)
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("superclass".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer superclass should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_behaviour_protocol_methods_no_warning() {
    // Integer methods — should NOT warn
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("methods".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer methods should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_behaviour_protocol_subclasses_no_warning() {
    // Integer subclasses — should NOT warn
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("subclasses".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer subclasses should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_behaviour_protocol_all_superclasses_no_warning() {
    // String allSuperclasses — should NOT warn
    let module = make_module(vec![msg_send(
        class_ref("String"),
        MessageSelector::Unary("allSuperclasses".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "String allSuperclasses should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_class_protocol_name_no_warning() {
    // Integer name — should NOT warn (name is a Class instance method)
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("name".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer name should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_class_protocol_is_class_no_warning() {
    // Integer isClass — should NOT warn
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("isClass".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer isClass should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_unknown_class_side_message_still_warns() {
    // Integer bogusClassMethod — SHOULD warn (not in Class chain or instance methods)
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Unary("bogusClassMethod".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert_eq!(
        dnu_warnings.len(),
        1,
        "Integer bogusClassMethod should produce exactly one warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_behaviour_protocol_can_understand_no_warning() {
    // Integer canUnderstand: #+ — should NOT warn (canUnderstand: is a Behaviour method)
    let module = make_module(vec![msg_send(
        class_ref("Integer"),
        MessageSelector::Keyword(vec![crate::ast::KeywordPart {
            keyword: "canUnderstand:".into(),
            span: span(),
        }]),
        vec![Expression::Literal(Literal::Symbol("+".into()), span())],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let dnu_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not understand"))
        .collect();
    assert!(
        dnu_warnings.is_empty(),
        "Integer canUnderstand: should produce no warning, got: {dnu_warnings:?}"
    );
}

#[test]
fn test_non_self_field_assignment_produces_warning_not_error() {
    // `other.x := 1` should produce a Severity::Warning, not Severity::Error,
    // consistent with the module's "Warnings only, never errors" design principle.
    use crate::source_analysis::Severity;
    let source = "Value subclass: Point\n  state: x\n  state: y\n  bad: other =>\n    other.x := 1";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = crate::semantic_analysis::ClassHierarchy::build(&module)
        .0
        .unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let field_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("Cannot assign"))
        .collect();
    assert_eq!(
        field_diags.len(),
        1,
        "Expected exactly one non-self field assignment diagnostic, got: {field_diags:?}"
    );
    assert_eq!(
        field_diags[0].severity,
        Severity::Warning,
        "Non-self field assignment should be a warning, not an error"
    );
}
