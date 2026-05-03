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

// --- BT-2135: is_protocol_type returns true even when protocol classes are registered ---

/// After `register_protocol_classes` adds synthetic class entries for protocols,
/// `is_protocol_type` must still recognise them as protocols (not real classes).
#[test]
fn test_is_protocol_type_with_registered_protocol_classes() {
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    let mut hierarchy = ClassHierarchy::with_builtins();

    // Create a protocol and register it
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("Printable", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("asString".into()),
                parameters: vec![],
                return_type: Some(TypeAnnotation::simple("String", span())),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };

    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&proto_module, &hierarchy);
    assert!(diags.is_empty());

    // Register protocol classes (this is what BT-1933 added — previously broke is_protocol_type)
    hierarchy.register_protocol_classes(&proto_module);

    // After registering protocol classes, hierarchy.has_class("Printable") is true
    assert!(
        hierarchy.has_class("Printable"),
        "Printable should be in the class hierarchy as a synthetic protocol class"
    );
    assert!(
        hierarchy.is_protocol_class("Printable"),
        "Printable should be marked as a protocol class"
    );

    // The bug: is_protocol_type used to return false because of the `!hierarchy.has_class()`
    // clause. With the fix, it correctly returns true for synthetic protocol class entries.
    assert!(
        TypeChecker::is_protocol_type("Printable", &hierarchy, &registry),
        "is_protocol_type must return true for a protocol even when registered as a protocol class"
    );
}

/// A real class with the same name as a protocol should NOT be recognised as a protocol type.
#[test]
fn test_is_protocol_type_real_class_not_protocol() {
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    let hierarchy = ClassHierarchy::with_builtins();
    let registry = ProtocolRegistry::new();

    // "Integer" is a real class and not in the protocol registry
    assert!(
        !TypeChecker::is_protocol_type("Integer", &hierarchy, &registry),
        "Real class Integer should not be reported as a protocol type"
    );
}

/// End-to-end: passing Dictionary to a method expecting Printable should NOT produce
/// a type-mismatch diagnostic, because Dictionary conforms to the Printable protocol.
#[test]
fn test_protocol_typed_param_no_false_positive_with_protocol_classes() {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    let mut hierarchy = ClassHierarchy::with_builtins();

    // Define a protocol requiring `asString`
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("Printable", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("asString".into()),
                parameters: vec![],
                return_type: Some(TypeAnnotation::simple("String", span())),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };

    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&proto_module, &hierarchy);
    assert!(diags.is_empty());

    // Register protocol classes (this triggers the pre-fix bug)
    hierarchy.register_protocol_classes(&proto_module);

    // Add a class "Json" with a class method "generate:" that expects Printable
    let json_info = ClassInfo {
        name: "Json".into(),
        superclass: Some("Object".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![MethodInfo {
            selector: "generate:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Json".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("String".into()),
            param_types: vec![Some("Printable".into())],
            doc: None,
        }],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    hierarchy.add_from_beam_meta(vec![json_info]);

    // Build a module that calls `Json generate: someDictionary`
    // We use a class with a method that calls Json generate: with a Dictionary
    let test_method = make_keyword_method(
        &["run:"],
        vec![("dict", Some("Dictionary"))],
        vec![msg_send(
            class_ref("Json"),
            MessageSelector::Keyword(vec![KeywordPart::new("generate:", span())]),
            vec![var("dict")],
        )],
    );
    let test_class = ClassDefinition::new(
        ident("TestRunner"),
        ident("Object"),
        vec![],
        vec![test_method],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![test_class]);

    // Run type checking with protocols
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // There should be NO "does not conform" warning — Dictionary has asString
    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert!(
        conformance_warnings.is_empty(),
        "Dictionary conforms to Printable (has asString) — no warning expected, got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// When a class genuinely does NOT conform to a protocol (missing required methods),
/// the structural conformance check should still fire, even after protocol classes
/// are registered in the hierarchy.
#[test]
fn test_protocol_typed_param_non_conforming_still_warns() {
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    let mut hierarchy = ClassHierarchy::with_builtins();

    // Define a protocol requiring `serialize` — Integer does NOT have this method
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("Serializable", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("serialize".into()),
                parameters: vec![],
                return_type: Some(TypeAnnotation::simple("String", span())),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };

    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&proto_module, &hierarchy);
    assert!(diags.is_empty());

    // Register protocol classes (same as production code does)
    hierarchy.register_protocol_classes(&proto_module);

    // Verify is_protocol_type works for this protocol
    assert!(
        TypeChecker::is_protocol_type("Serializable", &hierarchy, &registry),
        "Serializable should be recognised as a protocol type"
    );

    // Directly test conformance checking: Integer does NOT have `serialize`
    let mut checker = TypeChecker::new();
    checker.check_protocol_argument_conformance(
        &InferredType::Known {
            class_name: "Integer".into(),
            type_args: vec![],
            provenance: super::super::TypeProvenance::Inferred(span()),
        },
        "Serializable",
        span(),
        &hierarchy,
        &registry,
    );

    // There SHOULD be a "does not conform" warning — Integer lacks `serialize`
    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert_eq!(
        conformance_warnings.len(),
        1,
        "Integer does not conform to Serializable (missing `serialize`) — expected 1 warning, got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    assert!(
        conformance_warnings[0].message.contains("Integer"),
        "Warning should mention Integer, got: {}",
        conformance_warnings[0].message
    );
    assert!(
        conformance_warnings[0].message.contains("Serializable"),
        "Warning should mention Serializable, got: {}",
        conformance_warnings[0].message
    );
}
