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

/// Builds a fixture for class-side (Meta receiver) conformance checking
/// (BT-2761): registers the `Printable` (`asString`) and `Serializable`
/// (`serialize`) protocols, plus a `JsonEncoder` class whose **class-side** method
/// `generate:` declares its parameter as `param_type`. Returns
/// `(hierarchy, registry)`.
fn setup_json_class_side_fixture(
    param_type: &str,
) -> (
    ClassHierarchy,
    crate::semantic_analysis::protocol_registry::ProtocolRegistry,
) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    let mut hierarchy = ClassHierarchy::with_builtins();

    let proto_module = Module {
        protocols: vec![
            ProtocolDefinition {
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
            },
            ProtocolDefinition {
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
            },
        ],
        ..Module::new(vec![], span())
    };

    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&proto_module, &hierarchy);
    assert!(diags.is_empty());

    // Register protocol classes (this triggers the pre-fix BT-2135 bug)
    hierarchy.register_protocol_classes(&proto_module);

    // Add a class "JsonEncoder" with a class method "generate:" whose param is
    // protocol-typed. (NOT named "Json" — that's a *builtin*, which
    // `add_from_beam_meta` skips, silently substituting the stdlib class.)
    let json_info = ClassInfo {
        name: "JsonEncoder".into(),
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
            defined_in: "JsonEncoder".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("String".into()),
            param_types: vec![Some(param_type.into())],
            doc: None,
        }],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    hierarchy.add_from_beam_meta(vec![json_info]);

    (hierarchy, registry)
}

/// Builds a `TestRunner >> run:` module whose body sends
/// `JsonEncoder generate: <arg>` with the given argument type annotation.
///
/// The class-reference receiver gets a distinct span (`receiver_span`) so the
/// checker's `TypeMap` entry for the receiver doesn't collide with the other
/// synthetic nodes (which all share `span()` = `Span::new(0, 1)`) — the tests
/// look the receiver's inferred type up by that span.
fn make_json_generate_module(arg_name: &str, arg_type: &str, receiver_span: Span) -> Module {
    let receiver = Expression::ClassReference {
        name: Identifier::new("JsonEncoder", receiver_span),
        package: None,
        span: receiver_span,
    };
    let arg_ident = Identifier::new(arg_name, Span::new(20, 24));
    let test_method = make_keyword_method(
        &["run:"],
        vec![(arg_name, Some(arg_type))],
        vec![msg_send(
            receiver,
            MessageSelector::Keyword(vec![KeywordPart::new("generate:", span())]),
            vec![Expression::Identifier(arg_ident)],
        )],
    );
    let test_class = ClassDefinition::new(
        ident("TestRunner"),
        ident("Object"),
        vec![],
        vec![test_method],
        span(),
    );
    make_module_with_classes(vec![], vec![test_class])
}

/// End-to-end: passing Dictionary to a **class-side** method expecting
/// Printable should NOT produce a conformance diagnostic, because Dictionary
/// conforms to the Printable protocol.
///
/// De-vacuated for BT-2761: before Meta-typed receivers were handled by
/// `check_protocol_conformance_in_expr`, this test passed vacuously (the
/// `JsonEncoder` class-reference receiver inferred as `InferredType::Meta`, which
/// the conformance walk silently skipped). It now *asserts* that the checker
/// resolved the receiver as `Meta{JsonEncoder}` and that the class-side method is
/// resolvable — so the "no warning" outcome is a real conformance pass, not
/// an unreached check. The companion negative test
/// (`test_meta_receiver_class_method_protocol_param_non_conforming_warns`)
/// proves the same path fires a warning when the argument does not conform.
#[test]
fn test_protocol_typed_param_no_false_positive_with_protocol_classes() {
    let (hierarchy, registry) = setup_json_class_side_fixture("Printable");

    let receiver_span = Span::new(10, 14);
    let module = make_json_generate_module("dict", "Dictionary", receiver_span);

    // Run type checking with protocols
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // BT-2761: prove the Meta path was actually exercised — the receiver must
    // have been resolved to the metatype `Meta{JsonEncoder}` (not skipped), and the
    // class-side target method must be resolvable from it.
    let receiver_ty = checker.type_map().get(receiver_span).expect(
        "checker should have inferred a type for the `JsonEncoder` class-reference receiver",
    );
    assert!(
        matches!(receiver_ty, InferredType::Meta { class_name, .. } if class_name == "JsonEncoder"),
        "receiver should resolve as Meta{{JsonEncoder}}, got: {receiver_ty:?}"
    );
    assert!(
        hierarchy
            .find_class_method("JsonEncoder", "generate:")
            .is_some(),
        "class-side `generate:` must be resolvable — otherwise the conformance check is unreachable"
    );

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

/// Negative counterpart proving the Meta-receiver path genuinely fires
/// (BT-2761): `JsonEncoder generate:` (class-side) declares `:: Serializable`;
/// passing an Integer (no `serialize`) must warn.
#[test]
fn test_meta_receiver_class_method_protocol_param_non_conforming_warns() {
    let (hierarchy, registry) = setup_json_class_side_fixture("Serializable");

    let receiver_span = Span::new(10, 14);
    let module = make_json_generate_module("n", "Integer", receiver_span);

    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    // Sanity: the receiver resolved as Meta{JsonEncoder}, so the warning below can
    // only have come from the class-side (Meta) conformance path.
    let receiver_ty = checker.type_map().get(receiver_span).expect(
        "checker should have inferred a type for the `JsonEncoder` class-reference receiver",
    );
    assert!(
        matches!(receiver_ty, InferredType::Meta { class_name, .. } if class_name == "JsonEncoder"),
        "receiver should resolve as Meta{{JsonEncoder}}, got: {receiver_ty:?}"
    );

    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert_eq!(
        conformance_warnings.len(),
        1,
        "Integer does not conform to Serializable — expected exactly 1 warning, got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    assert!(
        conformance_warnings[0].message.contains("Integer")
            && conformance_warnings[0].message.contains("Serializable"),
        "Warning should mention Integer and Serializable, got: {}",
        conformance_warnings[0].message
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

// --- `&` intersection parameter types require conformance to BOTH protocol
// parts (ADR 0068 §Protocol Composition, ADR 0102 §1/§3, BT-2743) ---

/// Builds a `Zzyzx >> process:` **instance** method whose parameter is
/// annotated `Printable & Serializable` (as `type_name()` would render the
/// parsed `TypeAnnotation::Intersection`), plus the two protocols it
/// references. Returns `(hierarchy, registry)` with both protocols and
/// their classes registered, and the `Zzyzx` instance-method table installed.
///
/// Also installs single-protocol instance methods `store:` (`:: Serializable`)
/// and `describe:` (`:: Describable`, requiring `printString`) plus the
/// `Describable` protocol, used by the BT-2761 class-object-argument tests. (Class-side receivers are exercised separately via
/// `setup_json_class_side_fixture` — since BT-2761 the conformance walk
/// resolves `Meta` receivers through `find_class_method` too.)
#[allow(clippy::too_many_lines)] // declarative protocol/class fixture data
fn setup_intersection_param_fixture() -> (
    ClassHierarchy,
    crate::semantic_analysis::protocol_registry::ProtocolRegistry,
) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    use crate::semantic_analysis::protocol_registry::ProtocolRegistry;

    let mut hierarchy = ClassHierarchy::with_builtins();

    let proto_module = Module {
        protocols: vec![
            ProtocolDefinition {
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
            },
            ProtocolDefinition {
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
            },
            // `printString` is provided by the metaclass tower (Object), so
            // class objects satisfy Describable with an empty class side
            // (BT-2761 tower test).
            ProtocolDefinition {
                name: Identifier::new("Describable", span()),
                type_params: vec![],
                extending: None,
                method_signatures: vec![ProtocolMethodSignature {
                    selector: MessageSelector::Unary("printString".into()),
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
            },
        ],
        ..Module::new(vec![], span())
    };

    let mut registry = ProtocolRegistry::new();
    let diags = registry.register_module(&proto_module, &hierarchy);
    assert!(diags.is_empty());
    hierarchy.register_protocol_classes(&proto_module);

    let zzyzx_info = ClassInfo {
        name: "Zzyzx".into(),
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
        methods: vec![
            MethodInfo {
                selector: "process:".into(),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: "Zzyzx".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some("String".into()),
                // `type_name()` for `TypeAnnotation::Intersection` renders as
                // `"Printable & Serializable"` — this is exactly what
                // `class_info.rs` stores for a real `:: Printable & Serializable`
                // parameter annotation.
                param_types: vec![Some("Printable & Serializable".into())],
                doc: None,
            },
            MethodInfo {
                selector: "store:".into(),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: "Zzyzx".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some("String".into()),
                param_types: vec![Some("Serializable".into())],
                doc: None,
            },
            MethodInfo {
                selector: "describe:".into(),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: "Zzyzx".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some("String".into()),
                param_types: vec![Some("Describable".into())],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    hierarchy.add_from_beam_meta(vec![zzyzx_info]);

    (hierarchy, registry)
}

#[test]
fn test_intersection_param_missing_one_protocol_warns() {
    // `Integer` has `asString` (conforms to Printable) but no `serialize`
    // (does not conform to Serializable) — `Printable & Serializable`
    // requires BOTH, so exactly one "does not conform" warning (mentioning
    // Serializable) is expected.
    let (hierarchy, registry) = setup_intersection_param_fixture();

    // BT-2743 regression note: every synthetic node built by the `common`
    // helpers (`var`, `msg_send`, …) shares the single fixed `span()`
    // (`Span::new(0, 1)`), and `check_protocol_conformance_in_expr` looks up
    // the receiver/argument types by span in the checker's `TypeMap`. With a
    // shared span, the receiver's and argument's type_map entries collide and
    // clobber each other — so this test gives the receiver and argument
    // *distinct* spans (unlike the pre-existing class-side fixtures, which
    // only ever asserted "no warning" and so never observed the collision).
    let zzyzx_ident = Identifier::new("zzyzx", Span::new(10, 14));
    let n_ident = Identifier::new("n", Span::new(20, 21));
    let test_method = make_keyword_method(
        &["run:", "with:"],
        vec![("zzyzx", Some("Zzyzx")), ("n", Some("Integer"))],
        vec![msg_send(
            Expression::Identifier(zzyzx_ident),
            MessageSelector::Keyword(vec![KeywordPart::new("process:", span())]),
            vec![Expression::Identifier(n_ident)],
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

    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert_eq!(
        conformance_warnings.len(),
        1,
        "Integer conforms to Printable but not Serializable — expected exactly 1 warning, got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    assert!(
        conformance_warnings[0].message.contains("Serializable"),
        "Warning should mention the missing protocol Serializable, got: {}",
        conformance_warnings[0].message
    );
    assert!(
        !conformance_warnings[0].message.contains("Printable"),
        "Integer conforms to Printable — it should not be named in the warning, got: {}",
        conformance_warnings[0].message
    );
}

#[test]
fn test_intersection_param_conforms_to_both_no_warning() {
    // A class implementing both `asString` and `serialize` conforms to
    // `Printable & Serializable` — no warning expected.
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let (mut hierarchy, registry) = setup_intersection_param_fixture();

    let report_info = ClassInfo {
        name: "Report".into(),
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
        methods: vec![
            MethodInfo {
                selector: "asString".into(),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: "Report".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some("String".into()),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: "serialize".into(),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: "Report".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some("String".into()),
                param_types: vec![],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    hierarchy.add_from_beam_meta(vec![report_info]);

    // See the BT-2743 regression note in `test_intersection_param_missing_one_protocol_warns`
    // — the receiver and argument need distinct spans so their `TypeMap`
    // entries don't collide.
    let zzyzx_ident = Identifier::new("zzyzx", Span::new(10, 14));
    let r_ident = Identifier::new("r", Span::new(20, 21));
    let test_method = make_keyword_method(
        &["run:", "with:"],
        vec![("zzyzx", Some("Zzyzx")), ("r", Some("Report"))],
        vec![msg_send(
            Expression::Identifier(zzyzx_ident),
            MessageSelector::Keyword(vec![KeywordPart::new("process:", span())]),
            vec![Expression::Identifier(r_ident)],
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

    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert!(
        conformance_warnings.is_empty(),
        "Report conforms to both Printable and Serializable — no warning expected, got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

// --- BT-2761: class-object (Meta-typed) ARGUMENTS against protocol-typed
// parameters — `Meta{C}` satisfies `:: P` iff C's class side conforms to P ---

/// Adds a `Widget` class to the hierarchy whose class side has the given
/// zero-arity selectors. An empty list leaves the class side bare — the
/// metaclass tower still provides `printString`, `name`, etc., but nothing
/// protocol-specific like `serialize` or `asString`.
fn add_widget_class(hierarchy: &mut ClassHierarchy, class_side_selectors: &[&str]) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let class_methods = class_side_selectors
        .iter()
        .map(|sel| MethodInfo {
            selector: (*sel).into(),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: "Widget".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("String".into()),
            param_types: vec![],
            doc: None,
        })
        .collect();

    let widget_info = ClassInfo {
        name: "Widget".into(),
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
        class_methods,
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };
    hierarchy.add_from_beam_meta(vec![widget_info]);
}

/// Builds a `TestRunner >> run:` module sending `zzyzx <selector> Widget` —
/// a class-object argument (inferred `Meta{Widget}`) passed to a
/// protocol-typed parameter of the `Zzyzx` fixture. Receiver and argument
/// get distinct spans so their `TypeMap` entries don't collide (see the
/// BT-2743 regression note above).
fn make_widget_arg_module(selector: &str) -> Module {
    let zzyzx_ident = Identifier::new("zzyzx", Span::new(10, 14));
    let widget_span = Span::new(20, 26);
    let widget_ref = Expression::ClassReference {
        name: Identifier::new("Widget", widget_span),
        package: None,
        span: widget_span,
    };
    let test_method = make_keyword_method(
        &["run:"],
        vec![("zzyzx", Some("Zzyzx"))],
        vec![msg_send(
            Expression::Identifier(zzyzx_ident),
            MessageSelector::Keyword(vec![KeywordPart::new(selector, span())]),
            vec![widget_ref],
        )],
    );
    let test_class = ClassDefinition::new(
        ident("TestRunner"),
        ident("Object"),
        vec![],
        vec![test_method],
        span(),
    );
    make_module_with_classes(vec![], vec![test_class])
}

/// Positive: `Widget` has a class-side `serialize`, so the class object
/// `Widget` conforms to `Serializable` — no warning for `zzyzx store: Widget`.
#[test]
fn test_class_object_arg_conforms_via_class_side_method() {
    let (mut hierarchy, registry) = setup_intersection_param_fixture();
    add_widget_class(&mut hierarchy, &["serialize"]);

    let module = make_widget_arg_module("store:");
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert!(
        conformance_warnings.is_empty(),
        "Widget's class side has `serialize` — the class object conforms to Serializable, got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Negative: `Widget`'s class side (and the metaclass tower) has no
/// `serialize`, so passing the class object where `:: Serializable` is
/// expected must warn — naming the *class side* (`Widget class`).
#[test]
fn test_class_object_arg_non_conforming_warns() {
    let (mut hierarchy, registry) = setup_intersection_param_fixture();
    add_widget_class(&mut hierarchy, &[]);

    let module = make_widget_arg_module("store:");
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert_eq!(
        conformance_warnings.len(),
        1,
        "Widget class lacks `serialize` — expected exactly 1 warning, got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    assert!(
        conformance_warnings[0].message.contains("Widget class")
            && conformance_warnings[0].message.contains("Serializable"),
        "Warning should mention `Widget class` and Serializable, got: {}",
        conformance_warnings[0].message
    );
    assert!(
        conformance_warnings[0]
            .hint
            .as_deref()
            .is_some_and(|h| h.contains("serialize")),
        "Hint should list the missing `serialize` selector, got: {:?}",
        conformance_warnings[0].hint
    );
}

/// A class object satisfies `Describable` via the metaclass tower — every
/// class object responds to `printString` through
/// `Metaclass → Class → Behaviour → Object`, even with no class-side methods
/// of its own. This is the property that keeps class objects passed to
/// protocol params whose requirements are tower-provided warning-free.
#[test]
fn test_class_object_arg_conforms_via_metaclass_tower() {
    let (mut hierarchy, registry) = setup_intersection_param_fixture();
    add_widget_class(&mut hierarchy, &[]);

    let module = make_widget_arg_module("describe:");
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert!(
        conformance_warnings.is_empty(),
        "Class objects respond to `printString` via the metaclass tower — no warning expected, got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Intersection param + class-object argument (BT-2743 × BT-2761): the class
/// object `Widget` conforms to `Printable & Serializable` when its class side
/// has both `asString` and `serialize` (neither is tower-provided).
#[test]
fn test_class_object_arg_intersection_param_conforms_to_both() {
    let (mut hierarchy, registry) = setup_intersection_param_fixture();
    add_widget_class(&mut hierarchy, &["asString", "serialize"]);

    let module = make_widget_arg_module("process:");
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert!(
        conformance_warnings.is_empty(),
        "Widget class conforms to both parts of Printable & Serializable — got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
}

/// Intersection param + class-object argument, negative: with a class-side
/// `asString` but no `serialize`, `Widget` (class object) satisfies
/// `Printable` but not `Serializable` — exactly one warning naming the
/// failing part.
#[test]
fn test_class_object_arg_intersection_param_missing_one_warns() {
    let (mut hierarchy, registry) = setup_intersection_param_fixture();
    add_widget_class(&mut hierarchy, &["asString"]);

    let module = make_widget_arg_module("process:");
    let mut checker = TypeChecker::new();
    checker.check_module_with_protocols(&module, &hierarchy, &registry);

    let conformance_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not conform"))
        .collect();
    assert_eq!(
        conformance_warnings.len(),
        1,
        "Widget class satisfies Printable (class-side asString) but not Serializable — expected exactly 1 warning, got: {:?}",
        conformance_warnings
            .iter()
            .map(|d| &d.message)
            .collect::<Vec<_>>()
    );
    assert!(
        conformance_warnings[0].message.contains("Widget class")
            && conformance_warnings[0].message.contains("Serializable"),
        "Warning should mention `Widget class` and Serializable, got: {}",
        conformance_warnings[0].message
    );
    assert!(
        !conformance_warnings[0].message.contains("Printable"),
        "Printable is satisfied via the class-side `asString` — it should not be named, got: {}",
        conformance_warnings[0].message
    );
}

#[test]
fn test_split_intersection_type_string() {
    assert_eq!(
        TypeChecker::split_intersection_type_string("Printable & Serializable"),
        Some(vec!["Printable", "Serializable"])
    );
    // Chained `&` (left-associative AST, flat `type_name()` rendering).
    assert_eq!(
        TypeChecker::split_intersection_type_string("A & B & C"),
        Some(vec!["A", "B", "C"])
    );
    // No top-level `&` — not an intersection annotation.
    assert_eq!(
        TypeChecker::split_intersection_type_string("Printable"),
        None
    );
    // A `&` nested inside a generic type argument's own parens must not be
    // mistaken for a top-level split point.
    assert_eq!(
        TypeChecker::split_intersection_type_string("Collection(A & B)"),
        None
    );
}
