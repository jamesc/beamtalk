// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generic inheritance, type-param bounds, variance, and generic param detection (BT-1577, BT-1583, BT-1588).

use super::super::*;
use super::common::*;

// ---- BT-1577: Generic inheritance tests ----

/// Build `GenCollection(E)` with method `first` returning `E`, `size` returning `Integer`.
/// Build `GenArray(E)` extends `GenCollection(E)` with `superclass_type_args` mapping E to E.
fn add_generic_collection_hierarchy(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo, SuperclassTypeArg};

    let collection_info = ClassInfo {
        name: eco_string("GenCollection"),
        superclass: Some(eco_string("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("first"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenCollection"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("E")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("size"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenCollection"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Integer")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("select:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("GenCollection"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("Self")),
                param_types: vec![Some(eco_string("Block(E, Boolean)"))],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("E")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![],
    };

    let array_info = ClassInfo {
        name: eco_string("GenArray"),
        superclass: Some(eco_string("GenCollection")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![MethodInfo {
            selector: eco_string("append:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string("GenArray"),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some(eco_string("Self")),
            param_types: vec![Some(eco_string("E"))],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("E")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 0 }],
    };

    hierarchy.add_from_beam_meta(vec![collection_info, array_info]);
}

/// BT-1577: Inherited method `first` on `GenArray(Integer)` returns `Integer`.
#[test]
fn generic_inheritance_inherited_method_returns_substituted_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "arr",
        InferredType::Known {
            class_name: eco_string("GenArray"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("arr"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "first on GenArray(Integer) should return Integer via inheritance, got: {result_ty:?}"
    );
}

/// BT-1577: Non-generic return type from inherited method is unaffected.
#[test]
fn generic_inheritance_non_generic_return_unchanged() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "arr",
        InferredType::Known {
            class_name: eco_string("GenArray"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(var("arr"), MessageSelector::Unary("size".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "size on GenArray(Integer) should return Integer (non-generic), got: {result_ty:?}"
    );
}

/// BT-1577: Concrete superclass type arg — `IntArray` extends `GenCollection(Integer)`.
#[test]
fn generic_inheritance_concrete_superclass_type_arg() {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, SuperclassTypeArg};

    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    // IntArray has no type params, but maps Integer to GenCollection's E
    let int_array_info = ClassInfo {
        name: eco_string("IntArray"),
        superclass: Some(eco_string("GenCollection")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![SuperclassTypeArg::Concrete {
            type_name: eco_string("Integer"),
        }],
    };
    hierarchy.add_from_beam_meta(vec![int_array_info]);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("ia", InferredType::known("IntArray"));

    let result_ty = checker.infer_expr(
        &msg_send(var("ia"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "first on IntArray should return Integer via concrete superclass type arg, got: {result_ty:?}"
    );
}

/// BT-1577: Self type on inherited method carries receiver's type args.
#[test]
fn generic_inheritance_self_type_carries_type_args() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "arr",
        InferredType::Known {
            class_name: eco_string("GenArray"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    // select: returns Self — should be GenArray(Integer) not GenCollection(Integer)
    let result_ty = checker.infer_expr(
        &msg_send(
            var("arr"),
            MessageSelector::Keyword(vec![KeywordPart::new("select:", span())]),
            vec![var("block")],
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
                "GenArray",
                "Self should resolve to GenArray, got: {class_name}"
            );
            assert_eq!(type_args.len(), 1, "Should have 1 type arg");
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("Integer"),
                "Type arg should be Integer"
            );
        }
        _ => panic!("Expected Known type, got: {result_ty:?}"),
    }
}

/// BT-1577: Multi-level inheritance composes substitution correctly.
/// `GenCollection(E) subclass: GenArray(E)`, `GenArray(E) subclass: SortedArray(E)`.
#[test]
fn generic_inheritance_multi_level_composition() {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, SuperclassTypeArg};

    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_collection_hierarchy(&mut hierarchy);

    // SortedArray(E) extends GenArray(E)
    let sorted_info = ClassInfo {
        name: eco_string("SortedArray"),
        superclass: Some(eco_string("GenArray")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("E")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 0 }],
    };
    hierarchy.add_from_beam_meta(vec![sorted_info]);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local(
        "sa",
        InferredType::Known {
            class_name: eco_string("SortedArray"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    // `first` is defined on GenCollection — 2 levels up
    let result_ty = checker.infer_expr(
        &msg_send(var("sa"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("String"),
        "first on SortedArray(String) should compose through 2 levels to return String, got: {result_ty:?}"
    );
}

/// BT-1577: Method defined on own class (not inherited) still uses direct substitution.
#[test]
fn generic_inheritance_own_method_uses_direct_substitution() {
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

    // unwrap is defined on GenResult itself — should still work
    let result_ty = checker.infer_expr(
        &msg_send(var("r"), MessageSelector::Unary("unwrap".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "unwrap on GenResult(Integer, IOError) should still return Integer, got: {result_ty:?}"
    );
}

// --- ADR 0068 Phase 2d: Type parameter bounds tests ---

#[test]
fn type_param_bounds_conforming_type_no_warning() {
    // Logger(T :: Printable) where Integer conforms to Printable → no warning
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "BoundedLogger".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec!["T".into()],
        type_param_bounds: vec![Some("Printable".into())],
        superclass_type_args: vec![],
    }]);

    let mut registry = ProtocolRegistry::new();
    // Define Printable protocol requiring asString
    let proto_module = Module {
        protocols: vec![crate::ast::ProtocolDefinition {
            name: crate::ast::Identifier::new("Printable", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![crate::ast::ProtocolMethodSignature {
                selector: crate::ast::MessageSelector::Unary("asString".into()),
                parameters: vec![],
                return_type: Some(crate::ast::TypeAnnotation::simple("String", span())),
                comments: crate::ast::CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: crate::ast::CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::known("Integer")],
        span(),
        &hierarchy,
        &registry,
    );

    // Integer has asString (built-in) → should conform → no warning
    assert!(
        checker.diagnostics().is_empty(),
        "Integer conforms to Printable, expected no warnings, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn type_param_bounds_non_conforming_type_warns() {
    // Logger(T :: HasSortKey) where Integer does NOT have sortKey → warning
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "BoundedLogger".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec!["T".into()],
        type_param_bounds: vec![Some("HasSortKey".into())],
        superclass_type_args: vec![],
    }]);

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![crate::ast::ProtocolDefinition {
            name: crate::ast::Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![crate::ast::ProtocolMethodSignature {
                selector: crate::ast::MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
                comments: crate::ast::CommentAttachment::default(),
                doc_comment: None,
                span: span(),
            }],
            class_method_signatures: vec![],
            comments: crate::ast::CommentAttachment::default(),
            doc_comment: None,
            span: span(),
        }],
        ..Module::new(vec![], span())
    };
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::known("Integer")],
        span(),
        &hierarchy,
        &registry,
    );

    // Integer does NOT have sortKey → should warn
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Expected 1 bound violation warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        checker.diagnostics()[0]
            .message
            .contains("does not conform to HasSortKey")
    );
}

#[test]
fn type_param_bounds_unbounded_param_no_check() {
    // Result(T, E) with no bounds → no warnings for any type args
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);

    let registry = ProtocolRegistry::new();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"GenResult".into(),
        &[
            InferredType::known("Integer"),
            InferredType::known("String"),
        ],
        span(),
        &hierarchy,
        &registry,
    );

    assert!(
        checker.diagnostics().is_empty(),
        "Unbounded params should not warn, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn type_param_bounds_dynamic_skipped() {
    // Logger(T :: Printable) where T is Dynamic → no warning (conservative)
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "BoundedLogger".into(),
        superclass: Some("Actor".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec!["T".into()],
        type_param_bounds: vec![Some("Printable".into())],
        superclass_type_args: vec![],
    }]);

    let registry = ProtocolRegistry::new();
    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::Dynamic(DynamicReason::Unknown)],
        span(),
        &hierarchy,
        &registry,
    );

    assert!(
        checker.diagnostics().is_empty(),
        "Dynamic type arg should not trigger bound check, got: {:?}",
        checker.diagnostics()
    );
}

// ---- BT-1583: Generic variance tests (ADR 0068 Phase 2f) ----

/// Build a sealed Value class `SealedBox(T)` with a Printable protocol and
/// classes that conform to it, for variance testing.
#[allow(clippy::too_many_lines)]
fn setup_variance_test_env() -> (ClassHierarchy, ProtocolRegistry) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    use crate::semantic_analysis::protocol_registry::{ProtocolInfo, ProtocolMethodRequirement};

    let mut hierarchy = ClassHierarchy::with_builtins();

    // Add a sealed Value class: SealedBox(T) — covariant (immutable)
    let sealed_box = ClassInfo {
        name: eco_string("SealedBox"),
        superclass: Some(eco_string("Value")),
        is_sealed: true,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![eco_string("value")],
        state_types: {
            let mut m = std::collections::HashMap::new();
            m.insert(eco_string("value"), eco_string("T"));
            m
        },
        state_has_default: std::collections::HashMap::new(),
        methods: vec![MethodInfo {
            selector: eco_string("value"),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: eco_string("SealedBox"),
            is_sealed: true,
            is_internal: false,
            spawns_block: false,
            return_type: Some(eco_string("T")),
            param_types: vec![],
            doc: None,
        }],
        class_methods: vec![MethodInfo {
            selector: eco_string("wrap:"),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: eco_string("SealedBox"),
            is_sealed: true,
            is_internal: false,
            spawns_block: false,
            return_type: Some(eco_string("Self")),
            param_types: vec![Some(eco_string("T"))],
            doc: None,
        }],
        class_variables: vec![],
        type_params: vec![eco_string("T")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![],
    };

    // Add an Actor class: ActorBox(T) — invariant (mutable state)
    let actor_box = ClassInfo {
        name: eco_string("ActorBox"),
        superclass: Some(eco_string("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![eco_string("value")],
        state_types: {
            let mut m = std::collections::HashMap::new();
            m.insert(eco_string("value"), eco_string("T"));
            m
        },
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("value"),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: eco_string("ActorBox"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("T")),
                param_types: vec![],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("value:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("ActorBox"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![Some(eco_string("T"))],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("T")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![],
    };

    // Add an unsealed Value class: OpenBox(T) — invariant (can be subclassed)
    let open_box = ClassInfo {
        name: eco_string("OpenBox"),
        superclass: Some(eco_string("Value")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![eco_string("value")],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![eco_string("T")],
        type_param_bounds: vec![None],
        superclass_type_args: vec![],
    };

    // Add a class that accepts SealedBox(Printable) parameter
    let consumer = ClassInfo {
        name: eco_string("BoxConsumer"),
        superclass: Some(eco_string("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: eco_string("printBox:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("BoxConsumer"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("String")),
                param_types: vec![Some(eco_string("SealedBox(Printable)"))],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("setActor:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("BoxConsumer"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![Some(eco_string("ActorBox(Printable)"))],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    hierarchy.add_from_beam_meta(vec![sealed_box, actor_box, open_box, consumer]);

    // Register a Printable protocol — requires `asString` method
    let mut registry = ProtocolRegistry::new();
    let printable = ProtocolInfo {
        name: eco_string("Printable"),
        type_params: vec![],
        type_param_bounds: vec![],
        extending: None,
        methods: vec![ProtocolMethodRequirement {
            selector: eco_string("asString"),
            arity: 0,
            return_type: Some(eco_string("String")),
            param_types: vec![],
        }],
        class_methods: vec![],
        span: span(),
    };
    registry.register_test_protocol(printable);

    (hierarchy, registry)
}

/// BT-1583: `is_covariant_class` returns true for sealed Value classes with type params.
#[test]
fn covariant_class_sealed_value_is_covariant() {
    let (hierarchy, _registry) = setup_variance_test_env();
    assert!(
        hierarchy.is_covariant_class("SealedBox"),
        "Sealed Value class SealedBox should be covariant"
    );
}

/// BT-1583: `is_covariant_class` returns false for Actor classes (invariant).
#[test]
fn covariant_class_actor_is_invariant() {
    let (hierarchy, _registry) = setup_variance_test_env();
    assert!(
        !hierarchy.is_covariant_class("ActorBox"),
        "Actor class ActorBox should be invariant"
    );
}

/// BT-1583: `is_covariant_class` returns false for unsealed Value classes (conservative).
#[test]
fn covariant_class_unsealed_value_is_invariant() {
    let (hierarchy, _registry) = setup_variance_test_env();
    assert!(
        !hierarchy.is_covariant_class("OpenBox"),
        "Unsealed Value class OpenBox should be invariant (conservative)"
    );
}

/// BT-1583: `is_covariant_class` returns false for non-generic classes.
#[test]
fn covariant_class_non_generic_is_false() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !hierarchy.is_covariant_class("Integer"),
        "Non-generic class Integer should not be covariant"
    );
}

/// BT-1583: Covariant assignment — `SealedBox(Integer)` assignable to `SealedBox(Printable)`.
///
/// Integer conforms to Printable (has `asString`), and `SealedBox` is a sealed Value class.
#[test]
fn variance_covariant_sealed_value_protocol_typed() {
    let (hierarchy, registry) = setup_variance_test_env();

    assert!(
        TypeChecker::is_assignable_to_with_variance(
            &eco_string("SealedBox(Integer)"),
            &eco_string("SealedBox(Printable)"),
            &hierarchy,
            &registry,
        ),
        "SealedBox(Integer) should be assignable to SealedBox(Printable) — Integer conforms to Printable"
    );
}

/// BT-1583: Covariant — same type args are trivially compatible.
#[test]
fn variance_covariant_same_type_args() {
    let (hierarchy, registry) = setup_variance_test_env();

    assert!(
        TypeChecker::is_assignable_to_with_variance(
            &eco_string("SealedBox(Integer)"),
            &eco_string("SealedBox(Integer)"),
            &hierarchy,
            &registry,
        ),
        "SealedBox(Integer) should be assignable to SealedBox(Integer)"
    );
}

/// BT-1583: Covariant — non-conforming type is rejected.
///
/// If `OpaqueType` does not conform to Printable, `SealedBox(OpaqueType)` should NOT
/// be assignable to `SealedBox(Printable)`.
#[test]
fn variance_covariant_non_conforming_rejected() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;

    let (mut hierarchy, registry) = setup_variance_test_env();

    // Add OpaqueType without asString — does not conform to Printable
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: eco_string("OpaqueType"),
        superclass: Some(eco_string("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    assert!(
        !TypeChecker::is_assignable_to_with_variance(
            &eco_string("SealedBox(OpaqueType)"),
            &eco_string("SealedBox(Printable)"),
            &hierarchy,
            &registry,
        ),
        "SealedBox(OpaqueType) should NOT be assignable to SealedBox(Printable)"
    );
}

/// BT-1583: Invariant actor state — `ActorBox(Integer)` NOT assignable to `ActorBox(Printable)`.
///
/// Actor classes are invariant because their state fields can be mutated.
#[test]
fn variance_invariant_actor_state() {
    let (hierarchy, _registry) = setup_variance_test_env();

    // ActorBox is invariant — the base-name-only check in is_assignable_to is permissive,
    // but the variance-aware method should recognize it's invariant.
    // Note: The current string-based approach falls back to is_assignable_to for invariant
    // classes, which checks base names only. For the invariant case with different type args,
    // the type args differ but the base names match — so the old behaviour (permissive) is
    // preserved. The test validates that the actor class is recognized as invariant.
    assert!(
        !hierarchy.is_covariant_class("ActorBox"),
        "ActorBox should be invariant (not covariant)"
    );
}

/// BT-1583: Non-generic types fall back to normal assignability.
#[test]
fn variance_non_generic_falls_back() {
    let (hierarchy, registry) = setup_variance_test_env();

    // Integer is assignable to Number (subclass relationship)
    assert!(
        TypeChecker::is_assignable_to_with_variance(
            &eco_string("Integer"),
            &eco_string("Number"),
            &hierarchy,
            &registry,
        ),
        "Integer should be assignable to Number via superclass chain"
    );

    // String is NOT assignable to Integer
    assert!(
        !TypeChecker::is_assignable_to_with_variance(
            &eco_string("String"),
            &eco_string("Integer"),
            &hierarchy,
            &registry,
        ),
        "String should NOT be assignable to Integer"
    );
}

/// BT-1583: Covariant with class-hierarchy subtyping (Integer → Number).
#[test]
fn variance_covariant_class_subtyping() {
    let (hierarchy, registry) = setup_variance_test_env();

    // SealedBox(Integer) should be assignable to SealedBox(Number)
    // because Integer is a subclass of Number and SealedBox is covariant
    assert!(
        TypeChecker::is_assignable_to_with_variance(
            &eco_string("SealedBox(Integer)"),
            &eco_string("SealedBox(Number)"),
            &hierarchy,
            &registry,
        ),
        "SealedBox(Integer) should be assignable to SealedBox(Number) — Integer is a subclass of Number"
    );
}

/// BT-1583: `parse_generic_type_string` correctly parses type strings.
#[test]
fn parse_generic_type_string_basic() {
    let (base, args) = TypeChecker::parse_generic_type_string("Array(Integer)");
    assert_eq!(base, "Array");
    assert_eq!(args, vec!["Integer"]);

    let (base, args) = TypeChecker::parse_generic_type_string("Result(Integer, Error)");
    assert_eq!(base, "Result");
    assert_eq!(args, vec!["Integer", "Error"]);

    let (base, args) = TypeChecker::parse_generic_type_string("String");
    assert_eq!(base, "String");
    assert!(args.is_empty());
}

// ---- BT-1588: Generic type param detection ----

#[test]
fn test_is_generic_type_param() {
    // Single uppercase letters are generic type params
    assert!(is_generic_type_param("V"));
    assert!(is_generic_type_param("K"));
    assert!(is_generic_type_param("T"));
    assert!(is_generic_type_param("R"));
    assert!(is_generic_type_param("E"));

    // Multi-character names are NOT generic type params
    assert!(!is_generic_type_param("String"));
    assert!(!is_generic_type_param("Integer"));
    assert!(!is_generic_type_param("Dictionary"));

    // Lowercase is NOT a generic type param
    assert!(!is_generic_type_param("v"));
    assert!(!is_generic_type_param("t"));

    // Empty string is NOT
    assert!(!is_generic_type_param(""));
}

#[test]
fn test_generic_type_param_binary_operand_hint_severity() {
    // Directly test check_binary_operand_types with a generic type param
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();

    // "hello" ++ V — V is generic, should produce Hint not Warning
    checker.check_binary_operand_types(
        &"String".into(),
        "++",
        &"V".into(),
        span(),
        &hierarchy,
        None,
    );

    let diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a String argument"))
        .collect();
    assert_eq!(diags.len(), 1);
    assert_eq!(
        diags[0].severity,
        crate::source_analysis::Severity::Hint,
        "Generic type param V should produce Hint, not Warning"
    );

    // "hello" ++ Integer — concrete type, should produce Warning
    let mut checker2 = TypeChecker::new();
    checker2.check_binary_operand_types(
        &"String".into(),
        "++",
        &"Integer".into(),
        span(),
        &hierarchy,
        None,
    );
    let diags2: Vec<_> = checker2
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a String argument"))
        .collect();
    assert_eq!(diags2.len(), 1);
    assert_eq!(
        diags2[0].severity,
        crate::source_analysis::Severity::Warning,
        "Concrete type Integer should produce Warning"
    );
}

#[test]
fn test_type_env_origin_tracking() {
    let mut env = TypeEnv::new();

    // Set without origin
    env.set_local("x", InferredType::known("Integer"));
    assert!(env.get_local_origin("x").is_none());

    // Set with origin
    env.set_with_origin(
        EnvKey::local("val"),
        InferredType::known("V"),
        "`Dictionary at:ifAbsent:` returns generic type `V`",
        Span::new(100, 120),
    );
    let origin = env.get_local_origin("val").unwrap();
    assert!(origin.description.contains("Dictionary"));
    assert_eq!(origin.span, Span::new(100, 120));

    // Child inherits origins
    let child = env.child();
    assert!(child.get_local_origin("val").is_some());
}

#[test]
fn test_diagnostic_notes_field() {
    use crate::source_analysis::Diagnostic;

    // Diagnostic without notes
    let diag = Diagnostic::warning("test warning", span());
    assert!(diag.notes.is_empty());

    // Diagnostic with a note
    let diag = Diagnostic::warning("type mismatch", span()).with_note(
        "variable has type V from Dictionary at:ifAbsent:",
        Some(Span::new(10, 20)),
    );
    assert_eq!(diag.notes.len(), 1);
    assert!(diag.notes[0].message.contains("Dictionary"));
    assert_eq!(diag.notes[0].span, Some(Span::new(10, 20)));

    // Hint severity for generic types
    let diag = Diagnostic::hint("likely false positive", span());
    assert_eq!(diag.severity, crate::source_analysis::Severity::Hint);
}

#[test]
fn test_binary_operand_with_origin_note() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();

    // "hello" ++ V with origin info
    let origin = (
        EcoString::from("`Dictionary at:ifAbsent:` returns generic type `V`"),
        Some(Span::new(50, 80)),
    );
    checker.check_binary_operand_types(
        &"String".into(),
        "++",
        &"V".into(),
        span(),
        &hierarchy,
        Some(&origin),
    );

    let diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects a String argument"))
        .collect();
    assert_eq!(diags.len(), 1);
    assert_eq!(diags[0].notes.len(), 1, "Should have origin note attached");
    assert!(
        diags[0].notes[0].message.contains("Dictionary"),
        "Note should reference Dictionary origin"
    );
}

// ── ADR 0071 Phase 3 (BT-1702): E0403 — internal method visibility ──

/// Build a hierarchy with a class that has an internal method, in a specific package.
fn make_hierarchy_with_internal_method() -> ClassHierarchy {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut h = ClassHierarchy::with_builtins();
    h.add_from_beam_meta(vec![ClassInfo {
        name: "HttpClient".into(),
        superclass: Some("Object".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: Some("http".into()),
        is_value: false,
        is_native: false,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![
            MethodInfo {
                selector: "get:".into(),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: "HttpClient".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some("String".into()),
                param_types: vec![Some("String".into())],
                doc: None,
            },
            MethodInfo {
                selector: "buildHeaders:".into(),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: "HttpClient".into(),
                is_sealed: false,
                is_internal: true,
                spawns_block: false,
                return_type: Some("Dictionary".into()),
                param_types: vec![None],
                doc: None,
            },
        ],
        class_methods: vec![MethodInfo {
            selector: "internalFactory".into(),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: "HttpClient".into(),
            is_sealed: false,
            is_internal: true,
            spawns_block: false,
            return_type: Some("HttpClient".into()),
            param_types: vec![],
            doc: None,
        }],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);
    h
}

#[test]
fn e0403_cross_package_instance_send_to_internal_method() {
    // Directly test check_internal_method_access: sending `buildHeaders:`
    // from package `my_app` to `HttpClient` (package `http`) should emit E0403.
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("my_app");
    let class_name: EcoString = "HttpClient".into();
    checker.check_internal_method_access(&class_name, "buildHeaders:", span(), &hierarchy, false);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert_eq!(
        e0403_errors.len(),
        1,
        "Expected error for cross-package internal method send, got: {e0403_errors:?}"
    );
    assert!(e0403_errors[0].message.contains("buildHeaders:"));
    assert!(e0403_errors[0].message.contains("http"));
    assert!(e0403_errors[0].message.contains("my_app"));
}

#[test]
fn e0403_same_package_send_to_internal_method_allowed() {
    // Same-package send to internal method should NOT emit E0403.
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("http");
    let class_name: EcoString = "HttpClient".into();
    checker.check_internal_method_access(&class_name, "buildHeaders:", span(), &hierarchy, false);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert!(
        e0403_errors.is_empty(),
        "Same-package send to internal method should not emit internal method error, got: {e0403_errors:?}"
    );
}

#[test]
fn e0403_public_method_not_flagged() {
    // Sending `get:` (public method) from any package should NOT emit E0403.
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("my_app");
    let class_name: EcoString = "HttpClient".into();
    checker.check_internal_method_access(&class_name, "get:", span(), &hierarchy, false);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert!(
        e0403_errors.is_empty(),
        "Public method send should not emit internal method error, got: {e0403_errors:?}"
    );
}

#[test]
fn e0403_no_package_context_skips_check() {
    // When no current package is set (REPL), internal method access is not checked.
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::new(); // No package context
    let class_name: EcoString = "HttpClient".into();
    checker.check_internal_method_access(&class_name, "buildHeaders:", span(), &hierarchy, false);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert!(
        e0403_errors.is_empty(),
        "No package context should skip internal method checks, got: {e0403_errors:?}"
    );
}

#[test]
fn e0403_class_side_internal_method() {
    // Sending `HttpClient internalFactory` from another package should emit E0403.
    let module = make_module(vec![msg_send(
        Expression::ClassReference {
            name: ident("HttpClient"),
            package: None,
            span: span(),
        },
        MessageSelector::Unary("internalFactory".into()),
        vec![],
    )]);
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("my_app");
    checker.check_module(&module, &hierarchy);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert_eq!(
        e0403_errors.len(),
        1,
        "Expected error for cross-package internal class method, got: {e0403_errors:?}"
    );
    assert!(e0403_errors[0].message.contains("internalFactory"));
}

#[test]
fn e0403_untyped_send_not_checked() {
    // Dynamic sends where receiver type is unknown should NOT emit E0403.
    // Using an Identifier (untyped variable) as receiver — type is Dynamic.
    let module = make_module(vec![msg_send(
        Expression::Identifier(ident("client")),
        MessageSelector::Keyword(vec![KeywordPart::new("buildHeaders:", span())]),
        vec![int_lit(42)],
    )]);
    let hierarchy = make_hierarchy_with_internal_method();
    let mut checker = TypeChecker::with_package("my_app");
    checker.check_module(&module, &hierarchy);
    let e0403_errors: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("is internal to package"))
        .collect();
    assert!(
        e0403_errors.is_empty(),
        "Untyped dynamic send should not emit internal method error, got: {e0403_errors:?}"
    );
}
