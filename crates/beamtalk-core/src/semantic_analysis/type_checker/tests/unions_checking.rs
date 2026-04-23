// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Union arg/field/protocol/bounds checking and builtin param unions (BT-1834, BT-1835, BT-1877).

use super::super::*;
use super::common::*;

// --- Argument type checking with unions ---

#[test]
fn union_arg_all_compatible_no_warning() {
    // Union(Integer | Float) passed to param expecting Number → all compatible → no warning
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "Calculator".into(),
        superclass: Some("Object".into()),
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
        methods: vec![MethodInfo {
            selector: "compute:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Calculator".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("Number".into()),
            param_types: vec![Some("Number".into())],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &"Calculator".into(),
        "compute:",
        &[InferredType::simple_union(&["Integer", "Float"])],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "All union members (Integer, Float) are Numbers — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_arg_none_compatible_warns() {
    // Union(String | Symbol) passed to param expecting Integer → none compatible → warning
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "Calculator".into(),
        superclass: Some("Object".into()),
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
        methods: vec![MethodInfo {
            selector: "compute:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Calculator".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("Integer".into()),
            param_types: vec![Some("Integer".into())],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &"Calculator".into(),
        "compute:",
        &[InferredType::simple_union(&["String", "Symbol"])],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "No union member is Integer — expected 1 warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(checker.diagnostics()[0].message.contains("expects Integer"));
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Warning
    );
}

#[test]
fn union_arg_mixed_compatible_hints() {
    // Union(Integer | String) passed to param expecting Number → Integer matches, String doesn't → hint
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "Calculator".into(),
        superclass: Some("Object".into()),
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
        methods: vec![MethodInfo {
            selector: "compute:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Calculator".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("Number".into()),
            param_types: vec![Some("Number".into())],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &"Calculator".into(),
        "compute:",
        &[InferredType::simple_union(&["Integer", "String"])],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Mixed union — expected 1 hint, got: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Hint
    );
    assert!(checker.diagnostics()[0].message.contains("expects Number"));
}

#[test]
fn union_arg_dynamic_still_skips() {
    // Dynamic arg should still skip (no regression)
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "Calculator".into(),
        superclass: Some("Object".into()),
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
        methods: vec![MethodInfo {
            selector: "compute:".into(),
            arity: 1,
            kind: MethodKind::Primary,
            defined_in: "Calculator".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: Some("Integer".into()),
            param_types: vec![Some("Integer".into())],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut checker = TypeChecker::new();
    checker.check_argument_types(
        &"Calculator".into(),
        "compute:",
        &[InferredType::Dynamic(DynamicReason::Unknown)],
        span(),
        &hierarchy,
        false,
        None,
        None,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "Dynamic arg should skip — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

// ── BT-2038: Class literal compatibility with Behaviour/Class/Object parameters ──
//
// A class reference like `TestCase` is a first-class class value whose runtime
// type is `TestCase class` (a `Metaclass`, which inherits from `Class` →
// `Behaviour` → `Object` → `ProtoObject`). When such a literal is passed to a
// parameter declared as `Behaviour`, `Class`, `Object`, or `ProtoObject`, the
// argument conformance check must accept it even though the class's *instance*
// hierarchy (e.g., `TestCase` → `Value` → `Object`) does not pass through
// `Behaviour`.

fn test_hierarchy_with_class_literal_arg(
    param_type: &str,
) -> crate::semantic_analysis::class_hierarchy::ClassHierarchy {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![
        ClassInfo {
            name: "TestCase".into(),
            superclass: Some("Value".into()),
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
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        },
        ClassInfo {
            name: "ClassChecker".into(),
            superclass: Some("Object".into()),
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
                selector: "accept:".into(),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: "ClassChecker".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some("Boolean".into()),
                param_types: vec![Some(param_type.into())],
                doc: None,
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        },
    ]);
    hierarchy
}

#[test]
fn class_literal_arg_accepted_for_behaviour_param() {
    // BT-2038: Passing a class literal (TestCase) to a param expecting Behaviour
    // should not warn — class objects flow through Metaclass → Class → Behaviour.
    let hierarchy = test_hierarchy_with_class_literal_arg("Behaviour");
    let mut checker = TypeChecker::new();
    let class_ref_arg = class_ref("TestCase");
    checker.check_argument_types(
        &"ClassChecker".into(),
        "accept:",
        &[InferredType::known("TestCase")],
        span(),
        &hierarchy,
        false,
        Some(std::slice::from_ref(&class_ref_arg)),
        None,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "Class literal TestCase should type-check as Behaviour, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn class_literal_arg_accepted_for_class_param() {
    // A class literal should satisfy a Class-typed parameter.
    let hierarchy = test_hierarchy_with_class_literal_arg("Class");
    let mut checker = TypeChecker::new();
    let class_ref_arg = class_ref("TestCase");
    checker.check_argument_types(
        &"ClassChecker".into(),
        "accept:",
        &[InferredType::known("TestCase")],
        span(),
        &hierarchy,
        false,
        Some(std::slice::from_ref(&class_ref_arg)),
        None,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "Class literal TestCase should type-check as Class, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn class_literal_arg_accepted_for_object_param() {
    // Object sits above Behaviour in the metaclass tower, so a class
    // literal must satisfy an Object-typed parameter.
    let hierarchy = test_hierarchy_with_class_literal_arg("Object");
    let mut checker = TypeChecker::new();
    let class_ref_arg = class_ref("TestCase");
    checker.check_argument_types(
        &"ClassChecker".into(),
        "accept:",
        &[InferredType::known("TestCase")],
        span(),
        &hierarchy,
        false,
        Some(std::slice::from_ref(&class_ref_arg)),
        None,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "Class literal TestCase should type-check as Object, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn class_literal_arg_accepted_for_protoobject_param() {
    // ProtoObject is the root of the metaclass tower — every class literal
    // must satisfy it.
    let hierarchy = test_hierarchy_with_class_literal_arg("ProtoObject");
    let mut checker = TypeChecker::new();
    let class_ref_arg = class_ref("TestCase");
    checker.check_argument_types(
        &"ClassChecker".into(),
        "accept:",
        &[InferredType::known("TestCase")],
        span(),
        &hierarchy,
        false,
        Some(std::slice::from_ref(&class_ref_arg)),
        None,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "Class literal TestCase should type-check as ProtoObject, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn class_literal_arg_rejected_for_unrelated_param() {
    // A class literal whose metaclass chain does NOT include the expected
    // type (e.g., expected Integer) should still warn.
    let hierarchy = test_hierarchy_with_class_literal_arg("Integer");
    let mut checker = TypeChecker::new();
    let class_ref_arg = class_ref("TestCase");
    checker.check_argument_types(
        &"ClassChecker".into(),
        "accept:",
        &[InferredType::known("TestCase")],
        span(),
        &hierarchy,
        false,
        Some(std::slice::from_ref(&class_ref_arg)),
        None,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Class literal TestCase should NOT type-check as Integer"
    );
    assert!(
        checker.diagnostics()[0].message.contains("expects Integer"),
        "got: {}",
        checker.diagnostics()[0].message
    );
}

#[test]
fn instance_identifier_still_rejected_for_class_param() {
    // Regression guard (CodeRabbit on PR #2071): the BT-1877 `expected == "Class"`
    // shortcut in `is_type_compatible` previously accepted any known class name
    // unconditionally. With BT-2038's metaclass-tower check handling class
    // literals, that shortcut must stay scoped to class-literal arguments —
    // a plain identifier typed as `TestCase` is an instance, not a class,
    // and should not satisfy a `:: Class` parameter.
    let hierarchy = test_hierarchy_with_class_literal_arg("Class");
    let mut checker = TypeChecker::new();
    let ident_arg = var("aTestCase");
    checker.check_argument_types(
        &"ClassChecker".into(),
        "accept:",
        &[InferredType::known("TestCase")],
        span(),
        &hierarchy,
        false,
        Some(std::slice::from_ref(&ident_arg)),
        None,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "A TestCase instance (not class literal) should not type-check as Class, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        checker.diagnostics()[0].message.contains("expects Class"),
        "got: {}",
        checker.diagnostics()[0].message
    );
}

#[test]
fn instance_identifier_still_rejected_for_behaviour_param() {
    // Regression guard: the class-literal escape must NOT apply when the
    // argument is an identifier of a user class type — only class literals
    // carry the "is-a Behaviour" semantics.
    let hierarchy = test_hierarchy_with_class_literal_arg("Behaviour");
    let mut checker = TypeChecker::new();
    let ident_arg = var("aTestCase");
    checker.check_argument_types(
        &"ClassChecker".into(),
        "accept:",
        &[InferredType::known("TestCase")],
        span(),
        &hierarchy,
        false,
        Some(std::slice::from_ref(&ident_arg)),
        None,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "A TestCase instance (not class literal) should not type-check as Behaviour"
    );
    assert!(
        checker.diagnostics()[0]
            .message
            .contains("expects Behaviour"),
        "got: {}",
        checker.diagnostics()[0].message
    );
}

// --- Field assignment with unions ---

#[test]
fn union_field_assign_all_compatible_no_warning() {
    // state: count :: Number; self.count := (Integer | Float) → all compatible → no warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Number", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method(
        "setCount",
        vec![field_assign("count", int_lit(1))], // placeholder, we test directly below
    );
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Counter"));
    checker.check_field_assignment(
        &ident("count"),
        &InferredType::simple_union(&["Integer", "Float"]),
        span(),
        &hierarchy,
        &env,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "All union members compatible with Number — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_field_assign_none_compatible_warns() {
    // state: count :: Integer; self.count := (String | Symbol) → none compatible → warning
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Integer", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method("setCount", vec![field_assign("count", int_lit(1))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Counter"));
    checker.check_field_assignment(
        &ident("count"),
        &InferredType::simple_union(&["String", "Symbol"]),
        span(),
        &hierarchy,
        &env,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "No union member is Integer — expected 1 warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(checker.diagnostics()[0].message.contains("Type mismatch"));
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Warning
    );
}

#[test]
fn union_field_assign_mixed_compatible_hints() {
    // state: count :: Number; self.count := (Integer | String) → Integer matches, String doesn't → hint
    let state = vec![StateDeclaration::with_type_and_default(
        ident("count"),
        TypeAnnotation::simple("Number", span()),
        int_lit(0),
        span(),
    )];
    let method = make_method("setCount", vec![field_assign("count", int_lit(1))]);
    let class = counter_class_with_typed_state(vec![method], state);
    let module = make_module_with_classes(vec![], vec![class]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("self", InferredType::known("Counter"));
    checker.check_field_assignment(
        &ident("count"),
        &InferredType::simple_union(&["Integer", "String"]),
        span(),
        &hierarchy,
        &env,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Mixed union — expected 1 hint, got: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Hint
    );
}

// --- Protocol conformance with unions ---

#[test]
fn union_protocol_all_conform_no_warning() {
    // Integer | String both conform to Printable (both have asString) → no warning
    let hierarchy = ClassHierarchy::with_builtins();

    let mut registry = ProtocolRegistry::new();
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
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_protocol_argument_conformance(
        &InferredType::simple_union(&["Integer", "String"]),
        "Printable",
        span(),
        &hierarchy,
        &registry,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "Both Integer and String conform to Printable — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_protocol_none_conform_warns() {
    // Integer | String, neither has sortKey → warning
    let hierarchy = ClassHierarchy::with_builtins();

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
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
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_protocol_argument_conformance(
        &InferredType::simple_union(&["Integer", "String"]),
        "HasSortKey",
        span(),
        &hierarchy,
        &registry,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "No union member conforms to HasSortKey — expected 1 warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        checker.diagnostics()[0]
            .message
            .contains("does not conform to protocol HasSortKey")
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Warning
    );
}

#[test]
fn union_protocol_mixed_conformance_hints() {
    // Build a protocol "HasSortKey" requiring sortKey.
    // Integer has sortKey (we register it), String does not → hint.
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    // Add sortKey to Integer so it conforms
    hierarchy.add_from_beam_meta(vec![ClassInfo {
        name: "SortableInt".into(),
        superclass: Some("Integer".into()),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: true,
        is_native: false,
        state: vec![],
        state_types: HashMap::new(),
        state_has_default: HashMap::new(),
        methods: vec![MethodInfo {
            selector: "sortKey".into(),
            arity: 0,
            kind: MethodKind::Primary,
            defined_in: "SortableInt".into(),
            is_sealed: false,
            is_internal: false,
            spawns_block: false,
            return_type: None,
            param_types: vec![],
            doc: None,
        }],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }]);

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
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
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    // SortableInt conforms (has sortKey), String does not
    checker.check_protocol_argument_conformance(
        &InferredType::simple_union(&["SortableInt", "String"]),
        "HasSortKey",
        span(),
        &hierarchy,
        &registry,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Mixed conformance — expected 1 hint, got: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Hint
    );
    assert!(checker.diagnostics()[0].message.contains("Not all members"));
}

// --- Type parameter bounds with unions ---

#[test]
fn union_type_param_bounds_all_conform_no_warning() {
    // Logger(T :: Printable) where T = Integer | String → both conform → no warning
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
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::simple_union(&["Integer", "String"])],
        span(),
        &hierarchy,
        &registry,
    );
    assert!(
        checker.diagnostics().is_empty(),
        "All union members conform to Printable — no warning expected, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_type_param_bounds_none_conform_warns() {
    // Logger(T :: HasSortKey) where T = Integer | String → neither conforms → warning
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
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
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
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::simple_union(&["Integer", "String"])],
        span(),
        &hierarchy,
        &registry,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "No union member conforms to HasSortKey — expected 1 warning, got: {:?}",
        checker.diagnostics()
    );
    assert!(
        checker.diagnostics()[0]
            .message
            .contains("does not conform to HasSortKey")
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Warning
    );
}

#[test]
fn union_type_param_bounds_mixed_conformance_hints() {
    // Logger(T :: HasSortKey) where T = SortableInt | String → SortableInt conforms, String doesn't → hint
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};
    let mut hierarchy = ClassHierarchy::with_builtins();
    hierarchy.add_from_beam_meta(vec![
        ClassInfo {
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
        },
        ClassInfo {
            name: "SortableInt".into(),
            superclass: Some("Integer".into()),
            is_sealed: false,
            is_abstract: false,
            is_typed: false,
            is_internal: false,
            package: None,
            is_value: true,
            is_native: false,
            state: vec![],
            state_types: HashMap::new(),
            state_has_default: HashMap::new(),
            methods: vec![MethodInfo {
                selector: "sortKey".into(),
                arity: 0,
                kind: MethodKind::Primary,
                defined_in: "SortableInt".into(),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: None,
                param_types: vec![],
                doc: None,
            }],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            type_param_bounds: vec![],
            superclass_type_args: vec![],
        },
    ]);

    let mut registry = ProtocolRegistry::new();
    let proto_module = Module {
        protocols: vec![ProtocolDefinition {
            name: Identifier::new("HasSortKey", span()),
            type_params: vec![],
            extending: None,
            method_signatures: vec![ProtocolMethodSignature {
                selector: MessageSelector::Unary("sortKey".into()),
                parameters: vec![],
                return_type: None,
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
    registry.register_module(&proto_module, &hierarchy);

    let mut checker = TypeChecker::new();
    checker.check_type_param_bounds(
        &"BoundedLogger".into(),
        &[InferredType::simple_union(&["SortableInt", "String"])],
        span(),
        &hierarchy,
        &registry,
    );
    assert_eq!(
        checker.diagnostics().len(),
        1,
        "Mixed conformance — expected 1 hint, got: {:?}",
        checker.diagnostics()
    );
    assert_eq!(
        checker.diagnostics()[0].severity,
        crate::source_analysis::Severity::Hint
    );
    assert!(
        checker.diagnostics()[0]
            .message
            .contains("not all members conform to HasSortKey")
    );
}

// ---- BT-1834: Generic return type resolution ----

/// BT-1834: List(E) first returns E, resolved to concrete type via substitution.
#[test]
fn generic_list_first_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Simulate a variable typed as List(String)
    env.set_local(
        "names",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(var("names"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "List(String) first should return String, got: {result:?}"
    );
}

/// BT-1834: List(E) last returns E.
#[test]
fn generic_list_last_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set_local(
        "nums",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(var("nums"), MessageSelector::Unary("last".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Integer"),
        "List(Integer) last should return Integer, got: {result:?}"
    );
}

/// BT-1834: List(E) at: returns E.
#[test]
fn generic_list_at_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set_local(
        "items",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("Float")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("items"),
            MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
            vec![int_lit(1)],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Float"),
        "List(Float) at: should return Float, got: {result:?}"
    );
}

/// BT-1834: Array(E) at: returns E (existing functionality, regression check).
#[test]
fn generic_array_at_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set_local(
        "arr",
        InferredType::Known {
            class_name: eco_string("Array"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("arr"),
            MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
            vec![int_lit(1)],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "Array(String) at: should return String, got: {result:?}"
    );
}

/// BT-1834: Dictionary(K, V) at: returns V.
#[test]
fn generic_dictionary_at_returns_value_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set_local(
        "dict",
        InferredType::Known {
            class_name: eco_string("Dictionary"),
            type_args: vec![
                InferredType::known("String"),
                InferredType::known("Integer"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("dict"),
            MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
            vec![str_lit("key")],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Integer"),
        "Dictionary(String, Integer) at: should return Integer, got: {result:?}"
    );
}

/// BT-1834: Nested generics — Dictionary(String, Array(Integer)) at: returns Array(Integer).
#[test]
fn generic_nested_dictionary_at_returns_nested_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set_local(
        "nested",
        InferredType::Known {
            class_name: eco_string("Dictionary"),
            type_args: vec![
                InferredType::known("String"),
                InferredType::Known {
                    class_name: eco_string("Array"),
                    type_args: vec![InferredType::known("Integer")],
                    provenance: TypeProvenance::Declared(span()),
                },
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("nested"),
            MessageSelector::Keyword(vec![KeywordPart::new("at:", span())]),
            vec![str_lit("key")],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    // Should return Array(Integer)
    match &result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Array");
            assert_eq!(type_args.len(), 1);
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("Integer")
            );
        }
        other => panic!("Expected Known(Array(Integer)), got: {other:?}"),
    }
}

/// BT-1834: Block value returns the last type arg.
#[test]
fn generic_block_value_returns_last_type_arg() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Block(String) — zero-arg block returning String
    env.set_local(
        "blk",
        InferredType::Known {
            class_name: eco_string("Block"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(var("blk"), MessageSelector::Unary("value".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "Block(String) value should return String, got: {result:?}"
    );
}

/// BT-1834: Block(A, R) value: returns R (last type arg).
#[test]
fn generic_block_value_colon_returns_last_type_arg() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Block(Integer, String) — one-arg block taking Integer, returning String
    env.set_local(
        "blk",
        InferredType::Known {
            class_name: eco_string("Block"),
            type_args: vec![
                InferredType::known("Integer"),
                InferredType::known("String"),
            ],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result = checker.infer_expr(
        &msg_send(
            var("blk"),
            MessageSelector::Keyword(vec![KeywordPart::new("value:", span())]),
            vec![int_lit(42)],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "Block(Integer, String) value: should return String, got: {result:?}"
    );
}

/// BT-1834: Unresolvable type params fall back to Dynamic (no regression).
#[test]
fn generic_unresolved_type_param_falls_back_to_dynamic() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    // Bare List without type args — first should be Dynamic
    env.set_local("plain", InferredType::known("List"));

    let result = checker.infer_expr(
        &msg_send(var("plain"), MessageSelector::Unary("first".into()), vec![]),
        &hierarchy,
        &mut env,
        false,
    );
    assert!(
        matches!(result, InferredType::Dynamic(DynamicReason::Unknown)),
        "List (no type args) first should fall back to Dynamic, got: {result:?}"
    );
}

/// BT-1834: Plain param type inference — inject:into: resolves A from initial arg.
#[test]
fn generic_inject_into_resolves_accumulator_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set_local(
        "arr",
        InferredType::Known {
            class_name: eco_string("Array"),
            type_args: vec![InferredType::known("Integer")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    // arr inject: 0 into: [:acc :x | acc + x]
    // A should be inferred as Integer from the initial value argument
    let block = Expression::Block(Block::new(
        vec![
            crate::ast::BlockParameter::new("acc", span()),
            crate::ast::BlockParameter::new("x", span()),
        ],
        vec![bare(msg_send(
            var("acc"),
            MessageSelector::Binary("+".into()),
            vec![var("x")],
        ))],
        span(),
    ));

    let result = checker.infer_expr(
        &msg_send(
            var("arr"),
            MessageSelector::Keyword(vec![
                KeywordPart::new("inject:", span()),
                KeywordPart::new("into:", span()),
            ]),
            vec![int_lit(0), block],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("Integer"),
        "inject: 0 into: should return Integer (inferred from initial), got: {result:?}"
    );
}

/// BT-1834: List(E) detect: returns E.
#[test]
fn generic_list_detect_returns_element_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();

    env.set_local(
        "strs",
        InferredType::Known {
            class_name: eco_string("List"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let block = Expression::Block(Block::new(
        vec![crate::ast::BlockParameter::new("x", span())],
        vec![bare(msg_send(
            var("x"),
            MessageSelector::Binary(">".into()),
            vec![str_lit("a")],
        ))],
        span(),
    ));

    let result = checker.infer_expr(
        &msg_send(
            var("strs"),
            MessageSelector::Keyword(vec![KeywordPart::new("detect:", span())]),
            vec![block],
        ),
        &hierarchy,
        &mut env,
        false,
    );
    assert_eq!(
        result.as_known().map(EcoString::as_str),
        Some("String"),
        "List(String) detect: should return String, got: {result:?}"
    );
}

/// BT-1834: Behaviour superclass returns Behaviour | Nil.
#[test]
fn behaviour_superclass_returns_union_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    let method = hierarchy.find_method("Behaviour", "superclass");
    assert!(method.is_some(), "Behaviour should have superclass method");
    assert_eq!(
        method.unwrap().return_type.as_deref(),
        Some("Behaviour | Nil"),
        "superclass should return Behaviour | Nil"
    );
}

// --- BT-1835: Union syntax in builtin param_types ---

#[test]
fn union_param_type_integer_compatible() {
    // Integer arg to "Integer | Symbol" param → no warning
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setTimeout:",
        1,
        vec![Some("Integer | Symbol".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setTimeout:", span())]),
        vec![int_lit(42)],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Integer should be compatible with 'Integer | Symbol', got: {type_warnings:?}"
    );
}

#[test]
fn union_param_type_symbol_compatible() {
    // Symbol arg to "Integer | Symbol" param → no warning
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setTimeout:",
        1,
        vec![Some("Integer | Symbol".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setTimeout:", span())]),
        vec![sym_lit("infinity")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Symbol should be compatible with 'Integer | Symbol', got: {type_warnings:?}"
    );
}

#[test]
fn union_param_type_incompatible_warns() {
    // String arg to "Integer | Symbol" param → warning
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setTimeout:",
        1,
        vec![Some("Integer | Symbol".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setTimeout:", span())]),
        vec![str_lit("bad")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "String should be incompatible with 'Integer | Symbol', got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_param_type_subclass_compatible() {
    // Integer is a subclass of Number, so Integer arg to "Number | Symbol" should pass
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setTimeout:",
        1,
        vec![Some("Number | Symbol".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setTimeout:", span())]),
        vec![int_lit(42)],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Integer should be compatible with 'Number | Symbol' (subclass of Number), got: {type_warnings:?}"
    );
}

#[test]
fn non_union_param_type_unchanged() {
    // Non-union expected type still works as before: String arg to Integer param → warning
    let hierarchy = hierarchy_with_extension(
        "Object",
        "deposit:",
        1,
        vec![Some("Integer".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("deposit:", span())]),
        vec![str_lit("bad")],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Non-union type checking should still work, got: {:?}",
        checker.diagnostics()
    );
}

// ---------------------------------------------------------------------------
// BT-1877: Nil resolved to UndefinedObject in union param validation
// ---------------------------------------------------------------------------

#[test]
fn union_param_type_nil_does_not_disable_validation() {
    // BT-1877: `String | Nil` param with Integer arg should warn.
    // Before fix, `Nil` was not in the hierarchy so the conservative fallback
    // made it compatible with anything, silently disabling validation.
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setName:",
        1,
        vec![Some("String | Nil".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setName:", span())]),
        vec![int_lit(42)],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Integer should be incompatible with 'String | Nil' — Nil must not disable validation, got: {:?}",
        checker.diagnostics()
    );
}

#[test]
fn union_param_type_nil_compatible_with_object_union() {
    // `Object | Nil` param with Integer arg should pass (Integer is subclass of Object)
    let hierarchy = hierarchy_with_extension(
        "Object",
        "setValue:",
        1,
        vec![Some("Object | Nil".into())],
        Some("Nil".into()),
    );
    let module = make_module(vec![msg_send(
        msg_send(
            class_ref("Object"),
            MessageSelector::Unary("new".into()),
            vec![],
        ),
        MessageSelector::Keyword(vec![KeywordPart::new("setValue:", span())]),
        vec![int_lit(42)],
    )]);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Integer should be compatible with 'Object | Nil' (subclass of Object), got: {type_warnings:?}"
    );
}
