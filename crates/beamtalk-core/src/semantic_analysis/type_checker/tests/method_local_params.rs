// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Generalized method-local param inference and singleton types (BT-1818, BT-1830).

use super::super::*;
use super::common::*;

// ---- BT-1818: Generalized infer_method_local_params tests ----

/// Add a non-parametric `Processor` class with methods that accept parametric param types.
///
/// - `processResult: r :: GenResult(T, E) -> T` — extracts T from a Result argument
/// - `processArray: a :: Array(T) -> T` — extracts T from an Array argument
fn add_processor_class(hierarchy: &mut ClassHierarchy) {
    use crate::semantic_analysis::class_hierarchy::{ClassInfo, MethodInfo};

    let info = ClassInfo {
        name: eco_string("Processor"),
        superclass: Some(eco_string("Value")),
        is_sealed: true,
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
                selector: eco_string("processResult:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("Processor"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("T")),
                param_types: vec![Some(eco_string("GenResult(T, E)"))],
                doc: None,
            },
            MethodInfo {
                selector: eco_string("processArray:"),
                arity: 1,
                kind: MethodKind::Primary,
                defined_in: eco_string("Processor"),
                is_sealed: false,
                is_internal: false,
                spawns_block: false,
                return_type: Some(eco_string("T")),
                param_types: vec![Some(eco_string("Array(T)"))],
                doc: None,
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    hierarchy.add_from_beam_meta(vec![info]);
}

/// BT-1818: Block inference still works after generalization (regression).
/// `map:` on `GenResult(Integer, IOError)` with a `Block(Integer, String)` arg
/// should infer R=String and return `GenResult(String, IOError)`.
#[test]
fn method_local_params_block_regression() {
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
    // Simulate a block argument with type Block(Integer, String)
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

    let result_ty = checker.infer_expr(
        &msg_send(
            var("r"),
            MessageSelector::Keyword(vec![KeywordPart::new("map:", span())]),
            vec![var("blk")],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    // map: returns GenResult(R, E) where R=String (from block return type), E=IOError
    match &result_ty {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "GenResult");
            assert_eq!(type_args.len(), 2);
            assert_eq!(
                type_args[0].as_known().map(EcoString::as_str),
                Some("String"),
                "R should be String from block return type"
            );
            assert_eq!(
                type_args[1].as_known().map(EcoString::as_str),
                Some("IOError"),
                "E should remain IOError"
            );
        }
        other => panic!("Expected Known type, got: {other:?}"),
    }
}

/// BT-1818: Infer method-local params from Result(T, E) argument type.
/// `processResult:` on non-parametric Processor with `GenResult(Integer, IOError)` arg
/// should infer T=Integer and return Integer.
#[test]
fn method_local_params_from_result_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_generic_result_class(&mut hierarchy);
    add_processor_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("p", InferredType::known("Processor"));
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

    let result_ty = checker.infer_expr(
        &msg_send(
            var("p"),
            MessageSelector::Keyword(vec![KeywordPart::new("processResult:", span())]),
            vec![var("r")],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("Integer"),
        "processResult: with GenResult(Integer, IOError) should return Integer (T), got: {result_ty:?}"
    );
}

/// BT-1818: Infer method-local params from Array(T) argument type.
/// `processArray:` on non-parametric Processor with Array(String) arg
/// should infer T=String and return String.
#[test]
fn method_local_params_from_array_type() {
    let mut hierarchy = ClassHierarchy::with_builtins();
    add_processor_class(&mut hierarchy);

    let mut checker = TypeChecker::new();
    let mut env = TypeEnv::new();
    env.set_local("p", InferredType::known("Processor"));
    env.set_local(
        "a",
        InferredType::Known {
            class_name: eco_string("Array"),
            type_args: vec![InferredType::known("String")],
            provenance: TypeProvenance::Declared(span()),
        },
    );

    let result_ty = checker.infer_expr(
        &msg_send(
            var("p"),
            MessageSelector::Keyword(vec![KeywordPart::new("processArray:", span())]),
            vec![var("a")],
        ),
        &hierarchy,
        &mut env,
        false,
    );

    assert_eq!(
        result_ty.as_known().map(EcoString::as_str),
        Some("String"),
        "processArray: with Array(String) should return String (T), got: {result_ty:?}"
    );
}

// ---- BT-1830: Singleton type inference and validation ----

#[test]
fn is_assignable_to_singleton_exact_match() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"#ok".into(), &hierarchy),
        "#ok should be assignable to #ok"
    );
}

#[test]
fn is_assignable_to_singleton_mismatch() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !TypeChecker::is_assignable_to(&"#error".into(), &"#ok".into(), &hierarchy),
        "#error should not be assignable to #ok"
    );
}

#[test]
fn is_assignable_to_singleton_rejects_symbol() {
    // BT-1878: Symbol is NOT assignable to #ok — subtyping goes the other direction.
    // #ok is a subtype of Symbol, not the reverse.
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !TypeChecker::is_assignable_to(&"Symbol".into(), &"#ok".into(), &hierarchy),
        "Symbol should NOT be assignable to #ok (wrong subtyping direction)"
    );
}

#[test]
fn is_assignable_to_singleton_value_to_symbol() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"Symbol".into(), &hierarchy),
        "#ok should be assignable to Symbol (singleton is a subtype of Symbol)"
    );
}

#[test]
fn is_assignable_to_singleton_rejects_unrelated_type() {
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !TypeChecker::is_assignable_to(&"Integer".into(), &"#ok".into(), &hierarchy),
        "Integer should not be assignable to #ok"
    );
}

#[test]
fn singleton_return_type_mismatch_warns() {
    // method -> #ok => #error  — should warn
    let class_def = ClassDefinition::with_modifiers(
        ident("Checker"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("status".into()),
            vec![],
            vec![bare(symbol_lit("error"))], // Returns Symbol, declared #ok
            TypeAnnotation::singleton("ok", span()),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert_eq!(
        return_warnings.len(),
        1,
        "should warn about singleton return type mismatch: {return_warnings:?}"
    );
    assert!(return_warnings[0].message.contains("#ok"));
}

#[test]
fn singleton_return_type_match_no_warning() {
    // method -> #ok => #ok  — should NOT warn (Symbol literal is compatible)
    let class_def = ClassDefinition::with_modifiers(
        ident("Checker"),
        Some(ident("Object")),
        ClassModifiers::default(),
        vec![],
        vec![MethodDefinition::with_return_type(
            MessageSelector::Unary("status".into()),
            vec![],
            vec![bare(symbol_lit("ok"))], // Returns Symbol — compatible with #ok
            TypeAnnotation::singleton("ok", span()),
            span(),
        )],
        span(),
    );
    let module = make_module_with_classes(vec![], vec![class_def]);
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let return_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("declares return type"))
        .collect();
    assert!(
        return_warnings.is_empty(),
        "Symbol literal compatible with singleton return type should not warn: {return_warnings:?}"
    );
}

#[test]
fn is_assignable_to_singleton_to_symbol_union() {
    let hierarchy = ClassHierarchy::with_builtins();
    // #ok should be assignable to "Symbol | nil" (singleton is a subtype of Symbol)
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"Symbol | nil".into(), &hierarchy),
        "#ok should be assignable to Symbol | nil"
    );
}

#[test]
fn is_assignable_to_singleton_to_object() {
    let hierarchy = ClassHierarchy::with_builtins();
    // #ok should be assignable to Object (Symbol is a subclass of Object)
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"Object".into(), &hierarchy),
        "#ok should be assignable to Object via Symbol superclass chain"
    );
}

// ── BT-1878: Singleton/Symbol subtyping direction regression tests ──

#[test]
fn symbol_not_assignable_to_singleton_union() {
    // BT-1878: Symbol should NOT be assignable to a union of singletons
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        !TypeChecker::is_assignable_to(&"Symbol".into(), &"#ok | #error".into(), &hierarchy),
        "Symbol should NOT be assignable to #ok | #error"
    );
}

#[test]
fn singleton_assignable_to_singleton_union() {
    // Regression: #ok must be assignable to #ok | #error (singleton union).
    // Previously, starts_with('#') matched the union string before the union
    // check, causing an exact-match return that rejected valid members.
    let hierarchy = ClassHierarchy::with_builtins();
    assert!(
        TypeChecker::is_assignable_to(&"#ok".into(), &"#ok | #error".into(), &hierarchy),
        "#ok should be assignable to #ok | #error"
    );
    assert!(
        TypeChecker::is_assignable_to(&"#error".into(), &"#ok | #error".into(), &hierarchy),
        "#error should be assignable to #ok | #error"
    );
}

// ── BT-1832: Union type validation across all contexts ──────────────────
