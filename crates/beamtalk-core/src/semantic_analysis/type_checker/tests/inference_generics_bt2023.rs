// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-2023 generic-argument inference: nullable-union unification, nested-generic
//! block params, and FFI polymorphic return-type substitution (moved from
//! `inference.rs` per BT-3450).

use super::super::*;
use super::common::*;

// ---- BT-2023(A): Nullable-union arguments unify with generic params ----

#[test]
fn infer_method_local_params_nullable_list_union() {
    // Param type List(T), arg is List(String) | Nil → should extract T=String
    let method = method_info("process:", vec![Some("List(T)")], Some("List(T)"));
    let arg = InferredType::Union {
        members: vec![
            InferredType::Known {
                class_name: "List".into(),
                type_args: vec![InferredType::known("String")],
                provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
            },
            InferredType::known("UndefinedObject"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(
        result.get("T"),
        Some(&InferredType::known("String")),
        "Should extract T=String from List(String) | Nil"
    );
}

#[test]
fn infer_method_local_params_nullable_dictionary_union() {
    // Dictionary(K, V) | Nil → should extract K=String, V=Integer
    let method = method_info("lookup:", vec![Some("Dictionary(K, V)")], Some("V"));
    let arg = InferredType::Union {
        members: vec![
            InferredType::Known {
                class_name: "Dictionary".into(),
                type_args: vec![
                    InferredType::known("String"),
                    InferredType::known("Integer"),
                ],
                provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
            },
            InferredType::known("UndefinedObject"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(result.get("K"), Some(&InferredType::known("String")));
    assert_eq!(result.get("V"), Some(&InferredType::known("Integer")));
}

#[test]
fn infer_method_local_params_nullable_set_union() {
    // Set(T) | Nil → should extract T=Symbol
    let method = method_info("process:", vec![Some("Set(T)")], Some("T"));
    let arg = InferredType::Union {
        members: vec![
            InferredType::Known {
                class_name: "Set".into(),
                type_args: vec![InferredType::known("Symbol")],
                provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
            },
            InferredType::known("UndefinedObject"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(result.get("T"), Some(&InferredType::known("Symbol")));
}

#[test]
fn infer_method_local_params_nil_alias_in_union() {
    // Also handle "Nil" (not just "UndefinedObject") in unions
    let method = method_info("process:", vec![Some("List(T)")], Some("T"));
    let arg = InferredType::Union {
        members: vec![
            InferredType::Known {
                class_name: "List".into(),
                type_args: vec![InferredType::known("Integer")],
                provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
            },
            InferredType::known("Nil"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
}

#[test]
fn infer_method_local_params_non_nil_union_no_match() {
    // Union without nil members (e.g., String | Integer) should NOT match List(T)
    let method = method_info("process:", vec![Some("List(T)")], Some("T"));
    let arg = InferredType::Union {
        members: vec![
            InferredType::known("String"),
            InferredType::known("Integer"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert!(
        result.is_empty(),
        "Non-nil union members with no matching base class should not unify"
    );
}

#[test]
fn infer_method_local_params_plain_type_param_nullable_union() {
    // Plain type param A with nullable union arg: A binds to the whole
    // union (String | Nil), preserving nilability through the return type.
    // CodeRabbit on PR #2058: stripping Nil here would unsoundly turn
    // `identity: x :: A -> A` called with `String | Nil` into `A = String`.
    let method = method_info("inject:", vec![Some("A")], Some("A"));
    let nullable = InferredType::Union {
        members: vec![
            InferredType::known("String"),
            InferredType::known("UndefinedObject"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        std::slice::from_ref(&nullable),
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(
        result.get("A"),
        Some(&nullable),
        "Plain type param A should bind to the full nullable union"
    );
}

// ---- BT-2023(B): Nested-generic block params resolve ----

#[test]
fn resolve_type_param_nested_list_e() {
    // Block param type "List(E)" where E=String → should resolve to List(String)
    let mut class_subst = HashMap::new();
    class_subst.insert(EcoString::from("E"), InferredType::known("String"));
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::resolve_type_param(
        &DeclaredType::parse("List(E)"),
        &class_subst,
        &HashMap::new(),
        &hierarchy,
    );
    match result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "List");
            assert_eq!(type_args.len(), 1);
            assert_eq!(type_args[0], InferredType::known("String"));
        }
        other => panic!("Expected Known List(String), got {other:?}"),
    }
}

#[test]
fn resolve_type_param_nested_dictionary_k_list_v() {
    // Two levels: "Dictionary(K, List(V))" where K=String, V=Integer
    let mut method_subst = HashMap::new();
    method_subst.insert(EcoString::from("K"), InferredType::known("String"));
    method_subst.insert(EcoString::from("V"), InferredType::known("Integer"));
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::resolve_type_param(
        &DeclaredType::parse("Dictionary(K, List(V))"),
        &HashMap::new(),
        &method_subst,
        &hierarchy,
    );
    match result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Dictionary");
            assert_eq!(type_args.len(), 2);
            assert_eq!(type_args[0], InferredType::known("String"));
            match &type_args[1] {
                InferredType::Known {
                    class_name,
                    type_args,
                    ..
                } => {
                    assert_eq!(class_name.as_str(), "List");
                    assert_eq!(type_args.len(), 1);
                    assert_eq!(type_args[0], InferredType::known("Integer"));
                }
                other => panic!("Expected Known List(Integer), got {other:?}"),
            }
        }
        other => panic!("Expected Known Dictionary(String, List(Integer)), got {other:?}"),
    }
}

#[test]
fn resolve_type_param_bare_param_unchanged() {
    // A bare "E" still resolves via the substitution map (existing behaviour)
    let mut subst = HashMap::new();
    subst.insert(EcoString::from("E"), InferredType::known("String"));
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::resolve_type_param(
        &DeclaredType::parse("E"),
        &subst,
        &HashMap::new(),
        &hierarchy,
    );
    assert_eq!(result, InferredType::known("String"));
}

#[test]
fn resolve_type_param_known_class_unchanged() {
    // A known class name stays the same
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::resolve_type_param(
        &DeclaredType::parse("Integer"),
        &HashMap::new(),
        &HashMap::new(),
        &hierarchy,
    );
    assert_eq!(result, InferredType::known("Integer"));
}

// ---- BT-2023(C): FFI polymorphic return type substitution ----

#[test]
fn substitute_ffi_return_type_list_propagation() {
    // FFI sig: List -> List, call-site arg: List(String)
    // → return should be List(String)
    let ret = InferredType::known("List");
    let params = vec![super::super::native_type_registry::ParamType {
        keyword: Some("list".into()),
        type_: InferredType::known("List"),
    }];
    let arg = InferredType::Known {
        class_name: "List".into(),
        type_args: vec![InferredType::known("String")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
    match result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "List");
            assert_eq!(type_args.len(), 1);
            assert_eq!(type_args[0], InferredType::known("String"));
        }
        other => panic!("Expected Known List(String), got {other:?}"),
    }
}

#[test]
fn substitute_ffi_return_type_no_match() {
    // FFI sig: Integer -> String (different base classes), call-site arg: Integer
    // → return should stay String (no propagation)
    let ret = InferredType::known("String");
    let params = vec![super::super::native_type_registry::ParamType {
        keyword: Some("n".into()),
        type_: InferredType::known("Integer"),
    }];
    let arg = InferredType::known("Integer");
    let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
    assert_eq!(result, InferredType::known("String"));
}

#[test]
fn substitute_ffi_return_type_already_has_type_args() {
    // FFI sig: List(Integer) -> List(Integer) (already concrete)
    // → return should stay unchanged
    let ret = InferredType::Known {
        class_name: "List".into(),
        type_args: vec![InferredType::known("Integer")],
        provenance: crate::semantic_analysis::TypeProvenance::Extracted,
    };
    let params = vec![super::super::native_type_registry::ParamType {
        keyword: Some("list".into()),
        type_: InferredType::known("List"),
    }];
    let arg = InferredType::Known {
        class_name: "List".into(),
        type_args: vec![InferredType::known("String")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
    assert_eq!(result.as_known().unwrap().as_str(), "List");
    // Should keep the original type_args, not the arg's
    match result {
        InferredType::Known { type_args, .. } => {
            assert_eq!(type_args[0], InferredType::known("Integer"));
        }
        _ => unreachable!(),
    }
}

#[test]
fn substitute_ffi_return_type_arg_has_no_type_args() {
    // FFI sig: List -> List, call-site arg: List (no type_args)
    // → return should stay List (no type_args to propagate)
    let ret = InferredType::known("List");
    let params = vec![super::super::native_type_registry::ParamType {
        keyword: Some("list".into()),
        type_: InferredType::known("List"),
    }];
    let arg = InferredType::known("List");
    let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
    assert_eq!(result, InferredType::known("List"));
}

#[test]
fn substitute_ffi_return_type_dynamic_return() {
    // FFI sig: Dynamic return type → stays Dynamic
    let ret = InferredType::Dynamic(DynamicReason::Unknown);
    let params = vec![super::super::native_type_registry::ParamType {
        keyword: Some("list".into()),
        type_: InferredType::known("List"),
    }];
    let arg = InferredType::Known {
        class_name: "List".into(),
        type_args: vec![InferredType::known("String")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[arg]);
    assert!(matches!(result, InferredType::Dynamic(_)));
}

#[test]
fn substitute_ffi_return_type_skips_multi_arg() {
    // Models lists:map/2: Fun, [A] -> [B]. The element type of the input
    // list must NOT be propagated to the return — the return's element
    // type is the block's result type, not the input element type.
    let ret = InferredType::known("List");
    let params = vec![
        super::super::native_type_registry::ParamType {
            keyword: Some("fun".into()),
            type_: InferredType::known("Block"),
        },
        super::super::native_type_registry::ParamType {
            keyword: Some("list".into()),
            type_: InferredType::known("List"),
        },
    ];
    let fun_arg = InferredType::known("Block");
    let list_arg = InferredType::Known {
        class_name: "List".into(),
        type_args: vec![InferredType::known("String")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let result = TypeChecker::substitute_ffi_return_type(&ret, &params, &[fun_arg, list_arg]);
    // Stays bare List — no unsound propagation
    assert_eq!(result, InferredType::known("List"));
}
