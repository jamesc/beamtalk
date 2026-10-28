// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `infer_method_local_params`, block-inference regressions, and
//! `substitute_return_type` with method-local params.

use super::super::*;
use super::common::*;

// ---- infer_method_local_params ----

#[test]
fn infer_method_local_params_result_t_e() {
    // assertOk: has param type Result(T, E), arg is Result(Integer, Error) → T=Integer, E=Error
    let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
    let arg = InferredType::Known {
        class_name: "Result".into(),
        type_args: vec![InferredType::known("Integer"), InferredType::known("Error")],
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
    assert_eq!(result.len(), 2);
    assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
    assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
}

#[test]
fn infer_method_local_params_result_dictionary_string() {
    // assertOk: on Result(Dictionary, String) → T=Dictionary, E=String
    let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
    let arg = InferredType::Known {
        class_name: "Result".into(),
        type_args: vec![
            InferredType::known("Dictionary"),
            InferredType::known("String"),
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
    assert_eq!(result.get("T"), Some(&InferredType::known("Dictionary")));
    assert_eq!(result.get("E"), Some(&InferredType::known("String")));
}

#[test]
fn infer_method_local_params_assert_error_returns_e() {
    // assertError:equals: has param type Result(T, E), return type E
    let method = method_info(
        "assertError:equals:",
        vec![Some("Result(T, E)"), Some("Object")],
        Some("E"),
    );
    let arg = InferredType::Known {
        class_name: "Result".into(),
        type_args: vec![
            InferredType::known("Integer"),
            InferredType::known("Symbol"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg, InferredType::known("Symbol")],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(result.get("E"), Some(&InferredType::known("Symbol")));
    assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
}

#[test]
fn infer_method_local_params_array_t() {
    // Method with Array(T) param, arg is Array(String) → T=String
    let method = method_info("process:", vec![Some("Array(T)")], Some("T"));
    let arg = InferredType::Known {
        class_name: "Array".into(),
        type_args: vec![InferredType::known("String")],
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
    assert_eq!(result.len(), 1);
    assert_eq!(result.get("T"), Some(&InferredType::known("String")));
}

#[test]
fn infer_method_local_params_dictionary_k_v() {
    // Method with Dictionary(K, V) param → K=String, V=Integer
    let method = method_info("lookup:", vec![Some("Dictionary(K, V)")], Some("V"));
    let arg = InferredType::Known {
        class_name: "Dictionary".into(),
        type_args: vec![
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
    assert_eq!(result.len(), 2);
    assert_eq!(result.get("K"), Some(&InferredType::known("String")));
    assert_eq!(result.get("V"), Some(&InferredType::known("Integer")));
}

#[test]
fn infer_method_local_params_nested_result_array() {
    // Result(Array(Integer), Error) — nested parametric type
    let method = method_info("process:", vec![Some("Result(T, E)")], Some("T"));
    let inner_array = InferredType::Known {
        class_name: "Array".into(),
        type_args: vec![InferredType::known("Integer")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let arg = InferredType::Known {
        class_name: "Result".into(),
        type_args: vec![inner_array.clone(), InferredType::known("Error")],
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
    assert_eq!(result.get("T"), Some(&inner_array));
    assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
}

#[test]
fn infer_method_local_params_no_type_args_on_arg() {
    // Bare Result without type args — no inference possible
    let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
    let arg = InferredType::known("Result"); // no type_args
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert!(result.is_empty(), "No inference when arg has no type_args");
}

#[test]
fn infer_method_local_params_dynamic_arg() {
    // Dynamic argument — no inference possible
    let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
    let arg = InferredType::Dynamic(DynamicReason::Unknown);
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert!(result.is_empty(), "No inference from Dynamic arg");
}

#[test]
fn infer_method_local_params_base_class_mismatch() {
    // Param declares Result(T, E) but arg is Array(Integer) — base mismatch
    let method = method_info("assertOk:", vec![Some("Result(T, E)")], Some("T"));
    let arg = InferredType::Known {
        class_name: "Array".into(),
        type_args: vec![InferredType::known("Integer")],
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
        "No inference when base class name doesn't match"
    );
}

#[test]
fn infer_method_local_params_multiple_parametric_params() {
    // Method with two parametric params: process:with: Array(T), Dictionary(K, V)
    let method = method_info(
        "process:with:",
        vec![Some("Array(T)"), Some("Dictionary(K, V)")],
        Some("T"),
    );
    let arg1 = InferredType::Known {
        class_name: "Array".into(),
        type_args: vec![InferredType::known("String")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let arg2 = InferredType::Known {
        class_name: "Dictionary".into(),
        type_args: vec![
            InferredType::known("Symbol"),
            InferredType::known("Integer"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg1, arg2],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(result.len(), 3);
    assert_eq!(result.get("T"), Some(&InferredType::known("String")));
    assert_eq!(result.get("K"), Some(&InferredType::known("Symbol")));
    assert_eq!(result.get("V"), Some(&InferredType::known("Integer")));
}

#[test]
fn infer_method_local_params_skips_known_classes_in_param_type() {
    // Param type is Result(Integer, E) — Integer is a known class, not a type param
    let method = method_info("check:", vec![Some("Result(Integer, E)")], Some("E"));
    let arg = InferredType::Known {
        class_name: "Result".into(),
        type_args: vec![InferredType::known("Integer"), InferredType::known("Error")],
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
    // "Integer" is a known class, so it should NOT be inferred as a type param
    assert!(!result.contains_key("Integer"));
    assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
}

#[test]
fn infer_method_local_params_non_parametric_param_ignored() {
    // Method param type is just "Integer" (not parametric) — nothing to infer
    let method = method_info("add:", vec![Some("Integer")], Some("Integer"));
    let arg = InferredType::known("Integer");
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
        "Non-parametric param type yields no inference"
    );
}

#[test]
fn infer_method_local_params_missing_arg() {
    // Method expects 2 args but only 1 provided — second param skipped
    let method = method_info(
        "process:with:",
        vec![Some("Array(T)"), Some("Dictionary(K, V)")],
        Some("T"),
    );
    let arg1 = InferredType::Known {
        class_name: "Array".into(),
        type_args: vec![InferredType::known("String")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg1],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(result.len(), 1);
    assert_eq!(result.get("T"), Some(&InferredType::known("String")));
    assert!(!result.contains_key("K"));
    assert!(!result.contains_key("V"));
}

#[test]
fn infer_method_local_params_untyped_param_skipped() {
    // Method has a None param type — skipped
    let method = method_info("process:", vec![None], Some("Dynamic"));
    let arg = InferredType::known("Integer");
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert!(result.is_empty());
}

// ---- Block inference regression tests ----
// These test that Block(T, R) param types still work correctly.

#[test]
fn infer_method_local_params_block_t_r() {
    // collect: has param type Block(T, R), arg is Block with type_args
    let method = method_info("collect:", vec![Some("Block(T, R)")], Some("Array(R)"));
    let arg = InferredType::Known {
        class_name: "Block".into(),
        type_args: vec![
            InferredType::known("Integer"),
            InferredType::known("String"),
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
    assert_eq!(result.len(), 2);
    assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
    assert_eq!(result.get("R"), Some(&InferredType::known("String")));
}

#[test]
fn infer_method_local_params_map_block() {
    // map: has Block(T, R) param type
    let method = method_info("map:", vec![Some("Block(T, R)")], Some("Array(R)"));
    let arg = InferredType::Known {
        class_name: "Block".into(),
        type_args: vec![
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
    assert_eq!(result.get("T"), Some(&InferredType::known("String")));
    assert_eq!(result.get("R"), Some(&InferredType::known("Integer")));
}

#[test]
fn infer_method_local_params_inject_into_block() {
    // inject:into: has two params — initial value and Block(T, R)
    let method = method_info(
        "inject:into:",
        vec![Some("Object"), Some("Block(T, R)")],
        Some("R"),
    );
    let arg_init = InferredType::known("Integer");
    let arg_block = InferredType::Known {
        class_name: "Block".into(),
        type_args: vec![
            InferredType::known("Integer"),
            InferredType::known("Integer"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[arg_init, arg_block],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    // Only the Block param yields inference; Object is non-parametric
    assert_eq!(result.get("T"), Some(&InferredType::known("Integer")));
    assert_eq!(result.get("R"), Some(&InferredType::known("Integer")));
}

/// BT-4207 (supersedes BT-2039's branch-join rule): `Block(R) Block(R) -> R`
/// on `ifTrue:ifFalse:` is a *branch join* — exactly one arm runs — so a
/// Dynamic arm joined with a Known arm must not collapse to the Known type.
/// It becomes `Dynamic(DynamicBranch)`, a non-root-cause reason that keeps
/// BT-2039's intent (no second "Dynamic in typed class" warning on the
/// enclosing expression; see
/// `bt_2039_if_true_if_false_known_branch_suppresses_ffi_warning`).
#[test]
fn bt_4207_branch_join_of_known_and_dynamic_arms_is_dynamic_branch() {
    let method = method_info(
        "ifTrue:ifFalse:",
        vec![Some("Block(R)"), Some("Block(R)")],
        Some("R"),
    );
    let known_block = InferredType::Known {
        class_name: "Block".into(),
        type_args: vec![InferredType::known("Nil")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let dynamic_block = InferredType::Known {
        class_name: "Block".into(),
        type_args: vec![InferredType::Dynamic(DynamicReason::UntypedFfi)],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();

    for args in [
        [known_block.clone(), dynamic_block.clone()],
        [dynamic_block, known_block],
    ] {
        let result = TypeChecker::infer_method_local_params(
            &method,
            &args,
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(
            result.get("R"),
            Some(&InferredType::Dynamic(DynamicReason::DynamicBranch)),
            "a Dynamic arm must not be discarded by the branch join, got {:?}",
            result.get("R")
        );
    }
}

/// BT-2039/BT-2865 intent for *argument-position* unification is unchanged:
/// `inject: initial :: A into: Block(A, E, A)` binds `A` from the initial
/// value, the block's accumulator parameter, and the block's result; an
/// uninformative Dynamic in either block slot loses to the Known initial
/// value.
#[test]
fn bt_4207_argument_unification_keeps_known_over_dynamic() {
    let method = method_info(
        "inject:into:",
        vec![Some("A"), Some("Block(A, E, A)")],
        Some("A"),
    );
    let block = InferredType::Known {
        class_name: "Block".into(),
        type_args: vec![
            InferredType::Dynamic(DynamicReason::UnannotatedParam),
            InferredType::known("Integer"),
            InferredType::Dynamic(DynamicReason::UntypedFfi),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[InferredType::known("List"), block],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(result.get("A"), Some(&InferredType::known("List")));
}

/// BT-4207: a `Block(R)` param's result slot aligns with the argument
/// block's *last* type arg even when the block takes parameters the declared
/// type omits (`ifNotNil: Block(R)` given `[:v | nil]`), so `R` binds to the
/// block's result, not its parameter type.
#[test]
fn bt_4207_block_result_slot_aligns_with_last_type_arg() {
    let method = method_info(
        "ifNil:ifNotNil:",
        vec![Some("Block(R)"), Some("Block(R)")],
        Some("R"),
    );
    let nil_block = InferredType::Known {
        class_name: "Block".into(),
        type_args: vec![InferredType::known("Integer")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let one_arg_block = InferredType::Known {
        class_name: "Block".into(),
        type_args: vec![
            InferredType::known("String"),
            InferredType::known("Integer"),
        ],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    let result = TypeChecker::infer_method_local_params(
        &method,
        &[nil_block, one_arg_block],
        &HashMap::new(),
        &hierarchy,
        "TestCase",
    );
    assert_eq!(result.get("R"), Some(&InferredType::known("Integer")));
}

/// BT-4207: a Dynamic or bare `Block` argument has an unknown result, which
/// is an arm of the join like any other — the sibling `nil` arm must not
/// claim `R` alone.
#[test]
fn bt_4207_opaque_block_argument_is_an_unknown_arm() {
    let method = method_info(
        "ifTrue:ifFalse:",
        vec![Some("Block(R)"), Some("Block(R)")],
        Some("R"),
    );
    let nil_block = InferredType::Known {
        class_name: "Block".into(),
        type_args: vec![InferredType::known("Nil")],
        provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
    };
    let hierarchy = ClassHierarchy::with_builtins();
    for opaque in [
        InferredType::known("Block"),
        InferredType::Dynamic(DynamicReason::UnannotatedParam),
    ] {
        let result = TypeChecker::infer_method_local_params(
            &method,
            &[opaque.clone(), nil_block.clone()],
            &HashMap::new(),
            &hierarchy,
            "TestCase",
        );
        assert_eq!(
            result.get("R"),
            Some(&InferredType::Dynamic(DynamicReason::DynamicBranch)),
            "opaque arm {opaque:?} must keep R Dynamic, got {:?}",
            result.get("R")
        );
    }
}

#[test]
fn bt_4207_join_branch_arms_rules() {
    let int = InferredType::known("Integer");
    let nil = InferredType::known("Nil");
    let ffi = InferredType::Dynamic(DynamicReason::UntypedFfi);
    let explicit = InferredType::Dynamic(DynamicReason::ExplicitDynamic);

    // Known | Known => union of both.
    assert_eq!(
        TypeChecker::join_branch_arms(&[int.clone(), nil.clone()]),
        InferredType::union_of(&[int.clone(), nil.clone()])
    );
    // Known | Dynamic => DynamicBranch, either order.
    assert_eq!(
        TypeChecker::join_branch_arms(&[nil.clone(), ffi.clone()]),
        InferredType::Dynamic(DynamicReason::DynamicBranch)
    );
    assert_eq!(
        TypeChecker::join_branch_arms(&[ffi.clone(), nil.clone()]),
        InferredType::Dynamic(DynamicReason::DynamicBranch)
    );
    // ExplicitDynamic is authoritative (BT-2865).
    assert_eq!(
        TypeChecker::join_branch_arms(&[nil.clone(), explicit.clone()]),
        explicit
    );
    // Never (a diverging arm) is the identity: Dynamic | Never stays the
    // arm's own Dynamic, Known | Never stays Known.
    assert_eq!(
        TypeChecker::join_branch_arms(&[ffi.clone(), InferredType::Never]),
        ffi
    );
    assert_eq!(
        TypeChecker::join_branch_arms(&[InferredType::Never, int.clone()]),
        int
    );
}

#[test]
fn infer_method_local_params_skips_non_type_param_in_parametric() {
    // Param type is Result(Enumerable, E) where Enumerable is a protocol name,
    // not a type parameter. It should NOT be substituted even though it's not in the hierarchy.
    let method = method_info("check:", vec![Some("Result(Enumerable, E)")], Some("E"));
    let arg = InferredType::Known {
        class_name: "Result".into(),
        type_args: vec![InferredType::known("List"), InferredType::known("Error")],
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
    // "Enumerable" is not a single-letter type param, so it must NOT be inferred
    assert!(
        !result.contains_key("Enumerable"),
        "Non-type-param identifier 'Enumerable' should not be substituted"
    );
    // "E" IS a valid single-letter type param — it should still be inferred
    assert_eq!(result.get("E"), Some(&InferredType::known("Error")));
}

// ---- substitute_return_type with method-local params (end-to-end flow) ----

#[test]
fn substitute_return_type_from_method_local_result() {
    // Simulates: assertOk: with Result(Integer, Error) → return type T → Integer
    let mut method_subst = HashMap::new();
    method_subst.insert(EcoString::from("T"), InferredType::known("Integer"));
    method_subst.insert(EcoString::from("E"), InferredType::known("Error"));
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("T"),
        &method_subst,
        None,
        None,
        TypeStringContext::Substitution,
    );
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn substitute_return_type_generic_array_r() {
    // collect: returns Array(R) where R=String → Array(String)
    let mut method_subst = HashMap::new();
    method_subst.insert(EcoString::from("R"), InferredType::known("String"));
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Array(R)"),
        &method_subst,
        None,
        None,
        TypeStringContext::Substitution,
    );
    match result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Array");
            assert_eq!(type_args.len(), 1);
            assert_eq!(type_args[0], InferredType::known("String"));
        }
        other => panic!("Expected Known Array(String), got {other:?}"),
    }
}

#[test]
fn substitute_return_type_nested_result() {
    // Return type Result(T, E) with T=Array(Integer), E=Error
    // Substitution on "Result(T, E)" should produce Result(Array(Integer), Error)
    let mut method_subst = HashMap::new();
    method_subst.insert(
        EcoString::from("T"),
        InferredType::Known {
            class_name: "Array".into(),
            type_args: vec![InferredType::known("Integer")],
            provenance: crate::semantic_analysis::TypeProvenance::Inferred(span()),
        },
    );
    method_subst.insert(EcoString::from("E"), InferredType::known("Error"));
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Result(T, E)"),
        &method_subst,
        None,
        None,
        TypeStringContext::Substitution,
    );
    match result {
        InferredType::Known {
            class_name,
            type_args,
            ..
        } => {
            assert_eq!(class_name.as_str(), "Result");
            assert_eq!(type_args.len(), 2);
            match &type_args[0] {
                InferredType::Known {
                    class_name,
                    type_args,
                    ..
                } => {
                    assert_eq!(class_name.as_str(), "Array");
                    assert_eq!(type_args.len(), 1);
                    assert_eq!(type_args[0], InferredType::known("Integer"));
                }
                other => panic!("Expected Known Array(Integer), got {other:?}"),
            }
            assert_eq!(type_args[1], InferredType::known("Error"));
        }
        other => panic!("Expected Known Result(Array(Integer), Error), got {other:?}"),
    }
}

#[test]
fn substitute_return_type_union_with_type_param() {
    // "E | Nil" with E=Integer should produce a Union of Integer
    // and nil. `Nil` normalises to the canonical
    // `UndefinedObject` on this path too (like every other resolver), so
    // `isNil`/`ifNil:` narrowing recognises the member.
    let mut subst = HashMap::new();
    subst.insert(EcoString::from("E"), InferredType::known("Integer"));
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("E | Nil"),
        &subst,
        None,
        None,
        TypeStringContext::Substitution,
    );
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert_eq!(members[0], InferredType::known("Integer"));
            assert_eq!(members[1], InferredType::known("UndefinedObject"));
        }
        other => panic!("Expected Union(Integer, UndefinedObject), got {other:?}"),
    }
}

#[test]
fn substitute_return_type_union_no_params() {
    // "Behaviour | Nil" with no substitutions should pass through as Union
    // (with `Nil` normalised to `UndefinedObject`).
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Behaviour | Nil"),
        &type_resolver::SubstitutionMap::new(),
        None,
        None,
        TypeStringContext::Substitution,
    );
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert_eq!(members[0], InferredType::known("Behaviour"));
            assert_eq!(members[1], InferredType::known("UndefinedObject"));
        }
        other => panic!("Expected Union(Behaviour, UndefinedObject), got {other:?}"),
    }
}
