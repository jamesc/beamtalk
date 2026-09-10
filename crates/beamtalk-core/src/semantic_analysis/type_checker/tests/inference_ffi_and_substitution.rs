// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `split_type_params`, `extract_ffi_function_info`, `resolve_type_string`
//! substitution, and `set_param_types`.

use super::super::*;
use super::common::*;

// ---- split_type_params ----
//
// Thin wrapper over `string_utils::split_top_level`, whose exhaustive
// nesting/edge-case coverage lives at its definition site.
// This is a smoke test that the wrapper is wired up correctly.

#[test]
fn split_type_params_nested() {
    let result = TypeChecker::split_type_params("GenResult(A, B), E");
    assert_eq!(result, vec!["GenResult(A, B)", "E"]);
}

// ---- extract_ffi_function_info ----
//
// Pins the resolved binary-selector edge case: a binary selector has no
// FFI-proxy-call name (mirrors `erlang_function_name`/dispatch_codegen's
// `generate_direct_erlang_call`, both of which also decline to treat a
// binary send to `Erlang <module>` as a direct FFI call) — this used to
// disagree, deriving a name via `.split(':')` regardless of selector
// shape.

#[test]
fn extract_ffi_function_info_unary() {
    let selector = MessageSelector::Unary("reverse".into());
    assert_eq!(
        TypeChecker::extract_ffi_function_info(&selector, &[]),
        Some(("reverse".to_string(), 0))
    );
}

#[test]
fn extract_ffi_function_info_keyword() {
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("seq:", span()),
        KeywordPart::new("to:", span()),
    ]);
    let args = [var("a"), var("b")];
    assert_eq!(
        TypeChecker::extract_ffi_function_info(&selector, &args),
        Some(("seq".to_string(), 2))
    );
}

#[test]
fn extract_ffi_function_info_binary_is_none() {
    let selector = MessageSelector::Binary("+".into());
    let args = [var("a")];
    assert_eq!(
        TypeChecker::extract_ffi_function_info(&selector, &args),
        None
    );
}

// ---- resolve_type_string (substitution) ----

#[test]
fn substitute_direct_param() {
    let mut subst = HashMap::new();
    subst.insert(EcoString::from("T"), InferredType::known("Integer"));
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("T"),
        &subst,
        None,
        None,
        TypeStringContext::Substitution,
    );
    assert_eq!(result, InferredType::known("Integer"));
}

#[test]
fn substitute_method_local_param() {
    let mut method_subst = HashMap::new();
    method_subst.insert(EcoString::from("R"), InferredType::known("String"));
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("R"),
        &method_subst,
        None,
        None,
        TypeStringContext::Substitution,
    );
    assert_eq!(result, InferredType::known("String"));
}

#[test]
fn substitute_method_local_takes_priority() {
    let mut subst = HashMap::new();
    subst.insert(EcoString::from("R"), InferredType::known("Integer"));
    let mut method_subst = HashMap::new();
    method_subst.insert(EcoString::from("R"), InferredType::known("String"));
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("R"),
        &type_resolver::merge_substitutions(&subst, &method_subst, None),
        None,
        None,
        TypeStringContext::Substitution,
    );
    assert_eq!(result, InferredType::known("String"));
}

#[test]
fn substitute_generic_return_type() {
    let mut subst = HashMap::new();
    subst.insert(EcoString::from("T"), InferredType::known("Integer"));
    subst.insert(EcoString::from("E"), InferredType::known("IOError"));
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Result(T, E)"),
        &subst,
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
            assert_eq!(type_args[0], InferredType::known("Integer"));
            assert_eq!(type_args[1], InferredType::known("IOError"));
        }
        other => panic!("Expected Known, got {other:?}"),
    }
}

#[test]
fn substitute_no_match_passes_through() {
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("String"),
        &type_resolver::SubstitutionMap::new(),
        None,
        None,
        TypeStringContext::Substitution,
    );
    assert_eq!(result, InferredType::known("String"));
}

#[test]
fn substitute_generic_base_extracted() {
    // When return type is "Array(R)" and R is not in subst, base "Array" is still extracted
    // Unresolved type param R falls back to Dynamic instead of Known("R")
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Array(R)"),
        &type_resolver::SubstitutionMap::new(),
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
            // R not in subst → Dynamic
            assert_eq!(type_args[0], InferredType::Dynamic(DynamicReason::Unknown));
        }
        other => panic!("Expected Known, got {other:?}"),
    }
}

// ---- resolve_type_string with self_type ----

#[test]
fn substitute_self_in_generic_uses_full_receiver_type() {
    // `Result(Self, Error)` on a parameterised receiver `Box(Integer)`
    // should produce `Result(Box(Integer), Error)`, not `Result(Box, Error)`.
    let receiver_ty = InferredType::Known {
        class_name: EcoString::from("Box"),
        type_args: vec![InferredType::known("Integer")],
        provenance: TypeProvenance::Inferred(Span::default()),
    };
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Result(Self, Error)"),
        &type_resolver::merge_substitutions(
            &type_resolver::SubstitutionMap::new(),
            &type_resolver::SubstitutionMap::new(),
            Some(&receiver_ty),
        ),
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
            // Self should resolve to full Box(Integer), not bare Box
            assert_eq!(type_args[0], receiver_ty);
            assert_eq!(type_args[1], InferredType::known("Error"));
        }
        other => panic!("Expected Known, got {other:?}"),
    }
}

#[test]
fn substitute_self_in_generic_non_parameterised_receiver() {
    // Non-parameterised receiver: `Result(Self, Error)` on `Counter`
    // should produce `Result(Counter, Error)`.
    let receiver_ty = InferredType::known("Counter");
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Result(Self, Error)"),
        &type_resolver::merge_substitutions(
            &type_resolver::SubstitutionMap::new(),
            &type_resolver::SubstitutionMap::new(),
            Some(&receiver_ty),
        ),
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
            assert_eq!(type_args[0], InferredType::known("Counter"));
            assert_eq!(type_args[1], InferredType::known("Error"));
        }
        other => panic!("Expected Known, got {other:?}"),
    }
}

#[test]
fn substitute_self_in_union_uses_full_receiver_type() {
    // `Self | Error` on `Box(Integer)` should produce
    // `Box(Integer) | Error`.
    let receiver_ty = InferredType::Known {
        class_name: EcoString::from("Box"),
        type_args: vec![InferredType::known("Integer")],
        provenance: TypeProvenance::Inferred(Span::default()),
    };
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Self | Error"),
        &type_resolver::merge_substitutions(
            &type_resolver::SubstitutionMap::new(),
            &type_resolver::SubstitutionMap::new(),
            Some(&receiver_ty),
        ),
        None,
        None,
        TypeStringContext::Substitution,
    );
    match result {
        InferredType::Union { members, .. } => {
            assert_eq!(members.len(), 2);
            assert!(members.contains(&receiver_ty));
            assert!(members.contains(&InferredType::known("Error")));
        }
        other => panic!("Expected Union, got {other:?}"),
    }
}

#[test]
fn substitute_self_none_falls_back_to_dynamic() {
    // Without a threaded `Self` binding, a nested `Self` resolves to
    // `Dynamic` — matching the AST-built `Generic { parameters:
    // [SelfType] }` path (`resolve_declared_type`'s documented
    // no-binding fallback). `parse` now yields the structured `SelfType`
    // for the string spelling too, so the string path no longer passes
    // `Self` through as a phantom nominal `Known("Self")`.
    let result = type_resolver::resolve_declared_type(
        &DeclaredType::parse("Result(Self, Error)"),
        &type_resolver::SubstitutionMap::new(),
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
            assert!(
                matches!(type_args[0], InferredType::Dynamic(_)),
                "unbound Self should resolve to Dynamic, got {:?}",
                type_args[0]
            );
            assert_eq!(type_args[1], InferredType::known("Error"));
        }
        other => panic!("Expected Known, got {other:?}"),
    }
}

// ---- set_param_types ----

#[test]
fn set_param_types_untyped() {
    let params = vec![ParameterDefinition::new(ident("x"))];
    let mut env = TypeEnv::new();
    TypeChecker::set_param_types(
        &mut env,
        &params,
        None,
        &mut type_resolver::ResolutionContext::new(None, &mut std::collections::HashSet::new()),
    );
    assert_eq!(
        env.get_local("x"),
        Some(InferredType::Dynamic(DynamicReason::Unknown))
    );
}

#[test]
fn set_param_types_typed() {
    let params = vec![ParameterDefinition {
        name: ident("x"),
        type_annotation: Some(TypeAnnotation::Simple(ident("Integer"))),
    }];
    let mut env = TypeEnv::new();
    TypeChecker::set_param_types(
        &mut env,
        &params,
        None,
        &mut type_resolver::ResolutionContext::new(None, &mut std::collections::HashSet::new()),
    );
    assert_eq!(env.get_local("x"), Some(InferredType::known("Integer")));
}

#[test]
fn set_param_types_mixed() {
    let params = vec![
        ParameterDefinition {
            name: ident("x"),
            type_annotation: Some(TypeAnnotation::Simple(ident("String"))),
        },
        ParameterDefinition::new(ident("y")),
    ];
    let mut env = TypeEnv::new();
    TypeChecker::set_param_types(
        &mut env,
        &params,
        None,
        &mut type_resolver::ResolutionContext::new(None, &mut std::collections::HashSet::new()),
    );
    assert_eq!(env.get_local("x"), Some(InferredType::known("String")));
    assert_eq!(
        env.get_local("y"),
        Some(InferredType::Dynamic(DynamicReason::Unknown))
    );
}
