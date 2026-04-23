// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! FFI call type inference from `NativeTypeRegistry` (ADR 0075, BT-1880).

use super::super::*;
use super::common::*;

// ---------------------------------------------------------------------------
// ADR 0075: FFI call type inference from NativeTypeRegistry
// ---------------------------------------------------------------------------

/// Helper: Build a keyword selector from parts.
fn keyword_selector(parts: &[&str]) -> MessageSelector {
    MessageSelector::Keyword(
        parts
            .iter()
            .map(|kw| KeywordPart::new(*kw, span()))
            .collect(),
    )
}

/// Helper: Build a `NativeTypeRegistry` with lists module signatures.
fn lists_registry() -> NativeTypeRegistry {
    let mut reg = NativeTypeRegistry::new();
    reg.register_module(
        "lists",
        vec![
            FunctionSignature {
                name: "reverse".to_string(),
                arity: 1,
                params: vec![ParamType {
                    keyword: Some(ecow::EcoString::from("list")),
                    type_: InferredType::known("List"),
                }],
                return_type: InferredType::known("List"),
                provenance: TypeProvenance::Extracted,
                line: None,
            },
            FunctionSignature {
                name: "seq".to_string(),
                arity: 2,
                params: vec![
                    ParamType {
                        keyword: Some(ecow::EcoString::from("from")),
                        type_: InferredType::known("Integer"),
                    },
                    ParamType {
                        keyword: Some(ecow::EcoString::from("to")),
                        type_: InferredType::known("Integer"),
                    },
                ],
                return_type: InferredType::known("List"),
                provenance: TypeProvenance::Extracted,
                line: None,
            },
            FunctionSignature {
                name: "member".to_string(),
                arity: 2,
                params: vec![
                    ParamType {
                        keyword: Some(ecow::EcoString::from("elem")),
                        type_: InferredType::Dynamic(DynamicReason::Unknown),
                    },
                    ParamType {
                        keyword: Some(ecow::EcoString::from("list")),
                        type_: InferredType::known("List"),
                    },
                ],
                return_type: InferredType::known("Boolean"),
                provenance: TypeProvenance::Extracted,
                line: None,
            },
            FunctionSignature {
                name: "node".to_string(),
                arity: 0,
                params: vec![],
                return_type: InferredType::known("Symbol"),
                provenance: TypeProvenance::Extracted,
                line: None,
            },
        ],
    );
    reg
}

#[test]
fn test_ffi_call_returns_typed_result() {
    // Erlang lists reverse: #(1, 2, 3)
    // With registry, should infer List (not Dynamic)
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:"]),
        vec![Expression::ListLiteral {
            elements: vec![int_lit(1), int_lit(2), int_lit(3)],
            tail: None,
            span: span(),
        }],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    // Check the type map — the outer message send should have type List
    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("List")),
        "FFI call should infer List return type"
    );
}

#[test]
fn test_ffi_call_multi_arg_returns_typed_result() {
    // Erlang lists seq: 1 to: 10
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["seq:", "to:"]),
        vec![int_lit(1), int_lit(10)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("List")),
        "Multi-arg FFI call should infer List return type"
    );
}

#[test]
fn test_ffi_call_no_registry_falls_back_to_dynamic() {
    // Without registry, FFI calls should return Dynamic (no regression)
    // and should not produce any FFI-specific diagnostics.
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:"]),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    // No registry set
    checker.check_module(&module, &hierarchy);

    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| {
            d.message.contains("FFI keyword")
                || d.message.contains("expects List")
                || d.message.contains("lists:")
        })
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "Without registry, no FFI-specific diagnostics should be emitted. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_ffi_call_unknown_function_falls_back_to_dynamic() {
    // Known module, unknown function → Dynamic (no FFI diagnostics)
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["unknown_fn:"]),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("FFI keyword") || d.message.contains("lists:"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "Unknown function should not produce FFI diagnostics. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_ffi_call_unknown_module_falls_back_to_dynamic() {
    // Unknown module → Dynamic (no FFI diagnostics)
    let module = make_module(vec![msg_send(
        erlang_module_recv("unknown_mod"),
        keyword_selector(&["foo:"]),
        vec![int_lit(1)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("FFI keyword") || d.message.contains("unknown_mod:"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "Unknown module should not produce FFI diagnostics. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_ffi_variable_tracking_through_assignment() {
    // proxy := Erlang lists
    // proxy reverse: #(1, 2, 3)
    // → proxy should have type ErlangModule<lists>, reverse should infer List
    let module = make_module(vec![
        assign("proxy", erlang_module_recv("lists")),
        msg_send(
            var("proxy"),
            keyword_selector(&["reverse:"]),
            vec![Expression::ListLiteral {
                elements: vec![int_lit(1)],
                tail: None,
                span: span(),
            }],
        ),
    ]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("List")),
        "Variable-tracked FFI call should infer List return type"
    );
}

#[test]
fn test_ffi_keyword_mismatch_warning() {
    // Erlang lists seq: 1 foo: 10
    // Should warn: 'foo:' does not match 'to:' for lists:seq/2 parameter 2
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["seq:", "foo:"]),
        vec![int_lit(1), int_lit(10)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let keyword_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not match"))
        .collect();
    assert_eq!(
        keyword_warnings.len(),
        1,
        "Should emit keyword mismatch warning. Diagnostics: {:?}",
        checker.diagnostics()
    );
    assert!(
        keyword_warnings[0].message.contains("foo"),
        "Warning should mention the mismatched keyword 'foo'"
    );
    assert!(
        keyword_warnings[0].message.contains("to"),
        "Warning should mention the expected keyword 'to'"
    );
}

#[test]
fn test_ffi_keyword_with_suppressed() {
    // Erlang lists seq: 1 with: 10
    // `with:` is the universal fallback — should NOT warn
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["seq:", "with:"]),
        vec![int_lit(1), int_lit(10)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let keyword_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not match"))
        .collect();
    assert!(
        keyword_warnings.is_empty(),
        "with: fallback should suppress keyword mismatch warning. Got: {keyword_warnings:?}"
    );
}

#[test]
fn test_ffi_keyword_matching_no_warning() {
    // Erlang lists seq: 1 to: 10
    // Keywords match — no warning
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["seq:", "to:"]),
        vec![int_lit(1), int_lit(10)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let keyword_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not match"))
        .collect();
    assert!(
        keyword_warnings.is_empty(),
        "Matching keywords should not produce a warning. Got: {keyword_warnings:?}"
    );
}

#[test]
fn test_ffi_argument_type_mismatch_warning() {
    // Erlang lists reverse: 42
    // reverse expects List, got Integer
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:"]),
        vec![int_lit(42)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert_eq!(
        type_warnings.len(),
        1,
        "Should emit argument type mismatch warning. Diagnostics: {:?}",
        checker.diagnostics()
    );
    assert!(
        type_warnings[0].message.contains("List"),
        "Warning should mention expected type List"
    );
    assert!(
        type_warnings[0].message.contains("Integer"),
        "Warning should mention actual type Integer"
    );
}

#[test]
fn test_ffi_argument_dynamic_param_no_warning() {
    // Erlang lists member: "hello" in: #(1, 2, 3)
    // member param 1 is Dynamic — should not warn for any argument type
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["member:", "in:"]),
        vec![
            str_lit("hello"),
            Expression::ListLiteral {
                elements: vec![int_lit(1)],
                tail: None,
                span: span(),
            },
        ],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Dynamic param type should accept any argument. Got: {type_warnings:?}"
    );
}

#[test]
fn test_ffi_dynamic_module_name_falls_back() {
    // Erlang (someVar) reverse: xs → Dynamic (no static module name)
    // This is a keyword send on Erlang class, which goes through normal DNU-suppressed path
    let module = make_module(vec![msg_send(
        // Simulate Erlang (someVar) — receiver is MessageSend with keyword selector
        // Actually, `Erlang (someVar)` parses differently.
        // The parenthesized form is: ClassRef("Erlang") with a keyword/unary send
        // where the module is dynamic. For our purposes, a non-Unary selector
        // on Erlang falls through to normal class-side handling.
        class_ref("Erlang"),
        keyword_selector(&["reverse:"]),
        vec![var("xs")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    // Should not crash, and should not produce FFI-specific warnings
    let ffi_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("FFI keyword") || d.message.contains("expects"))
        .collect();
    assert!(
        ffi_warnings.is_empty(),
        "Dynamic module name should not produce FFI warnings. Got: {ffi_warnings:?}"
    );
}

#[test]
fn test_ffi_erlang_module_type_in_type_map() {
    // Erlang lists → should infer ErlangModule<lists> in type map
    let module = make_module(vec![erlang_module_recv("lists")]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // erlang_module_recv uses span (10,20)
    let erlang_type = checker.type_map().get(Span::new(10, 20));
    match erlang_type {
        Some(InferredType::Known {
            class_name,
            type_args,
            ..
        }) => {
            assert_eq!(class_name.as_str(), "ErlangModule");
            assert_eq!(type_args.len(), 1);
            if let InferredType::Known {
                class_name: mod_name,
                ..
            } = &type_args[0]
            {
                assert_eq!(mod_name.as_str(), "lists");
            } else {
                panic!("Expected Known type arg, got: {:?}", type_args[0]);
            }
        }
        other => panic!("Expected ErlangModule<lists> in type map, got: {other:?}"),
    }
}

#[test]
fn test_ffi_wrong_arity_falls_back_to_dynamic() {
    // Erlang lists reverse: a with: b → arity 2, but reverse is arity 1
    // Should fall back to Dynamic (no FFI type warnings)
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:", "with:"]),
        vec![int_lit(1), int_lit(2)],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let ffi_type_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("lists:reverse"))
        .collect();
    assert!(
        ffi_type_diags.is_empty(),
        "Wrong arity should not produce type check diagnostics. Got: {ffi_type_diags:?}"
    );
}

#[test]
fn test_ffi_member_returns_boolean() {
    // Erlang lists member: 1 in: myList → Boolean
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["member:", "in:"]),
        vec![int_lit(1), var("myList")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("Boolean")),
        "member should return Boolean"
    );
}

// ---- Object subtype acceptance and unnamed param keyword skip ----

#[test]
fn test_ffi_object_param_accepts_any_class() {
    // An FFI function expecting Object (from beamtalk_object() spec) should
    // accept any concrete class type without warning.
    let mut reg = NativeTypeRegistry::new();
    reg.register_module(
        "beamtalk_supervisor",
        vec![FunctionSignature {
            name: "startLink".to_string(),
            arity: 1,
            params: vec![ParamType {
                keyword: Some(ecow::EcoString::from("arg")),
                type_: InferredType::known("Object"),
            }],
            return_type: InferredType::Dynamic(DynamicReason::DynamicSpec),
            provenance: TypeProvenance::Extracted,
            line: None,
        }],
    );
    // Pass a class reference (Supervisor) as the argument — should be accepted
    let module = make_module(vec![msg_send(
        erlang_module_recv("beamtalk_supervisor"),
        keyword_selector(&["startLink:"]),
        vec![class_ref("Supervisor")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(reg);
    checker.check_module(&module, &hierarchy);

    let type_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("expects"))
        .collect();
    assert!(
        type_warnings.is_empty(),
        "Object param should accept any class type. Got: {type_warnings:?}"
    );
}

#[test]
fn test_ffi_unnamed_arg_param_skips_keyword_mismatch() {
    // An FFI function with unnamed params (keyword = "arg") should not warn
    // when the call site uses a different keyword name.
    let mut reg = NativeTypeRegistry::new();
    reg.register_module(
        "beamtalk_supervisor",
        vec![FunctionSignature {
            name: "terminateChild".to_string(),
            arity: 2,
            params: vec![
                ParamType {
                    keyword: Some(ecow::EcoString::from("arg")),
                    type_: InferredType::Dynamic(DynamicReason::DynamicSpec),
                },
                ParamType {
                    keyword: Some(ecow::EcoString::from("arg")),
                    type_: InferredType::Dynamic(DynamicReason::DynamicSpec),
                },
            ],
            return_type: InferredType::known("Nil"),
            provenance: TypeProvenance::Extracted,
            line: None,
        }],
    );
    // Call with child: keyword — should not warn about mismatch with "arg"
    let module = make_module(vec![msg_send(
        erlang_module_recv("beamtalk_supervisor"),
        keyword_selector(&["terminateChild:", "child:"]),
        vec![var("self"), var("child")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(reg);
    checker.check_module(&module, &hierarchy);

    let keyword_warnings: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("does not match"))
        .collect();
    assert!(
        keyword_warnings.is_empty(),
        "Unnamed 'arg' param should not trigger keyword mismatch. Got: {keyword_warnings:?}"
    );
}

// ---- BT-1880: Class protocol selectors vs FFI module lookups ----

#[test]
fn test_erlang_class_resolves_as_class_protocol_not_ffi() {
    // `Erlang class` should resolve as a class protocol message (returns Metaclass),
    // NOT as ErlangModule<class>.
    let module = make_module(vec![msg_send(
        class_ref("Erlang"),
        MessageSelector::Unary("class".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    // The result should NOT be ErlangModule<class>
    let send_type = checker.type_map().get(span());
    if let Some(InferredType::Known { class_name, .. }) = send_type {
        assert_ne!(
            class_name.as_str(),
            "ErlangModule",
            "`Erlang class` should not be inferred as ErlangModule"
        );
    }
    // No FFI-related diagnostics expected
    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("FFI") || d.message.contains("ErlangModule"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "`Erlang class` should not produce FFI diagnostics. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_erlang_new_resolves_as_class_protocol_not_ffi() {
    // `Erlang new` should resolve as a class protocol message,
    // NOT as ErlangModule<new>.
    let module = make_module(vec![msg_send(
        class_ref("Erlang"),
        MessageSelector::Unary("new".into()),
        vec![],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    if let Some(InferredType::Known { class_name, .. }) = send_type {
        assert_ne!(
            class_name.as_str(),
            "ErlangModule",
            "`Erlang new` should not be inferred as ErlangModule"
        );
    }
}

#[test]
fn test_erlang_module_proxy_class_uses_normal_dispatch() {
    // `proxy := Erlang lists; proxy class` — `class` on an ErlangModule instance
    // should use normal dispatch (class protocol), not FFI lookup.
    let module = make_module(vec![
        assign("proxy", erlang_module_recv("lists")),
        msg_send(var("proxy"), MessageSelector::Unary("class".into()), vec![]),
    ]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    // Should not produce any FFI-related diagnostics for `class`
    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("lists:class"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "`proxy class` should not trigger FFI lookup. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_erlang_module_proxy_equality_uses_normal_dispatch() {
    // `proxy := Erlang lists; proxy == other` — binary selector on ErlangModule
    // should use normal dispatch, not FFI lookup.
    let module = make_module(vec![
        assign("proxy", erlang_module_recv("lists")),
        msg_send(
            var("proxy"),
            MessageSelector::Binary("==".into()),
            vec![var("other")],
        ),
    ]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    // Should not produce FFI diagnostics for `==`
    let ffi_diags: Vec<_> = checker
        .diagnostics()
        .iter()
        .filter(|d| d.message.contains("lists:==") || d.message.contains("FFI"))
        .collect();
    assert!(
        ffi_diags.is_empty(),
        "`proxy == other` should not trigger FFI lookup. Got: {ffi_diags:?}"
    );
}

#[test]
fn test_erlang_lists_still_infers_ffi() {
    // Sanity check: `Erlang lists reverse: xs` should still go through FFI inference.
    let module = make_module(vec![msg_send(
        erlang_module_recv("lists"),
        keyword_selector(&["reverse:"]),
        vec![var("xs")],
    )]);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(lists_registry());
    checker.check_module(&module, &hierarchy);

    let send_type = checker.type_map().get(span());
    assert_eq!(
        send_type,
        Some(&InferredType::known("List")),
        "FFI call should still infer correct return type"
    );
}
