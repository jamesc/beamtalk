// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4765: a `self delegate` method on a `native:` class whose backing
//! function's spec proves the result needs no coercion, and whose arguments
//! (the receiver included) are proved to reach it unchanged, compiles to a
//! guarded direct call with `native_call/5` as the fallback arm, instead of
//! `native_call/5` alone. The proof table is BT-4485's/BT-4755's/BT-4759's;
//! these tests pin the emitted shape.

use beamtalk_core::semantic_analysis::type_checker::{
    FunctionSignature, NativeTypeRegistry, ParamType,
};
use beamtalk_core::semantic_analysis::{InferredType, TypeProvenance};

fn extracted(class_name: &str) -> InferredType {
    InferredType::Known {
        class_name: ecow::EcoString::from(class_name),
        type_args: vec![],
        provenance: TypeProvenance::Extracted,
    }
}

fn sig(name: &str, params: &[&str], ret: &str) -> FunctionSignature {
    FunctionSignature {
        name: name.to_string(),
        arity: u8::try_from(params.len()).expect("small arity"),
        params: params
            .iter()
            .map(|p| ParamType {
                keyword: None,
                type_: extracted(p),
            })
            .collect(),
        return_type: extracted(ret),
        provenance: TypeProvenance::Extracted,
        line: None,
    }
}

fn sig_result(name: &str, params: &[&str], value: &str) -> FunctionSignature {
    let mut s = sig(name, params, "Dynamic");
    s.return_type = InferredType::Known {
        class_name: ecow::EcoString::from("Result"),
        type_args: vec![
            // The spec reader maps an open type (a type variable, `any()`) to
            // the real `Dynamic`, not a class named "Dynamic".
            if value == "Dynamic" {
                InferredType::Dynamic(beamtalk_core::semantic_analysis::DynamicReason::DynamicSpec)
            } else {
                extracted(value)
            },
            extracted("Exception"),
        ],
        provenance: TypeProvenance::Extracted,
    };
    s
}

fn registry() -> NativeTypeRegistry {
    let mut registry = NativeTypeRegistry::new();
    registry.register_module(
        "beamtalk_binary",
        vec![
            sig("bitSize", &["Binary"], "Integer"),
            sig("asHex", &["Binary"], "Dynamic"),
            sig("fromList", &["List"], "Integer"),
            sig("count", &[], "Integer"),
            sig("scaled", &["Integer"], "Integer"),
        ],
    );
    registry.register_module(
        "beamtalk_probe",
        vec![
            sig("count", &[], "Integer"),
            sig("scaled", &["Integer"], "Integer"),
            sig("sized", &["Integer"], "Integer"),
            sig("listy", &["List"], "Integer"),
            sig("blocky", &["Block"], "Integer"),
            sig("untyped", &["Dynamic"], "Integer"),
            sig("sym", &[], "Symbol"),
        ],
    );
    // A stock-mode backing module (no `beamtalk_` prefix): its specs may
    // prove an inline `Result` (BT-4770).
    registry.register_module(
        "probe_native",
        vec![
            sig_result("found", &["Integer"], "Integer"),
            sig_result("lookup", &["Dictionary"], "Dynamic"),
            sig_result("listy", &["Integer"], "List"),
            sig("size", &["Dictionary"], "Integer"),
        ],
    );
    registry.trust_all_modules();
    registry
}

fn compile(src: &str, registry: NativeTypeRegistry) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diagnostics) = beamtalk_core::source_analysis::parse(tokens);
    assert!(
        diagnostics
            .iter()
            .all(|d| d.severity != beamtalk_core::source_analysis::Severity::Error),
        "fixture failed to parse: {diagnostics:?}"
    );
    crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("probe")
            .with_native_type_registry(Some(std::sync::Arc::new(registry))),
    )
    .expect("codegen should succeed")
}

fn binary_class() -> String {
    compile(
        "sealed typed Collection subclass: Binary native: beamtalk_binary\n  \
         bitSize -> Integer => self delegate\n  \
         asHex -> String => self delegate\n",
        registry(),
    )
}

fn probe_class(methods: &str) -> String {
    compile(
        &format!("sealed typed Object subclass: Probe native: beamtalk_probe\n{methods}"),
        registry(),
    )
}

/// The delegate's function definition (`'name'/N = fun ...`), up to the next
/// blank line; the export list also names `'name'/N`, so look for the `=`.
fn function_body<'a>(code: &'a str, name: &str) -> &'a str {
    let needle = format!("'{name}'/");
    let start = code
        .match_indices(&needle)
        .map(|(i, _)| i)
        .find(|&i| {
            let rest = &code[i + needle.len()..];
            let digits = rest.chars().take_while(char::is_ascii_digit).count();
            rest[digits..].starts_with(" =")
        })
        .unwrap_or_else(|| panic!("no definition of `{name}` in:\n{code}"));
    let rest = &code[start..];
    let end = rest.find("\n\n").unwrap_or(rest.len());
    &rest[..end]
}

/// `Binary>>bitSize -> Integer => self delegate`: the receiver is a
/// `Binary` (guarded by `is_binary`) and the spec result is an `Integer`.
#[test]
fn binary_bit_size_is_a_guarded_direct_call() {
    let code = binary_class();
    let body = function_body(&code, "bitSize");
    assert!(
        body.contains("try call 'beamtalk_binary':'bitSize'("),
        "bitSize is a direct call:\n{body}"
    );
    assert!(
        body.contains("call 'erlang':'is_binary'("),
        "the receiver is guarded by is_binary:\n{body}"
    );
    // The error path is the delegate's: same `{Class, Selector}` context, with
    // the pre-call class-variable snapshot a binary receiver needs.
    assert!(
        body.contains("{'Binary', 'bitSize'}") && body.contains("_FfiSnap@"),
        "ffi_call_raised/10 carries the delegate context and the snapshot:\n{body}"
    );
    assert!(
        body.contains("'ffi_call_raised'("),
        "error translation is kept:\n{body}"
    );
    // The fallback arm is the `native_call/5` the delegate emitted before, and
    // it is the only proxy hop: no `direct_call`, no dynamic send.
    assert_eq!(body.matches("'native_call'(").count(), 1, "{body}");
    assert!(
        body.contains(
            "<> when 'true' -> call 'beamtalk_erlang_proxy':'native_call'('beamtalk_binary', 'bitSize', ["
        ),
        "the fallback arm is native_call/5:\n{body}"
    );
    assert!(!body.contains("'direct_call'("), "{body}");
    assert!(
        !body.contains("'beamtalk_message_dispatch'"),
        "no new dynamic send:\n{body}"
    );
}

/// An unproved result (`String` is proved, but this spec is `Dynamic`) keeps
/// `native_call/5` alone — exactly the code generated before BT-4765.
#[test]
fn unproved_result_keeps_native_call_only() {
    let code = binary_class();
    let body = function_body(&code, "asHex");
    assert!(body.contains("'native_call'("), "{body}");
    assert!(!body.contains("try call"), "{body}");
    assert!(!body.contains("'ffi_call_raised'"), "{body}");
}

/// A class-side delegate with no arguments has nothing to guard: a direct
/// call whose error path carries the class-side `{Class, Selector}`.
#[test]
fn zero_arg_class_delegate_is_direct_without_a_guard() {
    let code = probe_class("  class count -> Integer => self delegate\n");
    let body = function_body(&code, "class_count");
    assert!(
        body.contains("try call 'beamtalk_probe':'count'()"),
        "{body}"
    );
    assert!(
        !body.contains("case <> of"),
        "no arguments, no guard:\n{body}"
    );
    assert!(!body.contains("'native_call'("), "no proxy hop:\n{body}");
    assert!(body.contains("{'Probe', 'count'}"), "{body}");
    assert!(
        !body.contains("'snapshot'"),
        "no binary, no snapshot:\n{body}"
    );
}

/// A scalar-typed argument: `is_number`/`is_atom`/`is_pid` guard, no
/// `is_binary`, no snapshot (`ffi_call_raised/10` takes `'none'`).
#[test]
fn scalar_arg_class_delegate_is_guarded_without_snapshot() {
    let code = probe_class("  class scaled: n :: Integer -> Integer => self delegate\n");
    let body = function_body(&code, "class_scaled:");
    assert!(
        body.contains("try call 'beamtalk_probe':'scaled'("),
        "{body}"
    );
    assert!(body.contains("call 'erlang':'is_number'("), "{body}");
    assert!(!body.contains("'is_binary'"), "{body}");
    assert!(!body.contains("'snapshot'"), "{body}");
    assert!(body.contains(", 'none', {'Probe', 'scaled:'}"), "{body}");
    assert!(
        body.contains("<> when 'true' -> call 'beamtalk_erlang_proxy':'native_call'("),
        "{body}"
    );
}

/// What cannot be proved keeps `native_call/5` alone: a charlist/`List`
/// argument, a `Block` argument (it could run Beamtalk code in the callee), an
/// untyped parameter, an unproved (`Symbol`) result.
#[test]
fn unproved_delegates_keep_native_call_only() {
    let code = probe_class(
        "  class listy: l :: List -> Integer => self delegate\n  \
         class blocky: b :: Block -> Integer => self delegate\n  \
         class untyped: u -> Integer => self delegate\n  \
         class sym -> Symbol => self delegate\n",
    );
    for name in [
        "class_listy:",
        "class_blocky:",
        "class_untyped:",
        "class_sym",
    ] {
        let body = function_body(&code, name);
        assert!(body.contains("'native_call'("), "{name}:\n{body}");
        assert!(!body.contains("try call"), "{name}:\n{body}");
        assert!(!body.contains("'ffi_call_raised'"), "{name}:\n{body}");
    }
}

/// A delegate whose backing function has no spec in the registry (or whose
/// module is untrusted) is not proved.
#[test]
fn untrusted_or_unspecced_backing_keeps_native_call_only() {
    let mut untrusted = NativeTypeRegistry::new();
    untrusted.register_module("beamtalk_probe", vec![sig("count", &[], "Integer")]);
    let src = "sealed typed Object subclass: Probe native: beamtalk_probe\n  \
               class count -> Integer => self delegate\n";
    let code = compile(src, untrusted);
    assert!(!function_body(&code, "class_count").contains("try call"));
    let code = compile(src, NativeTypeRegistry::new());
    assert!(!function_body(&code, "class_count").contains("try call"));
}

fn native_class(methods: &str) -> String {
    compile(
        &format!("sealed typed Object subclass: Probe native: probe_native\n{methods}"),
        registry(),
    )
}

/// BT-4770: a stock-mode delegate whose spec states an `{ok, V}` return builds
/// the `Result` at the call site, inside the `try`, exactly as an inline
/// `Erlang module fn:` send does, and still carries `{Class, Selector}`.
#[test]
fn ok_result_delegate_is_built_inline() {
    let code = native_class("  class found: n :: Integer -> Result => self delegate\n");
    let body = function_body(&code, "class_found:");
    assert!(
        body.contains("let _FfiRaw") && body.contains("= call 'probe_native':'found'("),
        "the raw result is bound inside the try:\n{body}"
    );
    assert!(
        body.contains("call 'beamtalk_result':'from_tagged_tuple'(_FfiRaw"),
        "{body}"
    );
    assert!(
        body.contains("call 'beamtalk_erlang_proxy':'coerce_ffi_result'('probe_native', _FfiOther"),
        "everything else is coerced by the proxy on the value already returned:\n{body}"
    );
    assert!(body.contains("{'Probe', 'found:'}"), "{body}");
    assert!(
        body.contains("<> when 'true' -> call 'beamtalk_erlang_proxy':'native_call'("),
        "the fallback is still native_call/5:\n{body}"
    );
}

/// A `Dictionary` parameter of a delegate is proved and guarded by `is_map`,
/// with the snapshot for the retry (`ffi_call_raised/10` takes it).
#[test]
fn dictionary_param_delegate_is_guarded_by_is_map() {
    let code = native_class("  class size: d :: Dictionary -> Integer => self delegate\n");
    let body = function_body(&code, "class_size:");
    assert!(body.contains("try call 'probe_native':'size'("), "{body}");
    assert!(body.contains("call 'erlang':'is_map'(_"), "{body}");
    assert!(
        body.contains("= call 'beamtalk_class_vars':'snapshot'() in try call"),
        "{body}"
    );
    assert!(body.contains("{'Probe', 'size:'}"), "{body}");
    assert!(!body.contains("'is_binary'"), "{body}");
    // `Dynamic`-valued `Result` through the same map parameter.
    let code = native_class("  class lookup: d :: Dictionary -> Result => self delegate\n");
    let body = function_body(&code, "class_lookup:");
    assert!(body.contains("'from_tagged_tuple'(_FfiRaw"), "{body}");
}

/// A `List`-valued `Result` and a `beamtalk_*` (Beamtalk-mode) backing module
/// keep `native_call/5` alone.
#[test]
fn list_valued_and_beamtalk_mode_results_keep_native_call_only() {
    let code = native_class("  class listy: n :: Integer -> Result => self delegate\n");
    let body = function_body(&code, "class_listy:");
    assert!(body.contains("'native_call'("), "{body}");
    assert!(!body.contains("try "), "{body}");
    let mut reg = registry();
    reg.register_module(
        "beamtalk_probe",
        vec![sig_result("found", &["Integer"], "Integer")],
    );
    reg.trust_all_modules();
    let code = compile(
        "sealed typed Object subclass: Probe native: beamtalk_probe\n  \
         class found: n :: Integer -> Result => self delegate\n",
        reg,
    );
    let body = function_body(&code, "class_found:");
    assert!(!body.contains("try "), "{body}");
}
