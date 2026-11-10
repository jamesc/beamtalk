// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4485: an inline FFI call whose spec proves the result needs no
//! coercion, and whose arguments are proved to reach the callee unchanged,
//! compiles to a direct remote call wrapped in the proxy's error translation
//! (`beamtalk_erlang_proxy:ffi_call_raised/8`). Anything the FFI type table
//! cannot prove keeps `beamtalk_erlang_proxy:direct_call/4`.

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

fn registry() -> NativeTypeRegistry {
    let mut registry = NativeTypeRegistry::new();
    registry.register_module(
        "erlang",
        vec![
            sig("monotonic_time", &[], "Integer"),
            sig("phash2", &["Dynamic"], "Integer"),
            sig("abs", &["Number"], "Number"),
            sig("integer_to_binary", &["Integer"], "Binary"),
            sig("get", &["Dynamic"], "Dynamic"),
            sig("node", &[], "Symbol"),
        ],
    );
    registry.register_module("beamtalk_process_ops", vec![sig("current", &[], "Pid")]);
    // OTP and runtime specs are trusted to prove a result (BT-4507).
    registry.trust_all_modules();
    registry
}

fn compile(body: &str) -> String {
    let src = format!(
        "Object subclass: Probe\n  class run: n :: Integer str: s :: String =>\n    {body}\n"
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
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
            .with_native_type_registry(Some(std::sync::Arc::new(registry()))),
    )
    .expect("codegen should succeed")
}

fn assert_direct(code: &str, module: &str, function: &str, mode: &str) {
    let call = format!("try call '{module}':'{function}'(");
    assert!(code.contains(&call), "expected `{call}` in:\n{code}");
    assert!(
        code.contains("call 'beamtalk_erlang_proxy':'ffi_call_raised'("),
        "a direct call keeps the proxy's error translation:\n{code}"
    );
    // The catch variable is the raw stacktrace; the helper takes the list,
    // plus the call site's frame, read only on the error path.
    assert!(
        code.contains("primop 'build_stacktrace'(_FfiStk"),
        "the raw stacktrace is built before the hand-off:\n{code}"
    );
    assert!(
        code.contains("call 'erlang':'process_info'(_FfiSelf"),
        "the call site is read in the catch:\n{code}"
    );
    let tail = format!("'{module}', '{function}', [");
    assert!(code.contains(&tail), "expected `{tail}` in:\n{code}");
    let mode_site = format!("], '{mode}', _FfiSite");
    assert!(
        code.contains(&mode_site),
        "expected `{mode_site}` in:\n{code}"
    );
}

/// A direct call with arguments checks them at run time and falls back to
/// the proxy for a value its static type did not promise.
fn assert_guarded(code: &str, module: &str, function: &str) {
    assert!(
        code.contains("case <> of <> when "),
        "expected a guard in:\n{code}"
    );
    for bif in beamtalk_core::semantic_analysis::type_checker::FFI_ARG_RUNTIME_GUARDS {
        let test = format!("call 'erlang':'{bif}'(_FfiArg");
        assert!(code.contains(&test), "expected `{test}` in:\n{code}");
    }
    let fallback = format!(
        "<> when 'true' -> call 'beamtalk_erlang_proxy':'direct_call'('{module}', '{function}', ["
    );
    assert!(
        code.contains(&fallback),
        "expected `{fallback}` in:\n{code}"
    );
}

fn assert_proxied(code: &str, module: &str, function: &str) {
    let call = format!("call 'beamtalk_erlang_proxy':'direct_call'('{module}', '{function}',");
    assert!(code.contains(&call), "expected `{call}` in:\n{code}");
    assert!(
        !code.contains("'ffi_call_raised'("),
        "an unproved call stays on the proxy:\n{code}"
    );
}

#[test]
fn zero_arity_scalar_result_is_direct() {
    assert_direct(
        &compile("Erlang erlang monotonic_time"),
        "erlang",
        "monotonic_time",
        "stock",
    );
}

#[test]
fn scalar_arg_scalar_result_is_direct() {
    let code = compile("Erlang erlang phash2: n");
    assert_direct(&code, "erlang", "phash2", "stock");
    assert_guarded(&code, "erlang", "phash2");
    // The argument is bound before the `try`, so a raise while evaluating it
    // is not translated.
    let bind = code.find("FfiArg");
    let try_pos = code.find("try call 'erlang':'phash2'(_FfiArg");
    assert!(
        matches!((bind, try_pos), (Some(b), Some(t)) if b < t)
            && code[..try_pos.unwrap_or(0)].contains(" = _n@"),
        "argument bound before the try:\n{code}"
    );
}

#[test]
fn literal_scalar_arg_is_direct() {
    let code = compile("Erlang erlang abs: -3");
    assert_direct(&code, "erlang", "abs", "stock");
    assert_guarded(&code, "erlang", "abs");
}

#[test]
fn beamtalk_module_keeps_its_mode() {
    assert_direct(
        &compile("Erlang beamtalk_process_ops current"),
        "beamtalk_process_ops",
        "current",
        "beamtalk",
    );
}

#[test]
fn string_arg_stays_on_proxy() {
    assert_proxied(&compile("Erlang erlang phash2: s"), "erlang", "phash2");
}

#[test]
fn block_arg_stays_on_proxy() {
    assert_proxied(&compile("Erlang erlang phash2: [n]"), "erlang", "phash2");
}

#[test]
fn binary_result_stays_on_proxy() {
    assert_proxied(
        &compile("Erlang erlang integer_to_binary: n"),
        "erlang",
        "integer_to_binary",
    );
}

#[test]
fn dynamic_result_stays_on_proxy() {
    assert_proxied(&compile("Erlang erlang get: #k"), "erlang", "get");
}

#[test]
fn symbol_result_stays_on_proxy() {
    // `atom()` includes `ok` and `error`, which the proxy coerces to a Result.
    assert_proxied(&compile("Erlang erlang node"), "erlang", "node");
}

#[test]
fn function_without_spec_stays_on_proxy() {
    assert_proxied(
        &compile("Erlang erlang unknownFn: n"),
        "erlang",
        "unknownFn",
    );
}

#[test]
fn no_registry_stays_on_proxy() {
    let src = "Object subclass: Probe\n  class run: n :: Integer =>\n    Erlang erlang phash2: n\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("probe"),
    )
    .expect("codegen should succeed");
    assert_proxied(&code, "erlang", "phash2");
}

#[test]
fn stub_declared_result_stays_on_proxy() {
    // A hand-written stub may misstate the return type: it never removes a
    // coercion, only an extracted `-spec` does.
    let mut registry = NativeTypeRegistry::new();
    let mut stub = sig("answer", &[], "Integer");
    stub.provenance = TypeProvenance::Declared(beamtalk_core::source_analysis::Span::new(0, 0));
    registry.register_module("native_mod", vec![stub]);
    registry.trust_all_modules();
    let src = "Object subclass: Probe\n  class run =>\n    Erlang native_mod answer\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("probe")
            .with_native_type_registry(Some(std::sync::Arc::new(registry))),
    )
    .expect("codegen should succeed");
    assert_proxied(&code, "native_mod", "answer");
}

#[test]
fn untrusted_dependency_spec_stays_on_proxy() {
    // BT-4507: a dependency's `-spec answer() -> integer()` is not enforced.
    // If it is wrong (the function really returns `{ok, X}`, `ok` or a
    // charlist), a direct call would hand the caller the raw term instead of
    // the proxy's `Result`/`String`, so only trusted modules prove a call.
    let mut registry = registry();
    let mut dep = NativeTypeRegistry::new();
    dep.register_module("dep_mod", vec![sig("answer", &[], "Integer")]);
    registry.merge(dep);
    let src = "Object subclass: Probe\n  class run =>\n    Erlang dep_mod answer\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("probe")
            .with_native_type_registry(Some(std::sync::Arc::new(registry))),
    )
    .expect("codegen should succeed");
    assert_proxied(&code, "dep_mod", "answer");
}
