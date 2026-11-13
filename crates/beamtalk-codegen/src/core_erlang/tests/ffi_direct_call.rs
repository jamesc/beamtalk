// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4485: an inline FFI call whose spec proves the result needs no
//! coercion, and whose arguments are proved to reach the callee unchanged,
//! compiles to a direct remote call wrapped in the proxy's error translation
//! (`beamtalk_erlang_proxy:ffi_call_raised/8`, `/9` for a binary or map argument). Anything the FFI type table
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

/// A signature whose return type is a union, as the spec reader emits for
/// `binary()` (`String | Binary`) and `string()` (`String | List`).
fn sig_union(name: &str, params: &[&str], members: &[&str]) -> FunctionSignature {
    let mut s = sig(name, params, "Dynamic");
    s.return_type = InferredType::simple_union(members);
    s
}

/// A signature whose return type is the `Result(V, Exception)` the spec reader
/// emits for an `{ok, V}` / `ok` / `{error, R}` spec.
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
        "erlang",
        vec![
            sig("monotonic_time", &[], "Integer"),
            sig("phash2", &["Dynamic"], "Integer"),
            sig("abs", &["Number"], "Number"),
            sig_union("integer_to_binary", &["Integer"], &["String", "Binary"]),
            sig_union("integer_to_list", &["Integer"], &["String", "List"]),
            sig("get", &["Dynamic"], "Dynamic"),
            sig("node", &[], "Symbol"),
            sig_result("find_int", &["Integer"], "Integer"),
            sig_result("find_bin", &["Integer"], "Binary"),
            sig_result("just_ok", &[], "Nil"),
            sig_result("find_list", &["Integer"], "List"),
            sig_result("find_dyn", &["Integer"], "Dynamic"),
            sig("map_size", &["Dictionary"], "Integer"),
            sig_result("find_map", &["Integer", "Dictionary"], "Dynamic"),
        ],
    );
    registry.register_module("beamtalk_process_ops", vec![sig("current", &[], "Pid")]);
    registry.register_module(
        "beamtalk_ops",
        vec![sig_result("find", &["Integer"], "Integer")],
    );
    registry.trust_all_modules();
    registry
}

fn compile_run_body(body: &str) -> String {
    let src = format!(
        "Object subclass: Probe\n  class run: n :: Integer str: s :: String dict: d :: Dictionary =>\n    {body}\n"
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
    // A plain direct call is `try call 'M':'F'(`; an inline `Result` (BT-4762)
    // binds the raw result first: `try let <Raw> = call 'M':'F'(`.
    let call = format!("call '{module}':'{function}'(");
    let try_call = format!("try {call}");
    let try_let = "try let _FfiRaw".to_string();
    assert!(
        code.contains(&try_call)
            || (code.contains(&try_let) && code.contains(&format!("= {call}"))),
        "expected a `try` around `{call}` in:\n{code}"
    );
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
        &compile_run_body("Erlang erlang monotonic_time"),
        "erlang",
        "monotonic_time",
        "stock",
    );
}

#[test]
fn scalar_arg_scalar_result_is_direct() {
    let code = compile_run_body("Erlang erlang phash2: n");
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
    let code = compile_run_body("Erlang erlang abs: -3");
    assert_direct(&code, "erlang", "abs", "stock");
    assert_guarded(&code, "erlang", "abs");
}

#[test]
fn beamtalk_module_keeps_its_mode() {
    assert_direct(
        &compile_run_body("Erlang beamtalk_process_ops current"),
        "beamtalk_process_ops",
        "current",
        "beamtalk",
    );
}

/// BT-4759: a `String`/`Binary` argument is proved behind the `is_binary`
/// guard: a raw binary (a `Binary`, or a `binary()` FFI result typed
/// `String | Binary`) takes the direct call; a tagged String fails the guard
/// and falls back to the proxy.
#[test]
fn string_arg_is_direct_behind_is_binary_guard() {
    let code = compile_run_body("Erlang erlang phash2: s");
    assert_direct(&code, "erlang", "phash2", "stock");
    assert_guarded(&code, "erlang", "phash2");
    assert!(
        code.contains("call 'erlang':'is_binary'(_FfiArg"),
        "the String argument is guarded by is_binary:\n{code}"
    );
    // A binary argument can retry on `badarg`, so the call site keeps the
    // pre-call class-variable snapshot for it (as the proxy does).
    assert!(
        code.contains("= call 'beamtalk_class_vars':'snapshot'() in try call 'erlang':'phash2'("),
        "the snapshot is read before the call:\n{code}"
    );
    assert!(
        code.contains("'stock', _FfiSite@") && code.contains(", _FfiSnap@"),
        "the snapshot is handed to ffi_call_raised/9:\n{code}"
    );
    // No proxy hop on the happy path: the only `direct_call` is the guard's
    // fallback arm.
    assert_eq!(
        code.matches("'direct_call'(").count(),
        1,
        "only the fallback arm calls the proxy:\n{code}"
    );
    assert!(
        !code.contains("'beamtalk_message_dispatch':'send'"),
        "no dynamic send in:\n{code}"
    );
}

/// A scalar-typed argument is checked exactly as before BT-4759: no
/// `is_binary`, no snapshot, `ffi_call_raised/8`.
#[test]
fn scalar_arg_has_no_binary_guard_or_snapshot() {
    let code = compile_run_body("Erlang erlang phash2: n");
    assert!(!code.contains("'is_binary'"), "{code}");
    assert!(!code.contains("'snapshot'"), "{code}");
}

/// A `string()` argument position holding a charlist (`List`) still proxies.
#[test]
fn list_arg_stays_on_proxy() {
    assert_proxied(
        &compile_run_body("Erlang erlang phash2: (List new)"),
        "erlang",
        "phash2",
    );
}

#[test]
fn block_arg_stays_on_proxy() {
    assert_proxied(
        &compile_run_body("Erlang erlang phash2: [n]"),
        "erlang",
        "phash2",
    );
}

/// BT-4755: a `binary()` result (`String | Binary`) crosses the proxy
/// unchanged in both modes, so the call is direct and keeps the proxy's error
/// translation.
#[test]
fn binary_result_is_direct() {
    let code = compile_run_body("Erlang erlang integer_to_binary: n");
    assert_direct(&code, "erlang", "integer_to_binary", "stock");
    assert_guarded(&code, "erlang", "integer_to_binary");
}

/// `string()` (`String | List`) is a charlist the proxy converts, so it keeps
/// the proxy even though its `String` member is now proved.
#[test]
fn charlist_result_stays_on_proxy() {
    assert_proxied(
        &compile_run_body("Erlang erlang integer_to_list: n"),
        "erlang",
        "integer_to_list",
    );
}

#[test]
fn dynamic_result_stays_on_proxy() {
    assert_proxied(&compile_run_body("Erlang erlang get: #k"), "erlang", "get");
}

#[test]
fn symbol_result_stays_on_proxy() {
    // `atom()` includes `ok` and `error`, which the proxy coerces to a Result.
    assert_proxied(&compile_run_body("Erlang erlang node"), "erlang", "node");
}

#[test]
fn function_without_spec_stays_on_proxy() {
    assert_proxied(
        &compile_run_body("Erlang erlang unknownFn: n"),
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
    // BT-4507: dialyzer specs are not enforced, so an extracted spec from a
    // hex/path dependency must not remove the result coercion.
    let mut registry = NativeTypeRegistry::new();
    registry.register_module("dep_mod", vec![sig("answer", &[], "Integer")]);
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

/// BT-4762: a stock-mode call whose spec is `Result(V, _)` with a proved `V`
/// builds the `Result` inside the `try`, with the proxy's `coerce_ffi_result/3`
/// as the fallback for every other shape. No new send or proxy hop: the call
/// is the same direct remote call, and the `Result` is `beamtalk_result`'s
/// own constructor.
#[test]
fn ok_value_result_is_built_inline() {
    let code = compile_run_body("Erlang erlang find_int: n");
    assert_direct(&code, "erlang", "find_int", "stock");
    assert_guarded(&code, "erlang", "find_int");
    assert!(
        code.contains("let _FfiRaw") && code.contains("= call 'erlang':'find_int'(_FfiArg"),
        "the raw result is bound inside the try:\n{code}"
    );
    assert!(
        code.contains("<{'ok', _FfiPayload") && code.contains("<{'error', _FfiPayload"),
        "the ok and error arms match the tagged tuples:\n{code}"
    );
    assert!(
        code.contains("call 'erlang':'not'(call 'erlang':'is_list'(_FfiPayload"),
        "a list payload is left to the proxy's charlist coercion:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_result':'from_tagged_tuple'(_FfiRaw"),
        "the Result is the constructor the proxy ends in:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_result':'from_tagged_tuple'({'ok', 'nil'})"),
        "a bare `ok` is `{{ok, nil}}`:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_erlang_proxy':'coerce_ffi_result'('erlang', _FfiOther"),
        "everything else is coerced by the proxy on the value already returned:\n{code}"
    );
    // No dynamic send, class-process call or `Result ok:` dispatch.
    for banned in [
        "'beamtalk_message_dispatcher'",
        "'beamtalk_class_registry'",
        "'ok:'",
    ] {
        assert!(!code.contains(banned), "unexpected {banned}:\n{code}");
    }
}

#[test]
fn binary_value_result_is_built_inline() {
    let code = compile_run_body("Erlang erlang find_bin: n");
    assert_direct(&code, "erlang", "find_bin", "stock");
    assert!(code.contains("'from_tagged_tuple'"), "{code}");
}

#[test]
fn bare_ok_result_is_built_inline_with_no_arguments() {
    let code = compile_run_body("Erlang erlang just_ok");
    assert_direct(&code, "erlang", "just_ok", "stock");
    assert!(
        code.contains("'from_tagged_tuple'({'ok', 'nil'})"),
        "{code}"
    );
}

#[test]
fn list_value_result_stays_on_proxy() {
    // `V` may be a charlist: stock mode must convert it.
    assert_proxied(
        &compile_run_body("Erlang erlang find_list: n"),
        "erlang",
        "find_list",
    );
}

/// BT-4770: a spec that leaves `V` open (`maps:find/2`'s `Value` type variable
/// reads as `Dynamic`) still builds the `Result` inline: the payload is
/// checked with `not is_list` at run time and a list goes to the proxy.
#[test]
fn dynamic_value_result_is_built_inline() {
    let code = compile_run_body("Erlang erlang find_dyn: n");
    assert_direct(&code, "erlang", "find_dyn", "stock");
    assert!(
        code.contains("call 'erlang':'not'(call 'erlang':'is_list'(_FfiPayload"),
        "a list payload is left to the proxy:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_erlang_proxy':'coerce_ffi_result'('erlang', _FfiOther"),
        "{code}"
    );
}

/// BT-4770: a `Dictionary` argument is proved (the stock-mode unwrap is shallow
/// and a map is never a tagged String), guarded at run time by `is_map`. The
/// `badarg` retry deep-unwraps a map, so the call site keeps the class-variable
/// snapshot for it (`ffi_call_raised/9`), as for a binary.
#[test]
fn dictionary_arg_is_direct_behind_is_map_guard() {
    let code = compile_run_body("Erlang erlang map_size: d");
    assert_direct(&code, "erlang", "map_size", "stock");
    assert_guarded(&code, "erlang", "map_size");
    assert!(
        code.contains("call 'erlang':'is_map'(_FfiArg"),
        "the Dictionary argument is guarded by is_map:\n{code}"
    );
    assert!(!code.contains("'is_binary'"), "{code}");
    assert!(
        code.contains("= call 'beamtalk_class_vars':'snapshot'() in try call 'erlang':'map_size'("),
        "the snapshot is read before the call:\n{code}"
    );
    assert!(
        code.contains("'stock', _FfiSite@") && code.contains(", _FfiSnap@"),
        "the snapshot is handed to ffi_call_raised/9:\n{code}"
    );
    // No proxy hop on the happy path and no dynamic send.
    assert_eq!(
        code.matches("'direct_call'(").count(),
        1,
        "only the fallback arm calls the proxy:\n{code}"
    );
    for banned in [
        "'unwrap_string_args'",
        "'apply_with_coercion'",
        "'beamtalk_message_dispatch':'send'",
    ] {
        assert!(!code.contains(banned), "unexpected {banned}:\n{code}");
    }
}

/// A scalar argument next to a `Dictionary` one: each is guarded on its own,
/// and only the map gets `is_map`.
#[test]
fn dictionary_and_scalar_args_are_guarded_separately() {
    let code = compile_run_body("Erlang erlang find_map: n dict: d");
    assert_direct(&code, "erlang", "find_map", "stock");
    assert_eq!(
        code.matches("call 'erlang':'is_map'(").count(),
        1,
        "only the Dictionary argument has is_map:\n{code}"
    );
    // `find_map`'s `Dynamic` value is built inline as well.
    assert!(code.contains("'from_tagged_tuple'(_FfiRaw"), "{code}");
}

/// A map *shape* or any other container stays on the proxy (a shape could hold
/// a tagged String the unwrap must see; only a plain `Dictionary` is proved).
#[test]
fn list_and_tuple_args_still_stay_on_proxy() {
    assert_proxied(
        &compile_run_body("Erlang erlang map_size: (List new)"),
        "erlang",
        "map_size",
    );
}

#[test]
fn beamtalk_module_result_stays_on_proxy() {
    // Beamtalk-mode coercion differs (raw-result opt-out, bare `error`).
    assert_proxied(
        &compile_run_body("Erlang beamtalk_ops find: n"),
        "beamtalk_ops",
        "find",
    );
}
