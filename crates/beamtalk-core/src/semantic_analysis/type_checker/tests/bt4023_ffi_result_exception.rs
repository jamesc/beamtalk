// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4023: an FFI-returned Result's error side is an `Exception`.
//!
//! The spec reader (`beamtalk_spec_reader:format_result_type/2`) maps every
//! `ok | {error, R}` / `{ok, T} | {error, R}` spec to `Result(T, Exception)`
//! (`Result(T, Error)` when `R` is `#beamtalk_error{}`),
//! because the ADR 0101 boundary wraps each error reason in an Exception.
//! These tests cover the Rust side of that contract: the wire strings the spec
//! reader now produces parse to `Result(_, Exception)`, `r error reason`
//! type-checks without a DNU warning, and comparing an `Exception` to a Symbol
//! literal with `=:=`, `==`, `=/=` or `/=` (either operand order) warns.

use super::super::native_types::map_type_name;
use super::common::*;
use crate::source_analysis::Severity;

/// Registry with two FFI functions typed exactly as the spec reader now
/// reports them for `file:read_file/1` (`{ok, binary()} | {error, posix()}`)
/// and a `ok | {error, atom()}` function.
fn ffi_registry() -> NativeTypeRegistry {
    let mut reg = NativeTypeRegistry::new();
    reg.register_module(
        "file",
        vec![FunctionSignature {
            name: "read_file".to_string(),
            arity: 1,
            params: vec![ParamType {
                keyword: Some(ecow::EcoString::from("path")),
                type_: InferredType::known("String"),
            }],
            return_type: map_type_name("Result(String | Binary, Exception)"),
            provenance: TypeProvenance::Extracted,
            line: None,
        }],
    );
    reg.register_module(
        "probe",
        vec![FunctionSignature {
            name: "touch".to_string(),
            arity: 0,
            params: vec![],
            return_type: map_type_name("Result(Nil, Exception)"),
            provenance: TypeProvenance::Extracted,
            line: None,
        }],
    );
    reg
}

fn check(body: &str) -> Vec<Diagnostic> {
    let src = format!("Object subclass: Probe\n  run =>\n{body}");
    let module = parse_source(&src);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(ffi_registry());
    checker.check_module(&module, &hierarchy);
    checker.take_diagnostics()
}

fn exception_symbol_warnings(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.message.contains("comparing an Exception to a Symbol"))
        .collect()
}

#[test]
fn wire_strings_parse_with_exception_error_side() {
    for (wire, ok) in [
        ("Result(Nil, Exception)", "UndefinedObject"),
        ("Result(Dynamic, Exception)", ""),
    ] {
        let ty = map_type_name(wire);
        let InferredType::Known {
            class_name,
            type_args,
            ..
        } = &ty
        else {
            panic!("expected Known Result for {wire}, got {ty:?}");
        };
        assert_eq!(class_name, "Result");
        assert_eq!(type_args.len(), 2, "{wire}");
        assert_eq!(type_args[1], InferredType::known("Exception"), "{wire}");
        if !ok.is_empty() {
            assert_eq!(type_args[0], InferredType::known(ok), "{wire}");
        }
    }
}

#[test]
fn ffi_result_error_reason_has_no_warning() {
    let diags = check(
        "    f := (Erlang file) read_file: \"/nonexistent/x\"\n    \
         f error reason =:= #enoent\n    \
         t := (Erlang probe) touch\n    \
         t error reason",
    );
    assert!(
        diags.is_empty(),
        "`r error reason` on an FFI Result must type-check cleanly, got: {diags:#?}"
    );
}

#[test]
fn ffi_result_error_compared_to_symbol_warns_for_every_operator_and_order() {
    for op in ["=:=", "==", "=/=", "/="] {
        for (body, suggestion) in [
            (
                format!("    f := (Erlang file) read_file: \"/x\"\n    f error {op} #closed"),
                format!("did you mean `f error reason {op} #closed`?"),
            ),
            (
                format!("    f := (Erlang file) read_file: \"/x\"\n    #closed {op} f error"),
                format!("did you mean `#closed {op} f error reason`?"),
            ),
        ] {
            let diags = check(&body);
            let hits = exception_symbol_warnings(&diags);
            assert_eq!(hits.len(), 1, "op `{op}`, body {body:?}: {diags:#?}");
            let d = hits[0];
            assert_eq!(d.severity, Severity::Warning);
            // A negated operator (`=/=`, `/=`) is always *true*.
            let outcome = if op.contains('/') { "true" } else { "false" };
            assert_eq!(
                d.message,
                format!(
                    "comparing an Exception to a Symbol with `{op}` is always {outcome}; \
                     use `f error reason`"
                )
            );
            assert_eq!(d.hint.as_deref(), Some(suggestion.as_str()), "op `{op}`");
        }
    }
}

#[test]
fn ffi_result_error_reason_compared_to_symbol_does_not_warn() {
    for op in ["=:=", "==", "=/=", "/="] {
        let diags = check(&format!(
            "    f := (Erlang file) read_file: \"/x\"\n    f error reason {op} #closed\n    \
             #closed {op} f error reason"
        ));
        assert!(
            exception_symbol_warnings(&diags).is_empty(),
            "op `{op}`: {diags:#?}"
        );
    }
}

/// Any `Exception`-typed expression (not only an FFI Result's error) is
/// covered, including subclasses — but only against a Symbol *literal*.
#[test]
fn exception_subclass_and_non_literal_operands() {
    // `on: Error do:` types the handler's block parameter as `Error`.
    let warned = check("    [nil] on: Error do: [:e | e == #oops]");
    let hits = exception_symbol_warnings(&warned);
    assert_eq!(hits.len(), 1, "{warned:#?}");
    assert_eq!(
        hits[0].hint.as_deref(),
        Some(
            "did you mean `e reason == #oops`? For a structured Beamtalk error, \
             compare its `kind` instead"
        )
    );

    let not_literal = check("    s := #oops\n    [nil] on: Exception do: [:e | e =:= s]");
    assert!(
        exception_symbol_warnings(&not_literal).is_empty(),
        "a Symbol-typed variable is not a Symbol literal: {not_literal:#?}"
    );

    let not_exception = check("    \"oops\" =:= #oops");
    assert!(exception_symbol_warnings(&not_exception).is_empty());
}

#[test]
fn warning_is_suppressible_with_expect_type() {
    let src = "Object subclass: Probe\n  run =>\n    \
        f := (Erlang file) read_file: \"/x\"\n    \
        @expect type\n    \
        f error =:= #closed";
    let module = parse_source(src);
    let hierarchy = ClassHierarchy::with_builtins();
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(ffi_registry());
    checker.check_module(&module, &hierarchy);
    let mut diags = checker.take_diagnostics();
    crate::compilation::diagnostics_policy::apply_expect_directives(&module, &mut diags);
    assert!(exception_symbol_warnings(&diags).is_empty(), "{diags:#?}");
}

/// A runtime shim specced `{ok, T} | {error, #beamtalk_error{}}` reads back as
/// `Result(T, Error)`, so the stdlib convention of declaring a wrapper
/// `-> Result(T, Error)` type-checks, while the same declaration over a
/// raw-reason (`Result(T, Exception)`) spec is a mismatch: `Exception` is
/// wider than `Error`.
#[test]
fn error_declared_wrapper_matches_only_beamtalk_error_specs() {
    let run = |wire: &str| {
        let mut reg = NativeTypeRegistry::new();
        reg.register_module(
            "shim",
            vec![FunctionSignature {
                name: "count".to_string(),
                arity: 0,
                params: vec![],
                return_type: map_type_name(wire),
                provenance: TypeProvenance::Extracted,
                line: None,
            }],
        );
        let module = parse_source(
            "typed Object subclass: Wrap\n  count -> Result(Integer, Error) =>\n    \
             (Erlang shim) count",
        );
        let hierarchy = ClassHierarchy::with_builtins();
        let mut checker = TypeChecker::new();
        checker.set_native_type_registry(reg);
        checker.check_module(&module, &hierarchy);
        checker
            .take_diagnostics()
            .into_iter()
            .filter(|d| d.message.contains("declares return type"))
            .count()
    };
    assert_eq!(run("Result(Integer, Error)"), 0);
    assert_eq!(run("Result(Integer, Exception)"), 1);
}
