// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3778 (ADR 0111 Addendum 17): `ThreadedIr` verifier findings from the
//! REPL expression codegen reach the `compile_expression` response's
//! `warnings`, and the eval still succeeds.

use super::*;
use crate::handlers::expression::{
    handle_compile_expression_trace_with_options, handle_compile_expression_with_options,
};
use beamtalk_codegen::core_erlang::CodegenOptions;

fn response_warnings(response: &Term) -> Vec<String> {
    let Term::Map(m) = response else {
        panic!("Expected map response: {response:?}");
    };
    let Some(Term::List(warnings)) = map_get(m, "warnings") else {
        panic!("Expected warnings list: {response:?}");
    };
    warnings
        .elements
        .iter()
        .filter_map(term_to_string)
        .collect()
}

fn injected() -> CodegenOptions {
    CodegenOptions::new("probe").with_injected_verifier_violation()
}

fn assert_internal_warning(response: &Term) {
    assert_eq!(response_status(response).as_deref(), Some("ok"));
    assert!(
        response_field_str(response, "core_erlang").is_some_and(|c| c.contains("eval")),
        "eval still produces code: {response:?}"
    );
    let warnings = response_warnings(response);
    assert!(
        warnings.iter().any(|w| w.starts_with("internal:")),
        "expected an `internal:` verifier warning, got: {warnings:?}"
    );
}

#[test]
fn expression_eval_surfaces_injected_verifier_violation_and_succeeds() {
    let request = compile_expression_request("1 + 2");
    assert_internal_warning(&handle_compile_expression_with_options(
        &request,
        &injected(),
    ));
}

#[test]
fn expression_eval_has_no_internal_warning_by_default() {
    let response = handle_compile_expression_with_options(
        &compile_expression_request("1 + 2"),
        &CodegenOptions::new("probe"),
    );
    assert_eq!(response_status(&response).as_deref(), Some("ok"));
    assert!(
        response_warnings(&response)
            .iter()
            .all(|w| !w.starts_with("internal:")),
        "valid code adds no warning: {response:?}"
    );
}

#[test]
fn trace_eval_surfaces_injected_verifier_violation_and_succeeds() {
    let request = compile_expression_request("1 + 2");
    assert_internal_warning(&handle_compile_expression_trace_with_options(
        &request,
        &injected(),
    ));
}

#[test]
fn inline_class_trailing_expression_surfaces_injected_verifier_violation() {
    let request = compile_expression_request(
        "sealed Object subclass: Facade\n  class sealed osName -> String => \"linux\"\n\nFacade osName",
    );
    let response = handle_compile_expression_with_options(&request, &injected());
    assert_eq!(response_status(&response).as_deref(), Some("ok"));
    assert!(
        response_warnings(&response)
            .iter()
            .any(|w| w.starts_with("internal:")),
        "trailing-expression finding reaches the class-definition response: {response:?}"
    );
}
