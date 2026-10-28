// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4243: the `ffi-nested-string` hint.
//!
//! The FFI boundary (`beamtalk_erlang_proxy:unwrap_string_args/1`) unwraps
//! only *top-level* `String` arguments of a stock Erlang call. A `String`
//! nested in a list, map, tuple or set argument reaches Erlang as a tagged
//! `{beamtalk_string, Bin}`, so an Erlang comparison against a raw binary
//! (`lists:member/2`, `lists:keyfind/3`, `maps:get/2`) silently misses. The
//! hint fires when an argument's static type nests a `String` inside one of
//! those collections, at any depth; it is silent on a top-level `String`, on
//! `Dynamic`/unknown element types (ADR 0100 Rule 1), and on Beamtalk-owned
//! modules (`beamtalk_*`, `bt@*`).

use super::common::*;
use crate::compilation::diagnostics_policy::{
    apply_diagnostics_table, apply_expect_directives, parse_diagnostics_table_from_manifest_toml,
};
use crate::source_analysis::Severity;

fn build(source: &str) -> (Module, ClassHierarchy) {
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "Parse failed: {parse_diags:?}");
    let hierarchy = ClassHierarchy::build(&module).0.unwrap();
    (module, hierarchy)
}

/// Type-checks `source` and applies `@expect` suppression, like the real
/// pipeline (the type checker itself never suppresses).
fn diagnostics(source: &str) -> Vec<Diagnostic> {
    let (module, hierarchy) = build(source);
    let mut checker = TypeChecker::new();
    checker.check_module(&module, &hierarchy);
    let mut diags = checker.take_diagnostics();
    apply_expect_directives(&module, &mut diags);
    diags
}

fn nested_string_hints(diags: &[Diagnostic]) -> Vec<&Diagnostic> {
    diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::FfiNestedString))
        .collect()
}

/// A class whose `run:` method takes one parameter typed `param_type` and
/// whose body is `body` (which may refer to the parameter as `xs`).
fn probe(param_type: &str, body: &str) -> String {
    format!("Object subclass: Probe\n  run: xs :: {param_type} =>\n    {body}\n")
}

fn assert_one_hint(source: &str, call: &str) {
    let diags = diagnostics(source);
    let hints = nested_string_hints(&diags);
    assert_eq!(
        hints.len(),
        1,
        "expected one hint for {source:?}: {diags:#?}"
    );
    let d = hints[0];
    assert_eq!(d.severity, Severity::Hint);
    assert_eq!(
        d.message,
        format!(
            "Strings nested in this argument reach {call} as tagged Beamtalk Strings; \
             an Erlang comparison against raw binaries won't match"
        )
    );
    assert_eq!(
        d.hint.as_deref(),
        Some(
            "use the Beamtalk-side operation (`includes:`, `at:`, `detect:`), \
             or pass Symbols/raw binaries"
        )
    );
}

fn assert_no_hint(source: &str) {
    let diags = diagnostics(source);
    assert!(
        nested_string_hints(&diags).is_empty(),
        "expected no ffi-nested-string hint for {source:?}: {diags:#?}"
    );
}

// ── Each nested shape fires ─────────────────────────────────────────────

#[test]
fn list_of_string_literal_argument_hints() {
    // The issue's repro: `lists:member/2` answers false.
    assert_one_hint(
        "Object subclass: Probe\n  run =>\n    Erlang lists member: \"a\" with: #(\"a\")\n",
        "lists:member/2",
    );
}

#[test]
fn list_of_string_typed_parameter_hints() {
    assert_one_hint(
        &probe("List(String)", "Erlang lists member: \"a\" with: xs"),
        "lists:member/2",
    );
}

#[test]
fn dictionary_string_keys_hint() {
    assert_one_hint(
        "Object subclass: Probe\n  run =>\n    Erlang maps get: \"k\" with: #{\"k\" => 1}\n",
        "maps:get/2",
    );
    assert_one_hint(
        &probe(
            "Dictionary(String, Integer)",
            "Erlang maps get: \"k\" with: xs",
        ),
        "maps:get/2",
    );
}

#[test]
fn dictionary_string_values_hint() {
    assert_one_hint(
        "Object subclass: Probe\n  run =>\n    Erlang maps values: #{#k => \"v\"}\n",
        "maps:values/1",
    );
    assert_one_hint(
        &probe("Dictionary(Symbol, String)", "Erlang maps values: xs"),
        "maps:values/1",
    );
}

#[test]
fn tuple_with_string_element_hints() {
    assert_one_hint(
        "Object subclass: Probe\n  run =>\n    Erlang erlang element: 1 with: {\"a\", 1}\n",
        "erlang:element/2",
    );
    assert_one_hint(
        &probe(
            "Tuple(Integer, String)",
            "Erlang erlang element: 2 with: xs",
        ),
        "erlang:element/2",
    );
}

#[test]
fn set_of_string_hints() {
    assert_one_hint(
        &probe("Set(String)", "Erlang sets is_element: \"a\" with: xs"),
        "sets:is_element/2",
    );
}

#[test]
fn deeper_nesting_hints() {
    // Recursive: a String two collections down is still tagged.
    assert_one_hint(
        &probe("List(List(String))", "Erlang lists flatten: xs"),
        "lists:flatten/1",
    );
    // `lists:keyfind/3` over a list of tuples — the issue's second example.
    assert_one_hint(
        "Object subclass: Probe\n  run =>\n    \
         Erlang lists keyfind: \"a\" with: 1 with: #({\"a\", 1})\n",
        "lists:keyfind/3",
    );
    assert_one_hint(
        &probe(
            "Dictionary(Symbol, List(String))",
            "Erlang maps get: #k with: xs",
        ),
        "maps:get/2",
    );
}

#[test]
fn union_members_count_as_evidence() {
    // A String element in a union element type, or a nullable collection.
    assert_one_hint(
        &probe("List(String | Nil)", "Erlang lists member: nil with: xs"),
        "lists:member/2",
    );
    assert_one_hint(
        &probe("List(String) | Nil", "Erlang lists member: \"a\" with: xs"),
        "lists:member/2",
    );
}

#[test]
fn hint_spans_the_offending_argument() {
    let src = "Object subclass: Probe\n  run =>\n    Erlang lists member: \"a\" with: #(\"a\")\n";
    let diags = diagnostics(src);
    let hints = nested_string_hints(&diags);
    assert_eq!(hints.len(), 1, "{diags:#?}");
    let span = hints[0].span;
    assert_eq!(&src[span.start() as usize..span.end() as usize], "#(\"a\")");
}

#[test]
fn hint_fires_whether_or_not_the_registry_knows_the_function() {
    // With a registry entry for `lists:member/2` the hint is unchanged.
    let (module, hierarchy) =
        build("Object subclass: Probe\n  run =>\n    Erlang lists member: \"a\" with: #(\"a\")\n");
    let mut reg = NativeTypeRegistry::new();
    reg.register_module(
        "lists",
        vec![FunctionSignature {
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
        }],
    );
    let mut checker = TypeChecker::new();
    checker.set_native_type_registry(reg);
    checker.check_module(&module, &hierarchy);
    let diags = checker.take_diagnostics();
    assert_eq!(nested_string_hints(&diags).len(), 1, "{diags:#?}");
}

// ── Silent cases ────────────────────────────────────────────────────────

#[test]
fn top_level_string_argument_is_silent() {
    // The proxy unwraps a top-level String.
    assert_no_hint("Object subclass: Probe\n  run =>\n    Erlang string length: \"hello\"\n");
    assert_no_hint(&probe("String", "Erlang binary split: xs with: \",\""));
    assert_no_hint(&probe("String | Nil", "Erlang string length: xs"));
}

#[test]
fn dynamic_or_unknown_types_are_silent() {
    // Untyped parameter: Dynamic.
    assert_no_hint(
        "Object subclass: Probe\n  run: xs =>\n    Erlang lists member: \"a\" with: xs\n",
    );
    // Bare collection, no element type.
    assert_no_hint(&probe("List", "Erlang lists member: \"a\" with: xs"));
    assert_no_hint(&probe("Dictionary", "Erlang maps get: \"k\" with: xs"));
    // Explicitly Dynamic element type.
    assert_no_hint(&probe(
        "List(Dynamic)",
        "Erlang lists member: \"a\" with: xs",
    ));
}

#[test]
fn non_string_elements_are_silent() {
    assert_no_hint(
        "Object subclass: Probe\n  run =>\n    Erlang lists member: #a with: #(#a, #b)\n",
    );
    assert_no_hint(&probe(
        "Dictionary(Symbol, Integer)",
        "Erlang maps get: #k with: xs",
    ));
    assert_no_hint(&probe(
        "Tuple(Symbol, Integer)",
        "Erlang erlang element: 1 with: xs",
    ));
}

#[test]
fn strings_inside_non_collection_generics_are_silent() {
    // A Result is a Beamtalk object, not a collection Erlang compares into.
    assert_no_hint(&probe("Result(String, Error)", "Erlang erlang phash2: xs"));
}

#[test]
fn beamtalk_owned_modules_are_silent() {
    assert_no_hint(&probe(
        "List(String)",
        "Erlang beamtalk_list includes: xs with: \"a\"",
    ));
}

// ── `native:` delegation ────────────────────────────────────────────────

#[test]
fn native_object_self_delegate_param_hints() {
    let src = "Object subclass: Names native: my_names\n  \
               has: names :: List(String) => self delegate\n  \
               class fromKeys: keys :: Dictionary(String, Integer) => self delegate\n";
    let diags = diagnostics(src);
    let hints = nested_string_hints(&diags);
    assert_eq!(hints.len(), 2, "{diags:#?}");
    // Instance method: `self` is prepended, so the Erlang arity is 2.
    assert_eq!(
        hints[0].message,
        "Strings nested in parameter `names` reach my_names:has/2 as tagged Beamtalk \
         Strings; an Erlang comparison against raw binaries won't match"
    );
    // Class method: no `self`.
    assert_eq!(
        hints[1].message,
        "Strings nested in parameter `keys` reach my_names:fromKeys/1 as tagged Beamtalk \
         Strings; an Erlang comparison against raw binaries won't match"
    );
    let span = hints[0].span;
    assert_eq!(&src[span.start() as usize..span.end() as usize], "names");
}

#[test]
fn native_delegate_silent_cases() {
    // Beamtalk-owned backing module.
    assert_no_hint(
        "Object subclass: Names native: beamtalk_names\n  \
         has: names :: List(String) => self delegate\n",
    );
    // Top-level String parameter.
    assert_no_hint(
        "Object subclass: Names native: my_names\n  \
         has: name :: String => self delegate\n",
    );
    // Not a `self delegate` body.
    assert_no_hint(
        "Object subclass: Names native: my_names\n  \
         has: names :: List(String) => names size\n",
    );
    // A native Actor delegates via gen_server:call, not the stock boundary.
    assert_no_hint(
        "Actor subclass: Names native: my_names\n  \
         has: names :: List(String) => self delegate\n",
    );
}

// ── Suppression ─────────────────────────────────────────────────────────

#[test]
fn expect_ffi_nested_string_suppresses_the_hint() {
    assert_eq!(
        crate::ast::ExpectCategory::from_name("ffi_nested_string"),
        Some(crate::ast::ExpectCategory::FfiNestedString)
    );
    assert!(crate::ast::ExpectCategory::valid_names().contains(&"ffi_nested_string"));

    let diags = diagnostics(
        "Object subclass: Probe\n  run =>\n    \
         @expect ffi_nested_string\n    \
         Erlang lists member: \"a\" with: #(\"a\")\n",
    );
    assert!(
        diags.is_empty(),
        "@expect ffi_nested_string should suppress the hint without going stale: {diags:#?}"
    );
}

#[test]
fn expect_ffi_nested_string_suppresses_the_native_delegate_hint() {
    let diags = diagnostics(
        "Object subclass: Names native: my_names\n  \
         @expect ffi_nested_string\n  \
         has: names :: List(String) => self delegate\n",
    );
    assert!(diags.is_empty(), "{diags:#?}");
}

#[test]
fn diagnostics_table_off_drops_the_hint() {
    let diags = diagnostics(
        "Object subclass: Probe\n  run =>\n    Erlang lists member: \"a\" with: #(\"a\")\n",
    );
    assert_eq!(nested_string_hints(&diags).len(), 1);

    let table = parse_diagnostics_table_from_manifest_toml(
        "[package]\nname = \"demo\"\n\n[diagnostics]\nffi-nested-string = \"off\"\n",
    )
    .unwrap();
    assert_eq!(
        table.get(&DiagnosticCategory::FfiNestedString),
        Some(&crate::compilation::diagnostics_policy::DiagnosticSeverityOverride::Off)
    );
    let diags = apply_diagnostics_table(diags, &table);
    assert!(nested_string_hints(&diags).is_empty(), "{diags:#?}");
}

#[test]
fn diagnostics_table_can_escalate_the_hint() {
    let diags = diagnostics(
        "Object subclass: Probe\n  run =>\n    Erlang lists member: \"a\" with: #(\"a\")\n",
    );
    let table =
        parse_diagnostics_table_from_manifest_toml("[diagnostics]\nffi-nested-string = \"warn\"\n")
            .unwrap();
    let diags = apply_diagnostics_table(diags, &table);
    let hints = nested_string_hints(&diags);
    assert_eq!(hints.len(), 1);
    assert_eq!(hints[0].severity, Severity::Warning);
}
