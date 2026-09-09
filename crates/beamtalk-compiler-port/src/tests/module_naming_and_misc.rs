// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `reindent_method_source`, `compile_expression_trace`, `resolve_completion_type` edge cases, class-hierarchy builtin skipping, `derive_class_module_name` overrides, inline/protocol-only class-definition module naming, and standalone method-definition signature reporting.

use super::*;

// --- reindent_method_source tests ---

#[test]
fn reindent_method_source_shifts_canonical_to_base() {
    let request = Map::from([
        (atom("command"), atom("reindent_method_source")),
        (atom("source"), binary("/// doc\ndecrement => self.v - 2\n")),
        (atom("base_indent"), binary("  ")),
    ]);
    let response = handle_reindent_method_source(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    assert_eq!(
        map_get(m, "source").and_then(term_to_string).as_deref(),
        Some("  /// doc\n  decrement => self.v - 2\n"),
    );
}

#[test]
fn reindent_method_source_empty_base_is_identity() {
    let request = Map::from([
        (atom("command"), atom("reindent_method_source")),
        (atom("source"), binary("foo => 1\n")),
        (atom("base_indent"), binary("")),
    ]);
    let response = handle_reindent_method_source(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(
        map_get(m, "source").and_then(term_to_string).as_deref(),
        Some("foo => 1\n"),
    );
}

#[test]
fn reindent_method_source_missing_base_indent_defaults_empty() {
    // `base_indent` is optional and defaults to empty (identity).
    let request = Map::from([
        (atom("command"), atom("reindent_method_source")),
        (atom("source"), binary("foo => 1\n")),
    ]);
    let response = handle_reindent_method_source(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")), "{response:?}");
    assert_eq!(
        map_get(m, "source").and_then(term_to_string).as_deref(),
        Some("foo => 1\n"),
    );
}

/// `compile_expression_trace` produces Core Erlang with trace list return.
#[test]
fn compile_expression_trace_single_expression() {
    use eetf::List;

    let request = Map::from([
        (atom("command"), atom("compile_expression_trace")),
        (atom("source"), binary("1 + 1.")),
        (atom("module"), binary("bt@trace_test")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
    ]);
    let response = handle_compile_expression_trace(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response, got: {response:?}");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "compile_expression_trace should succeed: {response:?}"
    );
    let core_erlang = map_get(m, "core_erlang")
        .and_then(term_to_string)
        .expect("core_erlang field must be present");
    // Trace module must export eval/1 and return a steps list, not a plain {Result, State}.
    assert!(
        core_erlang.contains("'eval'/1"),
        "Trace module must export eval/1: {core_erlang}"
    );
    // The return must be a cons list [...] wrapping step tuples, not a plain 2-tuple.
    assert!(
        core_erlang.contains("[{"),
        "Trace return must be a list of step tuples: {core_erlang}"
    );
}

/// `compile_expression_trace` rejects class definitions.
#[test]
fn compile_expression_trace_rejects_class_definition() {
    use eetf::List;

    let request = Map::from([
        (atom("command"), atom("compile_expression_trace")),
        (atom("source"), binary("Object subclass: Foo\n  bar => 42")),
        (atom("module"), binary("bt@trace_class_test")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
    ]);
    let response = handle_compile_expression_trace(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("error")),
        "compile_expression_trace should reject class definitions: {response:?}"
    );
}

#[test]
fn resolve_completion_type_unknown_expression() {
    let request = Map::from([
        (atom("command"), atom("resolve_completion_type")),
        (atom("expression"), binary("unknownVar")),
    ]);
    let response = handle_resolve_completion_type(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("not_found")));
}

#[test]
fn resolve_completion_type_empty_expression() {
    let request = Map::from([
        (atom("command"), atom("resolve_completion_type")),
        (atom("expression"), binary("")),
    ]);
    let response = handle_resolve_completion_type(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("not_found")));
}

#[test]
fn parse_class_hierarchy_skips_builtins() {
    use eetf::{FixInteger, List};

    let meta_map = Map::from([
        (atom("class"), atom("Integer")),
        (atom("superclass"), atom("Number")),
        (atom("meta_version"), Term::from(FixInteger::from(2))),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("false")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(List::from(vec![]))),
        (atom("field_types"), Term::from(Map::from([]))),
        (atom("method_info"), Term::from(Map::from([]))),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (atom("class_variables"), Term::from(List::from(vec![]))),
    ]);
    let term = Term::from(Map::from([(atom("Integer"), Term::from(meta_map))]));
    let classes = parse_class_hierarchy_from_term(&term);
    assert!(
        classes.is_empty(),
        "Integer is a builtin — should be skipped"
    );
}

/// `derive_class_module_name` uses override when provided.
#[test]
fn derive_class_module_name_with_override() {
    let result = derive_class_module_name("Counter", Some("bt@my_app@counter"), false);
    assert_eq!(result, "bt@my_app@counter");
}

/// `derive_class_module_name` falls back to `bt@{snake}` without override.
#[test]
fn derive_class_module_name_no_override() {
    let result = derive_class_module_name("MyCounter", None, false);
    assert_eq!(result, "bt@my_counter");
}

/// `derive_class_module_name` uses stdlib prefix in stdlib mode.
#[test]
fn derive_class_module_name_stdlib_mode() {
    let result = derive_class_module_name("Integer", None, true);
    assert_eq!(result, "bt@stdlib@integer");
}

/// `derive_class_module_name` override takes precedence over stdlib mode.
#[test]
fn derive_class_module_name_override_over_stdlib() {
    let result = derive_class_module_name("Integer", Some("bt@custom@integer"), true);
    assert_eq!(result, "bt@custom@integer");
}

/// Inline class definition uses `module_name` override when provided,
/// ensuring package-mode REPL class definitions get the same module name as
/// file-based compilation.
#[test]
fn inline_class_definition_with_module_name_override() {
    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (
            atom("source"),
            binary("Object subclass: MyThing\n  greet => \"hello\""),
        ),
        (atom("module"), binary("bt@repl_eval_1")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
        (atom("module_name"), binary("bt@my_app@my_thing")),
    ]);

    let response = handle_compile_expression(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected a map response, got: {response:?}");
    };

    // Should be a class_definition response
    let status = map_get(m, "status").and_then(term_to_atom);
    assert_eq!(status.as_deref(), Some("ok"));

    let resp_kind = map_get(m, "kind").and_then(term_to_atom);
    assert_eq!(resp_kind.as_deref(), Some("class_definition"));

    // The module name should use the override, not bt@my_thing
    let module_name = map_get(m, "module_name").and_then(term_to_string);
    assert_eq!(module_name.as_deref(), Some("bt@my_app@my_thing"));
}

/// Protocol-only file compilation succeeds and derives module name from protocol.
#[test]
fn compile_protocol_only_file() {
    let request = Map::from([
        (atom("command"), atom("compile")),
        (atom("source"), binary("Protocol define: Awaitable")),
    ]);

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected a map response, got: {response:?}");
    };

    let status = map_get(m, "status").and_then(term_to_atom);
    assert_eq!(
        status.as_deref(),
        Some("ok"),
        "Protocol-only file should compile successfully"
    );

    // Protocol-only files return a protocol_definition response
    let kind = map_get(m, "kind").and_then(term_to_atom);
    assert_eq!(
        kind.as_deref(),
        Some("protocol_definition"),
        "Protocol-only file should return protocol_definition kind"
    );

    let module_name = map_get(m, "module_name").and_then(term_to_string);
    assert_eq!(module_name.as_deref(), Some("bt@awaitable"));

    // Protocols list should contain the protocol name
    let protocols = map_get(m, "protocols").expect("response should include 'protocols' key");
    let Term::List(list) = protocols else {
        panic!("Expected protocols to be a list, got: {protocols:?}");
    };
    assert_eq!(list.elements.len(), 1, "Should have one protocol");
    assert_eq!(
        term_to_string(&list.elements[0]).as_deref(),
        Some("Awaitable")
    );

    // core_erlang should be present
    assert!(
        map_get(m, "core_erlang").is_some(),
        "Protocol response should include core_erlang"
    );
}

/// Protocol-only file compilation with `module_name` override.
#[test]
fn compile_protocol_only_file_with_override() {
    let request = Map::from([
        (atom("command"), atom("compile")),
        (atom("source"), binary("Protocol define: Awaitable")),
        (atom("module_name"), binary("bt@exdura@awaitable")),
    ]);

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected a map response, got: {response:?}");
    };

    let status = map_get(m, "status").and_then(term_to_atom);
    assert_eq!(status.as_deref(), Some("ok"));

    // Protocol-only files return protocol_definition kind
    let kind = map_get(m, "kind").and_then(term_to_atom);
    assert_eq!(kind.as_deref(), Some("protocol_definition"));

    let module_name = map_get(m, "module_name").and_then(term_to_string);
    assert_eq!(module_name.as_deref(), Some("bt@exdura@awaitable"));
}

/// Inline class definition without override uses default `bt@` prefix.
#[test]
fn inline_class_definition_without_module_name_override() {
    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (
            atom("source"),
            binary("Object subclass: MyThing\n  greet => \"hello\""),
        ),
        (atom("module"), binary("bt@repl_eval_1")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
    ]);

    let response = handle_compile_expression(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected a map response, got: {response:?}");
    };

    let resp_kind = map_get(m, "kind").and_then(term_to_atom);
    assert_eq!(resp_kind.as_deref(), Some("class_definition"));

    // Without override, should derive from class name: bt@my_thing
    let module_name = map_get(m, "module_name").and_then(term_to_string);
    assert_eq!(module_name.as_deref(), Some("bt@my_thing"));
}

/// ADR 0105 Phase 1: the standalone `Class >> sel` method-definition
/// response (the REPL `>>` live-patch path) must carry the declared signature
/// alongside the existing `method_source`, so the workspace can capture it into
/// the signature-generation store before the patch installs.
#[test]
fn standalone_method_definition_carries_declared_signature() {
    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (
            atom("source"),
            binary("Counter >> setValue: v :: Integer -> Object => self.value := v"),
        ),
        (atom("module"), binary("bt@repl_eval_1")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
    ]);

    let response = handle_compile_expression(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected a map response, got: {response:?}");
    };

    assert_eq!(
        map_get(m, "kind").and_then(term_to_atom).as_deref(),
        Some("method_definition")
    );
    assert_eq!(
        map_get(m, "return_type")
            .and_then(term_to_string)
            .as_deref(),
        Some("Object")
    );
    let Some(Term::List(param_types)) = map_get(m, "param_types") else {
        panic!("Expected param_types list, got: {m:?}");
    };
    let param_type_strs: Vec<String> = param_types
        .elements
        .iter()
        .filter_map(term_to_string)
        .collect();
    assert_eq!(param_type_strs, vec!["Integer".to_string()]);
}

/// Unannotated standalone method definitions report the `"Dynamic"` sentinel
/// rather than omitting the fields.
#[test]
fn standalone_method_definition_reports_dynamic_when_unannotated() {
    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (
            atom("source"),
            binary("Counter >> increment => self.value := self.value + 1"),
        ),
        (atom("module"), binary("bt@repl_eval_1")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
    ]);

    let response = handle_compile_expression(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected a map response, got: {response:?}");
    };

    assert_eq!(
        map_get(m, "return_type")
            .and_then(term_to_string)
            .as_deref(),
        Some("Dynamic")
    );
    assert_eq!(
        map_get(m, "param_types"),
        Some(&Term::from(List::from(Vec::<Term>::new())))
    );
}
