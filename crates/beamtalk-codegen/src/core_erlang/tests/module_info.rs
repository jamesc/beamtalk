// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4067: every generated module exports and defines `module_info/0,1`
//! exactly once, whatever generator built it, and still compiles.

use super::*;

fn parse_module(src: &str) -> Module {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    beamtalk_core::source_analysis::parse(tokens).0
}

fn generate_named(module: &Module, name: &str) -> String {
    generate_module(module, CodegenOptions::new(name)).expect("codegen should succeed")
}

/// Asserts the export entries and both definitions appear exactly once, with
/// the bodies erlc gives Erlang sources, then compiles through `erlc`.
fn assert_module_info(name: &str, code: &str) {
    let header = code.lines().next().expect("module has a header line");
    assert!(
        header.starts_with(&format!("module '{name}' [")),
        "unexpected header: {header}"
    );
    assert!(
        header.ends_with(", 'module_info'/0, 'module_info'/1]"),
        "module_info/0,1 not exported: {header}"
    );
    for (needle, what) in [
        ("'module_info'/0 = fun", "module_info/0 definition"),
        ("'module_info'/1 = fun", "module_info/1 definition"),
    ] {
        assert_eq!(
            code.matches(needle).count(),
            1,
            "{what} must appear once:\n{code}"
        );
    }
    assert!(
        code.contains(&format!("call 'erlang':'get_module_info'('{name}')\n")),
        "module_info/0 body:\n{code}"
    );
    assert!(
        code.contains(&format!("call 'erlang':'get_module_info'('{name}', Key)\n")),
        "module_info/1 body:\n{code}"
    );
    assert_compiles_through_erlc(name, code);
}

#[test]
fn actor_module_has_module_info() {
    let module =
        parse_module("Actor subclass: Counter\n  state: n = 0\n  bump => self.n := self.n + 1\n");
    assert_module_info("bt@counter", &generate_named(&module, "bt@counter"));
}

#[test]
fn value_module_has_module_info() {
    let module = parse_module("Value subclass: Point\n  field: x = 0\n  double => x * 2\n");
    assert_module_info("bt@point", &generate_named(&module, "bt@point"));
}

#[test]
fn object_module_has_module_info() {
    let module = parse_module("Object subclass: Greeter\n  hello => \"hi\"\n");
    assert_module_info("bt@greeter", &generate_named(&module, "bt@greeter"));
}

#[test]
fn supervisor_module_has_module_info() {
    let module = super::supervisor::make_static_supervisor_module();
    assert_module_info("bt@web_app", &generate_named(&module, "bt@web_app"));
}

#[test]
fn native_facade_module_has_module_info() {
    let module = parse_module("Actor subclass: Wrapper native: my_mod\n  doIt => self delegate\n");
    assert_module_info("bt@wrapper", &generate_named(&module, "bt@wrapper"));
}

#[test]
fn protocol_only_module_has_module_info() {
    let module = parse_module("Protocol define: Greeting\n  greet -> String\n");
    assert_module_info("bt@greeting", &generate_named(&module, "bt@greeting"));
}
