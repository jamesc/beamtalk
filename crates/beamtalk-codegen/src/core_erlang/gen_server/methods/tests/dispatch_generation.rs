// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `generate_register_class`/`generate_method_dispatch`/
//! `generate_class_method_dispatches`/`generate_class_method_functions`
//! coverage.

use super::*;
use beamtalk_core::ast::Module;
use beamtalk_core::test_helpers::test_support::make_actor_class;

#[test]
fn test_generate_register_class_empty_module_renders_empty() {
    let mut generator = CoreErlangGenerator::new("test");
    let module = Module {
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = generator.generate_register_class(&module, false).unwrap();
    assert_eq!(
        doc.to_pretty_string(),
        "",
        "empty module should produce empty doc"
    );
}

#[test]
fn test_generate_register_class_includes_class_name() {
    let mut generator = CoreErlangGenerator::new("test");
    let module = Module {
        classes: vec![make_actor_class("Counter")],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = generator.generate_register_class(&module, false).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'Counter'"),
        "register_class should include class name atom. Got: {output}"
    );
    assert!(
        output.contains("register_class"),
        "register_class should define register_class/0. Got: {output}"
    );
}

#[test]
fn test_generate_method_dispatch_unary_includes_selector() {
    let mut generator = CoreErlangGenerator::new("test");
    let method = simple_unary_method("increment");
    let doc = generator.generate_method_dispatch(&method, 2).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'increment'"),
        "method dispatch should include selector atom. Got: {output}"
    );
}

#[test]
fn test_generate_class_method_dispatches_empty_class() {
    let mut generator = CoreErlangGenerator::new("test");
    let class = make_actor_class("Counter");
    let doc = generator
        .generate_class_method_dispatches(&class, 2)
        .unwrap();
    assert_eq!(
        doc.to_pretty_string(),
        "",
        "class with no methods should produce empty dispatch doc"
    );
}

#[test]
fn test_generate_class_method_functions_empty_class() {
    let mut generator = CoreErlangGenerator::new("test");
    let class = make_actor_class("Counter");
    let doc = generator.generate_class_method_functions(&class).unwrap();
    assert_eq!(
        doc.to_pretty_string(),
        "",
        "class with no class methods should produce empty doc"
    );
}
