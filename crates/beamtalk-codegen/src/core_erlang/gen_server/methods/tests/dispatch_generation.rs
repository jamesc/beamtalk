// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `generate_method_dispatch`/`generate_class_method_dispatches`/
//! `generate_class_method_functions` coverage.
//!
//! `generate_register_class` coverage moved to `class_registry::tests`
//! alongside its production code.

use super::*;
use beamtalk_core::test_helpers::test_support::make_actor_class;

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
