// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `generate_register_class` coverage (ADR 0038 Phase 3).

use super::*;
use beamtalk_core::test_helpers::test_support::{empty_module, make_actor_class};

#[test]
fn test_generate_register_class_empty_module_renders_empty() {
    let mut generator = CoreErlangGenerator::new("test");
    let module = empty_module();
    let doc = generator.generate_register_class(&module, false);
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
        ..empty_module()
    };
    let doc = generator.generate_register_class(&module, false);
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
