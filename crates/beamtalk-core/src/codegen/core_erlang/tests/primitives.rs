// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for primitive binding Core Erlang code generation.
//!
//! Covers selector-based primitives, structural intrinsics, and
//! multi-parameter primitive dispatch.

use super::*;

#[test]
fn test_generate_primitive_selector_based() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Integer")));
    generator.current_method_params = vec!["Other".to_string()];

    let doc = generator
        .generate_primitive("+", true, Span::new(0, 0))
        .unwrap();
    // BT-340: Now emits direct Erlang BIF instead of dispatch delegation
    assert_eq!(doc.to_pretty_string(), "call 'erlang':'+'(Self, Other)");
}

#[test]
fn test_generate_primitive_structural_intrinsic() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Block")));
    generator.current_method_params = vec![];

    let doc = generator
        .generate_primitive("blockValue", false, Span::new(0, 0))
        .unwrap();
    assert_eq!(
        doc.to_pretty_string(),
        "call 'bt@stdlib@block':'dispatch'('blockValue', [], Self)"
    );
}

#[test]
fn test_generate_primitive_multiple_params() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Integer")));
    generator.current_method_params = vec!["End".to_string(), "Block".to_string()];

    let doc = generator
        .generate_primitive("toDo", false, Span::new(0, 0))
        .unwrap();
    assert_eq!(
        doc.to_pretty_string(),
        "call 'bt@stdlib@integer':'dispatch'('toDo', [End, Block], Self)"
    );
}
