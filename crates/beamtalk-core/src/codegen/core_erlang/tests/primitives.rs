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
    // BT-2812: `blockValue`/`blockValue1`/`blockValue2`/`blockValue3` no longer
    // hit the generic runtime-dispatch placeholder (see
    // `test_generate_primitive_block_value_structural_fallback` below) — use
    // `classOf`, a structural intrinsic still on the generic placeholder path,
    // to keep coverage of that fallback mechanism itself.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Block")));
    generator.current_method_params = vec![];

    let doc = generator
        .generate_primitive("classOf", false, Span::new(0, 0))
        .unwrap();
    assert_eq!(
        doc.to_pretty_string(),
        "call 'bt@stdlib@block':'dispatch'('classOf', [], Self)"
    );
}

#[test]
fn test_generate_primitive_block_value_structural_fallback() {
    // BT-2812: `blockValue`'s fallback body (reached via `perform:`, not the
    // call-site interception) discriminates Tier 1 (plain fun) from Tier 2
    // (stateful fun expecting a StateAcc) via erlang:is_function/2, instead of
    // self-dispatching to the wrong intrinsic name.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Block")));
    generator.current_method_params = vec![];

    let doc = generator
        .generate_primitive("blockValue", false, Span::new(0, 0))
        .unwrap();
    let rendered = doc.to_pretty_string();
    assert!(
        rendered.starts_with(
            "case call 'erlang':'is_function'(Self, 0) of <'true'> when 'true' -> call 'erlang':'apply'(Self, [])"
        ),
        "expected a Tier 1 erlang:apply fast path, got: {rendered}"
    );
    assert!(
        rendered.contains("'stateful_block_dispatch'"),
        "expected a Tier 2 stateful_block_dispatch error branch, got: {rendered}"
    );
    assert!(
        rendered.ends_with("call 'bt@stdlib@block':'dispatch'('blockValue', [], Self) end end"),
        "expected the original placeholder as the defensive non-function fallback, got: {rendered}"
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
