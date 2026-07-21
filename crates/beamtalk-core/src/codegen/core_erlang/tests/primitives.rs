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
fn test_generate_primitive_while_true_structural_fallback() {
    // BT-2908: `whileTrue`'s fallback body (reached via `perform:`) discriminates
    // Tier 1 (both condition and body plain funs) from Tier 2 (either a
    // stateful fun expecting a StateAcc) via erlang:is_function/2, and runs a
    // real generic loop for the Tier 1 case instead of self-dispatching to the
    // wrong intrinsic name.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Block")));
    generator.current_method_params = vec!["BodyBlock".to_string()];

    let doc = generator
        .generate_primitive("whileTrue", false, Span::new(0, 0))
        .unwrap();
    let rendered = doc.to_pretty_string();
    assert!(
        rendered.starts_with("case call 'erlang':'is_function'(Self, 0) of <'true'> when 'true' -> case call 'erlang':'is_function'(BodyBlock, 0) of <'true'> when 'true' -> letrec"),
        "expected a nested Self/BodyBlock Tier 1 check wrapping a letrec loop, got: {rendered}"
    );
    assert!(
        rendered.contains(
            "apply Self () of <'true'> when 'true' -> let _ = apply BodyBlock () in apply"
        ),
        "expected the loop body to apply the condition then the body block, got: {rendered}"
    );
    assert!(
        rendered.matches("'stateful_block_dispatch'").count() == 2,
        "expected a stateful_block_dispatch branch for both BodyBlock and Self, got: {rendered}"
    );
    assert!(
        rendered
            .ends_with("call 'bt@stdlib@block':'dispatch'('whileTrue', [BodyBlock], Self) end end"),
        "expected the original placeholder as the defensive non-function fallback, got: {rendered}"
    );
}

#[test]
fn test_generate_primitive_while_false_structural_fallback_negates_condition() {
    // BT-2908: `whileFalse` shares `whileTrue`'s fallback shape but continues
    // looping while the condition is 'false' instead of 'true'.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Block")));
    generator.current_method_params = vec!["BodyBlock".to_string()];

    let doc = generator
        .generate_primitive("whileFalse", false, Span::new(0, 0))
        .unwrap();
    let rendered = doc.to_pretty_string();
    assert!(
        rendered.contains("apply Self () of <'false'> when 'true' -> let _ = apply BodyBlock ()"),
        "expected whileFalse: to continue looping on 'false', got: {rendered}"
    );
    assert!(
        rendered.contains("<'true'> when 'true' -> 'nil'"),
        "expected whileFalse: to exit the loop on 'true', got: {rendered}"
    );
}

#[test]
fn test_generate_primitive_repeat_structural_fallback() {
    // BT-2908: `repeat`'s fallback body has no argument block to discriminate,
    // only the receiver itself.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Block")));
    generator.current_method_params = vec![];

    let doc = generator
        .generate_primitive("repeat", false, Span::new(0, 0))
        .unwrap();
    let rendered = doc.to_pretty_string();
    assert!(
        rendered.starts_with(
            "case call 'erlang':'is_function'(Self, 0) of <'true'> when 'true' -> letrec"
        ),
        "expected a Tier 1 check wrapping an infinite letrec loop, got: {rendered}"
    );
    assert!(
        rendered.contains("let _ = apply Self () in apply"),
        "expected the loop body to tail-apply Self indefinitely, got: {rendered}"
    );
    assert!(
        rendered.contains("'stateful_block_dispatch'"),
        "expected a Tier 2 stateful_block_dispatch error branch, got: {rendered}"
    );
    assert!(
        rendered.ends_with("call 'bt@stdlib@block':'dispatch'('repeat', [], Self) end end"),
        "expected the original placeholder as the defensive non-function fallback, got: {rendered}"
    );
}

#[test]
fn test_generate_primitive_ensure_structural_fallback() {
    // BT-2908: `ensure`'s fallback body discriminates Tier 1/Tier 2 for both
    // the protected block (Self) and the cleanup block, running a generic
    // try/catch with cleanup-on-both-paths for the Tier 1 case.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Block")));
    generator.current_method_params = vec!["CleanupBlock".to_string()];

    let doc = generator
        .generate_primitive("ensure", false, Span::new(0, 0))
        .unwrap();
    let rendered = doc.to_pretty_string();
    assert!(
        rendered.starts_with("case call 'erlang':'is_function'(Self, 0) of <'true'> when 'true' -> case call 'erlang':'is_function'(CleanupBlock, 0) of <'true'> when 'true' -> try"),
        "expected a nested Self/CleanupBlock Tier 1 check wrapping a try, got: {rendered}"
    );
    assert!(
        rendered.contains("apply Self ()") && rendered.contains("apply CleanupBlock ()"),
        "expected both the protected block and cleanup block to be applied, got: {rendered}"
    );
    assert!(
        rendered.contains("do apply CleanupBlock () primop 'raw_raise'"),
        "expected cleanup to run on the error path before re-raising, got: {rendered}"
    );
    assert!(
        rendered.matches("'stateful_block_dispatch'").count() == 2,
        "expected a stateful_block_dispatch branch for both CleanupBlock and Self, got: {rendered}"
    );
    assert!(
        rendered
            .ends_with("call 'bt@stdlib@block':'dispatch'('ensure', [CleanupBlock], Self) end end"),
        "expected the original placeholder as the defensive non-function fallback, got: {rendered}"
    );
}

#[test]
fn test_generate_primitive_on_do_structural_fallback() {
    // BT-2908: `onDo`'s fallback body discriminates Self's tier, then (for a
    // Tier 1 receiver) reuses the same NLR-passthrough + matches_class try/catch
    // shape `generate_on_do` produces, discriminating the handler's tier by
    // arity (0 = pure 0-arg, 1 = pure 1-arg, else = stateful) since a
    // generically dispatched handler's declared arity isn't known statically.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Block")));
    generator.current_method_params = vec!["ExClass".to_string(), "Handler".to_string()];

    let doc = generator
        .generate_primitive("onDo", false, Span::new(0, 0))
        .unwrap();
    let rendered = doc.to_pretty_string();
    assert!(
        rendered.starts_with(
            "case call 'erlang':'is_function'(Self, 0) of <'true'> when 'true' -> try apply Self ()"
        ),
        "expected a Self Tier 1 check wrapping a try/catch, got: {rendered}"
    );
    assert!(
        rendered.contains("'beamtalk_exception_handler':'ensure_wrapped'"),
        "expected the exception to be wrapped via ensure_wrapped, got: {rendered}"
    );
    assert!(
        rendered.contains("'beamtalk_exception_handler':'matches_class'(ExClass,"),
        "expected the exception class to be matched against the ExClass param, got: {rendered}"
    );
    assert!(
        rendered.contains("'$bt_nlr'"),
        "expected NLR throws to be detected and passed through, got: {rendered}"
    );
    assert!(
        rendered.contains("is_function'(Handler, 0) of <'true'> when 'true' -> apply Handler ()"),
        "expected a 0-arg handler tier check applying Handler with no args, got: {rendered}"
    );
    assert!(
        rendered.contains("is_function'(Handler, 1) of <'true'> when 'true' -> apply Handler ("),
        "expected a 1-arg handler tier check applying Handler with the exception object, got: {rendered}"
    );
    assert!(
        rendered.matches("'stateful_block_dispatch'").count() == 2,
        "expected a stateful_block_dispatch branch for both Handler and Self, got: {rendered}"
    );
    assert!(
        rendered.ends_with(
            "call 'bt@stdlib@block':'dispatch'('onDo', [ExClass, Handler], Self) end end"
        ),
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
