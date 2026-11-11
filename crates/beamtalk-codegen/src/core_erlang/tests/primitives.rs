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
    // Now emits direct Erlang BIF instead of dispatch delegation
    assert_eq!(doc.to_pretty_string(), "call 'erlang':'+'(Self, Other)");
}

#[test]
fn test_generate_primitive_structural_intrinsic() {
    // `blockValue`/`blockValue1`/`blockValue2`/`blockValue3` no longer
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
fn test_generate_primitive_error_body_raises_user_error() {
    // BT-4417: `Object>>error:`'s compiled body is reached by generic dispatch
    // (`perform: #error:`, and a class object without a class-side `error:`
    // through the class-object protocol fallthrough). It raises the same
    // `user_error` as the call-site intrinsic, instead of self-dispatching the
    // intrinsic name `error` (a `does_not_understand` naming `'error'`).
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Object")));
    generator.current_method_params = vec!["Message".to_string()];

    let doc = generator
        .generate_primitive("error", false, Span::new(0, 0))
        .unwrap();
    let rendered = doc.to_pretty_string();
    assert!(
        rendered.contains("call 'beamtalk_primitive':'class_of'(Self)")
            && rendered.contains("call 'beamtalk_error':'new'('user_error', ")
            && rendered.contains(", Message) in call 'beamtalk_error':'raise'("),
        "expected the Object>>error: raise, got: {rendered}"
    );
    assert!(
        !rendered.contains("'dispatch'"),
        "expected no self-dispatch of the intrinsic name, got: {rendered}"
    );
}

#[test]
fn test_generate_primitive_hash_body_is_phash2() {
    // BT-4432: `Object>>hash`'s compiled body is reached by generic dispatch
    // (`perform: #hash`). As a placeholder it self-dispatched `hash` back to
    // `Object>>hash` and looped forever; it is now the same `phash2` call the
    // call-site intrinsic emits.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Object")));
    generator.current_method_selector = Some("hash".to_string());
    generator.current_method_params = vec![];

    let doc = generator
        .generate_primitive("hash", false, Span::new(0, 0))
        .unwrap();
    assert_eq!(doc.to_pretty_string(), "call 'erlang':'phash2'(Self)");
}

#[test]
fn test_generate_primitive_rejects_self_redispatching_placeholder() {
    // BT-4432 guard: a placeholder intrinsic declared by a method whose
    // selector equals the intrinsic name would re-dispatch to itself forever.
    // `printString` is still a `Placeholder` entry, so declaring it from a
    // `printString` method must fail the build.
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_identity(Some(util::ClassIdentity::new("Object")));
    generator.current_method_selector = Some("printString".to_string());
    generator.current_method_params = vec![];

    let err = generator
        .generate_primitive("printString", false, Span::new(0, 0))
        .expect_err("a self-redispatching placeholder must be rejected");
    assert!(
        matches!(&err, CodeGenError::Internal(msg) if msg.contains("loop forever")),
        "expected an Internal self-redispatch error, got: {err:?}"
    );

    // The same placeholder under a different selector (as `respondsTo:`
    // declares `@intrinsic respondsTo`) is not a self-loop and still compiles.
    generator.current_method_selector = Some("printOn:".to_string());
    assert!(
        generator
            .generate_primitive("printString", false, Span::new(0, 0))
            .is_ok()
    );
}

#[test]
fn test_stdlib_object_hash_body_is_phash2() {
    // BT-4432: the real `Object>>hash` (stdlib/src/object.bt) compiles to a
    // `phash2` body, not a self-dispatch of `hash`.
    let src = std::fs::read_to_string(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../../stdlib/src/object.bt"
    ))
    .expect("stdlib/src/object.bt should be readable");
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = crate::core_erlang::generate_module(
        &module,
        crate::core_erlang::CodegenOptions::new("bt@stdlib@object").with_stdlib_mode(true),
    )
    .expect("object.bt codegen should succeed");
    assert!(
        !code.contains("'dispatch'('hash'"),
        "Object>>hash must not self-dispatch `hash` (BT-4432). Got:\n{code}"
    );
}

#[test]
fn test_generate_primitive_block_value_structural_fallback() {
    // `blockValue`'s fallback body (reached via `perform:`, not the
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
    // `whileTrue`'s fallback body (reached via `perform:`) discriminates
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
    // `whileFalse` shares `whileTrue`'s fallback shape but continues
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
    // `repeat`'s fallback body has no argument block to discriminate,
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
    // `ensure`'s fallback body discriminates Tier 1/Tier 2 for both
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
        rendered.contains(
            "do apply CleanupBlock () do call 'beamtalk_actor_state':'note_unwound'(_Error"
        ) && rendered.contains(") primop 'raw_raise'("),
        "expected cleanup to run on every exception path before re-raising, got: {rendered}"
    );
    // BT-4536: a `^` is re-raised as it came, like any other exception.
    assert!(
        !rendered.contains("'$bt_nlr'"),
        "expected no `^` relay clause, got: {rendered}"
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
    // `onDo`'s fallback body discriminates Self's tier, then (for a
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
            "case call 'erlang':'is_function'(Self, 0) of <'true'> when 'true' -> let ExClassSnap = call 'beamtalk_class_vars':'snapshot'() in try apply Self ()"
        ),
        "expected a Self Tier 1 check wrapping a snapshotted try/catch, got: {rendered}"
    );
    assert!(
        rendered.contains("do call 'beamtalk_class_vars':'restore'(ExClassSnap) let "),
        "expected the non-NLR catch arm to begin with the class-variable restore, got: {rendered}"
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
