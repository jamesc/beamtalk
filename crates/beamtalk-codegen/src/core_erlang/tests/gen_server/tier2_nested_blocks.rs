// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tier 2 stateful nested-block bodies: `self`-send mutation buried
//! inside nested `foldl`/`letrec` loop combinators, and class-builder
//! cascade state-version handling.

use super::*;

#[test]
fn test_nested_letrec_self_send_buried_in_conditional_compiles() {
    // A same-class self-send buried inside an
    // `ifTrue:` conditional (NOT a bare top-level statement) within an
    // inner `whileTrue:` that's itself nested inside an outer `whileTrue:`
    // must NOT be rejected. `Letrec`'s own real `threads_class_vars` gate
    // (`loop_body_threads_class_vars`) is narrowly top-level-only by
    // design — recursing into a conditional buried inside a `Letrec` body
    // is exactly the shape that predicate was narrowed to exclude (the
    // `class_var_sub_expr.bt` `tickInLoopConditional` regression documented
    // on `loop_body_threads_class_vars` itself), and it's also the shape
    // `class_var_sub_expr_test.bt`'s
    // `testTickInLoopConditionalCompilesAndRuns` already pins as
    // accepted, silently-non-threading behavior at a single loop level
    // (out of scope here). The inner loop was never going to
    // attempt `ClassVars` threading for this self-send in the first place,
    // so nothing is "lost" here for the outer loop to fail to recover —
    // rejecting only the nested-loop variant of this exact same shape
    // would be an inconsistent new restriction. Mirrors
    // `tickInLoopConditional` one loop level deeper.
    let src = "Object subclass: NestedCondSelfSend\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class run: n =>\n    j := 0\n    [j < n] whileTrue: [\n      i := 0\n      [i < n] whileTrue: [\n        (i >= 0) ifTrue: [self bump]\n        i := i + 1\n      ]\n      j := j + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(&module, CodegenOptions::new("bt@nestedcondselfsend"));
    result.unwrap_or_else(|e| {
        panic!(
            "A self-send buried inside a conditional (not a bare top-level statement) \
             inside a Letrec loop nested inside another Letrec loop must not be rejected \
             by ClassVarMutationLostAcrossNestedLoop — Letrec's own real threading gate \
             never attempts to thread it in the first place. Got: {e:?}"
        )
    });
}

#[test]
fn test_class_builder_cascade_in_second_field_assignment_does_not_corrupt_state_version() {
    // a `classBuilder … addClassMethod:body:`/`classMethods:` cascade
    // lowers its block via `generate_class_method_fun_from_block`, which resets
    // the instance-`State` version counter (`reset_state_version`) to give the
    // class-method fun its own fresh count — but nothing saved/restored the
    // ENCLOSING method's counter around that reset. When such a cascade sits in
    // the value of a field-assignment that is not the method's first (i.e. an
    // earlier statement already minted a `State` version), the reset rewinds
    // the enclosing counter, and the enclosing assignment's own next-minted
    // version collides with the earlier one — an ADR-0111 `NonLinearVersion`
    // ThreadedIr-verify violation (`producers: 2, consumers: 1`), which used to
    // hard-panic via `report_threaded_ir_verify_errors`'s `debug_assert!` (this
    // crate's dev/test profile builds with debug_assertions on).
    //
    // This requires an Actor with real `state:` elsewhere in the SAME compiled
    // module: instance field-assignment only threads through this
    // version-counted `State`/`maps:put` convention when the module needs
    // gen_server/actor semantics — otherwise fields thread through a separate
    // `Self`-counted convention `generate_class_method_fun_from_block` never
    // touches. Beamtalk's CLI enforces one class per `.bt` file and the REPL
    // compiles one expression per turn, so this exact shape is unreachable
    // through either surface — only a caller building a multi-class `Module`
    // directly (as `generate_module` allows, and as fuzzing does) can hit it.
    // That's exactly how the nightly `compile_pipeline` fuzz target found it: a
    // `CrossOver` mutation spliced fragments of an Actor fixture and
    // `class_builder_incremental_test.bt` into one fuzz input.
    let src = "Actor subclass: SlwActor\n  state: value = 41\n\nTestCase subclass: FooTest\n  field: cls = nil\n  field: parentCls = nil\n\n  testX =>\n    self.parentCls := 1\n    self.cls := Object classBuilder name: #Child;\n      superclass: Object;\n      addClassMethod: #greeting body: [:self | \"child\"];\n      register\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(
        diags.is_empty(),
        "Reproducer must parse cleanly. Got diagnostics: {diags:?}"
    );

    let result = generate_module(&module, CodegenOptions::new("bt3289_state_version_repro"));

    assert!(
        result.is_ok(),
        "generate_module must not fail/panic on a second field-assignment \
         whose value contains a classBuilder addClassMethod:body: cascade. Got: {result:?}"
    );
}

#[test]
fn test_nested_class_builder_cascade_does_not_corrupt_current_method_params() {
    // The same unguarded-reset shape as the `state_version` leak in
    // `test_class_builder_cascade_in_second_field_assignment_does_not_corrupt_state_version`
    // above, for `current_method_params`/`current_method_param_types` instead.
    // `generate_class_method_fun_from_block` unconditionally clears both at its
    // start to give the class-method fun its own fresh parameter list — but
    // neither is captured in `SavedClassMethodCtx`, so a builder cascade nested
    // INSIDE another builder class-method fun's own body clobbers the outer
    // fun's parameter list for any later statement in that same body that
    // reads `current_method_params` (e.g. the `erlangApply`/`erlangModuleLookup`
    // FFI intrinsics, or primitive-BIF codegen).
    //
    // Here the outer `greeting:` class-method fun has one parameter (`a`). Its
    // body first runs an inner `classBuilder … addClassMethod:body:` cascade
    // (building an unrelated `Inner` class), then ends with `@intrinsic
    // erlangApply`, which reads `current_method_params` to find the selector
    // argument to forward. Before the fix, the inner cascade's clear() wiped
    // out the outer fun's `a` parameter, so `erlangApply` fell back to a
    // hardcoded `"Selector"` var name that was never bound in this scope —
    // `beamtalk_erlang_proxy:dispatch(Selector, Arguments, Self)` — which
    // would fail `erlc` with an unbound-variable error rather than a clean
    // Rust-side panic (so, unlike the sibling test above, this isn't caught
    // by the ADR-0111 ThreadedIr verifier). Same reachability caveat: unreachable
    // through the CLI (one class per `.bt` file) or the REPL (one expression
    // per turn) — only direct `generate_module` library use, as fuzzing does,
    // can construct this AST shape.
    let src = "Actor subclass: SlwActor\n  state: value = 41\n\nTestCase subclass: FooTest\n  field: cls = nil\n  field: parentCls = nil\n\n  testX =>\n    self.parentCls := 1\n    self.cls := Object classBuilder name: #Outer;\n      superclass: Object;\n      addClassMethod: #greeting: body: [:self :a |\n        Object classBuilder name: #Inner;\n          superclass: Object;\n          addClassMethod: #answer body: [:self2 | 42];\n          register\n        @intrinsic erlangApply\n      ];\n      register\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(
        diags.is_empty(),
        "Reproducer must parse cleanly. Got diagnostics: {diags:?}"
    );

    let result = generate_module(&module, CodegenOptions::new("bt3300_params_repro"));
    let code = result.expect("generate_module must not fail on a nested classBuilder cascade");

    let marker = "'beamtalk_erlang_proxy':'dispatch'(";
    let call_pos = code
        .find(marker)
        .expect("expected an erlangApply-generated dispatch call in the output");
    let after = &code[call_pos + marker.len()..];
    assert!(
        !after.starts_with("Selector,"),
        "current_method_params leaked across the nested classBuilder cascade: the \
         outer `greeting:` fun's own `a` parameter should have been threaded into \
         the erlangApply call, not the hardcoded \"Selector\" fallback (which is \
         unbound in this scope and would fail erlc). Generated code:\n{code}"
    );
}
