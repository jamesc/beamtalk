// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tier 2 stateful nested-block bodies: `self`-send mutation buried
//! inside nested `foldl`/`letrec` loop combinators, and class-builder
//! cascade state-version handling.

use super::*;

#[test]
fn test_nested_foldl_self_send_in_inner_do_is_compile_error() {
    // The same silent-loss gap is
    // reachable via `Foldl*`-in-`Foldl*` nesting (`do:`-in-`do:`), not just
    // `Letrec`-in-`Letrec`. Confirmed empirically to be WORSE than silent
    // loss: the outer `do:`'s own `plan.threads_class_vars`
    // comes back `true` (Foldl's gate is `body_analysis.has_self_sends`,
    // which — unlike `loop_body_threads_class_vars` — recurses into the
    // nested `do:`'s own block and finds `self bump`), so the outer fold
    // expects to build its own `{ClassVars, StateAcc}` accumulator wrap —
    // but the inner `do:`'s own `next_class_var()` mint (from
    // `ThreadingPlan::foldl_call_doc`) permanently advances the generator's
    // single, unscoped class-var-name counter without that name ever being
    // surfaced back to the outer scope, so the outer wrap references a name
    // that was only ever bound inside the inner `do:`'s own (already
    // exited) fold closure — an `erlc` "unbound variable" compile crash,
    // not silent data loss, but broken all the same. Rejected at compile
    // time instead — see `CodeGenError::ClassVarMutationLostAcrossNestedLoop`'s
    // doc comment.
    let src = "Value subclass: NestedFoldClassVarMutation\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class nestedDo: aList =>\n    outerSeen := 0\n    aList\n      do: [:x |\n        total := 0\n        aList\n          do: [:y |\n            self bump\n            total := total + 1\n          ]\n        outerSeen := outerSeen + 1\n      ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedfoldclassvarmutation").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "'self bump'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a self-send inside a do: \
             nested inside another do:. Got: {other:?}"
        ),
    }
}

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
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedcondselfsend").with_workspace_mode(true),
    );
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
fn test_nested_foldl_self_send_buried_in_conditional_is_compile_error() {
    // Contrast case: the same "self-send buried
    // in a conditional, not a bare top-level statement" shape as the
    // Letrec test above, but inside a `Foldl*` (`do:`) body instead —
    // `Foldl*`'s own real `threads_class_vars` gate
    // (`!Actor && in_class_method() && body_analysis.has_self_sends`) IS
    // genuinely recursive (unlike Letrec's), so this shape must still be
    // rejected when nested inside another loop.
    let src = "Value subclass: NestedFoldCondSelfSend\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class nestedDo: aList =>\n    outerSeen := 0\n    aList\n      do: [:x |\n        total := 0\n        aList\n          do: [:y |\n            (y >= 0) ifTrue: [self bump]\n            total := total + 1\n          ]\n        outerSeen := outerSeen + 1\n      ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedfoldcondselfsend").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { .. })
        ),
        "Expected ClassVarMutationLostAcrossNestedLoop for a self-send buried in a \
         conditional inside a do: nested inside another do: — Foldl*'s own real \
         threading gate is recursive, unlike Letrec's. Got: {result:?}"
    );
}

#[test]
fn test_nested_detect_self_send_in_inner_detect_is_compile_error() {
    // `nested_loop_or_fold_body` must also cover
    // the predicate-based `Foldl*` shapes (`detect:`/`count:`/`takeWhile:`/
    // `dropWhile:`/`partition:`/`groupBy:`), not just `do:`/`collect:`/
    // `select:`/`reject:`/`anySatisfy:`/`allSatisfy:`/`inject:into:` —
    // `ThreadingPlan::new_impl`'s `threads_class_vars` gate
    // (`!Actor && in_class_method() && body_analysis.has_self_sends`)
    // applies uniformly to every non-`Letrec` `BodyKind`, so a class-var
    // self-send nested inside `detect:`, itself nested inside another
    // `detect:`, is exactly as vulnerable to the silent-loss/`erlc`-crash
    // bug as the `do:`-in-`do:` shape pinned above.
    let src = "Value subclass: NestedDetectClassVarMutation\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class nestedDetect: aList =>\n    outerSeen := 0\n    aList\n      detect: [:x |\n        total := 0\n        aList\n          detect: [:y |\n            self bump\n            total := total + 1\n            true\n          ]\n        outerSeen := outerSeen + 1\n        true\n      ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nesteddetectclassvarmutation").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "'self bump'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a self-send inside a detect: \
             nested inside another detect:. Got: {other:?}"
        ),
    }
}

#[test]
fn test_mixed_letrec_nested_in_foldl_is_compile_error() {
    // Mixed nesting — a
    // `Letrec` (`whileTrue:`) loop with a direct class-var field write,
    // nested inside a `Foldl*` (`do:`) body — hits the same gap. The inner
    // `whileTrue:`'s own `ThreadingPlan::threads_class_vars` (Letrec's
    // narrow, top-level-only `loop_body_threads_class_vars` gate) is `true`
    // for its own bare `self.runs := self.runs + 1`, but nothing in the
    // outer `Foldl*` machinery (which only knows how to unpack its OWN
    // `{ClassVars, StateAcc}` accumulator shape, not a nested `Letrec`
    // loop's extra tail-call `ClassVars` fun parameter) surfaces that
    // mutation back out.
    let src = "Object subclass: MixedLetrecFoldCounter\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class mixedBumpUpTo: n =>\n    seen := 0\n    #(1) do: [:x |\n      i := 0\n      [i < n] whileTrue: [\n        self.runs := self.runs + 1\n        i := i + 1\n      ]\n      seen := seen + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@mixedletrecfoldcounter").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "class variable 'runs'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a whileTrue: (with a direct \
             class-var write) nested inside a do:. Got: {other:?}"
        ),
    }
}

#[test]
fn test_mixed_foldl_nested_in_letrec_is_compile_error() {
    // The reverse mixed
    // nesting — a `Foldl*` (`do:`) body with a direct class-var field
    // write, nested inside a `Letrec` (`whileTrue:`) loop. The inner `do:`
    // never reaches Foldl's own `ClassVars` threading at all here (a bare
    // field write inside a `Foldl*` body is unconditionally rejected by
    // `reject_class_var_field_assignment` regardless of nesting — see
    // `nested_loop_lost_class_var_mutation`'s doc comment), so this pins
    // that the OUTER `Letrec`'s own top-level dispatch catches the shape
    // before ever generating the inner `do:` at all.
    let src = "Value subclass: MixedFoldLetrecCounter\n  classState: runs = 0\n\n  class mixedBumpAll: aList =>\n    outerSeen := 0\n    [outerSeen < 2] whileTrue: [\n      aList do: [:x |\n        self.runs := self.runs + 1\n      ]\n      outerSeen := outerSeen + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@mixedfoldletreccounter").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "class variable 'runs'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a do: (with a direct class-var \
             write) nested inside a whileTrue:. Got: {other:?}"
        ),
    }
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
