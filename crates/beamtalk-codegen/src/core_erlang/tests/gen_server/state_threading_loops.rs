// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! State threading through loop constructs (`whileTrue:`, `to:do:`,
//! `timesRepeat:`, `do:`, `select:`, `detect:`) and stored-closure
//! blocks when a class-method body sends to `self` inside them.

use super::*;

#[test]
fn test_class_method_local_var_assignment_of_self_class_method() {
    // class method `x := self classMethod` must NOT produce `in in`.
    // Previously generated invalid Core Erlang:
    //   let X = let _CMR = call ... in let ClassVars1 = ... in let _Unwrapped = ... in  in X
    let src = "Object subclass: Broken\n  class a =>\n    x := self b.\n    x\n\n  class b => 42";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@broken").with_workspace_mode(true),
    );
    assert!(result.is_ok(), "Codegen should succeed. Got: {result:?}");
    let code = result.unwrap();
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
    assert!(
        code.contains("'class_a'/2"),
        "Should generate class_a/2 function. Got:\n{code}"
    );
    assert!(
        code.contains("'class_b'/2"),
        "Should generate class_b/2 function. Got:\n{code}"
    );
}

#[test]
fn test_class_method_local_var_after_class_var_mutation() {
    // A class var mutation (`self.cv := expr`) preceding
    // a local var assignment (`x := plainExpr`) must NOT incorrectly treat the local var RHS as
    // a class-var-producing expression. Any stale producer state left over from the field
    // assignment must not leak into processing the local var's RHS.
    //
    // Pattern: class a => self.cv := 1. x := self b. x
    // Without the clear, x would be bound to the field-assignment's result var, not `self b`.
    let src = "Object subclass: CVThenLocal\n  class cv = 0\n  class a =>\n    self.cv := 1.\n    x := self b.\n    x\n\n  class b => 99";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@cvthenlocal").with_workspace_mode(true),
    );
    assert!(result.is_ok(), "Codegen should succeed. Got: {result:?}");
    let code = result.unwrap();
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
}

#[test]
fn test_class_method_self_send_in_while_loop_body_compiles_and_threads_class_vars() {
    // a self-send to a same-class class method (`self bump`)
    // used as a bare statement inside a `whileTrue:` loop body must compile
    // and correctly accumulate: `ClassVars` (ADR 0111 Addendum 9) threads
    // through the loop's own recursive tail call as an extra fun parameter,
    // the same way `StateAcc` does, rather than a plain syntax fix around
    // the self-send's `class_var_result` tuple-unwrapping (which would only
    // avoid the doubled `in in` from `emit_class_var_result_unwrap`'s open
    // let-chain being re-wrapped by the loop body's naive `let _ = <expr> in`
    // statement sequencing, while still silently discarding the mutation by
    // the time the loop finished). See
    // `stdlib/test/loop_class_var_mutation_test.bt`'s
    // `testSelfSendInWhileLoopAccumulates` for the
    // runtime-behavior pin (this test only pins the codegen shape).
    let src = "Value subclass: Driver\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: aBlock over: aList =>\n    i := 1\n    [i <= aList size] whileTrue: [\n      self bump\n      aBlock value: (aList at: i)\n      i := i + 1\n    ]\n    nil";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver").with_workspace_mode(true),
    );
    let code = result
        .unwrap_or_else(|e| panic!("self bump inside a whileTrue: body must compile. Got: {e:?}"));
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
    assert!(
        code.contains("fun (StateAcc, ClassVars)"),
        "The whileTrue: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
    assert!(
        code.contains("{'nil', StateAcc, ClassVars}"),
        "The whileTrue: exit arm must carry ClassVars through. Got:\n{code}"
    );
}

#[test]
fn test_class_method_self_send_in_to_do_loop_body_compiles_and_threads_class_vars() {
    // `to:do:`/`to:by:do:` compile through the
    // same `generate_counted_stateful_loop`/`BodyKind::Letrec` path as
    // `timesRepeat:` (see `control_flow/mod.rs`'s `generate_counted_stateful_loop`
    // doc comment), so the `ClassVars`-threading fix must cover this
    // construct too, not just `whileTrue:`/`timesRepeat:`.
    let src = "Value subclass: DriverToDo\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: n =>\n    total := 0\n    1 to: n do: [:i | self bump. total := total + i]\n    total";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivertodo").with_workspace_mode(true),
    );
    let code = result
        .unwrap_or_else(|e| panic!("self bump inside a to:do: body must compile. Got: {e:?}"));
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
    assert!(
        code.contains(", StateAcc, ClassVars) ->"),
        "The to:do: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
}

#[test]
fn test_class_method_self_send_as_local_var_assignment_rhs_in_while_loop_compiles() {
    // The `ClassMethodSelfSendInThreadedLoopBody`
    // guard only fires when a class-method self-send is itself the top-level
    // statement expression (`self bump` as a bare statement) — it doesn't walk
    // into `Expression::Assignment`, so `x := self bump` inside the same
    // `whileTrue:` body skipped the guard entirely and fell into
    // `try_generate_block_local_plain_let`, which used the non-open-scope-aware
    // `expression_doc` and wrapped it in `let X = <open chain> in`, reproducing
    // the exact doubled-`in` `core_parse_error` this PR exists to prevent (just
    // reached via assignment instead of a bare statement).
    //
    // Deliberately NOT rejected the way a bare self-send statement is: unlike a
    // discarded statement, `x`'s value here is genuinely captured and used
    // within the same iteration (`result := result + x`) — the same
    // "self-send return value matters" shape that made blanket-rejecting
    // `Foldl*` bodies wrong (see `test_class_method_self_send_as_collect_transform_still_compiles`).
    // So this is fixed as a compile bug (thread the self-send's class-var
    // mutation ahead of the assignment's own compile, mirroring the analogous
    // fix for the same shape inside blocks generally), not folded into the reject
    // list. `self.runs` not accumulating across
    // iterations is the same pre-existing, tracked `Letrec` limitation as
    // always (this test only pins that it compiles and runs without crashing).
    let src = "Value subclass: DriverAssign\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: aList =>\n    i := 1\n    result := 0\n    [i <= aList size] whileTrue: [\n      x := self bump\n      result := result + x\n      i := i + 1\n    ]\n    result";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverassign").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "x := self bump inside a whileTrue: body must compile. Got: {result:?}"
    );
    let code = result.unwrap();
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
}

#[test]
fn test_do_assigned_to_discarded_local_in_direct_params_loop_still_emits_foldl() {
    // `try_generate_block_local_plain_let`'s producer-aware handling must
    // not discard `val_doc` in the direct-params-loop "no single value" arm
    // — it must emit it first, like the ordinary-value arm right above it.
    // That arm fires for a mutation-threaded `do:` nested inside a
    // direct-params outer loop (see `test_do_nested_in_direct_params_loop`
    // in `control_flow/list_ops/tests.rs` for the bare-statement variant
    // this adapts) — there, `val_doc` isn't just "a value", it's the entire
    // generated `lists:foldl` call. Assigning such a `do:`'s result to a
    // discarded local var (`_y := items do: [...]`) inside a direct-params
    // loop must not silently drop the nested loop from the generated code —
    // no crash, just the nested `do:` (and any mutation it made, like
    // `seen` below) never executing, a regression from a loud
    // `core_parse_error` for this same shape to silently wrong code. This
    // pins that the `lists:foldl` call — and the loop it drives — survives.
    //
    // Confirmed via manual `beamtalk build` toggling (not just reasoning
    // about it) that this exact shape reproduces the drop when the handling
    // is wrong and is fixed when correct — several other plausible-looking
    // shapes (e.g. `_y := ...` as a `timesRepeat:` body's only/last
    // statement, or as a `class` method's `timesRepeat:` rather than an
    // `Actor` method's `to:do:`) turned out NOT to reach this code path at
    // all, so this test's shape matters and shouldn't be casually
    // "simplified".
    let src = "Actor subclass: CtrNested\n  state: x = 0\n  run: items =>\n    count := 0\n    seen := 0\n    1 to: 3 do: [:i |\n      _y := items do: [:item | seen := seen + 1]\n      count := count + 1\n    ]\n    count";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@ctrnested").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "do: assigned to a discarded local var inside a direct-params loop must compile. \
         Got: {result:?}"
    );
    let code = result.unwrap();
    assert!(
        code.contains("'lists':'foldl'"),
        "The nested do:'s lists:foldl call must not be dropped. Got:\n{code}"
    );
}

#[test]
fn test_class_method_self_send_alongside_local_in_times_repeat_body_compiles() {
    // the same gap reached via `timesRepeat:` instead of
    // `whileTrue:`, with a co-occurring local-variable mutation — a bare
    // self-send-only `timesRepeat:` body doesn't reach the state-threaded
    // loop codegen path at all (see
    // `test_bare_class_method_self_send_in_times_repeat_body_skips_loop_threading`
    // below), so this pins the shape that actually reaches it: a loop that
    // legitimately needs local threading (an accumulator) with a class-method
    // self-send alongside it. Now compiles and threads ClassVars correctly
    // (ADR 0111 Addendum 9) instead of being rejected.
    let src = "Value subclass: Driver5\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: n =>\n    total := 0\n    n timesRepeat: [\n      self bump\n      total := total + 1\n    ]\n    total";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver5").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!(
            "self bump alongside a local mutation inside a timesRepeat: body must compile. \
             Got: {e:?}"
        )
    });
    assert!(
        code.contains(", StateAcc, ClassVars) ->"),
        "The timesRepeat: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
}

#[test]
fn test_non_mutating_class_method_self_send_in_loop_body_also_compiles() {
    // every same-class class-method self-send routes
    // through the same `{class_var_result, ...}` unwrap convention
    // regardless of whether the callee actually touches class state — the
    // caller can't know that statically (the callee may be overridden, or
    // defined later in the file). `ClassVars` threading works
    // unconditionally for the same reason: it doesn't need to know whether
    // the self-send actually mutates anything, only that the callee's return
    // convention always carries a (possibly-unchanged) `ClassVars` value.
    let src = "Value subclass: Driver7\n  class helper: x => x * 2\n  class countedRun: aBlock over: aList =>\n    i := 1\n    [i <= aList size] whileTrue: [\n      self helper: i\n      aBlock value: (aList at: i)\n      i := i + 1\n    ]\n    nil";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver7").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "self helper: (a pure, non-class-var-mutating self-send) inside a whileTrue: body \
         must compile. Got: {result:?}"
    );
}

#[test]
fn test_bare_class_method_self_send_in_times_repeat_body_skips_loop_threading() {
    // contrast case: a `timesRepeat:` body with ONLY a class-method
    // self-send and no other local-variable mutation never needs state
    // threading (`needs_mutation_threading`) — it compiles as an
    // ordinary block passed to the runtime `timesRepeat:` helper, never
    // reaching `generate_threaded_loop_body` at all. Pinned
    // here as the boundary of this construct's scope, mirroring the
    // analogous bare-field-write contrast test.
    //
    // That "ordinary block" path is exactly `generate_block`'s
    // generic fallback, which is where the guard lives — so this
    // bare, mutation-losing self-send is a compile error rather than
    // silently compiling.
    let src = "Value subclass: Driver4\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: n =>\n    n timesRepeat: [\n      self bump\n      self bump\n    ]\n    nil";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver4").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A timesRepeat: body with only self-sends (no local var) skips the \
         threaded-loop codegen path, but the mutating self-send must now be \
         caught by BT-3151's unthreaded-block guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_after_loop_still_compiles() {
    // The contrast case: the pattern recommended by
    // `ClassMethodSelfSendInThreadedLoopBody`'s error message — accumulate a
    // local count inside the loop, then make the self-send once after the
    // loop, at the class method's own top frame (the already-proven
    // ADR 0110 shape) — must keep compiling.
    //
    // This is deliberately a single top-frame self-send, not
    // `count timesRepeat: [self bump]` — repeating the self-send N times
    // still requires wrapping it in a block, which is exactly the shape
    // the guard now (correctly) rejects. See
    // `test_bare_class_method_self_send_in_times_repeat_body_skips_loop_threading`
    // for that case.
    let src = "Value subclass: Driver8\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: aBlock over: aList =>\n    i := 1\n    count := 0\n    [i <= aList size] whileTrue: [\n      count := count + 1\n      aBlock value: (aList at: i)\n      i := i + 1\n    ]\n    self bump\n    nil";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driver8").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "A class-method self-send after (not inside) the loop body must still \
         compile. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_alongside_local_in_do_body_survives_via_class_vars_threading() {
    // A class-method self-send inside a `do:` block with a co-occurring
    // local mutation (which is what actually routes it through
    // `generate_threaded_loop_body_inner` in the first place) must not lose
    // its class-var mutation the way the analogous `Letrec`
    // (whileTrue:/timesRepeat:) shape would if `ThreadingPlan` threaded only
    // `threaded_locals` (user `:=` locals) through a fold's accumulator and
    // never `ClassVars`: the fold's accumulator becomes a `{ClassVars,
    // StateAcc}` 2-tuple whenever the body threads `ClassVars` (ADR 0111
    // Addendum 9, Question 6), so the mutation survives the loop and is
    // visible in the method's own `{'class_var_result', Result, ClassVarsN}`
    // return.
    //
    // Confirmed both by direct `erl` execution against the compiled
    // `.beam` (`runs` correctly ends at 3, not 0) and, structurally, here:
    // the compiled `class_countedRun:` fun's accumulator parameter and the
    // post-`lists:foldl` extraction both reference a *versioned*
    // `ClassVarsN` name (`N > 0`), never the bare, unmutated `ClassVars`.
    let src = "Value subclass: DriverDo\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class countedRun: aList =>\n    total := 0\n    aList do: [:x | self bump. total := total + x]\n    total";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverdo").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!("A class-method self-send inside a do: body must compile. Got: {e:?}")
    });
    let func = extract_core_fn(&code, "'class_countedRun:'/3 = fun")
        .expect("expected a class_countedRun:/3 function in the generated code");
    // The fold's own accumulator parameter must be a raw {ClassVars, StateAcc}
    // tuple (unwrapped via two `erlang:element/2` calls), not the bare
    // literal `StateAcc`.
    assert!(
        func.contains("call 'erlang':'element'(1,") && func.contains("call 'erlang':'element'(2,"),
        "the fold fun's accumulator must be unwrapped from a {{ClassVars, StateAcc}} \
         tuple via two element/2 calls. Got:\n{func}"
    );
    // The method's own final class-var-result reply must reference a
    // *versioned* ClassVars name (ClassVars1, ClassVars2, ...) — proof the
    // self-send's mutation, threaded through the fold, reached the method's
    // own top-level return, not the bare (unmutated, version-0) `ClassVars`
    // parameter.
    let reply_idx = func
        .rfind("{'class_var_result',")
        .expect("expected a final {'class_var_result', ...} reply");
    let reply_tail = &func[reply_idx..];
    assert!(
        !reply_tail.trim_end_matches(')').ends_with(", ClassVars}"),
        "the method's own final class_var_result reply must thread the \
         self-send's mutated (versioned) ClassVars forward, not the bare, \
         unmutated ClassVars parameter — this is the exact BT-3151 silent-loss \
         shape BT-3169 closes. Got:\n{reply_tail}"
    );
}

#[test]
fn test_class_method_self_send_as_select_predicate_alongside_local_survives_via_class_vars_threading()
 {
    // A class-method self-send in a `BodyKind::Foldl*` body, including
    // `select:`'s predicate position, must not be blanket-rejected — a real,
    // existing stdlib fixture (`test/fixtures/class_method_block.bt`) uses
    // pure (non-mutating) self-sends as the value feeding
    // `collect:`/`sort:`/`inject:into:` (see
    // `test_class_method_self_send_as_collect_transform_still_compiles`
    // below). Unlike `Letrec`, `select:`'s predicate result is NOT discarded
    // — it structurally IS the fold's output — so rejecting every self-send
    // there would have a real false-positive cost. The `Letrec`-only
    // compile-time guard is deliberately not widened to cover this shape.
    //
    // Instead, this shape is made correct rather than rejected: the
    // fold's accumulator threads `ClassVars` through a `{ClassVars, StateAcc}`
    // 2-tuple, so a class-var mutation performed by `check:` — hypothetically
    // — would survive rather than being silently lost. This fixture's
    // own `check:` is pure (no class var declared at all), so the test below
    // checks the *threading machinery* is in place — the fold fun's
    // accumulator unwrap and the assignment's own final ClassVars rebind —
    // not a specific mutated value, mirroring `class_method_block.bt`'s
    // deliberately-pure self-send shapes this fixture is modeled on.
    let src = "Value subclass: DriverSelect2\n  class check: x => x > 0\n  class positives: aList =>\n    seen := 0\n    result := aList select: [:x | seen := seen + 1. self check: x]\n    result";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverselect2").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!(
            "A class-method self-send used as select:'s predicate value, alongside a \
             co-occurring local mutation, must still compile. Got: {e:?}"
        )
    });
    let func = extract_core_fn(&code, "'class_positives:'/3 = fun")
        .expect("expected a class_positives:/3 function in the generated code");
    // The fold's own accumulator parameter must be a raw {ClassVars, AccSt}
    // tuple (unwrapped via two `erlang:element/2` calls before the
    // existing {AccList, StateAcc} unpack), not the bare AccSt.
    assert!(
        func.contains("call 'erlang':'element'(1,") && func.contains("call 'erlang':'element'(2,"),
        "the fold fun's accumulator must be unwrapped from a {{ClassVars, AccSt}} \
         tuple via two element/2 calls. Got:\n{func}"
    );
    // A fresh ClassVars name must be minted after the fold (from the raw,
    // still-wrapped `lists:foldl` result — `_RawFoldCV<N>`, per
    // `ThreadingPlan::foldl_call_doc`) to receive the threaded-through value
    // — proof the fold's own `{ClassVars, ...}` accumulator wrap and
    // post-`foldl` unwrap are both wired up for this predicate-position
    // self-send.
    assert!(
        func.contains("'erlang':'element'(1, _RawFoldCV"),
        "expected a post-fold ClassVars unwrap (element(1, _RawFoldCV...)) \
         rebinding the threaded-through value. Got:\n{func}"
    );
}

#[test]
fn test_class_method_self_send_as_collect_transform_still_compiles() {
    // Pins the exact pattern from the real stdlib
    // fixture (`test/fixtures/class_method_block.bt`) that an earlier,
    // over-broad version of this fix accidentally broke in CI — a pure
    // (non-mutating) self-send used as `collect:`'s per-item transform,
    // alongside a co-occurring local mutation that routes the body through
    // `generate_threaded_loop_body_inner`. Must keep compiling.
    let src = "Object subclass: ClassMethodBlockLike\n  class double: x => x * 2\n  class doubleAllCounting: items =>\n    seen := 0\n    items collect: [:item | seen := seen + 1. self double: item]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@classmethodblocklike").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "A pure class-method self-send used as collect:'s transform, alongside a \
         co-occurring local mutation, must still compile. Got: {result:?}"
    );
}

#[test]
fn test_bare_class_method_self_send_in_select_body_skips_loop_threading() {
    // Contrast case: a `select:`/`collect:`/`do:`
    // block with ONLY a class-method self-send and no other local-variable
    // mutation never needs state threading — mirroring
    // `test_bare_class_method_self_send_in_times_repeat_body_skips_loop_threading`
    // for `Letrec`, it compiles as an ordinary block (never reaching
    // `generate_threaded_loop_body`), routing instead through
    // `generate_block`'s generic fallback.
    //
    // this is the exact repro from that issue — confirmed
    // empirically that this shape would otherwise silently lose the
    // class-var mutation (`runs` staying 0). It must instead be caught at
    // compile time by the guard in `generate_block`.
    let src = "Value subclass: DriverSelect\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x > 0\n  class positives: aList =>\n    aList select: [:x | self check: x]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverselect").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A select: body with only a class-method self-send (no local var) skips the \
         threaded-loop codegen path, but the mutating self-send must now be caught \
         by BT-3151's unthreaded-block guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_block_compiles_when_class_has_no_class_vars() {
    // The unthreaded-block guard can't see a
    // self-send's target selector when it isn't locally defined on the
    // current class (e.g. inherited from a superclass in a different file —
    // `compute_class_var_mutating_selectors` only has this class's own
    // `class_methods` to analyze), so it conservatively treats any such
    // self-send as unsafe. That conservatism is unsound as a blanket rule:
    // `stdlib/src/subprocess.bt` self-sends `spawnWith:` (inherited from
    // `Actor`, `stdlib/src/actor.bt`) from inside a `tryDo:` block, and
    // `just build` failed on it once this guard landed.
    //
    // The fix: gate the whole check on the class actually declaring class
    // variables (`class_var_names`). With none, there is no classState a
    // self-send could possibly lose — an inherited method's body is fixed
    // at the *superclass's* compile time and can only reference class vars
    // declared there or above, never ones a subclass adds later. This class
    // has no `classState:`, so a self-send to `spawnWith:` — not locally
    // defined here, standing in for the real inherited-from-`Actor` case —
    // must still compile inside a bare `select:` block, hitting exactly the
    // "isn't defined locally in this class" conservative-fallback branch
    // this guard would otherwise trip.
    let src = "Value subclass: NoClassVarsDriver\n  class doubled: aList =>\n    aList select: [:x | self spawnWith: x]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@noclassvarsdriver").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "A self-send inside a bare block must not be rejected when the enclosing \
         class has no class variables at all — there is no classState mutation to \
         lose. Got: {result:?}"
    );
}

#[test]
fn test_class_method_mutating_self_send_as_second_cascade_message_in_block_is_compile_error() {
    // A cascade's 2nd+ message is sent to the same
    // shared receiver as the first (cascade semantics evaluate the receiver
    // once), but `analyze_expression`'s `Expression::Cascade` arm only ever
    // checked later messages for `is_self_field_value_send` — it never
    // recorded a self-send to `self_send_selectors`/`has_self_sends` for
    // them, unlike the `MessageSend` arm's handling of the cascade's first
    // message (folded into `receiver` by the parser). A mutating self-send
    // hidden behind an earlier *pure* cascade message inside a bare block
    // (`self pureLog: x; check: x`) was therefore invisible to
    // `check_no_unsafe_class_method_self_sends`, silently compiling and
    // losing the mutation — reproducing the exact bug this guard exists to
    // close, just one cascade message later.
    let src = "Value subclass: DriverCascade\n  classState: runs = 0\n  class pureLog: x => x\n  class check: x => self.runs := self.runs + 1. x > 0\n  class positives: aList =>\n    aList select: [:x | self pureLog: x; check: x]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivercascade").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send as the 2nd+ message of a cascade inside a bare block \
         must be caught by BT-3151's guard just like a standalone self-send. \
         Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_erlang_interop_block_is_compile_error() {
    // A block argument crossing the Erlang interop
    // boundary in a direct `(Erlang mod) fn: arg` call
    // (`generate_direct_erlang_call`'s keyword branch, `dispatch_codegen.rs`)
    // routes through the same `generate_erlang_interop_wrapper` →
    // `generate_block` mechanism as a `select:`/`do:` argument — same
    // process, same lossy in-process self-send optimization. Must be caught
    // the same way.
    let src = "Value subclass: DriverErlangInterop\n  classState: runs = 0\n  class bump => self.runs := self.runs + 1\n  class run: aList =>\n    (Erlang lists) foreach: [:x | self bump] over: aList\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivererlanginterop").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside a block crossing the Erlang interop boundary \
         must be caught by BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_any_satisfy_block_is_compile_error() {
    // `anySatisfy:`/`allSatisfy:` (and every other
    // sibling list-op in `control_flow/list_ops/` with the same
    // `block_needs_mutation_threading`-gated "fall through to a bare/BIF
    // call" shape — `detect:ifNone:`, `count:`, `flatMap:`, `takeWhile:`,
    // `dropWhile:`, `partition:`, `groupBy:`, `sort:`, plus the
    // `eachWithIndex:`/`do:separatedBy:` desugar fallbacks) were left
    // unguarded by this PR's first push even though they're structurally
    // identical to `select:`/`do:`/`collect:`. Now share the guard via
    // `check_bare_list_op_block_self_sends`. This pins the exact repro from
    // the review comment.
    let src = "Value subclass: DriverAnySatisfy\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x > 0\n  class positives: aList =>\n    aList anySatisfy: [:x | self check: x]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driveranysatisfy").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside an anySatisfy: block must be caught by \
         BT-3151's guard, matching every other bare-block list-op call site. \
         Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_sort_block_is_compile_error() {
    // Pins `sort:` (a 2-arg comparator block) as
    // another sibling covered by `check_bare_list_op_block_self_sends` — see
    // `test_class_method_self_send_in_any_satisfy_block_is_compile_error`'s
    // comment for the full list.
    let src = "Value subclass: DriverSort\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class sorted: aList =>\n    aList sort: [:a :b | (self check: a) < (self check: b)]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driversort").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside a sort: comparator block must be caught by \
         BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_each_with_index_block_is_compile_error() {
    // `eachWithIndex:` desugars to `inject:into:`
    // (`try_generate_each_with_index`, `enumeration_ops.rs`) only when the
    // user block needs mutation threading; a bare self-send-only block falls
    // through to `collection.bt`'s own self-hosted `eachWithIndex:` — a
    // same-process, in-process call, same as every other list-op call site.
    let src = "Value subclass: DriverEachWithIndex\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class run: aList =>\n    aList eachWithIndex: [:item :i | self check: item]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivereachwithindex").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside an eachWithIndex: block must be caught by \
         BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_do_separated_by_block_is_compile_error() {
    // `do:separatedBy:`'s desugar
    // (`try_generate_do_separated_by`, `enumeration_ops.rs`) has the same
    // bare-block fallthrough shape as `eachWithIndex:` above — checks both
    // the element and separator blocks.
    let src = "Value subclass: DriverDoSeparatedBy\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class run: aList =>\n    aList do: [:x | x] separatedBy: [self check: 0]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverdoseparatedby").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside a do:separatedBy: separator block must be caught \
         by BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_detect_if_none_block_alongside_mutating_predicate_is_compile_error()
 {
    // When `detect:ifNone:`'s predicate needs
    // mutation threading (a co-occurring local-var mutation), execution
    // routes through `generate_list_detect_if_none_with_mutations`, which
    // compiles `if_none` independently via `expression_doc` →
    // `generate_block` — a bare, unthreaded block regardless of what the
    // predicate does. A mutating self-send hidden only in `if_none` (not
    // the predicate) must still be caught.
    let src = "Value subclass: DriverDetectIfNone\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class run: aList =>\n    seen := 0\n    aList detect: [:x | seen := seen + 1. x > 1000] ifNone: [self check: 0]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverdetectifnone").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside detect:ifNone:'s ifNone block must be caught by \
         BT-3151's guard even when the predicate needs mutation threading. \
         Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_pure_inject_into_block_is_compile_error() {
    // `generate_list_inject`'s pure-block fast path calls
    // `generate_block_body` directly (to emit an inline `lists:foldl` with
    // zero wrapper overhead), bypassing `generate_block`'s own self-send
    // check entirely — so `generate_list_inject` must call
    // `check_no_unsafe_class_method_self_sends` itself. Without it, this
    // doesn't just silently lose the mutation — it crashes erlc with
    // malformed Core Erlang (`unbound variable 'ClassVars1'`), confirmed
    // empirically. Must be a clean compile-time error, matching every other
    // bare-block call site.
    let src = "Value subclass: DriverInject\n  classState: runs = 0\n  class check: x => self.runs := self.runs + 1. x\n  class sumChecked: aList =>\n    aList inject: 0 into: [:acc :x | acc + (self check: x)]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@driverinject").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside inject:into:'s pure-block fast path must be \
         caught by BT-3151's guard, not reach codegen and crash erlc. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_while_condition_block_is_compile_error() {
    // A `whileTrue:`/`whileFalse:` loop's *condition*
    // block is structurally the same kind of bare, unthreaded block as a
    // `select:`/`inject:into:` argument — `generate_while_loop` (and its
    // direct-params/hybrid-params variants) calls `generate_block_body` on
    // the condition directly, bypassing `generate_block`'s check the same
    // way `generate_list_inject`'s fast path did. A mutating self-send in
    // the condition silently loses its mutation on every iteration; must be
    // a compile-time error instead.
    let src = "Value subclass: DriverCond\n  classState: runs = 0\n  class shouldContinue: n => self.runs := self.runs + 1. self.runs < n\n  class run: n =>\n    i := 0\n    [self shouldContinue: n] whileTrue: [i := i + 1]\n    i";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@drivercond").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassMethodSelfSendInUnthreadedBlock { .. })
        ),
        "A mutating self-send inside a whileTrue: condition block must be caught by \
         BT-3151's guard. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_in_block() {
    // Class method self-send inside a block should produce valid Core Erlang.
    // Previously, the open-scope `let ... in ` from the self-send was not closed,
    // resulting in `syntax error before: ']'` from the Core Erlang parser.
    let src = r"Object subclass: Foo
  class compare: a with: b => a < b
  class sortItems: items =>
    items sort: [:a :b | self compare: a with: b]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@foo").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "Class method with self-send in block should compile. Got: {:?}",
        result.err()
    );
    let code = result.unwrap();
    // The block body should call class_compare:with: directly and close with the result var.
    // The open-scope `let ... in` must be closed, not left unclosed (which would produce a parse error).
    assert!(
        code.contains("'class_compare:with:'"),
        "Should call class_compare:with: directly. Got:\n{code}"
    );
    // Verify the block closes properly: the result var should appear before `])`
    // (closing the argument list), not after it.
    let fun_idx = code.find("fun (").expect("Should contain fun");
    let block_code = &code[fun_idx..];
    assert!(
        !block_code.contains("in ])"),
        "Block should not have unclosed scope before `])`. Got:\n{block_code}"
    );
}

#[test]
fn test_class_method_self_send_in_block_local_assignment() {
    // Local assignment with class method self-send as RHS inside a block.
    // The open-scope from the self-send must be emitted before the let binding.
    let src = r"Object subclass: Bar
  class double: x => x * 2
  class compare: a with: b => a < b
  class doubleAndSort: items =>
    items sort: [:a :b |
      da := self double: a
      db := self double: b
      self compare: da with: db
    ]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@bar").with_workspace_mode(true),
    );
    assert!(
        result.is_ok(),
        "Block with local := class-method-self-send should compile. Got: {:?}",
        result.err()
    );
    let code = result.unwrap();
    // Verify: local assignment `da := self double: a` should emit the open scope
    // THEN bind Da, not wrap the open scope in `let Da = ... in  in`.
    assert!(
        !code.contains("in  in"),
        "Should not have double `in` from unclosed open scope. Got:\n{code}"
    );
}
