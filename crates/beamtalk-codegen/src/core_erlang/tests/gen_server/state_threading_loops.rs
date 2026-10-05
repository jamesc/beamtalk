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
    let result = generate_module(&module, CodegenOptions::new("bt@broken"));
    assert!(result.is_ok(), "Codegen should succeed. Got: {result:?}");
    let code = result.unwrap();
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
    );
    assert!(
        code.contains("'class_a'/1"),
        "Should generate class_a/1 function. Got:\n{code}"
    );
    assert!(
        code.contains("'class_b'/1"),
        "Should generate class_b/1 function. Got:\n{code}"
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
    let result = generate_module(&module, CodegenOptions::new("bt@cvthenlocal"));
    assert!(result.is_ok(), "Codegen should succeed. Got: {result:?}");
    let code = result.unwrap();
    assert!(
        !code.contains("in  in"),
        "Should not contain doubled `in` keyword. Got:\n{code}"
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
    let result = generate_module(&module, CodegenOptions::new("bt@driverassign"));
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
    let result = generate_module(&module, CodegenOptions::new("bt@ctrnested"));
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
    let result = generate_module(&module, CodegenOptions::new("bt@driver7"));
    assert!(
        result.is_ok(),
        "self helper: (a pure, non-class-var-mutating self-send) inside a whileTrue: body \
         must compile. Got: {result:?}"
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
    let result = generate_module(&module, CodegenOptions::new("bt@driver8"));
    assert!(
        result.is_ok(),
        "A class-method self-send after (not inside) the loop body must still \
         compile. Got: {result:?}"
    );
}

#[test]
fn test_class_method_self_send_as_collect_transform_still_compiles() {
    // Pins the exact pattern from the real stdlib
    // fixture (`test/fixtures/class_method_block.bt`) that an earlier,
    // over-broad version of this fix accidentally broke in CI — a pure
    // (non-mutating) self-send used as `collect:`'s per-item transform,
    // alongside a co-occurring local mutation that routes the body through
    // `lower_foldl_body`. Must keep compiling.
    let src = "Object subclass: ClassMethodBlockLike\n  class double: x => x * 2\n  class doubleAllCounting: items =>\n    seen := 0\n    items collect: [:item | seen := seen + 1. self double: item]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(&module, CodegenOptions::new("bt@classmethodblocklike"));
    assert!(
        result.is_ok(),
        "A pure class-method self-send used as collect:'s transform, alongside a \
         co-occurring local mutation, must still compile. Got: {result:?}"
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
    let result = generate_module(&module, CodegenOptions::new("bt@noclassvarsdriver"));
    assert!(
        result.is_ok(),
        "A self-send inside a bare block must not be rejected when the enclosing \
         class has no class variables at all — there is no classState mutation to \
         lose. Got: {result:?}"
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
    let result = generate_module(&module, CodegenOptions::new("bt@foo"));
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
    let result = generate_module(&module, CodegenOptions::new("bt@bar"));
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

// ── BT-3562: bare discarded field-read statement mid-loop-body ──────────
//
// A bare (unassigned, non-last) `self.field` statement inside a
// `whileTrue:` body — no `:=` capturing the read — reproduced two distinct
// missing-`in`/sequencing defects, one per loop-compilation strategy:
//
// - **Plain (non-hybrid) `whileTrue:`, no field mutation anywhere in the
//   body** (`bt3562_plain_bare_field_read_in_while_loop_compiles`): the
//   loop threads only its local counter through a direct-params `letrec`
//   fun with NO `StateAcc` accumulator parameter at all. Field-read codegen
//   (`generate_field_access`/the bare-identifier fallback) nonetheless
//   unconditionally derived `StateAcc` from `in_loop_body` alone
//   (`CoreErlangGenerator::current_state_var`), so the read compiled to a
//   reference to a `StateAcc` variable the loop never bound — `erlc`:
//   "unbound variable 'StateAcc' in dispatch/4" (misattributed to the wrong
//   function by `erlc`'s own parse recovery). Fixed by capturing the real,
//   still-in-scope pre-loop state variable name
//   (`LoopMode::direct_params_outer_state_var`) and using it instead
//   (`CoreErlangGenerator::current_field_read_state_var`).
// - **Hybrid `whileTrue:` (a field mutation elsewhere in the same loop
//   forces hybrid mode)** (`bt3562_hybrid_bare_field_read_in_while_loop_compiles`):
//   the read-only field resolves correctly to its pre-extracted direct
//   parameter (e.g. `_ProcField3`), but `lower_letrec_non_assign_expr`'s
//   `in_direct_params_loop` branch emitted every non-assign statement
//   verbatim with no `let _ = … in` wrap — correct ONLY for a nested list
//   op's own open let-chain, not for an ordinary closed-value statement
//   like this field read. The next statement was glued directly onto it
//   with no separating `in` — `erlc`: "syntax error before: 'let'". Fixed
//   by checking `LoopMode::direct_params_do_open_chain` (the same signal
//   `generate_expression_as_value` already uses) before skipping the wrap.
//
// Confirmed NOT `late`-specific — the third test below reproduces the
// plain-loop defect identically for a `late state:` field.

#[test]
fn bt3562_plain_bare_field_read_in_while_loop_compiles() {
    // Direct-params `whileTrue:` (only `i` is threaded; `proc` is never
    // written anywhere in this method) with a bare, discarded `self.proc`
    // read as a non-last statement mid-body.
    let src = "typed Actor subclass: CodexClient\n  state: proc :: Integer = 0\n\n  pump: limit =>\n    i := 0\n    [i < limit] whileTrue: [\n      self.proc\n      i := i + 1\n    ]\n    nil\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(&module, CodegenOptions::new("bt3562_plain_bare_field_read"));
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        !code.contains("StateAcc)"),
        "a direct-params loop's letrec fun binds no StateAcc parameter — \
         a bare field read must reference the real captured state variable, \
         not `maps:get('proc', StateAcc)`. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3562_plain_bare_field_read", &code);
}

#[test]
fn bt3562_plain_bare_late_field_read_in_while_loop_compiles() {
    // Same shape as above, over a `late state:` field — confirms the
    // defect (and the fix) is general sequencing/state-var-naming, not
    // specific to `late`'s own `maps:find`/nil-arm read shape.
    let src = "typed Actor subclass: CodexClient\n  late state: proc :: Integer\n\n  pump: limit =>\n    i := 0\n    [i < limit] whileTrue: [\n      self.proc\n      i := i + 1\n    ]\n    nil\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3562_plain_bare_late_field_read"),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc("bt3562_plain_bare_late_field_read", &code);
}

#[test]
fn bt3562_hybrid_bare_field_read_in_while_loop_compiles() {
    // Hybrid `whileTrue:` — `other` is mutated in the loop body (forcing
    // hybrid full-extract mode) alongside a bare, discarded `self.proc`
    // read (never written anywhere in this method) as a non-last
    // statement mid-body.
    let src = "typed Actor subclass: CodexClient\n  state: proc :: Integer = 0\n  state: other :: Integer = 0\n\n  pump: limit =>\n    i := 0\n    [i < limit] whileTrue: [\n      self.proc\n      self.other := self.other + 1\n      i := i + 1\n    ]\n    nil\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3562_hybrid_bare_field_read"),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc("bt3562_hybrid_bare_field_read", &code);
}

#[test]
fn bt3562_plain_bare_field_read_in_nested_while_loop_compiles() {
    // BT-3562 follow-up (PR #3960 review): a plain direct-params
    // `whileTrue:` nested inside ANOTHER plain direct-params `whileTrue:`,
    // with the bare, discarded `self.proc` read inside the INNER loop. No
    // field write anywhere in this method, so both loops select
    // direct-params mode and neither `letrec` fun ever binds a `StateAcc`
    // parameter.
    //
    // The outer loop's own `generate_while_loop_direct` call captures
    // `LoopMode::direct_params_outer_state_var` BEFORE entering the inner
    // loop's body — by the time the inner loop makes its own capture,
    // `in_loop_body` is already `true` (set by the outer loop's own
    // `with_branch_context`), so a naive `current_state_var()` capture
    // would re-derive a bogus `StateAccN` name one level deeper instead of
    // reusing the outer loop's own already-captured (and still valid)
    // variable — the exact unbound-`StateAcc` defect this issue fixes, just
    // nested. Both capture sites now go through
    // `current_field_read_state_var()`, which returns the already-captured
    // outer value when one exists.
    let src = "typed Actor subclass: CodexClient\n  state: proc :: Integer = 0\n\n  pump: limit =>\n    i := 0\n    [i < limit] whileTrue: [\n      j := 0\n      [j < limit] whileTrue: [\n        self.proc\n        j := j + 1\n      ]\n      i := i + 1\n    ]\n    nil\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3562_plain_bare_field_read_nested"),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        !code.contains("StateAcc)"),
        "neither loop's letrec fun binds a StateAcc parameter — the inner \
         loop's bare field read must reference the real captured state \
         variable, not `maps:get('proc', StateAcc)`. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3562_plain_bare_field_read_nested", &code);
}

// ── BT-3623: two sibling mutation-threaded conditionals in one loop body ──
//
// `current_branch_frame()`/`current_frame()` read `branch_frame_counter`
// directly — a monotonic counter that mints a fresh `FrameId` per
// `enter_branch_context` call but is never decremented on exit. Inside a
// `whileTrue:` body, a SECOND `ifTrue:`/`ifFalse:`/`and:`/`or:` (or any
// other mutation-threaded conditional) captures its own enclosing frame via
// `current_frame()` too — but by then the FIRST sibling conditional's own
// `with_branch_context` has already opened and closed, leaving
// `branch_frame_counter` pointing at that now-closed sibling's frame
// instead of the loop's own still-active frame. The second conditional's
// `{Value, State}`-tuple extraction `Bind` gets tagged with that stale,
// already-popped `FrameId`, so `ThreadedIr::verify()` can't find its
// producer where it looks: `NonLinearVersion`/`UnboundVersion`. Fixed by
// tracking the innermost active branch frame as its own saved/restored
// generator field (`active_branch_frame`), separate from the
// never-restored minting counter.
//
// No non-local return (`^`) is needed to trigger this — the real-world
// exdura repro just happened to have one in its first `ifTrue:` block,
// which is why it read as an NLR-specific bug at first.

#[test]
fn bt3623_two_sibling_if_true_with_mutations_in_while_loop_compiles() {
    let src = "Actor subclass: Bt3623Repro\n  state: a = 0\n  state: b = 0\n  state: c = 0\n  state: d = 0\n  state: running = true\n\n  run =>\n    [self.running] whileTrue: [\n      self.a := self.a + 1.\n      self.b := self.b + 1.\n      (self.a > 3) ifTrue: [\n        self.b := self.b + 100.\n        self.c := self.c + 1\n      ].\n      (self.a > 5) ifTrue: [\n        self.d := self.d + 1.\n        self.b := self.b + 1000\n      ]\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(&module, CodegenOptions::new("bt3623_two_sibling_if_true"));
    let code = result.unwrap_or_else(|e| {
        panic!(
            "two sibling mutation-threaded ifTrue: blocks inside a whileTrue: body \
             must compile without tripping the ThreadedIr verifier. Got: {e:?}"
        )
    });
    assert_compiles_through_erlc("bt3623_two_sibling_if_true", &code);
}

#[test]
fn bt3623_nlr_in_first_sibling_if_true_in_while_loop_compiles() {
    // The exact shape from the Linear issue: the first sibling `ifTrue:`
    // block ALSO contains a non-local return (`^`), matching exdura's
    // `waitForSignal:`-style loop.
    let src = "Actor subclass: Bt3623NlrRepro\n  state: a = 0\n  state: b = 0\n  state: c = 0\n  state: d = 0\n  state: running = true\n\n  run =>\n    [self.running] whileTrue: [\n      self.a := self.a + 1.\n      self.b := self.b + 1.\n      (self.a > 3) ifTrue: [\n        self.b := self.b + 100.\n        self.c := self.c + 1.\n        ^self.c\n      ].\n      (self.a > 5) ifTrue: [\n        self.d := self.d + 1.\n        self.b := self.b + 1000\n      ]\n    ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(&module, CodegenOptions::new("bt3623_nlr_in_first_sibling"));
    let code = result.unwrap_or_else(|e| {
        panic!(
            "a non-local return inside the first of two sibling mutation-threaded \
             ifTrue: blocks in a whileTrue: body must compile. Got: {e:?}"
        )
    });
    assert_compiles_through_erlc("bt3623_nlr_in_first_sibling", &code);
}
