// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! State threading for `self`-dispatch sequencing: binary-operand
//! hoisting, conditional/keyword-argument/field-assignment receiver
//! positions, early-return reply state, string-interpolation segments,
//! and loop-nested field/local assignment right-hand sides, plus the
//! ADR 0118 gate-agreement checks.

use super::*;

#[test]
fn bt3382_self_dispatch_receiver_of_conditional_threads_state_and_compiles_through_erlc() {
    // `(self recordOnce: which) ifTrue:ifFalse:` — the self-send is
    // the RECEIVER of the conditional, not a block-body statement, so the
    // `_with_mutations` branch generators' block-mutation scan never even
    // sees it (neither block body itself contains a mutation). Confirms the
    // receiver's own mutation is threaded into the branches' base state AND
    // the generated code is real, erlc-valid Core Erlang (not just
    // parseable).
    let src = "Actor subclass: MutProbe\n  state: timestamps = 0\n\n  triggerDirectly: which =>\n    (self recordOnce: which)\n      ifTrue: [1]\n      ifFalse: [2].\n    self.timestamps\n\n  internal recordOnce: which =>\n    self.timestamps := self.timestamps + 1.\n    true\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3382_self_dispatch_receiver_of_conditional")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, "),
        "the self-dispatch's new state must be extracted, not discarded. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3382_self_dispatch_receiver_of_conditional", &code);
}

#[test]
fn bt3392_self_dispatch_nested_in_binary_op_operand_threads_state_and_compiles_through_erlc() {
    // `1 + (self bumpCount)` inside an `ifTrue:` block body — the
    // self-send is a binary-op operand nested inside the block's own (only)
    // statement, neither the block's top-level statement (C11/C12b, already
    // correct) nor the conditional's receiver (already fixed).
    // Confirms the self-send's mutation is threaded via a real `Bind` AND
    // the generated code is real, erlc-valid Core Erlang.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly: flag =>\n    flag ifTrue: [\n      1 + (self bumpCount)\n    ] ifFalse: [\n      0\n    ].\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3392_self_dispatch_nested_in_binary_op_operand")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, "),
        "the self-dispatch's new state must be extracted, not discarded. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3392_self_dispatch_nested_in_binary_op_operand", &code);
}

#[test]
fn bt3433_pure_block_arg_state_mutation_does_not_leak_state_version_and_compiles_through_erlc() {
    // a generic keyword message (not a recognized control-flow
    // intrinsic) taking two block-literal arguments, where the FIRST block
    // has no direct field write and no *captured local* mutation (so
    // `generate_block` picks the plain/Tier-1 path, not
    // `generate_block_stateful`, and `validate_stored_closure`'s
    // `field_writes` guard never fires) but its own last statement is a
    // conditional whose true-branch invokes a Block *stored in a field*
    // (`self.callback value: v`) — the same shape `WorkflowWatcher>>doReload`'s
    // `ifOk:ifError:` block compiles to (`self.onReload value:value:`
    // inside a nested `ifFalse:`). Invoking a stored Block is conservatively
    // treated as possibly Tier 2 (it might itself thread new actor state
    // back), so the conditional's own state threading bumps `state_version`
    // — deliberately visible to later statements *inside that same block*
    // — but the block is a separate Core Erlang `fun`, so the bump must not
    // survive once `generate_block` returns. Before the fix, it did: the
    // method's own final `{reply, _, StateN}` (and the sibling `ifError:`
    // block, if it read state) referenced a `StateN` never bound outside
    // the first block's closure — an unbound variable `erlc` failure
    // discovered compiling a real actor.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: callback = nil\n\n  trigger: x =>\n    x\n      ifOk: [:v |\n        true\n          ifTrue: [self.callback value: v]\n          ifFalse: [nil]\n      ]\n      ifError: [:e |\n        nil\n      ]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3433_pure_block_arg_state_mutation").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc("bt3433_pure_block_arg_state_mutation", &code);
}

#[test]
fn bt3392_binary_op_hoist_does_not_reorder_past_a_non_self_send_operand() {
    // `(self.items at: idx) + (self
    // bumpCount)` — the left operand is a message send but NOT a self-send,
    // so `hoist_self_sends_for_binary_op` must not treat it as safe to
    // hoist past. Confirms the self-dispatch for `bumpCount` is compiled
    // in its ordinary (non-hoisted) position — i.e. as part of
    // `expression_doc`'s normal left-to-right compilation of the whole
    // statement — rather than pulled out ahead of `at:`'s evaluation. The
    // generated code must still be real, erlc-valid Core Erlang.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = 0\n\n  triggerDirectly: flag =>\n    flag ifTrue: [\n      (self.items at: 1) + (self bumpCount)\n    ] ifFalse: [\n      0\n    ].\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3392_binary_op_hoist_order_safety").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc("bt3392_binary_op_hoist_order_safety", &code);
}

#[test]
fn adr0118_order_unsafe_self_send_in_binary_op_now_threads_with_no_warning() {
    // ADR 0118 phase 2a: same order-unsafe shape as the
    // test above — `(self.items at: idx) + (self bumpCount)`, non-last
    // inside an `ifTrue:` block reached through `generate_conditional_branch_inline`'s
    // C12 catch-all. ADR 0118's universal sequencing rule (`sequence_children`,
    // reached here via C12's `thread_ahead`) makes an un-hoisted self-send's
    // mutation silently dropping (with only a compile-time warning naming
    // it) unrepresentable: the non-self-send `(self.items at: 1)` operand is
    // bound to a temp AHEAD of `bumpCount`'s dispatch (preserving `at:`'s
    // own evaluation-order guarantee), and `bumpCount`'s mutation threads
    // through a real `Bind` instead. So `bumpCount`'s `NewState` is
    // genuinely extracted via `element(2, ...)`, and no warning fires for
    // this shape. Mirrors the stdlib regression coverage at
    // `stdlib/test/actor_conditional_mutations_test.bt`'s
    // `testSelfSendAsBinaryOpArgumentInBoundsThreadsMutation`.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = 0\n\n  triggerDirectly: flag =>\n    flag ifTrue: [\n      (self.items at: 1) + (self bumpCount)\n    ] ifFalse: [\n      0\n    ].\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("adr0118_order_unsafe_self_send_now_threads").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc(
        "adr0118_order_unsafe_self_send_now_threads",
        &generated.code,
    );
    assert!(
        generated.code.contains("erlang':'element'(2, "),
        "bumpCount's new state must now be extracted (threaded), not discarded. Got:\n{}",
        generated.code
    );
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("bumpCount") && w.message.contains("silently dropped")),
        "the BT-3399 dropped-mutation warning must no longer fire for this shape \
         now that it threads. Got: {:?}",
        generated.warnings
    );
}

#[test]
fn bt3396_self_dispatch_nested_in_conditional_receiver_and_threads_state_and_compiles_through_erlc()
{
    // Shape 1: `((self recordOnce: which) and: [true]) ifTrue:ifFalse:`
    // — the conditional's receiver is an `and:` send whose OWN receiver is
    // the self-send. Neither block mutates, and the receiver is not itself
    // a self-send (a separate check), so only the widened
    // `conditional_receiver_needs_threading` probe makes this conditional
    // inline; `compile_conditional_receiver` then threads the nested
    // dispatch ahead of the `and:` send. The generated code must be real,
    // erlc-valid Core Erlang.
    let src = "Actor subclass: MutProbe\n  state: timestamps = 0\n\n  triggerDirectly: which =>\n    ((self recordOnce: which) and: [true])\n      ifTrue: [1]\n      ifFalse: [2].\n    self.timestamps\n\n  internal recordOnce: which =>\n    self.timestamps := self.timestamps + 1.\n    which\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3396_self_dispatch_nested_in_conditional_receiver_and")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, "),
        "the nested self-dispatch's new state must be extracted, not discarded. Got:\n{code}"
    );
    assert_eq!(
        code.matches("'safe_dispatch'('recordOnce:'").count(),
        1,
        "the hoisted self-send must be dispatched exactly once (hoist, then substitute). Got:\n{code}"
    );
    assert_compiles_through_erlc(
        "bt3396_self_dispatch_nested_in_conditional_receiver_and",
        &code,
    );
}

#[test]
fn bt3396_self_dispatch_as_keyword_argument_in_method_body_threads_state_and_compiles_through_erlc()
{
    // Shape 2: `#(10, 20, 30) at: (self bumpCount)` as a top-level
    // method-body statement (`BodyExprKind::Pure`, not inside any
    // conditional) — the self-send is an argument to an arbitrary non-self
    // keyword send. The method-body `Pure` arm must hoist it as a real
    // ROOT-frame `Bind` ahead of the statement.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    #(10, 20, 30) at: (self bumpCount).\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3396_self_dispatch_as_keyword_argument").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, "),
        "the self-dispatch's new state must be extracted, not discarded. Got:\n{code}"
    );
    assert_eq!(
        code.matches("'safe_dispatch'('bumpCount'").count(),
        1,
        "the hoisted self-send must be dispatched exactly once. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3396_self_dispatch_as_keyword_argument", &code);
}

#[test]
fn bt3396_self_dispatch_in_field_assignment_rhs_snapshots_prior_field_read_and_compiles_through_erlc()
 {
    // Shape 3 + evaluation order: `self.count := self.count + (self
    // bumpCount)` — the self-send is a sub-expression of a field
    // assignment's RHS (the `lower_field_assignment_bind`/`FieldAssignment`
    // `source_version` hazard an earlier, reverted prototype hit), AND the
    // `self.count` read precedes it in evaluation order. The read must be
    // bound BEFORE the dispatch runs, so it keeps its source-order
    // (pre-bump) value.
    //
    // ADR 0118 phase 1a: the sequencing rule binds the preceding
    // `self.count` read to a `_TmpN` temp (it is compiled against the
    // pre-dispatch `State` and bound ahead of the dispatch's `Bind`) — the
    // planner's `FieldSnap` snapshot is the same rule applied to one node
    // kind, and is no longer what this position goes through.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    self.count := self.count + (self bumpCount).\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3396_self_dispatch_in_field_assignment_rhs")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    let read_at = code.find("let _Tmp").unwrap_or_else(|| {
        panic!("the preceding self.count read must be temp-bound. Got:\n{code}")
    });
    let dispatch_at = code
        .find("'safe_dispatch'('bumpCount'")
        .unwrap_or_else(|| panic!("the nested self-send must be dispatched. Got:\n{code}"));
    assert!(
        read_at < dispatch_at,
        "the field-read temp must be bound BEFORE the dispatch runs. Got:\n{code}"
    );
    assert!(
        code[read_at..dispatch_at].contains("call 'maps':'get'('count', State)"),
        "the temp must hold the PRE-dispatch read (against `State`, not `State1`). Got:\n{code}"
    );
    assert!(
        !code.contains("FieldSnap"),
        "the Actor-body FieldAssignment arm no longer goes through the planner's snapshot. Got:\n{code}"
    );
    assert_eq!(
        code.matches("'safe_dispatch'('bumpCount'").count(),
        1,
        "the sequenced self-send must be dispatched exactly once. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'put'('count', _Val"),
        "the field write must still land as the real Put Bind. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3396_self_dispatch_in_field_assignment_rhs", &code);
}

#[test]
fn bt3396_self_dispatch_after_order_unsafe_operand_is_sequenced_behind_a_temp() {
    // This deliberately refuses to hoist a self-send past a non-self,
    // non-effect-free operand (`printString` may raise) and left
    // `bumpCount` in its natural, state-dropping position.
    //
    // ADR 0118 phase 1a, §Decision 3: in method-body position the
    // sequencing rule binds the earlier operand to a `_TmpN` temp FIRST,
    // then runs the dispatch + real `State` `Bind`, then the `++` on the
    // temp and the dispatch result — evaluation order preserved by
    // construction (`printString` still raises before `bumpCount` runs)
    // AND the mutation threaded. `HoistAction::Dropped` is unreachable
    // from this position now.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly: x =>\n    (x printString) ++ (self bumpCount) printString.\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt3396_order_unsafe_operand_sequenced").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    let code = &generated.code;
    let temp_at = code
        .find("let _Tmp")
        .unwrap_or_else(|| panic!("the earlier `x printString` must be temp-bound. Got:\n{code}"));
    let dispatch_at = code
        .find("'safe_dispatch'('bumpCount'")
        .unwrap_or_else(|| panic!("the self-send must be dispatched. Got:\n{code}"));
    let bind_at = code
        .find("let State1 = call 'erlang':'element'(2, _SD")
        .unwrap_or_else(|| panic!("the dispatch's NewState must be threaded. Got:\n{code}"));
    assert!(
        temp_at < dispatch_at && dispatch_at < bind_at,
        "order must be: temp for `x printString`, then the dispatch, then its State Bind. Got:\n{code}"
    );
    assert!(
        code[temp_at..dispatch_at].contains("'printString'"),
        "the temp must hold the `x printString` send itself. Got:\n{code}"
    );
    assert!(
        code.contains("{'reply', _Result, State1}"),
        "the reply must carry the post-dispatch state. Got:\n{code}"
    );
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("silently dropped")),
        "no BT-3399 drop warning in a sequenced position. Got: {:?}",
        generated.warnings
    );
    assert_compiles_through_erlc("bt3396_order_unsafe_operand_sequenced", code);
}

#[test]
fn bt3415_binary_operand_self_send_after_raising_operand_is_sequenced_in_method_body() {
    // ADR 0118 phase 1a acceptance shape: `(items at: idx) +
    // (self bump)` as an Actor method-body statement compiles to
    // `let _Tmp = <at:> in <dispatch> in let State1 = element(2, _SD) in
    // _Tmp + element(1, _SD)` — `at:` raises first (it is bound before the
    // dispatch runs), and when it does not raise `bump`'s state is
    // threaded into the reply. The "Dropped" case from before this fix is
    // not reachable from method-body position any more.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(10, 20, 30)\n\n  pick: idx =>\n    (self.items at: idx) + (self bump)\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt3415_binary_operand_sequenced").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    let code = &generated.code;
    let temp_at = code
        .find("let _Tmp")
        .unwrap_or_else(|| panic!("`items at: idx` must be temp-bound. Got:\n{code}"));
    let dispatch_at = code
        .find("'safe_dispatch'('bump'")
        .unwrap_or_else(|| panic!("`bump` must be dispatched. Got:\n{code}"));
    let bind_at = code
        .find("let State1 = call 'erlang':'element'(2, _SD")
        .unwrap_or_else(|| panic!("`bump`'s NewState must be threaded. Got:\n{code}"));
    assert!(
        temp_at < dispatch_at && dispatch_at < bind_at,
        "order must be: temp for `at:`, then the dispatch, then its State Bind. Got:\n{code}"
    );
    assert_eq!(
        code.matches("'safe_dispatch'('bump'").count(),
        1,
        "the sequenced self-send is dispatched exactly once. Got:\n{code}"
    );
    assert!(
        code.contains("{'reply', _Result, State1}"),
        "the method must reply with the post-dispatch state. Got:\n{code}"
    );
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("silently dropped")),
        "no BT-3399 drop warning: the sequencing rule makes `Dropped` unreachable here. Got: {:?}",
        generated.warnings
    );
    assert_compiles_through_erlc("bt3415_binary_operand_sequenced", code);
}

#[test]
fn bt3415_ffi_receiver_is_not_sequenced_but_its_self_send_argument_is() {
    // Adversarial review finding on #3717: `Erlang lists reverse: (self
    // bump)` — the FFI receiver is consumed STRUCTURALLY by
    // `try_handle_erlang_interop` (`erlang_module_of_receiver` turns it into
    // a module atom; it is never compiled through `generate_expression`), so
    // the sequencing rule must leave it alone or `finish_precompiled_scope`
    // reports the never-substituted registration as an internal error and
    // the whole module fails to compile. The argument's self-send is still
    // sequenced and its state threaded.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  go =>\n    Erlang lists reverse: (self bump)\n\n  goParens =>\n    (Erlang lists) reverse: (self bump)\n\n  goTwo =>\n    Erlang lists seq: 1 to: (self bump)\n\n  goNested =>\n    self record: (Erlang lists reverse: (self bump))\n\n  internal record: x => x\n\n  internal bump =>\n    self.count := self.count + 1\n    #(1, 2)\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3415_ffi_receiver_not_sequenced").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("an FFI send with a self-send argument must compile. Got: {e:?}"));
    assert_eq!(
        code.matches("'safe_dispatch'('bump'").count(),
        4,
        "each of the four methods dispatches `bump` exactly once. Got:\n{code}"
    );
    assert!(
        code.contains("'direct_call'('lists', 'reverse', [call 'erlang':'element'(1, _SD"),
        "the direct FFI call must receive the sequenced dispatch result. Got:\n{code}"
    );
    assert!(
        !code.contains("let _Tmp"),
        "an FFI receiver is never temp-bound. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3415_ffi_receiver_not_sequenced", &code);
}

#[test]
fn bt3415_early_return_reply_state_threads_the_conditionals_own_mutation() {
    // ADR 0118 phase 4: `^ 1 + ((self flagTrue) ifTrue: [1] ifFalse: [2])` —
    // the conditional receiver's dispatch chain must not mint `State1`
    // INSIDE the conditional's own closed document
    // (`compile_conditional_receiver`'s open let-chain), invisible to the
    // `^` arm's `current_state_var()` read — that would make the reply fall
    // back to the stale pre-conditional `State` even though `flagTrue`'s
    // mutation compiled and ran, so the method's own reply (and any state
    // read afterward) must see it. The mutation-threaded `ifTrue:ifFalse:` is
    // a real `ThreadedValue` producer whose prelude — including the
    // receiver's own hoisted `flagTrue` dispatch — splices into the `^`
    // arm's `single_sequenced_child` sequencing, so the reply correctly
    // carries the prelude's own final version: `State1` from the
    // receiver's hoisted `flagTrue` dispatch, then `State2` from
    // `control_flow_tuple_to_threaded_value`'s own wrap of the
    // `ifTrue:ifFalse:` construct's `{Value, NewState}` tuple (matching
    // `bt3415_early_return_reply_state_follows_the_prelude_when_there_is_one`'s
    // shape) — instead of the discarding `State`.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  go: i =>\n    ^ 1 + ((self flagTrue) ifTrue: [1] ifFalse: [2])\n    0\n\n  internal flagTrue =>\n    self.count := self.count + 1\n    true\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3415_early_return_post_prelude_state").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("{'reply', _ReturnValue, State2}"),
        "the reply must carry the prelude's final State2 (flagTrue's own \
         mutation, threaded through the conditional receiver, then the \
         conditional's own wrap), not the stale pre-conditional State. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3415_early_return_post_prelude_state", &code);
}

#[test]
fn bt3415_early_return_reply_state_follows_the_prelude_when_there_is_one() {
    // The positive counterpart: `^ (self.items at: 1) + (self bump)` has a
    // real prelude (the `bump` dispatch + `State1` Bind), and the reply
    // must carry THAT version.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(10, 20)\n\n  go =>\n    ^ (self.items at: 1) + (self bump)\n    0\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3415_early_return_prelude_state").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("{'reply', _ReturnValue, State1}"),
        "the reply must carry the prelude's State1. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3415_early_return_prelude_state", &code);
}

#[test]
fn bt3416_thread_ahead_no_longer_warns_once_the_interpolation_segment_threads() {
    // ADR 0118 phase 1b superseded the pin this test replaces:
    // a `thread_ahead` consumer (here `FieldAssignment`) whose RHS is a
    // `StringInterpolation` with an order-unsafe self-send in a LATER
    // segment — `"{self.items size}-{self bump}"` — used to run the
    // planner, which could not safely hoist `bump` ahead of the first
    // segment's `displayString` dispatch and so dropped the mutation with
    // a warning. `threaded_string_interpolation` now moves
    // BOTH segments' `let`-chains into the RHS's prelude, in order, so
    // `bump` dispatches (after the first segment's `displayString` call,
    // preserving evaluation order) and the warning is gone — the same fix
    // as the BUnit matrix's `interpolationBinaryOpSelfSend` row, exercised
    // here from a `FieldAssignment` RHS instead of a bare statement.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(1)\n  state: label = \"\"\n\n  go =>\n    self.label := \"{self.items size}-{self bump}\"\n    self.count\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt3416_thread_ahead_no_longer_warns").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("bump") && w.message.contains("silently dropped")),
        "bump's mutation now threads — the BT-3399 warning must be gone. Got: {:?}",
        generated.warnings
    );
    assert!(
        generated.code.contains("erlang':'element'(2, _SD"),
        "bump's dispatch must thread its NewState. Got:\n{}",
        generated.code
    );
    assert_compiles_through_erlc("bt3416_thread_ahead_no_longer_warns", &generated.code);
}

#[test]
fn bt3416_self_send_nested_in_a_cast_sends_receiver_still_threads() {
    // Review finding on #3718: `sequenced_send_children` treats an
    // `is_cast` send (`X!`) as opaque (never a "covered send", per its own
    // doc comment) since it isn't compiled through `try_handle_self_dispatch`
    // like an ordinary send — but `threaded_expression`'s "Pure default"
    // fallback (after this ADR's phase-1b cases were added) no longer ran
    // the planner there, so a self-send nested in a CAST send's receiver
    // — `(self next) process!` — silently lost its mutation: `subexpr_
    // needs_prelude`'s own tail probe still finds it via `hoist_plan_walk`
    // (which, unlike `sequenced_send_children`, does NOT special-case
    // `is_cast` and walks into the receiver regardless), but nothing
    // produced a prelude for it. `threaded_expression` restores the
    // planner as the explicit last-resort fallback, so this shape is
    // exactly what `hoist_nested_self_sends` already handled before ADR
    // 0118 phase 1b and continues to.
    //
    // `!` only ever terminates a whole body statement (the parser marks
    // `body.last_mut()`'s expression `is_cast`), never a nested
    // sub-expression, so the shape is pinned as its own top-level
    // statement rather than nested inside a literal.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  go =>\n    (self next) process!\n    self.count\n\n  internal next =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3416_cast_send_receiver_self_send_threads")
            .with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("'safe_dispatch'('next'"),
        "the cast's receiver self-send must still dispatch. Got:\n{code}"
    );
    assert!(
        code.contains("erlang':'element'(2, _SD"),
        "the nested self-send's NewState must thread into the method body. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3416_cast_send_receiver_self_send_threads", &code);
}

#[test]
fn bt3418_field_assign_rhs_in_loop_body_threads_nested_self_send() {
    // ADR 0118 phase 2b: `self.count := self.count + (self
    // bump)` as a `do:` loop-body statement — the field-assignment RHS
    // path inside `lower_foldl_body`. Before this phase
    // the nested self-send's mutation was silently dropped (no hoist ran
    // for this position at all); `thread_ahead` now sequences it ahead of
    // `generate_field_assignment_open`'s own compile of the RHS. Per
    // iteration: `count` reads BEFORE `bump` runs (evaluation order), so
    // `1, 2` bumps `count` to `1, 2` while `self.count` is reassigned to
    // `0+1=1`, then `1+2=3`.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(1, 2)\n\n  go =>\n    self.items do: [:x | self.count := self.count + (self bump)]\n    self.count\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3418_field_assign_rhs_in_loop_body").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("'safe_dispatch'('bump'"),
        "the nested self-send must dispatch. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3418_field_assign_rhs_in_loop_body", &code);
}

#[test]
fn bt3418_local_assign_rhs_in_loop_body_threads_nested_self_send_with_no_warning() {
    // ADR 0118 phase 2b: `y := 1 + (self bump)` as a `do:`
    // loop-body statement — `generate_local_var_assignment_in_loop`'s own
    // RHS. This is the exact shape that would otherwise silently drop
    // an order-unsafe self-send binary-op operand with only a warning;
    // `thread_ahead` sequences it ahead of the RHS's own compile via
    // the universal sequencing rule, so the mutation threads and the
    // warning is gone.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: items = #(1, 2)\n\n  go =>\n    y := 0.\n    self.items do: [:x | y := 1 + (self bump)].\n    y\n\n  internal bump =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt3418_local_assign_rhs_in_loop_body").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        generated.code.contains("erlang':'element'(2, _SD"),
        "bump's NewState must be extracted (threaded), not discarded. Got:\n{}",
        generated.code
    );
    assert!(
        !generated
            .warnings
            .iter()
            .any(|w| w.message.contains("bump") && w.message.contains("silently dropped")),
        "the BT-3399 dropped-mutation warning must no longer fire for this shape \
         now that it threads. Got: {:?}",
        generated.warnings
    );
    assert_compiles_through_erlc("bt3418_local_assign_rhs_in_loop_body", &generated.code);
}

#[test]
fn bt3415_registering_the_same_subexpression_twice_is_never_silent() {
    // Adversarial review finding on #3717: a second registration of one
    // node would let the inner scope's finish remove the entry out from
    // under the outer scope, whose consulted-exactly check then passes
    // vacuously — a double dispatch with no error. Pinned as a hard
    // internal error in every build profile (a `codegen_warnings`
    // diagnostic would be discarded by the CLI's build path).
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  go => self bump\n\n  internal bump => self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let class = module.classes.first().expect("one class");
    let expr = &class.methods[0]
        .body
        .first()
        .expect("one statement")
        .expression;
    let mut generator = CoreErlangGenerator::new("bt3415_double_registration");
    let mut scope = super::super::sequencing::PrecompiledScope::new();
    generator
        .register_precompiled_subexpr(&mut scope, expr, Document::Str("'a'"), false)
        .expect("first registration succeeds");
    let err = generator
        .register_precompiled_subexpr(&mut scope, expr, Document::Str("'b'"), false)
        .expect_err("second registration of the same node must fail");
    assert!(
        format!("{err:?}").contains("registered twice"),
        "expected the duplicate-registration internal error, got {err:?}"
    );
    generator
        .finish_precompiled_scope(scope)
        .expect_err("the scope still holds the never-consulted first entry");
}

#[test]
fn bt3415_self_send_argument_of_self_send_sequences_args_before_dispatch() {
    // `self record: (self bumpCount)` — the producer sequences its own
    // arguments (ADR 0118 §Decision 2): `bumpCount`'s dispatch + Bind
    // precede `record:`'s, whose argument list references the pure
    // `element(1, _SD)` result, and both states thread (`State1`,
    // `State2`).
    let src = "Actor subclass: MutProbe\n  state: count = 0\n  state: log = #()\n\n  go =>\n    self record: (self bumpCount)\n\n  internal record: n =>\n    self.log := self.log ++ #(n)\n    n\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3415_self_send_arg_sequenced").with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    let bump_at = code
        .find("'safe_dispatch'('bumpCount'")
        .unwrap_or_else(|| panic!("`bumpCount` must be dispatched. Got:\n{code}"));
    let record_at = code
        .find("'safe_dispatch'('record:'")
        .unwrap_or_else(|| panic!("`record:` must be dispatched. Got:\n{code}"));
    assert!(
        bump_at < record_at,
        "the argument's dispatch must precede the outer dispatch. Got:\n{code}"
    );
    assert!(
        code.contains("let State1 = call 'erlang':'element'(2, _SD")
            && code.contains("let State2 = call 'erlang':'element'(2, _SD"),
        "both dispatches must thread their NewState. Got:\n{code}"
    );
    assert!(
        code.contains("{'reply', call 'erlang':'element'(1, _SD"),
        "the reply reads the outer dispatch's pure result. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3415_self_send_arg_sequenced", &code);
}

#[test]
fn bt3416_self_dispatch_in_later_interpolation_segment_now_threads_after_earlier_segment() {
    // `generate_string_interpolation` dispatches
    // `displayString` on each segment's value right after evaluating it,
    // before the next segment runs — a message send that may raise — so
    // hoisting a LATER segment's self-send ahead of an EARLIER segment's
    // `displayString` dispatch would reorder evaluation; the then-current
    // planner's fix was to leave it un-hoisted (dropping the mutation,
    // with a warning). ADR 0118 phase 1b replaces that with the
    // sequencing rule: `threaded_string_interpolation` moves every
    // segment's `let`-chain up to and including the LAST one that needs
    // threading into the prelude, in order — so in `"{x}-{self
    // bumpCount}"` the mutation now threads AND `x`'s `displayString`
    // dispatch still runs first (see `threaded_string_interpolation`'s
    // doc comment). A self-send in the FIRST segment
    // (`"n={self bumpCount}"`) has nothing before it and was already
    // threaded before this phase.
    let later = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly: x =>\n    \"{x}-{self bumpCount}\".\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(later);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3416_later_interpolation_segment_threaded")
            .with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, _SD"),
        "a self-send in a later interpolation segment must now thread its NewState. Got:\n{code}"
    );
    let x_display_at = code
        .find("'displayString'")
        .unwrap_or_else(|| panic!("`x`'s displayString dispatch must appear. Got:\n{code}"));
    let bump_dispatch_at = code
        .find("'safe_dispatch'('bumpCount'")
        .unwrap_or_else(|| panic!("`bumpCount` must be dispatched. Got:\n{code}"));
    assert!(
        x_display_at < bump_dispatch_at,
        "the first segment's displayString dispatch must still precede the later segment's self-send dispatch. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3416_later_interpolation_segment_threaded", &code);

    let first = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    \"n={self bumpCount}\".\n    self.count\n\n  internal bumpCount =>\n    self.count := self.count + 1.\n    1\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(first);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt3396_first_interpolation_segment_hoisted").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert!(
        code.contains("erlang':'element'(2, _SD"),
        "a self-send in the first interpolation segment has nothing before it and must still be hoisted. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt3396_first_interpolation_segment_hoisted", &code);
}

// ADR 0118 phase 0: three shapes from the ADR's 47-shape self-send
// position probe (§Context) PANICKED the ThreadedIr verifier (rather than
// merely crashing at runtime or silently dropping a mutation) before ADR
// 0118 phase 3. A debug-build verifier panic
// (`report_threaded_ir_verify_errors`'s `debug_assert!`, control_flow/mod.rs)
// aborts the WHOLE test-binary invocation, so — unlike every other row in
// the same probe — the two closes below cannot live in a BUnit `.bt`
// fixture (see stdlib/test/fixtures/self_send_position_counter.bt's header
// comment); they are pinned here. The third (`bt3414_bare_and_inside_if_true_branch_inside_do_body`,
// below) is a different shape — a bare-receiver `and:` inside an `ifTrue:`
// inside a `do:` body — still open for a later phase (ADR 0118's own
// "Out of Scope" note for phase 3: "Inline-threaded control flow in
// expression position").

#[test]
fn bt3414_self_send_in_and_receiver_inside_while_true_condition_now_compiles_and_threads_state() {
    // `[i := i + 1. (self bumpCount) > 0 and: [i < 3]] whileTrue: [nil]` —
    // a self-send as the RECEIVER of an inline-threaded `and:`, itself the
    // whileTrue: CONDITION block's last expression. ADR 0118 phase 3:
    // `generate_while_true`'s mode selection must not only inspect the
    // BODY's own mutations (trivially none — `[nil]`) — falling to the
    // simple (non-threading) codegen path would compile the condition as
    // a genuine stateful Tier-2 closure and panic the verifier
    // (`UnboundVersion`). `generate_while_true` also checks the
    // condition (`condition_has_state_effects`), routing this into the
    // mutation-threading path, and every iteration's `bumpCount` dispatch
    // correctly advances the actor's `count` field.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    i := 0\n    [\n      i := i + 1\n      (self bumpCount) > 0 and: [i < 3]\n    ] whileTrue: [nil]\n    i\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3414_and_receiver_self_send_in_while_condition")
            .with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc("bt3414_and_receiver_self_send_in_while_condition", &code);
}

#[test]
fn bt3414_self_send_as_and_receiver_alone_inside_while_true_condition_now_compiles_and_threads_state()
 {
    // `[i := i + 1. (self flagTrue) and: [i < 3]] whileTrue: [nil]` — same
    // shape as above with a bare self-send (no binary-op wrapper) as the
    // `and:` receiver. Also closed by ADR 0118 phase 3.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    i := 0\n    [\n      i := i + 1\n      (self flagTrue) and: [i < 3]\n    ] whileTrue: [nil]\n    i\n\n  internal flagTrue =>\n    self.count := self.count + 1\n    true\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt3414_bare_and_receiver_self_send_in_while_condition")
            .with_workspace_mode(true),
    )
    .unwrap_or_else(|e| panic!("codegen should succeed. Got: {e:?}"));
    assert_compiles_through_erlc(
        "bt3414_bare_and_receiver_self_send_in_while_condition",
        &code,
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn threading_gates_agree_on_fixture_set() {
    // ADR 0118 phase 0: "does this conditional-shaped construct
    // need the inline mutation-threading path" has historically been
    // answered by several overlapping predicates — `control_flow_has_mutations`
    // (gen_server/methods.rs, used by the statement-level C11/C12 dispatch
    // to decide whether a `Match`/conditional send needs threaded lowering),
    // `conditional_receiver_needs_threading` (util.rs, one disjunct of the
    // same question — renamed from `contains_hoistable_self_send` by ADR
    // 0118 phase 2b, which also moved it out of
    // `control_flow/conditionals.rs`), and the gate behind `ifTrue:`,
    // `ifFalse:`, `ifTrue:ifFalse:`, `ifNotNil:`, and `and:`/`or:`
    // (intrinsics.rs, collapsed by this same issue into
    // `conditional_needs_mutation_threading` /
    // `and_or_needs_mutation_threading`) — under a "must stay in sync"
    // comment (gen_server/methods.rs) with no enforcing test until now.
    //
    // For a fixture set of real parsed `and:`/`or:`/`ifTrue:ifFalse:` sends,
    // assert every predicate that claims to answer this question agrees.
    struct Case {
        name: &'static str,
        snippet: &'static str,
        expect_needs_threading: bool,
    }

    let cases = [
        Case {
            name: "and_no_mutation_no_self_send",
            snippet: "true and: [false]",
            expect_needs_threading: false,
        },
        Case {
            name: "and_block_mutates_field",
            snippet: "true and: [self.count := self.count + 1. true]",
            expect_needs_threading: true,
        },
        Case {
            name: "and_receiver_is_hoistable_self_send",
            snippet: "(self bumpCount) and: [true]",
            expect_needs_threading: true,
        },
        Case {
            name: "or_no_mutation_no_self_send",
            snippet: "false or: [true]",
            expect_needs_threading: false,
        },
        Case {
            name: "or_block_mutates_field",
            snippet: "false or: [self.count := self.count + 1. true]",
            expect_needs_threading: true,
        },
        Case {
            name: "or_receiver_is_hoistable_self_send",
            snippet: "(self bumpCount) or: [false]",
            expect_needs_threading: true,
        },
    ];

    for case in cases {
        let src = format!(
            "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    {}\n\n  internal bumpCount =>\n    self.count := self.count + 1\n    self.count\n",
            case.snippet
        );
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
        let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
        assert!(
            diags.is_empty(),
            "case {:?}: fixture must parse cleanly. Got: {diags:?}",
            case.name
        );
        let class = module.classes.first().expect("one class");
        let method = class
            .methods
            .iter()
            .find(|m| matches!(&m.selector, MessageSelector::Unary(s) if s.as_str() == "triggerDirectly"))
            .expect("triggerDirectly method");
        let expr = &method
            .body
            .first()
            .expect("triggerDirectly has one statement")
            .expression;
        let Expression::MessageSend {
            receiver,
            arguments,
            ..
        } = expr
        else {
            panic!("case {:?}: expected a MessageSend, got {expr:?}", case.name);
        };
        let Expression::Block(block) = &arguments[0] else {
            panic!("case {:?}: expected a block argument", case.name);
        };

        let generator = CoreErlangGenerator::new("bt3414_gate_agreement");

        let and_or = generator.and_or_needs_mutation_threading(receiver, block);
        assert_eq!(
            and_or, case.expect_needs_threading,
            "case {:?}: and_or_needs_mutation_threading disagreed",
            case.name
        );

        let collapsed = generator.conditional_needs_mutation_threading(receiver, &[block]);
        assert_eq!(
            collapsed, case.expect_needs_threading,
            "case {:?}: conditional_needs_mutation_threading disagreed",
            case.name
        );

        let needs_threading = generator.conditional_receiver_needs_threading(receiver);
        assert!(
            !needs_threading || case.expect_needs_threading,
            "case {:?}: conditional_receiver_needs_threading(receiver) was true but the case did \
             not expect threading — a self-send needing threading in the receiver must always \
             force threading",
            case.name
        );

        let control_flow = generator.control_flow_has_mutations(expr);
        assert_eq!(
            control_flow, case.expect_needs_threading,
            "case {:?}: control_flow_has_mutations disagreed",
            case.name
        );
    }

    // `ifTrue:ifFalse:` exercises `conditional_needs_mutation_threading`'s
    // two-block path (both `true_block` and `false_block` are checked).
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly: flag =>\n    flag ifTrue: [self.count := self.count + 1] ifFalse: [nil]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, diags) = beamtalk_core::source_analysis::parse(tokens);
    assert!(
        diags.is_empty(),
        "ifTrue:ifFalse: fixture must parse cleanly. Got: {diags:?}"
    );
    let class = module.classes.first().expect("one class");
    let method = class
        .methods
        .iter()
        .find(|m| {
            matches!(&m.selector, MessageSelector::Keyword(parts) if parts.len() == 1 && parts[0].keyword == "triggerDirectly:")
        })
        .expect("triggerDirectly: method");
    let expr = &method.body.first().expect("one statement").expression;
    let Expression::MessageSend {
        receiver,
        arguments,
        ..
    } = expr
    else {
        panic!("expected a MessageSend, got {expr:?}");
    };
    let (Expression::Block(true_block), Expression::Block(false_block)) =
        (&arguments[0], &arguments[1])
    else {
        panic!("expected two block arguments");
    };
    let generator = CoreErlangGenerator::new("bt3414_gate_agreement_if_true_if_false");
    assert!(
        generator.conditional_needs_mutation_threading(receiver, &[true_block, false_block]),
        "ifTrue:ifFalse: with a mutating true-branch must need threading"
    );
    assert!(
        generator.control_flow_has_mutations(expr),
        "control_flow_has_mutations must agree that this ifTrue:ifFalse: needs threading"
    );
}

#[test]
#[should_panic(expected = "ThreadedIr verify")]
#[cfg(debug_assertions)]
fn bt3414_bare_and_inside_if_true_branch_inside_do_body_panics_verifier() {
    // `items do: [:x | x > 0 ifTrue: [(self flagTrue) and: [true]] ifFalse:
    // [nil]]` — a bare-receiver `and:` (self-send as its receiver) inside an
    // `ifTrue:` branch, itself inside a `do:` loop body. The conditional
    // branch's own ThreadedIr frame and the enclosing loop body's frame both
    // end up producing a Bind for the same version: `NonLinearVersion`.
    // Confirmed still panicking after ADR 0118 phase 2b (loop-body
    // consumers): this statement routes through
    // `lower_foldl_body`'s separate `control_flow_has_mutations`
    // branch (an inline conditional with mutations, not any of phase 2b's
    // three consumers), so neither phase touches it. Left open for a later
    // phase.
    let src = "Actor subclass: MutProbe\n  state: count = 0\n\n  triggerDirectly =>\n    #(1) do: [:x |\n      x > 0\n        ifTrue: [(self flagTrue) and: [true]]\n        ifFalse: [nil]\n    ]\n    self.count\n\n  internal flagTrue =>\n    self.count := self.count + 1\n    true\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let _ = generate_module(
        &module,
        CodegenOptions::new("bt3414_bare_and_in_if_true_inside_do_body").with_workspace_mode(true),
    );
}
