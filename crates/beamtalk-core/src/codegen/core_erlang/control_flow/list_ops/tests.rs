// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

use crate::codegen::core_erlang::tests::codegen;

#[test]
fn test_list_do_pure_generates_foreach() {
    // Pure do: (no mutations) generates lists:foreach
    let src =
        "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items do: [:item | item]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foreach'"),
        "Pure do: should generate lists:foreach. Got:\n{code}"
    );
}

#[test]
fn test_list_collect_generates_map() {
    // collect: (no mutations) generates lists:map
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items collect: [:item | item]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'map'"),
        "collect: should generate lists:map. Got:\n{code}"
    );
}

#[test]
fn test_list_inject_into_pure_generates_inline_foldl() {
    // BT-1327: inject:into: (no mutations) with a literal block emits inline
    // lists:foldl with the block body compiled directly in foldl arg order
    // (Elem, Acc) — no wrapper function, no runtime helper call.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items inject: 0 into: [:acc :item | acc + item]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "Pure inject:into: should emit inline lists:foldl. Got:\n{code}"
    );
    // Should NOT call the runtime helper
    assert!(
        !code.contains("'beamtalk_collection':'inject_into'"),
        "Pure inject:into: should NOT delegate to beamtalk_collection:inject_into. Got:\n{code}"
    );
    // Should NOT have a wrapper apply (literal block compiles body directly)
    assert!(
        !code.contains("fun (Elem, Acc) -> apply"),
        "Literal block should compile body directly, not via wrapper apply. Got:\n{code}"
    );
}

#[test]
fn test_list_inject_into_non_literal_generates_wrapper() {
    // BT-1327: inject:into: with a non-literal block (variable) emits inline
    // lists:foldl with an arg-swap wrapper: fun (Elem, Acc) -> apply Block (Acc, Elem).
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items with: block =>\n    items inject: 0 into: block\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "Non-literal inject:into: should emit inline lists:foldl. Got:\n{code}"
    );
    // Non-literal needs an arg-swap wrapper
    assert!(
        code.contains("fun (Elem, Acc) -> apply"),
        "Non-literal inject:into: should emit arg-swap wrapper. Got:\n{code}"
    );
    // Should NOT call the runtime helper
    assert!(
        !code.contains("'beamtalk_collection':'inject_into'"),
        "Non-literal inject:into: should NOT delegate to runtime helper. Got:\n{code}"
    );
}

#[test]
fn test_list_do_with_field_mutation_threads_state() {
    // do: with field mutation generates a state-threading foldl (not foreach)
    let src = "Actor subclass: Ctr\n  state: sum = 0\n\n  run: items =>\n    items do: [:item | self.sum := self.sum + item]\n";
    let code = codegen(src);
    // Mutation-threading uses foldl to thread state
    assert!(
        code.contains("'lists':'foldl'"),
        "do: with mutation should use lists:foldl for state threading. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('sum'"),
        "do: body should update 'sum' via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_bt1290_local_var_captured_by_nested_timer_block() {
    // BT-1290: local var `y` assigned in do: block must be capturable by a nested block.
    // Before the fix, `let Y = ... in <Timer_case_expr> in StateAcc` was generated,
    // which is invalid Core Erlang (orphaned `in StateAcc` after a closed expression).
    // After the fix, `let Y = ... in let _ = <Timer_case_expr> in StateAcc` is generated.
    let src = concat!(
        "Actor subclass: BugDemo\n",
        "  tick =>\n",
        "    #(1, 2, 3) do: [:x |\n",
        "      y := x + 1\n",
        "      Timer after: 0 do: [self use: y]\n",
        "    ]\n",
        "  use: n => nil\n"
    );
    let code = codegen(src);
    // Y (CoreErlang name for y) must appear inside the nested fun's argument list
    assert!(
        code.contains("'use:', [Y]"),
        "Y should be captured by nested block. Got:\n{code}"
    );
    // The foldl lambda must use `let _ = <Timer_expr> in StateAcc`, not bare `<Timer_expr> in StateAcc`
    // BT-1639: Timer is now a direct call (no class_registry lookup), so check
    // for the `let _ =` wrapping of the Timer class method call.
    assert!(
        code.contains("let _ = call 'bt@stdlib@timer':'class_after:do:'"),
        "Last expr in do: body with plain lets must use let _ = binding. Got:\n{code}"
    );
}

#[test]
fn test_bt1291_destructure_then_on_do_last_expr() {
    // BT-1291: #[...] list destructuring (has_plain_lets=true) followed by
    // `on:Exception do:` as the last expression produced the same invalid
    // `<bindings> in <on_do_expr> in StateAcc` pattern fixed by BT-1290.
    let src = concat!(
        "Actor subclass: BugDemo\n",
        "  tick =>\n",
        "    #() do: [:te |\n",
        "      #[a, b] := te\n",
        "      [a printString] on: Exception do: [:e | nil]\n",
        "    ]\n"
    );
    // Must compile without panic (before the fix this generated invalid Core Erlang)
    let code = codegen(src);
    // The last expr must be wrapped with `let _ =` to be valid Core Erlang
    assert!(
        code.contains("let _ = "),
        "Last expr after destructure must use let _ = binding. Got:\n{code}"
    );
}

#[test]
fn test_bt1290_field_mutation_then_general_last_expr() {
    // BT-1290: same fix also applies when has_mutations=true (field write before general last expr).
    // Before the fix: `let StateAcc1 = maps:put(...) in <external_call> in StateAcc1` — invalid.
    // After the fix: `let StateAcc1 = maps:put(...) in let _ = <external_call> in StateAcc1`.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: n = 0\n",
        "  run: items =>\n",
        "    items do: [:item | self.n := self.n + item. Timer after: 0 do: [item printString]]\n"
    );
    let code = codegen(src);
    // The last expr (Timer send) must be wrapped with `let _ =`
    // BT-1639: Timer is now a direct call (no class_registry lookup), so check
    // for the `let _ =` wrapping of the Timer class method call.
    assert!(
        code.contains("let _ = call 'bt@stdlib@timer':'class_after:do:'"),
        "Last expr after field mutation must use let _ = binding. Got:\n{code}"
    );
}

#[test]
fn test_list_do_multi_stmt_first_is_pure_generates_let_underscore() {
    // Multi-statement do: body where the first statement is a pure expression
    // must emit `let _ = <expr> in ...` (not bare `<expr> in ...`) — Core Erlang requires
    // non-last expressions to be bound.  The "+" call must appear as the RHS of a let.
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items do: [:item | item + 1. self.n := self.n + 1]\n";
    let code = codegen(src);
    // The first expression (item + 1) is non-last; it must be bound as `let _ = ... in`
    // not emitted bare as `<expr> in` which is invalid Core Erlang.
    let has_bare_expr_in = code.contains("call 'erlang':'%2B'") && {
        // Find the position of the '+' call and check what precedes it
        if let Some(pos) = code.find("call 'erlang':'%2B'") {
            // Look for "let _ = " immediately before the + call (within 20 chars)
            let before = &code[pos.saturating_sub(20)..pos];
            !before.contains("let _ = ") && !before.contains("let _")
        } else {
            false
        }
    };
    assert!(
        !has_bare_expr_in,
        "First non-last pure expr in do: body must emit 'let _ = ...' binding, not bare expr. Got:\n{code}"
    );
}

#[test]
fn test_list_collect_multi_stmt_first_is_pure_generates_let_underscore() {
    // Same fix for collect: — first non-last pure expr must be bound.
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items collect: [:item | item + 1. self.n := self.n + 1. item * 2]\n";
    let code = codegen(src);
    // item + 1 is first and non-last — must be wrapped with let _ = ... in
    let has_bare_expr_in = code.contains("call 'erlang':'%2B'") && {
        if let Some(pos) = code.find("call 'erlang':'%2B'") {
            let before = &code[pos.saturating_sub(20)..pos];
            !before.contains("let _ = ") && !before.contains("let _")
        } else {
            false
        }
    };
    assert!(
        !has_bare_expr_in,
        "First non-last pure expr in collect: body must emit 'let _ = ...' binding. Got:\n{code}"
    );
}

// ── BT-1276: Tuple-accumulator tests ──────────────────────────────────

#[test]
fn test_do_with_local_mutation_uses_tuple_acc() {
    // do: with only local mutation should use tuple accumulator:
    // - element(N, ...) inside the lambda (not maps:get per iteration)
    // - exactly 1 maps:get outside the loop (outer method body extraction)
    // - exactly 1 maps:put outside the loop (repack for outer method body)
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    total := 0\n    items do: [:item | total := total + item]\n    total\n";
    let code = codegen(src);
    // In tuple mode: 1 maps:get (outer extraction), 1 maps:put (repack).
    // In StateAcc mode: ≥2 maps:get, ≥2 maps:put (inside lambda + extract_suffix/pack).
    let get_count = code.matches("maps':'get'('__local__total'").count();
    let put_count = code.matches("maps':'put'('__local__total'").count();
    assert_eq!(
        get_count, 1,
        "do: tuple mode should have exactly 1 maps:get (outer extraction). Got:\n{code}"
    );
    assert_eq!(
        put_count, 1,
        "do: tuple mode should have exactly 1 maps:put (repack after loop). Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'element'(1,"),
        "do: tuple mode should use element(1, ...) to read threaded local. Got:\n{code}"
    );
}

#[test]
fn test_do_with_field_mutation_uses_stateacc() {
    // do: with field mutation must still use maps:put (no tuple acc — state effects present).
    let src = "Actor subclass: Ctr\n  state: sum = 0\n\n  run: items =>\n    items do: [:item | self.sum := self.sum + item]\n";
    let code = codegen(src);
    assert!(
        code.contains("maps':'put'('sum'"),
        "do: with field mutation must still use maps:put for field. Got:\n{code}"
    );
}

#[test]
fn test_collect_with_local_mutation_uses_tuple_acc() {
    // collect: with only local mutation should use element/2 (tuple acc).
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    count := 0\n    items collect: [:item | count := count + 1. item * 2]\n";
    let code = codegen(src);
    assert!(
        !code.contains("maps':'get'('__local__count'"),
        "collect: with local mutation should NOT use maps:get inside lambda. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'element'(2,"),
        "collect: tuple acc should use element(2, ...) for first threaded var. Got:\n{code}"
    );
}

#[test]
fn test_inject_with_local_mutation_uses_tuple_acc() {
    // inject:into: with only local mutation should use element/2 (tuple acc).
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    count := 0\n    items inject: 0 into: [:acc :item | count := count + 1. acc + item]\n";
    let code = codegen(src);
    assert!(
        !code.contains("maps':'get'('__local__count'"),
        "inject: with local mutation should NOT use maps:get inside lambda. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'element'(2,"),
        "inject: tuple acc should use element(2, ...) for first threaded var. Got:\n{code}"
    );
}

#[test]
fn test_inject_literal_initial_elides_acc_maybe_await() {
    // BT-1304 (now trivially passing since BT-1321 removed all maybe_await from binary ops):
    // maybe_await is never emitted on the fold accumulator.
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    count := 0\n    items inject: 0 into: [:acc :item | count := count + 1. acc + item]\n";
    let code = codegen(src);
    assert!(
        !code.contains("'maybe_await'(Acc)"),
        "BT-1321: maybe_await should not appear on the fold accumulator. Got:\n{code}"
    );
}

#[test]
fn test_inject_sync_var_initial_elides_acc_maybe_await() {
    // BT-1304 (now trivially passing since BT-1321 removed all maybe_await from binary ops):
    // maybe_await is never emitted on the fold accumulator.
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items start: start =>\n    count := 0\n    items inject: start into: [:acc :item | count := count + 1. acc + item]\n";
    let code = codegen(src);
    assert!(
        !code.contains("'maybe_await'(Acc)"),
        "BT-1321: maybe_await should not appear on the fold accumulator. Got:\n{code}"
    );
}

#[test]
fn test_inject_non_literal_initial_no_maybe_await() {
    // BT-1321: Binary op codegen no longer emits maybe_await on any operand (ADR-0043).
    // Even when the initial accumulator is a non-literal field read, the generated
    // binary op for `acc + item` must not wrap either operand with maybe_await.
    let src = "Actor subclass: Ctr\n  state: x = 0\n  state: initial = 0\n\n  run: items =>\n    count := 0\n    items inject: self.initial into: [:acc :item | count := count + 1. acc + item]\n";
    let code = codegen(src);
    assert!(
        !code.contains("'maybe_await'(Acc)"),
        "BT-1321: binary op should not wrap acc with maybe_await. Got:\n{code}"
    );
    assert!(
        !code.contains("'maybe_await'(Item)"),
        "BT-1321: binary op should not wrap item with maybe_await. Got:\n{code}"
    );
}

#[test]
fn test_inject_non_literal_initial_no_maybe_await_map_acc() {
    // BT-1321: Binary op codegen no longer emits maybe_await (ADR-0043), even on the
    // map-accumulator path when the initial is a non-literal field read.
    let src = "Actor subclass: Ctr\n  state: count = 0\n  state: initial = 0\n\n  run: items =>\n    items inject: self.initial into: [:acc :item | self.count := self.count + 1. acc + item]\n";
    let code = codegen(src);
    assert!(
        !code.contains("'maybe_await'(Acc)"),
        "BT-1321: binary op should not wrap acc with maybe_await (map-acc path). Got:\n{code}"
    );
    assert!(
        !code.contains("'maybe_await'(Item)"),
        "BT-1321: binary op should not wrap item with maybe_await (map-acc path). Got:\n{code}"
    );
}

#[test]
fn test_inject_nested_scope_no_maybe_await() {
    // BT-1321: Binary op codegen no longer emits maybe_await on any operand (ADR-0043).
    // Verifies that neither the outer nor the inner fold's accumulator is wrapped.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: initial = 0\n\n",
        "  run: outerList with: innerList =>\n",
        "    sharedCount := 0\n",
        "    outerList inject: 0 into: [:acc :x |\n",
        "      sharedCount := sharedCount + 1.\n",
        "      innerList inject: self.initial into: [:acc :y |\n",
        "        sharedCount := sharedCount + y.\n",
        "        acc + y]]\n",
    );
    let code = codegen(src);
    assert!(
        !code.contains("'maybe_await'(Acc)"),
        "BT-1321: no fold accumulator should be wrapped with maybe_await. Got:\n{code}"
    );
}

#[test]
fn test_filter_with_local_mutation_uses_tuple_acc() {
    // BT-1276: select: with only local mutation should use tuple accumulator.
    // element(2, ...) reads the first threaded var (slot 1 is AccList).
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    count := 0\n    items select: [:item | count := count + 1. item > 0]\n";
    let code = codegen(src);
    assert!(
        !code.contains("maps':'get'('__local__count'"),
        "select: with local mutation should NOT use maps:get inside lambda. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'element'(2,"),
        "select: tuple acc should use element(2, ...) for first threaded var. Got:\n{code}"
    );
}

#[test]
fn test_collect_with_mutation_has_list_like_result() {
    // BT-1489/BT-2342: collect: with mutations reconstructs the result to match the
    // receiver via beamtalk_collection:from_list_like/2 (String → binary, Array → Array,
    // list → list), mirroring the pure collect: path.
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items collect: [:x | self.n := self.n + 1. x]\n";
    let code = codegen(src);
    assert!(
        code.contains("'beamtalk_collection':'from_list_like'("),
        "BT-2342: collect: with mutations should reconstruct the result via from_list_like. Got:\n{code}"
    );
}

#[test]
fn test_select_with_mutation_has_list_like_result() {
    // BT-1489/BT-2342: select: with mutations reconstructs the result to match the
    // receiver via beamtalk_collection:from_list_like/2.
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items select: [:x | self.n := self.n + 1. x > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'beamtalk_collection':'from_list_like'("),
        "BT-2342: select: with mutations should reconstruct the result via from_list_like. Got:\n{code}"
    );
}

// ── BT-1487: takeWhile/dropWhile/groupBy/partition/sort ──────────────

#[test]
fn test_take_while_with_mutation_compiles() {
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items takeWhile: [:x | self.n := self.n + 1. x < 10]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "takeWhile: with mutation should use foldl. Got:\n{code}"
    );
    assert!(
        code.contains("StillTaking"),
        "takeWhile: should track StillTaking flag. Got:\n{code}"
    );
}

#[test]
fn test_drop_while_with_mutation_compiles() {
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items dropWhile: [:x | self.n := self.n + 1. x < 10]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "dropWhile: with mutation should use foldl. Got:\n{code}"
    );
    assert!(
        code.contains("StillDropping"),
        "dropWhile: should track StillDropping flag. Got:\n{code}"
    );
}

#[test]
fn test_partition_with_mutation_compiles() {
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items partition: [:x | self.n := self.n + 1. x > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "partition: with mutation should use foldl. Got:\n{code}"
    );
    assert!(
        code.contains("MatchList"),
        "partition: should have MatchList accumulator. Got:\n{code}"
    );
}

#[test]
fn test_group_by_with_mutation_compiles() {
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items groupBy: [:x | self.n := self.n + 1. x > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "groupBy: with mutation should use foldl. Got:\n{code}"
    );
    assert!(
        code.contains("GroupMap"),
        "groupBy: should have GroupMap accumulator. Got:\n{code}"
    );
}

#[test]
fn test_sort_with_mutation_compiles() {
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items sort: [:a :b | self.n := self.n + 1. a < b]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'sort'"),
        "sort: with mutation should use lists:sort. Got:\n{code}"
    );
    assert!(
        code.contains("'$bt_sort_state'"),
        "sort: should use process dictionary for state. Got:\n{code}"
    );
}

#[test]
fn test_take_while_pure_generates_takewhile() {
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items takeWhile: [:x | x < 10]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'takewhile'"),
        "Pure takeWhile: should use lists:takewhile. Got:\n{code}"
    );
}

#[test]
fn test_drop_while_pure_generates_dropwhile() {
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items dropWhile: [:x | x < 10]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'dropwhile'"),
        "Pure dropWhile: should use lists:dropwhile. Got:\n{code}"
    );
}

// ── Search ops: anySatisfy:, allSatisfy:, detect:, detect:ifNone: ──────

#[test]
fn test_any_satisfy_pure_generates_lists_any() {
    // BT-1481: Pure anySatisfy: (no mutations) delegates to lists:any/2 with an
    // is_list guard so non-list receivers fall back to beamtalk_primitive:send.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items anySatisfy: [:item | item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'any'"),
        "Pure anySatisfy: should generate lists:any. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_list'("),
        "Pure anySatisfy: should guard with erlang:is_list. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_primitive':'send'("),
        "Pure anySatisfy: should fall back to beamtalk_primitive:send for non-lists. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure anySatisfy: should NOT use lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_all_satisfy_pure_generates_lists_all() {
    // BT-1481: Pure allSatisfy: (no mutations) delegates to lists:all/2.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items allSatisfy: [:item | item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'all'"),
        "Pure allSatisfy: should generate lists:all. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure allSatisfy: should NOT use lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_detect_pure_generates_beamtalk_list_detect() {
    // BT-1486: Pure detect: (no mutations) delegates to beamtalk_list:detect/2
    // with an is_list guard for non-list fallback via beamtalk_primitive:send.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items detect: [:item | item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'beamtalk_list':'detect'"),
        "Pure detect: should generate beamtalk_list:detect. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure detect: should NOT use lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_detect_if_none_pure_dispatches_to_runtime() {
    // BT-1486: Pure detect:ifNone: (no mutations) dispatches to runtime via
    // beamtalk_primitive:send with the predicate and ifNone block as arguments.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items detect: [:item | item > 0] ifNone: [42]\n";
    let code = codegen(src);
    assert!(
        code.contains("'detect:ifNone:'"),
        "Pure detect:ifNone: should dispatch with selector 'detect:ifNone:'. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_primitive':'send'"),
        "Pure detect:ifNone: should use beamtalk_primitive:send. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure detect:ifNone: should NOT use lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_any_satisfy_with_field_mutation_threads_state() {
    // BT-1481: anySatisfy: with a field mutation in its body cannot short-circuit
    // (mutations must run for every element), so it uses lists:foldl with a bool
    // accumulator starting false.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  run: items =>\n    items anySatisfy: [:item | self.count := self.count + 1. item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "anySatisfy: with field mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'any'"),
        "anySatisfy: with field mutation must NOT use short-circuiting lists:any. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('count'"),
        "anySatisfy: body should update 'count' field via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_all_satisfy_with_field_mutation_threads_state() {
    // BT-1481: allSatisfy: with field mutation uses foldl with bool accumulator
    // starting true (all-satisfy assumption, set to false on first failure).
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  run: items =>\n    items allSatisfy: [:item | self.count := self.count + 1. item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "allSatisfy: with field mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'all'"),
        "allSatisfy: with field mutation must NOT use short-circuiting lists:all. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('count'"),
        "allSatisfy: body should update 'count' field via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_detect_with_field_mutation_threads_state() {
    // BT-1486: detect: with field mutation uses foldl with a {FoundItem, FoundFlag, State...}
    // accumulator so that mutations execute for every element (no short-circuit).
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  run: items =>\n    items detect: [:item | self.count := self.count + 1. item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "detect: with field mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("FoundFlag"),
        "detect: with field mutation should use FoundFlag accumulator. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('count'"),
        "detect: body should update 'count' field via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_detect_if_none_with_field_mutation_threads_state() {
    // BT-1486: detect:ifNone: with field mutation uses foldl + FoundFlag to distinguish
    // "found nil" from "nothing matched", then evaluates the ifNone block when unmatched.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  run: items =>\n    items detect: [:item | self.count := self.count + 1. item > 0] ifNone: [42]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "detect:ifNone: with field mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("FoundFlag"),
        "detect:ifNone: with mutation should use FoundFlag to distinguish no-match. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('count'"),
        "detect:ifNone: body should update 'count' field via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_any_satisfy_with_local_mutation_uses_tuple_acc() {
    // BT-1481 + BT-1276: anySatisfy: with only a local variable mutation uses the
    // tuple-accumulator path: {BoolAcc, Var1, ...}. Locals are unpacked via
    // element(N, AccSt) inside the lambda — not via maps:get.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    count := 0\n",
        "    items anySatisfy: [:item | count := count + 1. item > 0]\n",
        "    count\n",
    );
    let code = codegen(src);
    // BoolAcc is at position 1; 'count' is the first threaded local so it lives at
    // position 2 inside the lambda accumulator tuple.
    assert!(
        code.contains("let Count = call 'erlang':'element'(2, "),
        "anySatisfy: with local mutation should extract 'count' via element(2, AccSt) in tuple-acc lambda. Got:\n{code}"
    );
    // BT-2356: the fold packs the final 'count' into the StateAcc map at exit so the
    // outer method body can thread it back via maps:get — the tuple accumulator is an
    // internal-to-the-fold optimisation, not a reason to drop the outer-local threading.
    assert!(
        code.contains("maps':'put'('__local__count'"),
        "anySatisfy: with local mutation should pack '__local__count' into the StateAcc at fold exit. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('__local__count'"),
        "anySatisfy: with local mutation should thread 'count' back via maps:get after the fold. Got:\n{code}"
    );
}

#[test]
fn test_detect_with_local_mutation_uses_tuple_acc() {
    // BT-1486 + BT-1276: detect: with only a local variable mutation uses the
    // tuple-accumulator path: {FoundItem, FoundFlag, Var1, ...}. Locals are
    // unpacked via element(N, AccSt) inside the lambda — not via maps:get.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    count := 0\n",
        "    items detect: [:item | count := count + 1. item > 0]\n",
        "    count\n",
    );
    let code = codegen(src);
    // FoundItem=1, FoundFlag=2; 'count' is the first threaded local at position 3.
    assert!(
        code.contains("let Count = call 'erlang':'element'(3, "),
        "detect: with local mutation should extract 'count' via element(3, AccSt) in tuple-acc lambda. Got:\n{code}"
    );
    assert!(
        code.contains("FoundFlag"),
        "detect: with local mutation should still use FoundFlag accumulator. Got:\n{code}"
    );
    // BT-2355: the fold packs the final 'count' into the StateAcc map at exit so the
    // outer method body can thread it back via maps:get — the tuple accumulator is an
    // internal-to-the-fold optimisation, not a reason to drop the outer-local threading.
    assert!(
        code.contains("maps':'put'('__local__count'"),
        "detect: with local mutation should pack '__local__count' into the StateAcc at fold exit. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('__local__count'"),
        "detect: with local mutation should thread 'count' back via maps:get after the fold. Got:\n{code}"
    );
}

#[test]
fn test_all_satisfy_with_local_mutation_uses_tuple_acc() {
    // BT-1481 + BT-1276: allSatisfy: with only a local variable mutation uses the
    // tuple-accumulator path: {BoolAcc, Var1, ...}. Locals are unpacked via
    // element(N, AccSt) inside the lambda — not via maps:get.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    count := 0\n",
        "    items allSatisfy: [:item | count := count + 1. item > 0]\n",
        "    count\n",
    );
    let code = codegen(src);
    // BoolAcc is at position 1; 'count' is the first threaded local so it lives at
    // position 2 inside the lambda accumulator tuple.
    assert!(
        code.contains("let Count = call 'erlang':'element'(2, "),
        "allSatisfy: with local mutation should extract 'count' via element(2, AccSt) in tuple-acc lambda. Got:\n{code}"
    );
    // BT-2356: the fold packs the final 'count' into the StateAcc map at exit so the
    // outer method body can thread it back via maps:get — the tuple accumulator is an
    // internal-to-the-fold optimisation, not a reason to drop the outer-local threading.
    assert!(
        code.contains("maps':'put'('__local__count'"),
        "allSatisfy: with local mutation should pack '__local__count' into the StateAcc at fold exit. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('__local__count'"),
        "allSatisfy: with local mutation should thread 'count' back via maps:get after the fold. Got:\n{code}"
    );
}

#[test]
fn test_detect_if_none_with_local_mutation_uses_tuple_acc() {
    // BT-1486 + BT-1276: detect:ifNone: with only a local variable mutation uses the
    // tuple-accumulator path: {FoundItem, FoundFlag, Var1, ...}. Locals are
    // unpacked via element(N, AccSt) inside the lambda — not via maps:get.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    count := 0\n",
        "    items detect: [:item | count := count + 1. item > 0] ifNone: [42]\n",
        "    count\n",
    );
    let code = codegen(src);
    // FoundItem=1, FoundFlag=2; 'count' is the first threaded local at position 3.
    assert!(
        code.contains("let Count = call 'erlang':'element'(3, "),
        "detect:ifNone: with local mutation should extract 'count' via element(3, AccSt) in tuple-acc lambda. Got:\n{code}"
    );
    assert!(
        code.contains("FoundFlag"),
        "detect:ifNone: with local mutation should still use FoundFlag accumulator. Got:\n{code}"
    );
    // BT-2355: pack the final 'count' into StateAcc at fold exit and thread it back
    // to the outer method body via maps:get.
    assert!(
        code.contains("maps':'put'('__local__count'"),
        "detect:ifNone: with local mutation should pack '__local__count' into the StateAcc at fold exit. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('__local__count'"),
        "detect:ifNone: with local mutation should thread 'count' back via maps:get after the fold. Got:\n{code}"
    );
}

// ── BT-2561: in_direct_params_loop path for search ops ────────────────
// Each test nests a search op with a local-var mutation inside a to:do:
// loop that has its own local-var mutation. The outer loop's threaded-locals
// are purely local (count + seen/inner), so it chooses use_direct_params=true
// and sets in_direct_params_loop=true before compiling the body. The inner
// search op then hits the in_direct_params_loop=true branch inside
// generate_list_bool_predicate_with_mutations / generate_list_detect_with_mutations /
// generate_list_detect_if_none_with_mutations.

#[test]
fn test_any_satisfy_nested_in_direct_params_loop() {
    // BT-2561: anySatisfy: with a local mutation nested inside a direct-params
    // to:do: loop. The outer loop uses use_direct_params=true (local-only mutations
    // on `count` and `seen`), which sets in_direct_params_loop=true before
    // compiling the body. The inner anySatisfy: picks up in_direct_params_loop=true
    // and skips the StateAcc repack in generate_list_bool_predicate_with_mutations.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    count := 0\n",
        "    seen := 0\n",
        "    1 to: 3 do: [:i |\n",
        "      count := count + 1\n",
        "      items anySatisfy: [:item | seen := seen + 1. item > 0]\n",
        "    ]\n",
        "    count\n",
    );
    let code = codegen(src);
    // Outer loop is a letrec (counted loop with local mutations).
    assert!(
        code.contains("letrec"),
        "Outer to:do: should generate a letrec. Got:\n{code}"
    );
    // Inner anySatisfy: must use foldl (mutation threading for `seen`).
    assert!(
        code.contains("'lists':'foldl'"),
        "anySatisfy: with local mutation should use lists:foldl. Got:\n{code}"
    );
    // Pure lists:any must NOT appear (mutation path was taken).
    assert!(
        !code.contains("'lists':'any'"),
        "Mutation-threaded anySatisfy: must NOT use lists:any. Got:\n{code}"
    );
    // Direct-params outer loop rebuilds StateAcc exactly once at exit (ExitSA).
    assert!(
        code.contains("ExitSA"),
        "Direct-params outer loop should rebuild StateAcc at exit. Got:\n{code}"
    );
}

#[test]
fn test_all_satisfy_nested_in_direct_params_loop() {
    // BT-2561: allSatisfy: with a local mutation nested inside a direct-params
    // to:do: loop. Exercises generate_list_bool_predicate_with_mutations(is_all=true)
    // with in_direct_params_loop=true.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    count := 0\n",
        "    seen := 0\n",
        "    1 to: 3 do: [:i |\n",
        "      count := count + 1\n",
        "      items allSatisfy: [:item | seen := seen + 1. item > 0]\n",
        "    ]\n",
        "    count\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("letrec"),
        "Outer to:do: should generate a letrec. Got:\n{code}"
    );
    assert!(
        code.contains("'lists':'foldl'"),
        "allSatisfy: with local mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'all'"),
        "Mutation-threaded allSatisfy: must NOT use lists:all. Got:\n{code}"
    );
    assert!(
        code.contains("ExitSA"),
        "Direct-params outer loop should rebuild StateAcc at exit. Got:\n{code}"
    );
}

#[test]
fn test_detect_nested_in_direct_params_loop() {
    // BT-2561: detect: with a local mutation nested inside a direct-params
    // to:do: loop. Exercises generate_list_detect_with_mutations with
    // in_direct_params_loop=true (skips the StateAcc repack in the non-direct path).
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    count := 0\n",
        "    seen := 0\n",
        "    1 to: 3 do: [:i |\n",
        "      count := count + 1\n",
        "      items detect: [:item | seen := seen + 1. item > 0]\n",
        "    ]\n",
        "    count\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("letrec"),
        "Outer to:do: should generate a letrec. Got:\n{code}"
    );
    assert!(
        code.contains("'lists':'foldl'"),
        "detect: with local mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("FoundFlag"),
        "detect: should use FoundFlag accumulator. Got:\n{code}"
    );
    assert!(
        !code.contains("'beamtalk_list':'detect'"),
        "Mutation-threaded detect: must NOT use beamtalk_list:detect. Got:\n{code}"
    );
    assert!(
        code.contains("ExitSA"),
        "Direct-params outer loop should rebuild StateAcc at exit. Got:\n{code}"
    );
}

#[test]
fn test_detect_if_none_nested_in_direct_params_loop() {
    // BT-2561: detect:ifNone: with a local mutation nested inside a direct-params
    // to:do: loop. Exercises generate_list_detect_if_none_with_mutations with
    // in_direct_params_loop=true.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    count := 0\n",
        "    seen := 0\n",
        "    1 to: 3 do: [:i |\n",
        "      count := count + 1\n",
        "      items detect: [:item | seen := seen + 1. item > 0] ifNone: [42]\n",
        "    ]\n",
        "    count\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("letrec"),
        "Outer to:do: should generate a letrec. Got:\n{code}"
    );
    assert!(
        code.contains("'lists':'foldl'"),
        "detect:ifNone: with local mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("FoundFlag"),
        "detect:ifNone: should use FoundFlag accumulator. Got:\n{code}"
    );
    assert!(
        code.contains("ExitSA"),
        "Direct-params outer loop should rebuild StateAcc at exit. Got:\n{code}"
    );
}

// ── BT-1486: count: ───────────────────────────────────────────────────

#[test]
fn test_count_pure_generates_filter_then_length() {
    // Pure count: (no mutations) uses lists:filter + erlang:length on the filtered result.
    // Falls back to beamtalk_primitive:send for non-list receivers.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items count: [:item | item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'filter'"),
        "Pure count: should filter with lists:filter. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'length'"),
        "Pure count: should compute length with erlang:length. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure count: should NOT use lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_count_with_field_mutation_uses_foldl() {
    // count: with a field mutation cannot short-circuit via filter/length; it must
    // process every element for state threading, using lists:foldl with a count accumulator.
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items count: [:item | self.n := self.n + 1. item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "count: with field mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'filter'"),
        "count: with field mutation should NOT use lists:filter. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('n'"),
        "count: body should update 'n' field via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_count_with_local_mutation_uses_tuple_acc() {
    // BT-1276: count: with only a local variable mutation uses the tuple-accumulator
    // path: {CountAcc, N, ...}. The local is unpacked inside the lambda via
    // element(2, AccSt) — not via maps:get.
    // `n` is both read inside the predicate (n := n + 1) and used after the loop, so
    // the threading plan must carry it — guarding against a future optimization that
    // drops unreferenced locals.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    n := 0\n",
        "    items count: [:item | n := n + 1. item > 0]\n",
        "    n\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "count: with local mutation should use lists:foldl. Got:\n{code}"
    );
    // CountAcc is at position 1; 'n' is the first threaded local at position 2.
    assert!(
        code.contains("let N = call 'erlang':'element'(2, "),
        "count: tuple-acc should extract 'n' via element(2, AccSt) inside the lambda. Got:\n{code}"
    );
    // BT-2356 pattern: pack 'n' into StateAcc at fold exit and thread it back via maps:get.
    assert!(
        code.contains("maps':'put'('__local__n'"),
        "count: with local mutation should pack '__local__n' into StateAcc at fold exit. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'get'('__local__n'"),
        "count: with local mutation should thread '__local__n' back via maps:get after the fold. Got:\n{code}"
    );
}

// ── flatMap: ──────────────────────────────────────────────────────────

#[test]
fn test_flat_map_pure_generates_lists_flatmap() {
    // Pure flatMap: (no mutations) delegates to lists:flatmap with an is_list guard.
    // Non-list receivers fall back to beamtalk_primitive:send.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items flatMap: [:item | #(item, item)]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'flatmap'"),
        "Pure flatMap: should generate lists:flatmap. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure flatMap: should NOT use lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_primitive':'send'("),
        "Pure flatMap: should fall back to beamtalk_primitive:send for non-lists. Got:\n{code}"
    );
}

#[test]
fn test_flat_map_with_field_mutation_uses_foldl() {
    // flatMap: with a field mutation must process every element for state threading;
    // it uses lists:foldl with a result-list accumulator instead of lists:flatmap.
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items flatMap: [:item | self.n := self.n + 1. #(item, item)]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "flatMap: with field mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'flatmap'"),
        "flatMap: with field mutation should NOT use lists:flatmap. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('n'"),
        "flatMap: body should update 'n' field via maps:put. Got:\n{code}"
    );
}

// ── flatMap: with local mutation (tuple-acc path) ────────────────────

#[test]
fn test_flat_map_with_local_mutation_uses_tuple_acc() {
    // flatMap: where the block mutates a local variable uses the tuple-accumulator
    // path (use_tuple_acc=true). Result accumulator is at element(1, AccSt) and
    // the local 'n' is unpacked at element(2, AccSt) inside the lambda.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    n := 0\n",
        "    items flatMap: [:item | n := n + 1. #(item, item)]\n",
        "    n\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "flatMap: with local mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'flatmap'"),
        "flatMap: with local mutation should NOT use lists:flatmap. Got:\n{code}"
    );
    assert!(
        code.contains("let N = call 'erlang':'element'(2, "),
        "flatMap: tuple-acc should unpack 'n' at element(2, AccSt) inside the lambda. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('__local__n'"),
        "flatMap: with local mutation should repack '__local__n' into StateAcc at fold exit. Got:\n{code}"
    );
}

// ── takeWhile: ────────────────────────────────────────────────────────

#[test]
fn test_take_while_with_local_mutation_uses_tuple_acc() {
    // takeWhile: where the block mutates a local uses the tuple-accumulator path.
    // Accumulator structure is {AccList, StillTaking, LocalVars...}.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    n := 0\n",
        "    items takeWhile: [:item | n := n + 1. item < 10]\n",
        "    n\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "takeWhile: with local mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("StillTaking"),
        "takeWhile: with local mutation should carry StillTaking in the accumulator. Got:\n{code}"
    );
    assert!(
        // State vars start at position 3 (after AccList and StillTaking).
        code.contains("let N = call 'erlang':'element'(3, "),
        "takeWhile: tuple-acc should unpack 'n' at element(3, AccSt) inside the lambda. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('__local__n'"),
        "takeWhile: with local mutation should repack '__local__n' into StateAcc at fold exit. Got:\n{code}"
    );
}

// ── dropWhile: ────────────────────────────────────────────────────────

#[test]
fn test_drop_while_with_local_mutation_uses_tuple_acc() {
    // dropWhile: where the block mutates a local uses the tuple-accumulator path.
    // Accumulator structure is {AccList, StillDropping, LocalVars...}.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    n := 0\n",
        "    items dropWhile: [:item | n := n + 1. item < 3]\n",
        "    n\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "dropWhile: with local mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("StillDropping"),
        "dropWhile: with local mutation should carry StillDropping in the accumulator. Got:\n{code}"
    );
    assert!(
        // State vars start at position 3 (after AccList and StillDropping).
        code.contains("let N = call 'erlang':'element'(3, "),
        "dropWhile: tuple-acc should unpack 'n' at element(3, AccSt) inside the lambda. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('__local__n'"),
        "dropWhile: with local mutation should repack '__local__n' into StateAcc at fold exit. Got:\n{code}"
    );
}

// ── partition: ────────────────────────────────────────────────────────

#[test]
fn test_partition_pure_generates_beamtalk_list_partition() {
    // Pure partition: (no mutations) delegates to beamtalk_list:partition.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items partition: [:item | item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'beamtalk_list':'partition'"),
        "Pure partition: should generate beamtalk_list:partition. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure partition: should NOT use lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_partition_with_local_mutation_uses_tuple_acc() {
    // partition: where the block mutates a local uses the tuple-accumulator path.
    // Accumulator structure is {MatchList, NoMatchList, LocalVars...}.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    n := 0\n",
        "    items partition: [:item | n := n + 1. item > 3]\n",
        "    n\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "partition: with local mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("MatchList"),
        "partition: with local mutation should carry MatchList in the accumulator. Got:\n{code}"
    );
    assert!(
        // State vars start at position 3 (after MatchList and NoMatchList).
        code.contains("let N = call 'erlang':'element'(3, "),
        "partition: tuple-acc should unpack 'n' at element(3, AccSt) inside the lambda. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('__local__n'"),
        "partition: with local mutation should repack '__local__n' into StateAcc at fold exit. Got:\n{code}"
    );
}

// ── groupBy: ──────────────────────────────────────────────────────────

#[test]
fn test_group_by_pure_generates_beamtalk_list_group_by() {
    // Pure groupBy: (no mutations) delegates to beamtalk_list:group_by.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items groupBy: [:item | item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'beamtalk_list':'group_by'"),
        "Pure groupBy: should generate beamtalk_list:group_by. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure groupBy: should NOT use lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_group_by_with_local_mutation_uses_tuple_acc() {
    // groupBy: where the block mutates a local uses the tuple-accumulator path.
    // Accumulator structure is {GroupMap, LocalVars...}.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    n := 0\n",
        "    items groupBy: [:item | n := n + 1. item > 3]\n",
        "    n\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "groupBy: with local mutation should use lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("GroupMap"),
        "groupBy: with local mutation should carry GroupMap in the accumulator. Got:\n{code}"
    );
    assert!(
        // State vars start at position 2 (after GroupMap only).
        code.contains("let N = call 'erlang':'element'(2, "),
        "groupBy: tuple-acc should unpack 'n' at element(2, AccSt) inside the lambda. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('__local__n'"),
        "groupBy: with local mutation should repack '__local__n' into StateAcc at fold exit. Got:\n{code}"
    );
}

// ── select: pure (no mutations) ───────────────────────────────────────

#[test]
fn test_list_select_pure_generates_lists_filter() {
    // Pure select: with a literal block and no mutations delegates to lists:filter.
    // This exercises the path: generate_list_select → generate_simple_list_op("filter")
    // covering filter_ops.rs:38 and the "filter" => "select:" match arm in mod.rs.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items select: [:item | item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'filter'"),
        "Pure select: should generate lists:filter. Got:\n{code}"
    );
    // Runtime fallback for non-list receivers uses the 'select:' selector (mapped
    // from the 'filter' operation string by generate_simple_list_op).
    assert!(
        code.contains("'select:'"),
        "Runtime fallback should use 'select:' selector. Got:\n{code}"
    );
    // Pure path must NOT use foldl (that's the mutation-threading path).
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure select: must NOT use lists:foldl. Got:\n{code}"
    );
}

// ── BT-909: non-literal callable in simple list ops ───────────────────

#[test]
fn test_list_do_non_literal_callable_emits_arity_check() {
    // BT-909: do: with a non-literal callable (method parameter) must emit an
    // is_function/2 arity check so that Tier-2 (2-arg) blocks are wrapped to
    // satisfy lists:foreach's arity-1 contract.
    let src =
        "Actor subclass: Srv\n  state: x = 0\n\n  run: items with: block =>\n    items do: block\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foreach'"),
        "Non-literal callable do: should still generate lists:foreach. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_function'"),
        "Non-literal callable do: should emit is_function/2 arity check (BT-909). Got:\n{code}"
    );
}

#[test]
fn test_list_collect_non_literal_callable_emits_arity_check() {
    // BT-909: collect: with a non-literal callable emits the same arity-check
    // wrapper as do:, here wrapping for lists:map (arity-1 contract).
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items with: block =>\n    items collect: block\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'map'"),
        "Non-literal callable collect: should still generate lists:map. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_function'"),
        "Non-literal callable collect: should emit is_function/2 arity check (BT-909). Got:\n{code}"
    );
}

#[test]
fn test_list_select_non_literal_callable_emits_arity_check() {
    // BT-909: select: with a non-literal callable also emits the arity-check wrapper.
    // This additionally covers the "filter" => "select:" match arm in mod.rs via the
    // non-literal code path (the non-literal else branch still uses the selector).
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items with: block =>\n    items select: block\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'filter'"),
        "Non-literal callable select: should generate lists:filter. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_function'"),
        "Non-literal callable select: should emit is_function/2 arity check (BT-909). Got:\n{code}"
    );
    // Runtime fallback selector is 'select:' (mapped from 'filter').
    assert!(
        code.contains("'select:'"),
        "Non-literal callable select: fallback should use 'select:' selector. Got:\n{code}"
    );
}

// ── BT-2478: ValueType context — list-op codegen ──────────────────────

#[test]
fn test_value_type_do_with_local_mutation_open_chain() {
    // ValueType + do: + local mutation → generate_value_type_do_open (value_type_codegen.rs).
    // Unlike Actor mode (which produces a closed expression returning {'nil', StateAcc}),
    // ValueType emits an open let-chain: foldl → let X = maps:get(key, FoldResult) in
    // so updated locals are visible to all subsequent method-body expressions.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run =>\n    total := 0\n    #(1, 2, 3) do: [:item | total := total + item]\n    total\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "ValueType do: with mutation should use lists:foldl. Got:\n{code}"
    );
    // Open chain: no ThreadedResult wrapper variable (unlike collect: / select:).
    // The caller (method body) directly continues the chain with the next expression.
    assert!(
        !code.contains("ThreadedResult"),
        "ValueType do: open chain should NOT emit a ThreadedResult wrapper. Got:\n{code}"
    );
    // Open chain: extracts local via maps:get (map-acc), not element(N, ...) (tuple-acc).
    assert!(
        code.contains("maps':'get'('__local__total'"),
        "ValueType do: should extract updated local via maps:get. Got:\n{code}"
    );
}

#[test]
fn test_value_type_collect_with_local_mutation_threaded_result_wrapper() {
    // ValueType + collect: + local mutation → generate_list_collect_with_mutations
    // (basic_ops.rs) in map-acc mode (ValueType forces map-acc; tuple-acc is disabled).
    // The result is wrapped in a {List, StateAcc} pair; the outer method body unpacks
    // it via element(1, ThreadedResult).
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run =>\n    count := 0\n    #(1, 2, 3) collect: [:item | count := count + 1. item * 2]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "ValueType collect: with mutation should use lists:foldl. Got:\n{code}"
    );
    // The outer method body wraps the result via a ThreadedResult variable.
    assert!(
        code.contains("ThreadedResult"),
        "ValueType collect: should produce a ThreadedResult wrapper. Got:\n{code}"
    );
    // BT-1489: String-aware result wrapping via from_list_like.
    assert!(
        code.contains("'beamtalk_collection':'from_list_like'"),
        "ValueType collect: should use from_list_like for string-aware result. Got:\n{code}"
    );
}

#[test]
fn test_value_type_select_with_local_mutation_threaded_result_wrapper() {
    // ValueType + select: + local mutation → generate_list_filter_with_mutations
    // (filter_ops.rs) in map-acc mode (ValueType forces map-acc).
    // Like collect:, the result is wrapped in a {List, StateAcc} pair that the outer
    // method body unpacks via element(1, ThreadedResult).
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run =>\n    count := 0\n    #(1, -2, 3) select: [:item | count := count + 1. item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "ValueType select: with mutation should use lists:foldl. Got:\n{code}"
    );
    // The outer method body wraps the result via a ThreadedResult variable.
    assert!(
        code.contains("ThreadedResult"),
        "ValueType select: should produce a ThreadedResult wrapper. Got:\n{code}"
    );
    // BT-1489: String-aware result wrapping via from_list_like.
    assert!(
        code.contains("'beamtalk_collection':'from_list_like'"),
        "ValueType select: should use from_list_like for string-aware result. Got:\n{code}"
    );
}

#[test]
fn test_value_type_do_non_literal_callable_seeds_empty_state() {
    // ValueType + do: with non-literal callable → generate_simple_list_op (mod.rs lines 141–150).
    // In ValueType context there is no actor State var in scope, so the Tier-2 wrapper is
    // seeded with ~{}~ (empty map) rather than the current StateAcc.
    let src = "Value subclass: V\n  state: dummy = 0\n\n  run: items with: block =>\n    items do: block\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foreach'"),
        "ValueType do: with non-literal callable should use lists:foreach. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_function'"),
        "ValueType non-literal do: should emit is_function/2 arity check (BT-909). Got:\n{code}"
    );
    // ValueType seeds the Tier-2 wrapper with ~{}~ (no actor State in scope).
    assert!(
        code.contains("~{}~"),
        "ValueType non-literal do: should seed wrapper with ~{{}}~ (empty map). Got:\n{code}"
    );
}

// ── reject: ───────────────────────────────────────────────────────────

#[test]
fn test_list_reject_pure_generates_negated_filter() {
    // Pure reject: (no mutations) wraps the predicate in an `erlang:not` negating fun
    // and delegates to lists:filter. An is_list guard routes non-list receivers to
    // beamtalk_primitive:send with 'reject:' selector.
    let src = "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items reject: [:item | item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'filter'"),
        "Pure reject: should generate lists:filter (negated). Got:\n{code}"
    );
    // The predicate is wrapped with erlang:not to invert the inclusion decision.
    assert!(
        code.contains("call 'erlang':'not'"),
        "Pure reject: must wrap predicate with erlang:not. Got:\n{code}"
    );
    // Runtime fallback selector must be 'reject:' (not 'select:' or 'filter').
    assert!(
        code.contains("'reject:'"),
        "Runtime fallback for reject: should use 'reject:' selector. Got:\n{code}"
    );
    // Pure path must NOT fall through to the stateful foldl path.
    assert!(
        !code.contains("'lists':'foldl'"),
        "Pure reject: must NOT use lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_list_reject_with_field_mutation_uses_foldl() {
    // reject: with a field mutation cannot use the pure negating-filter path; it must
    // thread state via lists:foldl. The negate=true flag produces `call 'erlang':'not'`
    // around the predicate result in the filter condition (BodyKind::FoldlFilter{negate:true}).
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items reject: [:item | self.n := self.n + 1. item > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "reject: with field mutation should use lists:foldl. Got:\n{code}"
    );
    // negate=true: the fold body uses erlang:not to invert the predicate in the case expression.
    assert!(
        code.contains("call 'erlang':'not'"),
        "reject: with field mutation should emit erlang:not for the negate=true condition. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('n'"),
        "reject: body should update 'n' field via maps:put. Got:\n{code}"
    );
}

#[test]
fn test_list_reject_with_local_mutation_uses_tuple_acc() {
    // BT-1276 + BT-2342: reject: with only a local variable mutation uses the
    // tuple-accumulator path: {ResultList, Var1, ...}. Locals are unpacked via
    // element(2, AccSt) inside the lambda, and the result is wrapped via
    // from_list_like for string-aware reconstruction — parity with select:.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: items =>\n",
        "    count := 0\n",
        "    items reject: [:item | count := count + 1. item > 0]\n",
        "    count\n",
    );
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "reject: with local mutation should use lists:foldl. Got:\n{code}"
    );
    // ResultList is at element(1, AccSt); 'count' is the first threaded local at element(2, ...).
    assert!(
        code.contains("let Count = call 'erlang':'element'(2, "),
        "reject: tuple-acc should extract 'count' via element(2, AccSt) inside lambda. Got:\n{code}"
    );
    // negate=true: the tuple-acc case expression wraps the predicate with erlang:not.
    assert!(
        code.contains("call 'erlang':'not'"),
        "reject: with local mutation should emit erlang:not for the negate=true case condition. Got:\n{code}"
    );
    // BT-2342: result must be reconstructed via from_list_like (String → binary, Array → Array).
    assert!(
        code.contains("'beamtalk_collection':'from_list_like'("),
        "reject: with local mutation should use from_list_like for string-aware result. Got:\n{code}"
    );
}

// ── sort: pure path ───────────────────────────────────────────────────

#[test]
fn test_sort_pure_generates_beamtalk_list_sort_with() {
    // Pure sort: (no mutations) delegates to beamtalk_list:sort_with/2 with an
    // is_list guard. Non-list receivers fall back to beamtalk_primitive:send with
    // 'sort:' selector. The mutation path (lists:sort + process-dict) must NOT appear.
    let src =
        "Actor subclass: Srv\n  state: x = 0\n\n  run: items =>\n    items sort: [:a :b | a < b]\n";
    let code = codegen(src);
    assert!(
        code.contains("'beamtalk_list':'sort_with'"),
        "Pure sort: should generate beamtalk_list:sort_with. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'is_list'("),
        "Pure sort: should guard with erlang:is_list. Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_primitive':'send'("),
        "Pure sort: should fall back to beamtalk_primitive:send for non-lists. Got:\n{code}"
    );
    // Runtime fallback selector is 'sort:' (consistent with 'select:' / 'reject:' pattern).
    assert!(
        code.contains("'sort:'"),
        "Pure sort: runtime fallback should use 'sort:' selector. Got:\n{code}"
    );
    // lists:sort is used only in the mutation path (process-dictionary state threading).
    assert!(
        !code.contains("'lists':'sort'"),
        "Pure sort: should NOT use lists:sort (that is the mutation path). Got:\n{code}"
    );
}

// ── BT-2703: eachWithIndex: / do:separatedBy: desugar ────────────────────────
//
// When a block mutates actor state (field or local), `eachWithIndex:` and
// `do:separatedBy:` desugar into an `inject:into:` fold so the mutations are
// correctly threaded.  Non-mutating blocks fall through to the `Collection.bt`
// dispatch.  The selector still appears in the generated method metadata, so
// negative assertions must target the *dispatch body*, not the whole module.

#[test]
fn test_each_with_index_field_mutation_desugars_in_actor() {
    // A 2-arg block that assigns a field must desugar to lists:foldl and be
    // re-projected to the nil-with-state tuple (the actor state-thread contract).
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: items =>\n    items eachWithIndex: [:item :i | self.total := self.total + (item * i)]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "eachWithIndex: field mutation: should desugar to lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("'maps':'put'('total'"),
        "eachWithIndex: field mutation: should thread the 'total' field. Got:\n{code}"
    );
    assert!(
        code.contains("{'nil', call 'erlang':'element'(2,"),
        "eachWithIndex: field mutation: actor should re-project result to {{'nil', NewState}}. Got:\n{code}"
    );
}

#[test]
fn test_each_with_index_local_mutation_desugars_in_actor() {
    // A 2-arg block that assigns a local var must also desugar; the local is
    // threaded through the fold accumulator.  Locals are initialised with
    // bare assignment (no `| var |` declaration — that is not Beamtalk syntax).
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    acc := 0\n    items eachWithIndex: [:item :i | acc := acc + i]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "eachWithIndex: local mutation: should desugar to lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("'maps':'put'('__local__acc'"),
        "eachWithIndex: local mutation: should thread the 'acc' local. Got:\n{code}"
    );
    assert!(
        code.contains("{'nil', call 'erlang':'element'(2,"),
        "eachWithIndex: local mutation: actor should re-project to {{'nil', NewState}}. Got:\n{code}"
    );
}

#[test]
fn test_each_with_index_pure_block_falls_through() {
    // A block that does not mutate any state must NOT desugar; it dispatches to
    // the Collection.bt method (covers the `!needs_threading` early-return).
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    items eachWithIndex: [:item :i | item + i]\n";
    let code = codegen(src);
    assert!(
        code.contains("'eachWithIndex:'"),
        "eachWithIndex: pure block: should fall through to eachWithIndex: dispatch. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "eachWithIndex: pure block: should NOT desugar to lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_each_with_index_wrong_arity_block_falls_through() {
    // A 1-arg block (wrong arity for eachWithIndex:) must not desugar; the
    // Collection.bt method will raise the correct runtime error (covers the
    // arity guard in try_generate_each_with_index).
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: items =>\n    items eachWithIndex: [:x | self.total := self.total + x]\n";
    let code = codegen(src);
    assert!(
        code.contains("'eachWithIndex:'"),
        "eachWithIndex: wrong arity: should fall through to eachWithIndex: dispatch. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "eachWithIndex: wrong arity: should NOT desugar to lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_each_with_index_value_type_desugars_without_reprojection() {
    // In a Value-type method the desugar fires (a mutating local still needs
    // threading) but the actor nil-with-state re-projection must NOT appear —
    // the fold accumulator value is the result directly (covers the else branch
    // of finalize_enumeration_fold).
    let src = "Value subclass: Accumulator\n  state: total = 0\n\n  run: items =>\n    acc := 0\n    items eachWithIndex: [:item :i | acc := acc + i]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "eachWithIndex: Value type: should still desugar to lists:foldl. Got:\n{code}"
    );
    assert!(
        !code.contains("{'nil', call 'erlang':'element'(2,"),
        "eachWithIndex: Value type: should NOT re-project (not an actor). Got:\n{code}"
    );
}

#[test]
fn test_do_separated_by_element_block_mutation_desugars_in_actor() {
    // When the element block mutates a field the call desugars to a
    // lists:foldl with the "is-first" flag as accumulator, and the actor
    // nil-with-state re-projection is emitted.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: items =>\n    items do: [:x | self.total := self.total + x] separatedBy: [nil]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "do:separatedBy: element mutation: should desugar to lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("'maps':'put'('total'"),
        "do:separatedBy: element mutation: should thread the 'total' field. Got:\n{code}"
    );
    assert!(
        code.contains("{'nil', call 'erlang':'element'(2,"),
        "do:separatedBy: element mutation: actor should re-project to {{'nil', NewState}}. Got:\n{code}"
    );
}

#[test]
fn test_do_separated_by_separator_block_mutation_desugars_in_actor() {
    // Even when only the *separator* block mutates state the call must desugar.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  run: items =>\n    items do: [:x | x printString] separatedBy: [self.count := self.count + 1]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "do:separatedBy: separator mutation: should desugar to lists:foldl. Got:\n{code}"
    );
    assert!(
        code.contains("'maps':'put'('count'"),
        "do:separatedBy: separator mutation: should thread the 'count' field. Got:\n{code}"
    );
    assert!(
        code.contains("{'nil', call 'erlang':'element'(2,"),
        "do:separatedBy: separator mutation: actor should re-project to {{'nil', NewState}}. Got:\n{code}"
    );
}

#[test]
fn test_do_separated_by_pure_blocks_fall_through() {
    // When both blocks are pure the call must NOT desugar; it dispatches to
    // the Collection.bt method (covers the both-pure early-return in
    // try_generate_do_separated_by).
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  run: items =>\n    items do: [:item | item + 1] separatedBy: [nil]\n";
    let code = codegen(src);
    assert!(
        code.contains("'do:separatedBy:'"),
        "do:separatedBy: pure blocks: should fall through to do:separatedBy: dispatch. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "do:separatedBy: pure blocks: should NOT desugar to lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_each_with_index_degenerate_param_names_falls_through() {
    // `[:x :x | …]` — elem and index sharing a name — must not desugar; the
    // guard in try_generate_each_with_index leaves it to the normal dispatch
    // path's own diagnostics rather than building a fold with a shadowed
    // accumulator parameter.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: items =>\n    items eachWithIndex: [:x :x | self.total := self.total + x]\n";
    let code = codegen(src);
    assert!(
        code.contains("'eachWithIndex:'"),
        "eachWithIndex: degenerate params: should fall through to eachWithIndex: dispatch. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "eachWithIndex: degenerate params: should NOT desugar to lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_each_with_index_non_literal_callable_falls_through() {
    // A block *variable* (not a literal `[ … ]`) cannot be desugared — the
    // synthetic AST builders need the actual parameter names and body, so the
    // call dispatches to Collection.bt as an ordinary callable send.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: items =>\n    blk := [:item :i | self.total := self.total + item]\n    items eachWithIndex: blk\n";
    let code = codegen(src);
    assert!(
        code.contains("'beamtalk_message_dispatch':'send'"),
        "eachWithIndex: non-literal callable: should dispatch via message send. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "eachWithIndex: non-literal callable: should NOT desugar to lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_do_separated_by_non_literal_callable_falls_through() {
    // Same non-literal guard as above, but for the `do:separatedBy:` element
    // block argument.
    let src = "Actor subclass: Ctr\n  state: total = 0\n\n  run: items =>\n    blk := [:x | self.total := self.total + x]\n    items do: blk separatedBy: [nil]\n";
    let code = codegen(src);
    assert!(
        code.contains("'beamtalk_message_dispatch':'send'"),
        "do:separatedBy: non-literal callable: should dispatch via message send. Got:\n{code}"
    );
    assert!(
        !code.contains("'lists':'foldl'"),
        "do:separatedBy: non-literal callable: should NOT desugar to lists:foldl. Got:\n{code}"
    );
}

#[test]
fn test_do_separated_by_value_type_desugars_without_reprojection() {
    // Mirrors test_each_with_index_value_type_desugars_without_reprojection for
    // do:separatedBy:: in a Value-type method the desugar still fires for a
    // mutating local, but the actor nil-with-state re-projection must NOT
    // appear (covers the else branch of finalize_enumeration_fold for this
    // selector).
    let src = "Value subclass: Accumulator\n  state: total = 0\n\n  run: items =>\n    acc := 0\n    items do: [:x | x printString] separatedBy: [acc := acc + 1]\n";
    let code = codegen(src);
    assert!(
        code.contains("'lists':'foldl'"),
        "do:separatedBy: Value type: should still desugar to lists:foldl. Got:\n{code}"
    );
    assert!(
        !code.contains("{'nil', call 'erlang':'element'(2,"),
        "do:separatedBy: Value type: should NOT re-project (not an actor). Got:\n{code}"
    );
}
