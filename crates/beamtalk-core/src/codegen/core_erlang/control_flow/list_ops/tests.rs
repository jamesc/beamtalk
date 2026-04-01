// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

fn codegen(src: &str) -> String {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);
    crate::codegen::core_erlang::generate_module(
        &module,
        crate::codegen::core_erlang::CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("codegen should succeed")
}

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
    let has_bare_expr_in = code.contains("call 'erlang':'+'") && {
        // Find the position of the '+' call and check what precedes it
        if let Some(pos) = code.find("call 'erlang':'+'") {
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
    let has_bare_expr_in = code.contains("call 'erlang':'+'") && {
        if let Some(pos) = code.find("call 'erlang':'+'") {
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
fn test_collect_with_mutation_has_string_aware_result() {
    // BT-1489: collect: with mutations should emit is_binary guard so String
    // receivers get iolist_to_binary applied to the result list.
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items collect: [:x | self.n := self.n + 1. x]\n";
    let code = codegen(src);
    assert!(
        code.contains("'erlang':'is_binary'("),
        "BT-1489: collect: with mutations should check is_binary on receiver. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'iolist_to_binary'("),
        "BT-1489: collect: with mutations should call iolist_to_binary for string receivers. Got:\n{code}"
    );
}

#[test]
fn test_select_with_mutation_has_string_aware_result() {
    // BT-1489: select: with mutations should emit is_binary guard.
    let src = "Actor subclass: Ctr\n  state: n = 0\n\n  run: items =>\n    items select: [:x | self.n := self.n + 1. x > 0]\n";
    let code = codegen(src);
    assert!(
        code.contains("'erlang':'is_binary'("),
        "BT-1489: select: with mutations should check is_binary on receiver. Got:\n{code}"
    );
    assert!(
        code.contains("'erlang':'iolist_to_binary'("),
        "BT-1489: select: with mutations should call iolist_to_binary for string receivers. Got:\n{code}"
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
