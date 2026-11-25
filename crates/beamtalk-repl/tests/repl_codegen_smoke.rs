// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL-boundary Core Erlang codegen tests, exercised through
//! `beamtalk-repl::codegen`'s public functions.
//!
//! Lives here rather than in `beamtalk-core` (ADR 0117 Decision step 4):
//! every one of these tests exercises `beamtalk-repl`'s public
//! REPL-codegen entry points against public `beamtalk-core` AST types --
//! none of them touch `CoreErlangGenerator` or other codegen internals
//! directly. They belong here, in `beamtalk-repl`'s own test suite,
//! exercising its own public API.
//!
//! Kept as a Cargo integration test (linking `beamtalk-repl` and
//! `beamtalk-core` as ordinary external dependencies) rather than folded
//! into `beamtalk-repl::codegen`'s existing `#[cfg(test)] mod tests` --
//! that keeps this large, scenario-driven suite out of the unit-test module
//! that exercises `codegen`'s internals directly.

use beamtalk_core::ast::*;
use beamtalk_core::test_helpers::test_support::parse_ok;
use beamtalk_repl::codegen::{
    generate_repl_expression, generate_repl_expressions, generate_repl_expressions_with_index,
    generate_test_expression,
};

/// Asserts the generated Core Erlang contains `needle`; a failure prints both.
#[track_caller]
fn assert_has(code: &str, needle: &str) {
    assert!(
        code.contains(needle),
        "expected generated code to contain {needle:?}; got:\n{code}"
    );
}

/// Asserts the generated Core Erlang does not contain `needle`.
#[track_caller]
fn assert_lacks(code: &str, needle: &str) {
    assert!(
        !code.contains(needle),
        "expected generated code not to contain {needle:?}; got:\n{code}"
    );
}

/// REPL Core Erlang for a single-expression `src` (parsed with `parse_ok`).
fn repl_expression_code(src: &str, module_name: &str) -> String {
    let module = parse_ok(src);
    let [statement] = module.expressions.as_slice() else {
        panic!("expected exactly one expression in {src:?}");
    };
    generate_repl_expression(&statement.expression, module_name).expect("codegen should work")
}

/// Core Erlang from `generate_test_expression` for a single-expression `src`.
fn repl_test_expression_code(src: &str, module_name: &str) -> String {
    let module = parse_ok(src);
    let [statement] = module.expressions.as_slice() else {
        panic!("expected exactly one expression in {src:?}");
    };
    generate_test_expression(&statement.expression, module_name).expect("codegen should work")
}

/// REPL Core Erlang for every statement of `src` (parsed with `parse_ok`).
fn repl_expressions_code(src: &str, module_name: &str) -> String {
    let expressions: Vec<Expression> = parse_ok(src)
        .expressions
        .iter()
        .map(|statement| statement.expression.clone())
        .collect();
    generate_repl_expressions(&expressions, module_name).expect("codegen should work")
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:72-97 ----

#[test]
fn test_generate_repl_module_aliases_state_to_bindings() {
    // REPL modules must alias State to Bindings for identifier lookups
    let code = repl_expression_code("x", "repl_test");

    // Check that the module aliases State to Bindings
    assert_has(&code, "let State = Bindings in");

    // ADR 0081 Phase 1: a free REPL identifier now resolves via a
    // locals maps:find against State with a runtime resolve_name fallthrough,
    // instead of a bare maps:get (which would throw {badkey,_} once workspace
    // globals are no longer eagerly injected into State).
    assert_has(&code, "call 'maps':'find'('x', State)");
    assert_has(
        &code,
        "call 'beamtalk_workspace':'resolve_name'(State, 'x')",
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:98-135 ----

#[test]
fn test_generate_repl_module_block_value_call() {
    // Test full REPL module generation for block value call
    // Expression: [:x | x + 1] value: 5
    let code = repl_expression_code("[:x | x + 1] value: 5", "test_block_repl");

    // Check basic structure
    assert_has(&code, "let State = Bindings in");
    assert_has(&code, "apply");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:136-156 ----

#[test]
fn test_generate_repl_module_returns_tuple_with_state() {
    // REPL eval/1 should return {Result, UpdatedBindings}
    let code = repl_expression_code("42", "repl_tuple_test");

    // Check that the result is wrapped in a tuple with State
    assert_has(&code, "let Result =");
    assert_has(&code, "{Result, State}");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:157-226 ----

#[test]
fn test_generate_repl_module_with_times_repeat_mutation() {
    // REPL with mutation should return updated state
    // Expression: 5 timesRepeat: [count := count + 1]

    // Build the block: [count := count + 1]
    let code = repl_expression_code("5 timesRepeat: [count := count + 1]", "repl_times_test");

    // For mutation-threaded loops, return {Result, State} tuple.
    // REPL extracts via element/2: let _LoopResult = element(1, Result) ...
    assert_has(&code, "'element'(1, Result)");
    assert_has(&code, "'element'(2, Result)");

    // Loop termination should return {nil, StateAcc}
    assert_has(&code, "{'nil', StateAcc}");

    // Verify mutation threading details
    assert_has(&code, "letrec 'repeat'/2");
    assert_has(&code, "maps':'put'('count'");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:227-300 ----

#[test]
fn test_generate_repl_module_with_to_do_mutation() {
    // REPL with to:do: mutation should return updated state
    // Expression: 1 to: 5 do: [:n | total := total + n]

    // Build the block: [:n | total := total + n]
    let code = repl_expression_code("1 to: 5 do: [:n | total := total + n]", "repl_to_do_test");

    // For mutation-threaded loops, return {Result, State} tuple.
    assert_has(&code, "'element'(1, Result)");
    assert_has(&code, "'element'(2, Result)");

    // Verify to:do: mutation threading
    assert_has(&code, "letrec 'loop'/2");
    assert_has(&code, "maps':'put'('total'");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:301-384 ----

#[test]
fn test_generate_repl_module_with_while_true_mutation() {
    // REPL with whileTrue: mutation should read condition from StateAcc
    // Expression: [x < 5] whileTrue: [x := x + 1]

    // Build the condition: [x < 5]
    let code = repl_expression_code("[x < 5] whileTrue: [x := x + 1]", "repl_while_test");

    // Condition lambda should take StateAcc parameter
    assert_has(&code, "fun (StateAcc) ->");
    // Condition should read x from StateAcc, not outer scope.
    // Lazy resolution (ADR 0081 Phase 1) now applies inside loop bodies too, so
    // the read is a `maps:find` against StateAcc with a `resolve_name`
    // fallthrough rather than a bare `maps:get` (which would `badkey` on a miss).
    assert_has(&code, "maps':'find'('x', StateAcc)");
    assert_has(
        &code,
        "call 'beamtalk_workspace':'resolve_name'(StateAcc, 'x')",
    );
    // Condition should be applied with StateAcc argument
    assert_has(&code, "apply");
    assert_has(&code, "(StateAcc)");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:385-431 ----

#[test]
fn test_repl_multi_stmt_times_repeat_intermediate() {
    // `x := 1. 5 timesRepeat: [x := x + 1]. x` should return 6.
    // The loop is in intermediate (non-last) position — its StateAcc must be threaded
    // to the final `x` lookup, not discarded.
    let src = "x := 1. 5 timesRepeat: [x := x + 1]. x";
    let module = parse_ok(src);
    let code = generate_repl_expressions(
        &module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect::<Vec<_>>(),
        "repl_multi_times_test",
    )
    .expect("codegen should work");

    // The loop StateAcc must be extracted and threaded: element(2, _R@2) → StateN
    assert_has(&code, "'element'(2, _R@2)");

    // The final `x` lookup must use the updated state (StateN), not a stale State1
    // StateN is whatever state comes after threading the loop result
    assert_lacks(&code, "maps':'get'('x', State1)");

    // The return tuple must NOT use element/2 unwrapping for the last (plain identifier) expr
    assert_lacks(&code, "'element'(1, Result)");

    // The return tuple should be plain {Result, StateN}
    assert_has(&code, "{Result, State");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:432-470 ----

#[test]
fn test_repl_multi_stmt_while_true_intermediate() {
    // `x := 0. [x < 3] whileTrue: [x := x + 1]. x` — whileTrue: in intermediate
    // position must thread its StateAcc so the final `x` lookup sees the updated value.
    let src = "x := 0. [x < 3] whileTrue: [x := x + 1]. x";
    let module = parse_ok(src);
    let code = generate_repl_expressions(
        &module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect::<Vec<_>>(),
        "repl_multi_while_test",
    )
    .expect("codegen should work");

    // The loop StateAcc must be extracted from the intermediate result
    assert_has(&code, "'element'(2, _R@2)");

    // The final x lookup must NOT read from State1 (which has x=0 from init only)
    assert_lacks(&code, "maps':'get'('x', State1)");

    // Return tuple must be plain {Result, StateN}, not element/2 wrapped
    assert_lacks(&code, "'element'(1, Result)");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:471-516 ----

#[test]
fn test_repl_multi_stmt_assignment_then_loop_then_plain() {
    // Regression — multiple intermediate expressions including assignment + loop.
    // `count := 0. 3 timesRepeat: [count := count + 1]. count` generates state chain:
    //   State → State1 (from assignment) → State2 (from loop StateAcc) → {Result, State2}
    let src = "count := 0. 3 timesRepeat: [count := count + 1]. count";
    let module = parse_ok(src);
    let code = generate_repl_expressions(
        &module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect::<Vec<_>>(),
        "repl_multi_chain_test",
    )
    .expect("codegen should work");

    // Assignment creates State1
    assert_has(&code, "State1 = call 'maps':'put'('count'");

    // Loop in intermediate position: StateAcc extracted as State2
    assert_has(&code, "'element'(2, _R@2)");

    // Final count lookup must not use stale State1
    assert_lacks(&code, "maps':'get'('count', State1)");

    // No element/2 unwrapping on the final return
    assert_lacks(&code, "'element'(1, Result)");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:517-557 ----

#[test]
fn test_generate_repl_module_with_arithmetic() {
    // Verify complex expressions with variable references work
    // Expression: x + 1
    let code = repl_expression_code("x + 1", "repl_arith");

    // Check State aliasing
    assert_has(&code, "let State = Bindings in");

    // Check that x lookup works through State. ADR 0081 Phase 1: a free
    // REPL identifier resolves via a locals maps:find with a resolve_name
    // fallthrough rather than a bare maps:get.
    assert_has(&code, "call 'maps':'find'('x', State)");
    assert_has(
        &code,
        "call 'beamtalk_workspace':'resolve_name'(State, 'x')",
    );

    // Check the arithmetic operation
    assert_has(&code, "call 'erlang':'+'(");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:558-641 ----

#[test]
fn test_generate_repl_multi_stmt_times_repeat_then_read() {
    // x := 1. 5 timesRepeat: [x := x + 1]. x
    // The loop in intermediate position must thread its updated state to the final `x` read.

    let code = repl_expressions_code(
        "x := 1. 5 timesRepeat: [x := x + 1]. x",
        "repl_multi_loop_test",
    );

    // The loop in intermediate position must have its StateAcc extracted
    assert_has(&code, "call 'erlang':'element'(2,");

    // The final x read must use a state that was updated by the loop
    // (not the original State or State1 from the x := 1 assignment)
    assert_has(&code, "let Result =");

    // The overall structure: should have State1 from assignment, then state extraction from loop
    assert_has(&code, "maps':'put'('x'");
    assert_has(&code, "letrec 'repeat'/2");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:642-733 ----

#[test]
fn test_generate_repl_multi_stmt_while_true_then_read() {
    // x := 0. [x < 5] whileTrue: [x := x + 1]. x
    // whileTrue: in intermediate position must thread state to the final x read.

    let code = repl_expressions_code(
        "x := 0. [x < 5] whileTrue: [x := x + 1]. x",
        "repl_multi_while_test",
    );

    // The loop in intermediate position must have its StateAcc extracted
    assert_has(&code, "call 'erlang':'element'(2,");

    // Should bind the final result
    assert_has(&code, "let Result =");

    // Should use whileTrue: mutation-threaded structure
    assert_has(&code, "letrec 'while'/1");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:734-812 ----

#[test]
fn test_generate_repl_multi_stmt_loop_does_not_corrupt_final_expr() {
    // repl_loop_mutated must be reset before the final expression.
    // x := 1. 5 timesRepeat: [x := x + 1]. 42
    // The final expression `42` is not a loop, so it must NOT use element/2 unwrapping.

    let code = repl_expressions_code(
        "x := 1. 5 timesRepeat: [x := x + 1]. 42",
        "repl_multi_loop_no_corrupt_test",
    );

    // Final expression is a literal — must NOT apply element/2 unwrapping on Result
    // The return tuple must be {Result, StateN} where Result = 42 (not extracted from a tuple)
    assert_lacks(&code, "'element'(1, Result)");
    assert_lacks(&code, "'element'(2, Result)");

    // The intermediate loop SHOULD still extract state (element(2, _R2))
    assert_has(&code, "call 'erlang':'element'(2,");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:813-893 ----

#[test]
fn test_repl_loop_mutations_accumulate_plain_key() {
    // In REPL mode, loop writes must use plain key so reads accumulate.
    // Expression: 5 timesRepeat: [x := x + 1]
    // Write path must use 'x' not '__local__x' so that each iteration reads the
    // value written by the previous iteration from StateAcc.

    let code = repl_expression_code("5 timesRepeat: [x := x + 1]", "bt800_test");

    // REPL mode must use plain key 'x' (not '__local__x') so reads match writes.
    assert_has(&code, "maps':'put'('x'");
    assert_lacks(&code, "__local__x");

    // Reads inside loop body must use StateAcc (not State) so
    // they get the accumulated value from the previous iteration. Lazy
    // resolution (ADR 0081 Phase 1) now applies inside loop bodies too, so the
    // read is a `maps:find` against StateAcc with a `resolve_name` fallthrough
    // rather than a bare `maps:get` (which would `badkey` on a miss).
    assert_has(&code, "maps':'find'('x', StateAcc)");
    assert_has(
        &code,
        "call 'beamtalk_workspace':'resolve_name'(StateAcc, 'x')",
    );

    // Loop must thread state correctly (arity-2 letrec, returns {nil, StateAcc}).
    assert_has(&code, "letrec 'repeat'/2");
    assert_has(&code, "{'nil', StateAcc}");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:894-985 ----

#[test]
fn test_repl_multi_stmt_loop_accumulates_from_zero() {
    // x := 0. 5 timesRepeat: [x := x + 1]. x
    // Acceptance criteria: starting from zero, result must be 5.
    // Validates that the multi-statement path threads state correctly through the loop.

    let code = repl_expressions_code("x := 0. 5 timesRepeat: [x := x + 1]. x", "bt800_zero_test");

    // Loop write must use plain key (no __local__ prefix in REPL mode)
    assert_lacks(&code, "__local__");

    // The loop must be applied with the state containing x=0
    assert_has(&code, "apply 'repeat'/2 (1, State1)");

    // The intermediate loop result must be unpacked to thread state forward
    assert_has(&code, "call 'erlang':'element'(2,");

    // Final read of x must use the state produced by the loop (State2+).
    // ADR 0081 Phase 1: the post-loop free-identifier read resolves via
    // a locals maps:find (with a resolve_name fallthrough) against State2.
    assert_has(&code, "maps':'find'('x', State2)");
    assert_has(
        &code,
        "call 'beamtalk_workspace':'resolve_name'(State2, 'x')",
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:1589-1671 ----

#[test]
fn test_class_method_call_generation() {
    // Test that ClassReference message sends generate appropriate code
    // ADR 0019: All classes (including Transcript, Beamtalk, Workspace)
    //         use standard class dispatch via class_send

    // Test 1: Beamtalk class uses standard class_send dispatch (no special case)
    let code = repl_expression_code("Beamtalk allClasses", "repl_eval");

    // ADR 0129 §7: REPL sends check session bindings, then the class registry
    // via class_send. There is no singleton step any more.
    assert_has(&code, "maps':'find");
    assert_has(&code, "class_send");
    assert_lacks(&code, "resolve_singleton_instance");
    assert_lacks(&code, "persistent_term");

    // Test 2: Non-binding class (Point) dispatches via class_send in REPL
    let code2 = repl_expression_code("Point new", "repl_eval2");

    // ADR 0129 §7: REPL class sends check session bindings, then class_send.
    assert_has(&code2, "maps':'find");
    assert_has(&code2, "class_send");
    assert_lacks(&code2, "persistent_term");

    // Test 3: ClassReference spawn in REPL uses generate_actor_spawn with registry
    let code3 = repl_expression_code("InitCounter spawn", "repl_eval3");

    // Spawn on ClassReference calls Module:spawn() directly; the REPL no
    // longer registers the pid anywhere (ADR 0150, BT-4951).
    assert_has(&code3, "call 'bt@init_counter':'spawn'()");
    assert_lacks(&code3, "register_spawned");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:2107-2134 ----

#[test]
fn test_generate_repl_list_reject() {
    // reject: must generate valid Core Erlang with properly bound wrapper fun
    // The wrapper fun must be bound via `let` — not inlined in the call args,
    // because Core Erlang lambdas don't use `end` and can't be inlined in calls.
    let src = "#(1, 2, 3, 4, 5) reject: [:x | x > 2]";
    let module = parse_ok(src);
    let expr = &module.expressions[0].expression;
    let code = generate_repl_expression(expr, "test_reject_repl").expect("codegen should work");

    // Wrapper fun must be bound to a variable, not inlined in filter call
    assert_has(&code, "call 'lists':'filter'(");
    assert_has(&code, "call 'erlang':'not'(");
    // Verify the fun is let-bound (not inlined) — the filter call arg must be a temp var
    // e.g. "call 'lists':'filter'(_temp4," not "call 'lists':'filter'(fun (X)"
    assert_lacks(&code, "'filter'(fun");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:4662-4806 ----

#[test]
#[allow(clippy::too_many_lines)]
fn test_repl_destructure_mutation_threaded_rhs_unwraps_element() {
    // When the RHS of a REPL destructuring assignment is a mutation-threaded
    // expression (a loop containing a REPL variable mutation), the generated code must
    // unwrap the {Result, StateAcc} tuple with element/2 before extracting pattern
    // variables. Without the unwrap, pattern extraction would operate on the
    // {Result, StateAcc} wrapper instead of the actual value.
    //
    // Expression: #(a, b) := #(10, 20) inject: #(0, 0) into: [:acc :item | x := x + 1. #(item, x)]
    //
    // The inject:into: block mutates `x` (a REPL-bound variable), which triggers
    // repl_loop_mutated = true and causes the expression to return {FinalAcc, StateAcc}.
    //
    // Expected generated structure:
    //   let Rhs1 = <inject:into: expression>     -> returns {FinalAcc, StateAcc}
    //   let RhsVal1 = element(1, Rhs1)            -> FinalAcc = #(20, 2)
    //   let State1 = element(2, Rhs1)             -> StateAcc with x=2
    //   let A1 = send(RhsVal1, 'at:', [1])        -> extract a from FinalAcc (not from Rhs1!)
    //   let B1 = send(RhsVal1, 'at:', [2])        -> extract b from FinalAcc (not from Rhs1!)
    //   let State2 = maps:put('a', A1, State1)
    //   let State3 = maps:put('b', B1, State2)

    let code = repl_expression_code(
        "#(a, b) := #(10, 20) inject: #(0, 0) into: [:acc :item | x := x + 1. #(item, x)]",
        "bt1283_mutation_threaded_test",
    );

    // The RHS is mutation-threaded, so it returns {FinalAcc, StateAcc}.
    // element(1, Rhs) extracts FinalAcc; element(2, Rhs) advances REPL state.
    assert_has(&code, "call 'erlang':'element'(1,");
    assert_has(&code, "call 'erlang':'element'(2,");

    // Pattern extraction must use the unwrapped _RhsVal (not the raw {Acc,State} _Rhs).
    // The 'at:' dispatch must receive the _RhsVal variable (fresh_temp_var("RhsVal") prefix),
    // not _Rhs (the raw mutation-threaded result holding {FinalAcc, StateAcc}).
    assert_has(&code, "'send'(_RhsVal");

    // The pattern variables must be persisted to the REPL state map.
    assert_has(&code, "call 'maps':'put'('a'");
    assert_has(&code, "call 'maps':'put'('b'");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:1025-1078 ----

#[test]
fn test_cascade_repl_expression() {
    // Test cascade in a full REPL module context
    // x negated; abs
    let code = repl_expression_code("x negated; abs", "test_cascade");

    // Should have module structure
    assert_has(
        &code,
        "module 'test_cascade' ['eval'/1, 'module_info'/0, 'module_info'/1]",
    );

    // Should bind the underlying receiver once. ADR 0081 Phase 1: a
    // free REPL identifier now resolves via a locals maps:find with a runtime
    // resolve_name fallthrough rather than a bare maps:get.
    assert_has(
        &code,
        "let _Receiver@1 = case call 'maps':'find'('x', State) of",
    );
    assert_has(
        &code,
        "call 'beamtalk_workspace':'resolve_name'(State, 'x')",
    );

    // Should send both messages
    assert_has(&code, "'negated'");
    assert_has(&code, "'abs'");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:1232-1283 ----

#[test]
fn test_standalone_class_reference_uses_dynamic_module_name() {
    // Test that standalone ClassReference uses module_name/1 dynamically
    // Review comment: Should match generate_beamtalk_class_named pattern (lines 915-922)
    let code = repl_expression_code("Point", "repl_eval");

    // ADR 0129 §7: a REPL class reference checks the session map first (a
    // binding shadows the class), then the class registry inline — no runtime
    // resolver.
    assert_has(&code, "call 'maps':'find'('Point', ");
    assert_has(&code, "call 'erlang':'whereis'('beamtalk_class_Point')");
    assert_lacks(&code, "resolve_class_reference");
    assert_has(&code, "'Point class'");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:1284-1330 ----

#[test]
fn test_standalone_class_reference_validates_undefined_classes() {
    // Test that standalone ClassReference raises class_not_found error for undefined classes
    let code = repl_expression_code("NonExistentClass", "repl_eval");

    // ADR 0129 §10: an undefined class raises class_not_found from the
    // registry lookup itself, identically to batch-compiled code.
    assert_has(&code, "call 'maps':'find'('NonExistentClass', ");
    assert_has(
        &code,
        "call 'erlang':'whereis'('beamtalk_class_NonExistentClass')",
    );
    assert_has(&code, "'class_not_found', 'NonExistentClass'");
    assert_lacks(&code, "resolve_class_reference");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:2453-2486 ----

#[test]
fn test_repl_expression_spawn_uses_class_module_index() {
    // When a REPL expression like `Counter spawn` is compiled in a workspace
    // with package "getting_started", the class_module_index must be consulted
    // so the generated code calls 'bt@getting_started@counter':'spawn'()
    // instead of the heuristic fallback 'bt@counter':'spawn'().
    let src = "Counter spawn";
    let module = parse_ok(src);
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();

    let mut index = std::collections::HashMap::new();
    index.insert(
        "Counter".to_string(),
        "bt@getting_started@counter".to_string(),
    );

    let code = generate_repl_expressions_with_index(&expressions, "repl_test_mod", index)
        .expect("codegen should work");

    assert_has(&code, "'bt@getting_started@counter':'spawn'");
    assert_lacks(&code, "'bt@counter':'spawn'");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:2487-2507 ----

#[test]
fn test_repl_expression_spawn_without_index_uses_heuristic() {
    // Without class_module_index, spawn falls back to the heuristic bt@ prefix.
    let src = "Counter spawn";
    let module = parse_ok(src);
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();

    let code =
        generate_repl_expressions(&expressions, "repl_test_mod").expect("codegen should work");

    assert_has(&code, "'bt@counter':'spawn'");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:2508-2538 ----

#[test]
fn test_repl_expression_spawn_with_args_uses_class_module_index() {
    // `Counter spawnWith: #{#value => 10}` must also use class_module_index.
    let src = "Counter spawnWith: #{#value => 10}";
    let module = parse_ok(src);
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();

    let mut index = std::collections::HashMap::new();
    index.insert(
        "Counter".to_string(),
        "bt@getting_started@counter".to_string(),
    );

    let code = generate_repl_expressions_with_index(&expressions, "repl_test_mod", index)
        .expect("codegen should work");

    assert_has(&code, "'bt@getting_started@counter':'spawn'");
    assert_lacks(&code, "'bt@counter':'spawn'");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:3124-3164 ----

#[test]
fn test_qualified_class_reference_standalone() {
    // ADR 0070 Phase 2: `json@Parser` as a standalone expression should use
    // the class registry with the short name 'Parser' and produce the
    // canonical tag 'Parser class' in the class object tuple (BT-4074).
    let code = repl_expression_code("json@Parser", "repl_eval");

    // Registry lookup uses the short class name
    assert_has(&code, "call 'erlang':'whereis'('beamtalk_class_Parser')");

    // BT-4074: the class object carries the canonical tag, not a qualified one
    assert_has(&code, "{'beamtalk_object', 'Parser class', ");
    assert_lacks(&code, "json@Parser class");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/expressions.rs (4 string-interpolation tests) ----

#[test]
fn test_string_interpolation_simple_variable() {
    // "Hello, {name}!" — variable interpolation
    let code = repl_test_expression_code("\"Hello, {name}!\"", "test_interp");
    // Should dispatch displayString via beamtalk_message_dispatch
    assert_has(&code, "'displayString'");
    assert_has(&code, "beamtalk_message_dispatch':'send'");
    // Binary construction with byte segments and binary variable
    assert_has(&code, "#<");
    assert_has(&code, "('all',8,'binary',['unsigned'|['big']])");
}

#[test]
fn test_string_interpolation_multiple_expressions() {
    // "a{x}b{y}c" — multiple expression segments
    let code = repl_test_expression_code("\"a{x}b{y}c\"", "test_multi");
    // Should have two displayString dispatches
    let dispatch_count = code.matches("'displayString'").count();
    assert_eq!(
        dispatch_count, 2,
        "Should have 2 displayString dispatches. Got {dispatch_count}:\n{code}"
    );
}

#[test]
fn test_string_interpolation_only_expression() {
    // "{name}" — only an interpolation, no literal segments
    let code = repl_test_expression_code("\"{name}\"", "test_bare");
    assert_has(&code, "'displayString'");
    // Binary should contain only the variable segment
    assert_has(&code, "('all',8,'binary',['unsigned'|['big']])");
}

#[test]
fn test_string_interpolation_integer_expression() {
    // "{42}" — integer literal in interpolation
    let code = repl_test_expression_code("\"{42}\"", "test_int");
    // Should dispatch displayString on the integer
    assert_has(&code, "'displayString'");
}

// ---- extracted from crates/beamtalk-core/src/source_analysis/parser/tests/literal_tests.rs:921-931 ----

#[test]
fn codegen_simple_match() {
    let module = parse_ok("42 match: [_ -> 99]");
    let expr = &module.expressions[0].expression;
    let result = beamtalk_repl::codegen::generate_test_expression(expr, "test_match");
    assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
    let code = result.unwrap();
    assert_has(&code, "case");
}

// ---- extracted from crates/beamtalk-core/src/source_analysis/parser/tests/literal_tests.rs:932-942 ----

#[test]
fn codegen_match_with_arms() {
    let module = parse_ok("1 match: [1 -> \"one\"; 2 -> \"two\"; _ -> \"other\"]");
    let expr = &module.expressions[0].expression;
    let result = beamtalk_repl::codegen::generate_test_expression(expr, "test_match");
    assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
    let code = result.unwrap();
    assert_has(&code, "case");
}

// ---- extracted from crates/beamtalk-core/src/source_analysis/parser/tests/literal_tests.rs:943-950 ----

#[test]
fn codegen_empty_match_errors() {
    let module = parse_ok("42 match: []");
    let expr = &module.expressions[0].expression;
    let result = beamtalk_repl::codegen::generate_test_expression(expr, "test_match");
    assert!(result.is_err(), "Empty match should fail codegen");
}

// ---- BT-3637 (ADR 0129 Phase 0a): direct-call eligibility in REPL codegen ----

const FACADE_SOURCE: &str = "\
sealed Object subclass: Facade
  class sealed osName -> String => \"linux\"
  class plain -> Integer => 1
";

/// Generates REPL Core Erlang for `expr_source` against a hierarchy built from
/// [`FACADE_SOURCE`].
fn repl_code_with_facade_hierarchy(expr_source: &str) -> String {
    let (hierarchy, _diags) =
        beamtalk_core::semantic_analysis::ClassHierarchy::build(&parse_ok(FACADE_SOURCE));
    let hierarchy = hierarchy.expect("hierarchy should build");
    let module = parse_ok(expr_source);
    let exprs: Vec<Expression> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    let index = std::collections::HashMap::from([("Facade".to_string(), "bt@facade".to_string())]);
    beamtalk_repl::codegen::generate_repl_expressions_with_hierarchy(
        &exprs,
        "repl_direct_call",
        index,
        &hierarchy,
    )
    .expect("codegen should work")
}

#[test]
fn repl_sealed_stateless_class_method_emits_direct_call() {
    let code = repl_code_with_facade_hierarchy("Facade osName");
    assert_has(&code, "call 'bt@facade':");
    // BT-4301 (ADR 0142 §2): guarded, with `class_send` as the fallback.
    assert_has(
        &code,
        "call 'beamtalk_class_shadow_flags':'static_call_target'({",
    );
    assert_has(&code, ", 'Facade class'}, 'Facade', ");
}

/// BT-4301 (ADR 0142 §1): sealing plays no part; REPL codegen shares module
/// codegen's rule, so a method that is not `class sealed` is the same guarded
/// direct call.
#[test]
fn repl_non_sealed_class_method_emits_guarded_direct_call() {
    let code = repl_code_with_facade_hierarchy("Facade plain");
    assert_has(&code, "call 'bt@facade':'class_plain'(");
    assert_has(
        &code,
        "call 'beamtalk_class_shadow_flags':'static_call_target'({",
    );
    assert_has(&code, ", 'Facade class'}, 'Facade', ");
}

#[test]
fn repl_without_hierarchy_keeps_class_send() {
    let module = parse_ok("Facade osName");
    let exprs: Vec<Expression> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    let code = generate_repl_expressions_with_index(
        &exprs,
        "repl_no_hierarchy",
        std::collections::HashMap::new(),
    )
    .expect("codegen should work");
    assert_has(&code, "'class_send'");
}

// ---- BT-3640 (ADR 0129 Phase 1): `Beamtalk` is a class-side facade ----

#[test]
fn repl_beamtalk_class_named_emits_direct_call() {
    let hierarchy = beamtalk_core::semantic_analysis::ClassHierarchy::with_builtins();
    let module = parse_ok("Beamtalk classNamed: #Integer");
    let exprs: Vec<Expression> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    let index = std::collections::HashMap::from([(
        "Beamtalk".to_string(),
        "bt@stdlib@beamtalk".to_string(),
    )]);
    let code = beamtalk_repl::codegen::generate_repl_expressions_with_hierarchy(
        &exprs,
        "repl_beamtalk_direct",
        index,
        &hierarchy,
    )
    .expect("codegen should work");
    assert_has(&code, "call 'bt@stdlib@beamtalk':");
    assert_has(
        &code,
        "call 'beamtalk_class_shadow_flags':'static_call_target'({",
    );
    assert_has(&code, ", 'Beamtalk class'}, 'Beamtalk', ");
}

// ---- BT-3778 (ADR 0111 Addendum 17): verifier diagnostics surface from REPL codegen ----

#[test]
fn test_repl_surfacing_verifier_reports_only_internal_findings() {
    use beamtalk_core::source_analysis::DiagnosticCategory;
    use beamtalk_repl::codegen::{
        generate_repl_expressions_surfacing_verifier,
        generate_repl_expressions_traced_surfacing_verifier,
        generate_test_expression_surfacing_verifier,
    };

    let src = "x := 1. x + 2";
    let module = parse_ok(src);
    let exprs: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();

    // Valid code: no diagnostics, and identical code to the plain entry point.
    let (code, diags) = generate_repl_expressions_surfacing_verifier(
        &exprs,
        &PipelineSends::default(),
        &beamtalk_core::ast::LiteralComments::default(),
        "repl_surf",
        std::collections::HashMap::new(),
        None,
        false,
    )
    .expect("codegen should work");
    assert!(diags.is_empty(), "valid code has no findings: {diags:?}");
    assert_eq!(
        code,
        generate_repl_expressions(&exprs, "repl_surf").expect("codegen should work")
    );

    // Injected finding: surfaced, categorised, and generation still succeeds.
    let (code, diags) = generate_repl_expressions_surfacing_verifier(
        &exprs,
        &PipelineSends::default(),
        &beamtalk_core::ast::LiteralComments::default(),
        "repl_surf",
        std::collections::HashMap::new(),
        None,
        true,
    )
    .expect("a verifier finding must not fail generation");
    assert_has(&code, "eval");
    assert_eq!(diags.len(), 1, "{diags:?}");
    assert_eq!(
        diags[0].category,
        Some(DiagnosticCategory::InternalVerifier)
    );
    assert!(diags[0].message.starts_with("internal:"));

    let (_, traced) = generate_repl_expressions_traced_surfacing_verifier(
        &exprs,
        &PipelineSends::default(),
        &beamtalk_core::ast::LiteralComments::default(),
        src,
        "repl_surf_t",
        std::collections::HashMap::new(),
        None,
        true,
    )
    .expect("trace codegen should work");
    assert_eq!(traced.len(), 1, "{traced:?}");

    // test-stdlib entry point: valid code has no findings.
    let (_, diags) = generate_test_expression_surfacing_verifier(
        &exprs[1],
        &PipelineSends::default(),
        &beamtalk_core::ast::LiteralComments::default(),
        "test_surf",
    )
    .expect("codegen should work");
    assert!(diags.is_empty(), "{diags:?}");
}

/// BT-4517: a `ClassBuilder` block method typed at the REPL reflects its `|>`
/// sends as written. The REPL compiles the module's expressions without the
/// module, so the entry takes the module's `|>` record alongside them.
#[test]
fn test_repl_class_builder_block_method_reflects_pipeline_as_written() {
    use beamtalk_repl::codegen::{
        generate_repl_expressions_surfacing_verifier,
        generate_repl_expressions_traced_surfacing_verifier,
    };

    let src = "Object classBuilder name: #Made; \
               methods: #{#m => [:self | self foo |> bar baz |> qux: 1]}; register";
    let module = parse_ok(src);
    let exprs: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    let entry = format!(
        "#{{{}}}#",
        beamtalk_cerl_doc::binary::binary_byte_segments("m => self foo |> bar baz |> qux: 1")
    );

    let (code, _) = generate_repl_expressions_surfacing_verifier(
        &exprs,
        &module.pipeline_sends,
        &module.literal_comments,
        "repl_pipe",
        std::collections::HashMap::new(),
        None,
        false,
    )
    .expect("codegen should work");
    assert_has(&code, &entry);

    let (code, _) = generate_repl_expressions_traced_surfacing_verifier(
        &exprs,
        &module.pipeline_sends,
        &module.literal_comments,
        src,
        "repl_pipe_t",
        std::collections::HashMap::new(),
        None,
        false,
    )
    .expect("trace codegen should work");
    assert_has(&code, &entry);
}

/// BT-4708: a `ClassBuilder` block method typed at the REPL reflects the
/// comments inside its collection literals. The REPL compiles the module's
/// expressions without the module, so the entries take the module's
/// `LiteralComments` record alongside them.
#[test]
fn test_repl_class_builder_block_method_reflects_literal_comments() {
    use beamtalk_repl::codegen::{
        generate_repl_expressions_surfacing_verifier,
        generate_repl_expressions_traced_surfacing_verifier,
    };

    let src = "Object classBuilder name: #Made; \
               methods: #{#m => [:self |\n  {\n    1, // first\n    2\n  }\n]}; register";
    let module = parse_ok(src);
    let exprs: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();
    let entry = format!(
        "#{{{}}}#",
        beamtalk_cerl_doc::binary::binary_byte_segments("m =>\n  {\n    1, // first\n    2\n  }")
    );

    let (code, _) = generate_repl_expressions_surfacing_verifier(
        &exprs,
        &module.pipeline_sends,
        &module.literal_comments,
        "repl_lit",
        std::collections::HashMap::new(),
        None,
        false,
    )
    .expect("codegen should work");
    assert_has(&code, &entry);

    let (code, _) = generate_repl_expressions_traced_surfacing_verifier(
        &exprs,
        &module.pipeline_sends,
        &module.literal_comments,
        src,
        "repl_lit_t",
        std::collections::HashMap::new(),
        None,
        false,
    )
    .expect("trace codegen should work");
    assert_has(&code, &entry);
}
