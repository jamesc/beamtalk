// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! REPL-boundary Core Erlang codegen tests, exercised through
//! `beamtalk-repl::codegen`'s public functions.
//!
//! BT-3340 (ADR 0117 Decision step 2): a Cargo integration test, not a unit
//! test embedded in `beamtalk-core::src` -- these tests were originally unit
//! tests inside `codegen::core_erlang::tests::{gen_server,dispatch,
//! expressions}` and `source_analysis::parser::tests::literal_tests`
//! (white-box tests of `CoreErlangGenerator` internals via `use super::*`),
//! but every one of them actually only exercises `beamtalk-repl`'s public
//! REPL-codegen entry points against public `beamtalk-core` AST types -- none
//! of them touch `CoreErlangGenerator` or other codegen internals directly.
//!
//! They can't stay unit tests now that `repl` has moved to the standalone
//! `beamtalk-repl` crate (which depends on `beamtalk-core`): a unit test
//! compiled as part of `beamtalk-core` itself bakes `--cfg test` into the
//! same compilation as the library, so `beamtalk-repl`'s build against
//! `beamtalk-core` (no `--cfg test`) and this crate's own test build become
//! two different-cfg copies of the same package -- Cargo reports that as
//! "multiple different versions of crate `beamtalk_core`" and refuses to
//! compile. An integration test avoids this: it links `beamtalk-core`
//! normally (no `--cfg test` on the library itself), matching the copy
//! `beamtalk-repl` already depends on, so there is exactly one
//! `beamtalk-core` in the graph -- see `tests/codegen_property_tests.rs` for
//! the same reasoning applied to the REPL property tests.

use beamtalk_core::ast::*;
use beamtalk_core::source_analysis::{Severity, Span, lex_with_eof, parse};
use beamtalk_repl::codegen::{
    generate_repl_expression, generate_repl_expressions, generate_repl_expressions_with_index,
    generate_test_expression,
};

/// Wraps `ExpressionStatement::bare` -- mirrors
/// `codegen::core_erlang::tests::bare` in the source these tests were
/// extracted from, kept as a local shim so the extracted bodies below are
/// otherwise unmodified copies of the original test code.
fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

/// Parses `source` and asserts there are no error/warning diagnostics
/// (lint diagnostics are ignored). Mirrors
/// `source_analysis::parser::tests::parse_ok` in the source these tests
/// were extracted from.
fn parse_ok(source: &str) -> Module {
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    let non_lint: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity != Severity::Lint)
        .collect();
    assert!(non_lint.is_empty(), "Expected no errors, got: {non_lint:?}");
    module
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:72-97 ----

#[test]
fn test_generate_repl_module_aliases_state_to_bindings() {
    // BT-57: REPL modules must alias State to Bindings for identifier lookups
    let expression = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let code = generate_repl_expression(&expression, "repl_test").expect("codegen should work");

    // Check that the module aliases State to Bindings
    assert!(
        code.contains("let State = Bindings in"),
        "REPL module should alias State to Bindings. Got:\n{code}"
    );

    // BT-2365 (ADR 0081 Phase 1): a free REPL identifier now resolves via a
    // locals maps:find against State with a runtime resolve_name fallthrough,
    // instead of a bare maps:get (which would throw {badkey,_} once workspace
    // globals are no longer eagerly injected into State).
    assert!(
        code.contains("call 'maps':'find'('x', State)"),
        "Identifier lookup should check locals (State, aliased to Bindings). Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_name'(State, 'x')"),
        "Identifier lookup should fall through to resolve_name. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:98-135 ----

#[test]
fn test_generate_repl_module_block_value_call() {
    // Test full REPL module generation for block value call
    // Expression: [:x | x + 1] value: 5
    let block = Block::new(
        vec![BlockParameter::new("x", Span::new(1, 2))],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "x",
                Span::new(5, 6),
            ))),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(9, 10))],
            is_cast: false,
            span: Span::new(5, 10),
        })],
        Span::new(0, 12),
    );

    let expression = Expression::MessageSend {
        receiver: Box::new(Expression::Block(block)),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(13, 19))]),
        arguments: vec![Expression::Literal(Literal::Integer(5), Span::new(20, 21))],
        is_cast: false,
        span: Span::new(0, 22),
    };

    let code =
        generate_repl_expression(&expression, "test_block_repl").expect("codegen should work");

    // Check basic structure
    assert!(
        code.contains("let State = Bindings in"),
        "Should alias State to Bindings"
    );
    assert!(code.contains("apply"), "Should use apply for block call");
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:136-156 ----

#[test]
fn test_generate_repl_module_returns_tuple_with_state() {
    // BT-153: REPL eval/1 should return {Result, UpdatedBindings}
    let expression = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
    let code =
        generate_repl_expression(&expression, "repl_tuple_test").expect("codegen should work");

    eprintln!("Generated code for literal 42:");
    eprintln!("{code}");

    // Check that the result is wrapped in a tuple with State
    assert!(
        code.contains("let Result ="),
        "Should bind the result to Result variable. Got:\n{code}"
    );
    assert!(
        code.contains("{Result, State}"),
        "Should return tuple {{Result, State}}. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:157-226 ----

#[test]
fn test_generate_repl_module_with_times_repeat_mutation() {
    // BT-153: REPL with mutation should return updated state
    // Expression: 5 timesRepeat: [count := count + 1]

    // Build the block: [count := count + 1]
    let count_id = Expression::Identifier(Identifier::new("count", Span::new(0, 5)));
    let one = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
    let add = Expression::MessageSend {
        receiver: Box::new(count_id.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span: Span::new(0, 15),
    };
    let assignment = Expression::Assignment {
        target: Box::new(count_id),
        value: Box::new(add),
        type_annotation: None,
        span: Span::new(0, 20),
    };
    let body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(assignment)],
        span: Span::new(0, 25),
    });

    // Build: 5 timesRepeat: [...]
    let five = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span: Span::new(2, 14),
        }]),
        arguments: vec![body],
        is_cast: false,
        span: Span::new(0, 40),
    };

    let code =
        generate_repl_expression(&times_repeat, "repl_times_test").expect("codegen should work");

    eprintln!("Generated code for 5 timesRepeat: [count := count + 1]:");
    eprintln!("{code}");

    // BT-483: For mutation-threaded loops, return {Result, State} tuple.
    // REPL extracts via element/2: let _LoopResult = element(1, Result) ...
    assert!(
        code.contains("'element'(1, Result)") && code.contains("'element'(2, Result)"),
        "Should extract Result tuple elements via element/2 for mutation loop. Got:\n{code}"
    );

    // BT-483: Loop termination should return {nil, StateAcc}
    assert!(
        code.contains("{'nil', StateAcc}"),
        "Loop should return {{'nil', StateAcc}} on termination. Got:\n{code}"
    );

    // Verify mutation threading details
    assert!(
        code.contains("letrec 'repeat'/2"),
        "Should use arity-2 repeat function (I, StateAcc). Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('count'"),
        "Should update 'count' in StateAcc (plain key in REPL mode, BT-790). Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:227-300 ----

#[test]
fn test_generate_repl_module_with_to_do_mutation() {
    use beamtalk_core::ast::BlockParameter;

    // BT-153: REPL with to:do: mutation should return updated state
    // Expression: 1 to: 5 do: [:n | total := total + n]

    // Build the block: [:n | total := total + n]
    let total_id = Expression::Identifier(Identifier::new("total", Span::new(0, 5)));
    let n_id = Expression::Identifier(Identifier::new("n", Span::new(0, 1)));
    let add = Expression::MessageSend {
        receiver: Box::new(total_id.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![n_id],
        is_cast: false,
        span: Span::new(0, 15),
    };
    let assignment = Expression::Assignment {
        target: Box::new(total_id),
        value: Box::new(add),
        type_annotation: None,
        span: Span::new(0, 20),
    };
    let body = Expression::Block(Block {
        parameters: vec![BlockParameter {
            name: "n".into(),
            span: Span::new(0, 1),
        }],
        body: vec![bare(assignment)],
        span: Span::new(0, 25),
    });

    // Build: 1 to: 5 do: [...]
    let one = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
    let five = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let to_do = Expression::MessageSend {
        receiver: Box::new(one),
        selector: MessageSelector::Keyword(vec![
            KeywordPart {
                keyword: "to:".into(),
                span: Span::new(2, 5),
            },
            KeywordPart {
                keyword: "do:".into(),
                span: Span::new(8, 11),
            },
        ]),
        arguments: vec![five, body],
        is_cast: false,
        span: Span::new(0, 40),
    };

    let code = generate_repl_expression(&to_do, "repl_to_do_test").expect("codegen should work");

    eprintln!("Generated code for 1 to: 5 do: [:n | total := total + n]:");
    eprintln!("{code}");

    // BT-483: For mutation-threaded loops, return {Result, State} tuple.
    assert!(
        code.contains("'element'(1, Result)") && code.contains("'element'(2, Result)"),
        "Should extract Result tuple elements via element/2 for mutation loop. Got:\n{code}"
    );

    // Verify to:do: mutation threading
    assert!(
        code.contains("letrec 'loop'/2"),
        "Should use arity-2 loop function (I, StateAcc). Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('total'"),
        "Should update 'total' in StateAcc (plain key in REPL mode, BT-790). Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:301-384 ----

#[test]
fn test_generate_repl_module_with_while_true_mutation() {
    // BT-181: REPL with whileTrue: mutation should read condition from StateAcc
    // Expression: [x < 5] whileTrue: [x := x + 1]

    // Build the condition: [x < 5]
    let x_id = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let five = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let compare = Expression::MessageSend {
        receiver: Box::new(x_id.clone()),
        selector: MessageSelector::Binary("<".into()),
        arguments: vec![five],
        is_cast: false,
        span: Span::new(0, 10),
    };
    let condition = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(compare)],
        span: Span::new(0, 12),
    });

    // Build the body: [x := x + 1]
    let one = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
    let add = Expression::MessageSend {
        receiver: Box::new(x_id.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span: Span::new(0, 10),
    };
    let assignment = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(add),
        type_annotation: None,
        span: Span::new(0, 15),
    };
    let body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(assignment)],
        span: Span::new(0, 17),
    });

    // Build: [x < 5] whileTrue: [x := x + 1]
    let while_true = Expression::MessageSend {
        receiver: Box::new(condition),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "whileTrue:".into(),
            span: Span::new(10, 20),
        }]),
        arguments: vec![body],
        is_cast: false,
        span: Span::new(0, 40),
    };

    let code =
        generate_repl_expression(&while_true, "repl_while_test").expect("codegen should work");

    eprintln!("Generated code for [x < 5] whileTrue: [x := x + 1]:");
    eprintln!("{code}");

    // BT-181: Condition lambda should take StateAcc parameter
    assert!(
        code.contains("fun (StateAcc) ->"),
        "Condition lambda should accept StateAcc parameter. Got:\n{code}"
    );
    // BT-181 + BT-2365: Condition should read x from StateAcc, not outer scope.
    // Lazy resolution (ADR 0081 Phase 1) now applies inside loop bodies too, so
    // the read is a `maps:find` against StateAcc with a `resolve_name`
    // fallthrough rather than a bare `maps:get` (which would `badkey` on a miss).
    assert!(
        code.contains("maps':'find'('x', StateAcc)"),
        "Condition should look up x in StateAcc via maps:find. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_name'(StateAcc, 'x')"),
        "Condition should fall through to resolve_name on a StateAcc miss. Got:\n{code}"
    );
    // BT-181: Condition should be applied with StateAcc argument
    assert!(
        code.contains("apply") && code.contains("(StateAcc)"),
        "Condition should be applied with StateAcc argument. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:385-431 ----

#[test]
fn test_repl_multi_stmt_times_repeat_intermediate() {
    // BT-790: `x := 1. 5 timesRepeat: [x := x + 1]. x` should return 6.
    // The loop is in intermediate (non-last) position — its StateAcc must be threaded
    // to the final `x` lookup, not discarded.
    let src = "x := 1. 5 timesRepeat: [x := x + 1]. x";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_repl_expressions(
        &module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect::<Vec<_>>(),
        "repl_multi_times_test",
    )
    .expect("codegen should work");

    eprintln!("Generated code for `x := 1. 5 timesRepeat: [x := x + 1]. x`:");
    eprintln!("{code}");

    // The loop StateAcc must be extracted and threaded: element(2, _R2) → StateN
    assert!(
        code.contains("'element'(2, _R2)"),
        "Loop StateAcc must be extracted via element(2, _R2). Got:\n{code}"
    );

    // The final `x` lookup must use the updated state (StateN), not a stale State1
    // StateN is whatever state comes after threading the loop result
    assert!(
        !code.contains("maps':'get'('x', State1)"),
        "Final x lookup must not use stale State1. Got:\n{code}"
    );

    // The return tuple must NOT use element/2 unwrapping for the last (plain identifier) expr
    assert!(
        !code.contains("'element'(1, Result)"),
        "Final expr is a plain identifier — must not apply element/2 to Result. Got:\n{code}"
    );

    // The return tuple should be plain {Result, StateN}
    assert!(
        code.contains("{Result, State"),
        "Return tuple should be {{Result, StateN}}. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:432-470 ----

#[test]
fn test_repl_multi_stmt_while_true_intermediate() {
    // BT-790: `x := 0. [x < 3] whileTrue: [x := x + 1]. x` — whileTrue: in intermediate
    // position must thread its StateAcc so the final `x` lookup sees the updated value.
    let src = "x := 0. [x < 3] whileTrue: [x := x + 1]. x";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_repl_expressions(
        &module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect::<Vec<_>>(),
        "repl_multi_while_test",
    )
    .expect("codegen should work");

    eprintln!("Generated code for `x := 0. [x < 3] whileTrue: [x := x + 1]. x`:");
    eprintln!("{code}");

    // The loop StateAcc must be extracted from the intermediate result
    assert!(
        code.contains("'element'(2, _R2)"),
        "Loop StateAcc must be extracted via element(2, _R2). Got:\n{code}"
    );

    // The final x lookup must NOT read from State1 (which has x=0 from init only)
    assert!(
        !code.contains("maps':'get'('x', State1)"),
        "Final x lookup must not use stale State1. Got:\n{code}"
    );

    // Return tuple must be plain {Result, StateN}, not element/2 wrapped
    assert!(
        !code.contains("'element'(1, Result)"),
        "Final expr is a plain identifier — must not apply element/2 to Result. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:471-516 ----

#[test]
fn test_repl_multi_stmt_assignment_then_loop_then_plain() {
    // BT-790: Regression — multiple intermediate expressions including assignment + loop.
    // `count := 0. 3 timesRepeat: [count := count + 1]. count` generates state chain:
    //   State → State1 (from assignment) → State2 (from loop StateAcc) → {Result, State2}
    let src = "count := 0. 3 timesRepeat: [count := count + 1]. count";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_repl_expressions(
        &module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect::<Vec<_>>(),
        "repl_multi_chain_test",
    )
    .expect("codegen should work");

    eprintln!("Generated code for `count := 0. 3 timesRepeat: [count := count + 1]. count`:");
    eprintln!("{code}");

    // Assignment creates State1
    assert!(
        code.contains("State1 = call 'maps':'put'('count'"),
        "Assignment should create State1. Got:\n{code}"
    );

    // Loop in intermediate position: StateAcc extracted as State2
    assert!(
        code.contains("'element'(2, _R2)"),
        "Loop StateAcc must be extracted via element(2, _R2). Got:\n{code}"
    );

    // Final count lookup must not use stale State1
    assert!(
        !code.contains("maps':'get'('count', State1)"),
        "Final count lookup must not use stale State1. Got:\n{code}"
    );

    // No element/2 unwrapping on the final return
    assert!(
        !code.contains("'element'(1, Result)"),
        "Final plain identifier expr must not use element/2. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:517-557 ----

#[test]
fn test_generate_repl_module_with_arithmetic() {
    // BT-57: Verify complex expressions with variable references work
    // Expression: x + 1
    let x_ref = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let one = Expression::Literal(Literal::Integer(1), Span::new(4, 5));
    let expression = Expression::MessageSend {
        receiver: Box::new(x_ref),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span: Span::new(0, 5),
    };

    let code = generate_repl_expression(&expression, "repl_arith").expect("codegen should work");

    // Check State aliasing
    assert!(
        code.contains("let State = Bindings in"),
        "REPL module should alias State to Bindings. Got:\n{code}"
    );

    // Check that x lookup works through State. BT-2365 (ADR 0081 Phase 1): a free
    // REPL identifier resolves via a locals maps:find with a resolve_name
    // fallthrough rather than a bare maps:get.
    assert!(
        code.contains("call 'maps':'find'('x', State)"),
        "Variable x should be looked up from State (locals). Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_name'(State, 'x')"),
        "Variable x lookup should fall through to resolve_name. Got:\n{code}"
    );

    // Check the arithmetic operation
    assert!(
        code.contains("call 'erlang':'+'("),
        "Should have addition operation. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:558-641 ----

#[test]
fn test_generate_repl_multi_stmt_times_repeat_then_read() {
    // BT-790: x := 1. 5 timesRepeat: [x := x + 1]. x
    // The loop in intermediate position must thread its updated state to the final `x` read.

    let span = Span::new(0, 1);

    // x := 1
    let x_id = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let assign_x = Expression::Assignment {
        target: Box::new(x_id.clone()),
        value: Box::new(one),
        type_annotation: None,
        span,
    };

    // 5 timesRepeat: [x := x + 1]
    let x_id2 = Expression::Identifier(Identifier::new("x", span));
    let one2 = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_id2.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one2],
        is_cast: false,
        span,
    };
    let loop_assign = Expression::Assignment {
        target: Box::new(x_id2),
        value: Box::new(add),
        type_annotation: None,
        span,
    };
    let loop_body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(loop_assign)],
        span,
    });
    let five = Expression::Literal(Literal::Integer(5), span);
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span,
        }]),
        arguments: vec![loop_body],
        is_cast: false,
        span,
    };

    // x (final read)
    let x_read = Expression::Identifier(Identifier::new("x", span));

    let expressions = vec![assign_x, times_repeat, x_read];
    let code = generate_repl_expressions(&expressions, "repl_multi_loop_test")
        .expect("codegen should work");

    eprintln!("Generated code for x := 1. 5 timesRepeat: [x := x + 1]. x:");
    eprintln!("{code}");

    // BT-790: The loop in intermediate position must have its StateAcc extracted
    assert!(
        code.contains("call 'erlang':'element'(2,"),
        "Should extract StateAcc from loop result in intermediate position. Got:\n{code}"
    );

    // BT-790: The final x read must use a state that was updated by the loop
    // (not the original State or State1 from the x := 1 assignment)
    assert!(
        code.contains("let Result ="),
        "Should bind final result. Got:\n{code}"
    );

    // The overall structure: should have State1 from assignment, then state extraction from loop
    assert!(
        code.contains("maps':'put'('x'"),
        "Should have maps:put for x assignment. Got:\n{code}"
    );
    assert!(
        code.contains("letrec 'repeat'/2"),
        "Should use arity-2 repeat function for mutation loop. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:642-733 ----

#[test]
fn test_generate_repl_multi_stmt_while_true_then_read() {
    // BT-790: x := 0. [x < 5] whileTrue: [x := x + 1]. x
    // whileTrue: in intermediate position must thread state to the final x read.

    let span = Span::new(0, 1);

    // x := 0
    let x_id = Expression::Identifier(Identifier::new("x", span));
    let zero = Expression::Literal(Literal::Integer(0), span);
    let assign_x = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(zero),
        type_annotation: None,
        span,
    };

    // [x < 5] whileTrue: [x := x + 1]
    let x_cond = Expression::Identifier(Identifier::new("x", span));
    let five = Expression::Literal(Literal::Integer(5), span);
    let cmp = Expression::MessageSend {
        receiver: Box::new(x_cond),
        selector: MessageSelector::Binary("<".into()),
        arguments: vec![five],
        is_cast: false,
        span,
    };
    let condition = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(cmp)],
        span,
    });
    let x_body = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_body.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span,
    };
    let loop_assign = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("x", span))),
        value: Box::new(add),
        type_annotation: None,
        span,
    };
    let loop_body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(loop_assign)],
        span,
    });
    let while_true = Expression::MessageSend {
        receiver: Box::new(condition),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "whileTrue:".into(),
            span,
        }]),
        arguments: vec![loop_body],
        is_cast: false,
        span,
    };

    // x (final read)
    let x_read = Expression::Identifier(Identifier::new("x", span));

    let expressions = vec![assign_x, while_true, x_read];
    let code = generate_repl_expressions(&expressions, "repl_multi_while_test")
        .expect("codegen should work");

    eprintln!("Generated code for x := 0. [x < 5] whileTrue: [x := x + 1]. x:");
    eprintln!("{code}");

    // BT-790: The loop in intermediate position must have its StateAcc extracted
    assert!(
        code.contains("call 'erlang':'element'(2,"),
        "Should extract StateAcc from whileTrue: loop result in intermediate position. Got:\n{code}"
    );

    // Should bind the final result
    assert!(
        code.contains("let Result ="),
        "Should bind final result. Got:\n{code}"
    );

    // Should use whileTrue: mutation-threaded structure
    assert!(
        code.contains("letrec 'while'/1"),
        "Should use whileTrue: loop function. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:734-812 ----

#[test]
fn test_generate_repl_multi_stmt_loop_does_not_corrupt_final_expr() {
    // BT-790: repl_loop_mutated must be reset before the final expression.
    // x := 1. 5 timesRepeat: [x := x + 1]. 42
    // The final expression `42` is not a loop, so it must NOT use element/2 unwrapping.

    let span = Span::new(0, 1);

    // x := 1
    let x_id = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let assign_x = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(one),
        type_annotation: None,
        span,
    };

    // 5 timesRepeat: [x := x + 1]
    let x_id2 = Expression::Identifier(Identifier::new("x", span));
    let one2 = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_id2.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one2],
        is_cast: false,
        span,
    };
    let loop_assign = Expression::Assignment {
        target: Box::new(x_id2),
        value: Box::new(add),
        type_annotation: None,
        span,
    };
    let loop_body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(loop_assign)],
        span,
    });
    let five = Expression::Literal(Literal::Integer(5), span);
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span,
        }]),
        arguments: vec![loop_body],
        is_cast: false,
        span,
    };

    // 42 (final literal - not a loop)
    let forty_two = Expression::Literal(Literal::Integer(42), span);

    let expressions = vec![assign_x, times_repeat, forty_two];
    let code = generate_repl_expressions(&expressions, "repl_multi_loop_no_corrupt_test")
        .expect("codegen should work");

    eprintln!("Generated code for x := 1. 5 timesRepeat: [x := x + 1]. 42:");
    eprintln!("{code}");

    // BT-790: Final expression is a literal — must NOT apply element/2 unwrapping on Result
    // The return tuple must be {Result, StateN} where Result = 42 (not extracted from a tuple)
    assert!(
        !code.contains("'element'(1, Result)"),
        "Final non-loop expression must NOT apply element(1, Result) unwrapping. Got:\n{code}"
    );
    assert!(
        !code.contains("'element'(2, Result)"),
        "Final non-loop expression must NOT apply element(2, Result) unwrapping. Got:\n{code}"
    );

    // The intermediate loop SHOULD still extract state (element(2, _R2))
    assert!(
        code.contains("call 'erlang':'element'(2,"),
        "Intermediate loop must extract StateAcc. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:813-893 ----

#[test]
fn test_repl_loop_mutations_accumulate_plain_key() {
    // BT-800: In REPL mode, loop writes must use plain key so reads accumulate.
    // Expression: 5 timesRepeat: [x := x + 1]
    // Write path must use 'x' not '__local__x' so that each iteration reads the
    // value written by the previous iteration from StateAcc.

    let span = Span::new(0, 1);

    let x_id = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_id.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span,
    };
    let assignment = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(add),
        type_annotation: None,
        span,
    };
    let body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(assignment)],
        span,
    });
    let five = Expression::Literal(Literal::Integer(5), span);
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span,
        }]),
        arguments: vec![body],
        is_cast: false,
        span,
    };

    let code = generate_repl_expression(&times_repeat, "bt800_test").expect("codegen should work");

    eprintln!("BT-800: Generated code for 5 timesRepeat: [x := x + 1]:");
    eprintln!("{code}");

    // BT-800: REPL mode must use plain key 'x' (not '__local__x') so reads match writes.
    assert!(
        code.contains("maps':'put'('x'"),
        "BT-800: REPL write must use plain key 'x', not '__local__x'. Got:\n{code}"
    );
    assert!(
        !code.contains("__local__x"),
        "BT-800: REPL mode must never use __local__ prefix for x. Got:\n{code}"
    );

    // BT-800 + BT-2365: Reads inside loop body must use StateAcc (not State) so
    // they get the accumulated value from the previous iteration. Lazy
    // resolution (ADR 0081 Phase 1) now applies inside loop bodies too, so the
    // read is a `maps:find` against StateAcc with a `resolve_name` fallthrough
    // rather than a bare `maps:get` (which would `badkey` on a miss).
    assert!(
        code.contains("maps':'find'('x', StateAcc)"),
        "BT-800: Read inside loop must look up x in StateAcc via maps:find. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_name'(StateAcc, 'x')"),
        "BT-800: Read inside loop must fall through to resolve_name on a StateAcc miss. Got:\n{code}"
    );

    // BT-800: Loop must thread state correctly (arity-2 letrec, returns {nil, StateAcc}).
    assert!(
        code.contains("letrec 'repeat'/2"),
        "BT-800: Must use arity-2 repeat for state threading. Got:\n{code}"
    );
    assert!(
        code.contains("{'nil', StateAcc}"),
        "BT-800: Loop must return {{nil, StateAcc}} so caller can extract updated state. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:894-985 ----

#[test]
fn test_repl_multi_stmt_loop_accumulates_from_zero() {
    // BT-800: x := 0. 5 timesRepeat: [x := x + 1]. x
    // Acceptance criteria: starting from zero, result must be 5.
    // Validates that the multi-statement path threads state correctly through the loop.

    let span = Span::new(0, 1);

    // x := 0
    let x_id = Expression::Identifier(Identifier::new("x", span));
    let zero = Expression::Literal(Literal::Integer(0), span);
    let assign_x = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(zero),
        type_annotation: None,
        span,
    };

    // 5 timesRepeat: [x := x + 1]
    let x_id2 = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_id2.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span,
    };
    let loop_assign = Expression::Assignment {
        target: Box::new(x_id2),
        value: Box::new(add),
        type_annotation: None,
        span,
    };
    let loop_body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(loop_assign)],
        span,
    });
    let five = Expression::Literal(Literal::Integer(5), span);
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span,
        }]),
        arguments: vec![loop_body],
        is_cast: false,
        span,
    };

    // x (final read)
    let x_read = Expression::Identifier(Identifier::new("x", span));

    let expressions = vec![assign_x, times_repeat, x_read];
    let code =
        generate_repl_expressions(&expressions, "bt800_zero_test").expect("codegen should work");

    eprintln!("BT-800: Generated code for x := 0. 5 timesRepeat: [x := x + 1]. x:");
    eprintln!("{code}");

    // BT-800: Loop write must use plain key (no __local__ prefix in REPL mode)
    assert!(
        !code.contains("__local__"),
        "BT-800: REPL mode must never use __local__ prefix. Got:\n{code}"
    );

    // BT-800: The loop must be applied with the state containing x=0
    assert!(
        code.contains("apply 'repeat'/2 (1, State1)"),
        "BT-800: Loop must start with State1 (after x := 0 binding). Got:\n{code}"
    );

    // BT-800: The intermediate loop result must be unpacked to thread state forward
    assert!(
        code.contains("call 'erlang':'element'(2,"),
        "BT-800: Must extract updated StateAcc from loop result. Got:\n{code}"
    );

    // BT-800: Final read of x must use the state produced by the loop (State2+).
    // BT-2365 (ADR 0081 Phase 1): the post-loop free-identifier read resolves via
    // a locals maps:find (with a resolve_name fallthrough) against State2.
    assert!(
        code.contains("maps':'find'('x', State2)"),
        "BT-800: Final x read must use loop-updated state (State2). Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_name'(State2, 'x')"),
        "BT-2365: Final x read must fall through to resolve_name. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:1589-1671 ----

#[test]
fn test_class_method_call_generation() {
    // BT-215: Test that ClassReference message sends generate appropriate code
    // BT-490 / ADR 0019: All classes (including Transcript, Beamtalk, Workspace)
    //         use standard class dispatch via class_send
    use beamtalk_core::ast::{Expression, Identifier, MessageSelector};
    use beamtalk_core::source_analysis::Span;

    // Test 1: Beamtalk class uses standard class_send dispatch (no special case)
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Beamtalk", Span::new(0, 8)),
            span: Span::new(0, 8),
            package: None,
        }),
        selector: MessageSelector::Unary("allClasses".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 20),
    };

    let code = generate_repl_expression(&expr, "repl_eval").expect("codegen should succeed");

    // ADR 0019: Beamtalk should check bindings first, then class_send fallback
    assert!(
        code.contains("maps':'find") && code.contains("class_send"),
        "Beamtalk should check bindings then class_send. Got:\n{code}"
    );
    assert!(
        !code.contains("persistent_term"),
        "Beamtalk should NOT use persistent_term. Got:\n{code}"
    );

    // Test 2: Non-binding class (Point) dispatches via class_send in REPL
    let expr2 = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Point", Span::new(0, 5)),
            span: Span::new(0, 5),
            package: None,
        }),
        selector: MessageSelector::Unary("new".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 10),
    };

    let code2 = generate_repl_expression(&expr2, "repl_eval2")
        .expect("codegen should succeed for non-binding class");

    // BT-411/ADR 0019: In REPL, all class references check bindings then class_send
    assert!(
        code2.contains("maps':'find") && code2.contains("class_send"),
        "Non-binding class should check bindings then class_send. Got:\n{code2}"
    );
    assert!(
        !code2.contains("persistent_term"),
        "Non-binding class should NOT use persistent_term. Got:\n{code2}"
    );

    // Test 3: ClassReference spawn in REPL uses generate_actor_spawn with registry
    let expr3 = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("InitCounter", Span::new(0, 11)),
            span: Span::new(0, 11),
            package: None,
        }),
        selector: MessageSelector::Unary("spawn".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 17),
    };

    let code3 =
        generate_repl_expression(&expr3, "repl_eval3").expect("codegen should succeed for spawn");

    // Spawn on ClassReference uses generate_actor_spawn which calls
    // Module:spawn() with REPL registry integration
    assert!(
        code3.contains("'initcounter':'spawn'") || code3.contains("register_spawned"),
        "REPL spawn should use direct module spawn (with optional registry). Got:\n{code3}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:2107-2134 ----

#[test]
fn test_generate_repl_list_reject() {
    // BT-408: reject: must generate valid Core Erlang with properly bound wrapper fun
    // The wrapper fun must be bound via `let` — not inlined in the call args,
    // because Core Erlang lambdas don't use `end` and can't be inlined in calls.
    let src = "#(1, 2, 3, 4, 5) reject: [:x | x > 2]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let expr = &module.expressions[0].expression;
    let code = generate_repl_expression(expr, "test_reject_repl").expect("codegen should work");

    // Wrapper fun must be bound to a variable, not inlined in filter call
    assert!(
        code.contains("call 'lists':'filter'("),
        "Should use lists:filter. Got:\n{code}"
    );
    assert!(
        code.contains("call 'erlang':'not'("),
        "Should negate predicate. Got:\n{code}"
    );
    // Verify the fun is let-bound (not inlined) — the filter call arg must be a temp var
    // e.g. "call 'lists':'filter'(_temp4," not "call 'lists':'filter'(fun (X)"
    assert!(
        !code.contains("'filter'(fun"),
        "Wrapper fun must be let-bound, not inlined in filter call. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/gen_server.rs:4662-4806 ----

#[test]
#[allow(clippy::too_many_lines)]
fn test_repl_destructure_mutation_threaded_rhs_unwraps_element() {
    // BT-1283: When the RHS of a REPL destructuring assignment is a mutation-threaded
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

    let span = Span::new(0, 1);

    // Build: x := x + 1. #(item, x)  (block body with mutation + list return value)
    let x_mut = {
        let x_id = Expression::Identifier(Identifier::new("x", span));
        let one = Expression::Literal(Literal::Integer(1), span);
        let add = Expression::MessageSend {
            receiver: Box::new(x_id.clone()),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![one],
            is_cast: false,
            span,
        };
        Expression::Assignment {
            target: Box::new(x_id),
            value: Box::new(add),
            type_annotation: None,
            span,
        }
    };
    let item_id = Expression::Identifier(Identifier::new("item", span));
    let x_id2 = Expression::Identifier(Identifier::new("x", span));
    let list_result = Expression::ListLiteral {
        elements: vec![item_id, x_id2],
        tail: None,
        span,
    };
    let inject_block = Expression::Block(Block {
        parameters: vec![
            BlockParameter::new("acc", span),
            BlockParameter::new("item", span),
        ],
        body: vec![bare(x_mut), bare(list_result)],
        span,
    });

    // Build: #(10, 20) inject: #(0, 0) into: <inject_block>
    let receiver = Expression::ListLiteral {
        elements: vec![
            Expression::Literal(Literal::Integer(10), span),
            Expression::Literal(Literal::Integer(20), span),
        ],
        tail: None,
        span,
    };
    let initial_acc = Expression::ListLiteral {
        elements: vec![
            Expression::Literal(Literal::Integer(0), span),
            Expression::Literal(Literal::Integer(0), span),
        ],
        tail: None,
        span,
    };
    let inject_into = Expression::MessageSend {
        receiver: Box::new(receiver),
        selector: MessageSelector::Keyword(vec![
            KeywordPart {
                keyword: "inject:".into(),
                span,
            },
            KeywordPart {
                keyword: "into:".into(),
                span,
            },
        ]),
        arguments: vec![initial_acc, inject_block],
        is_cast: false,
        span,
    };

    // Build: #(a, b) := <inject_into>
    let pattern = Pattern::Array {
        elements: vec![
            Pattern::Variable(Identifier::new("a", span)),
            Pattern::Variable(Identifier::new("b", span)),
        ],
        list_syntax: true,
        rest: None,
        span,
    };
    let destructure = Expression::DestructureAssignment {
        pattern,
        value: Box::new(inject_into),
        span,
    };

    let code = generate_repl_expression(&destructure, "bt1283_mutation_threaded_test")
        .expect("codegen should succeed");

    eprintln!("BT-1283: Generated code for #(a, b) := #(10,20) inject: #(0,0) into: [...]:");
    eprintln!("{code}");

    // The RHS is mutation-threaded, so it returns {FinalAcc, StateAcc}.
    // element(1, Rhs) extracts FinalAcc; element(2, Rhs) advances REPL state.
    assert!(
        code.contains("call 'erlang':'element'(1,"),
        "Must unwrap element(1,) from mutation-threaded RHS. Got:\n{code}"
    );
    assert!(
        code.contains("call 'erlang':'element'(2,"),
        "Must extract StateAcc via element(2,) from mutation-threaded RHS. Got:\n{code}"
    );

    // Pattern extraction must use the unwrapped _RhsVal (not the raw {Acc,State} _Rhs).
    // The 'at:' dispatch must receive the _RhsVal variable (fresh_temp_var("RhsVal") prefix),
    // not _Rhs (the raw mutation-threaded result holding {FinalAcc, StateAcc}).
    assert!(
        code.contains("'send'(_RhsVal"),
        "Pattern extraction must use the unwrapped _RhsVal, not the raw _Rhs tuple. Got:\n{code}"
    );

    // The pattern variables must be persisted to the REPL state map.
    assert!(
        code.contains("call 'maps':'put'('a'"),
        "Must persist 'a' to REPL state map. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'put'('b'"),
        "Must persist 'b' to REPL state map. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:1025-1078 ----

#[test]
fn test_cascade_repl_expression() {
    // Test cascade in a full REPL module context
    // x negated; abs
    let x_ident = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let first_msg = Expression::MessageSend {
        receiver: Box::new(x_ident),
        selector: MessageSelector::Unary("negated".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 9),
    };

    let cascade = Expression::Cascade {
        receiver: Box::new(first_msg),
        messages: vec![CascadeMessage::new(
            MessageSelector::Unary("abs".into()),
            vec![],
            Span::new(11, 14),
        )],
        span: Span::new(0, 14),
    };

    let code = generate_repl_expression(&cascade, "test_cascade").expect("codegen should work");

    // Should have module structure
    assert!(
        code.contains("module 'test_cascade' ['eval'/1]"),
        "Should have module header. Got:\n{code}"
    );

    // Should bind the underlying receiver once. BT-2365 (ADR 0081 Phase 1): a
    // free REPL identifier now resolves via a locals maps:find with a runtime
    // resolve_name fallthrough rather than a bare maps:get.
    assert!(
        code.contains("let _Receiver1 = case call 'maps':'find'('x', State) of"),
        "Should bind receiver x via locals find. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_name'(State, 'x')"),
        "Should fall through to resolve_name for free identifier x. Got:\n{code}"
    );

    // Should send both messages
    assert!(
        code.contains("'negated'"),
        "Should have first message negated. Got:\n{code}"
    );
    assert!(
        code.contains("'abs'"),
        "Should have second message abs. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:1232-1283 ----

#[test]
fn test_standalone_class_reference_uses_dynamic_module_name() {
    // BT-215: Test that standalone ClassReference uses module_name/1 dynamically
    // Review comment: Should match generate_beamtalk_class_named pattern (lines 915-922)
    use beamtalk_core::ast::{Expression, Identifier, Module};
    use beamtalk_core::source_analysis::Span;

    // Create expression: Point (standalone class reference)
    let expr = Expression::ClassReference {
        name: Identifier::new("Point", Span::new(0, 5)),
        span: Span::new(0, 5),
        package: None,
    };

    let module = Module {
        type_aliases: Vec::new(),
        expressions: vec![bare(expr)],
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 5),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_repl_expression(&module.expressions[0].expression, "repl_eval")
        .expect("codegen should succeed");

    // BT-2365 (ADR 0081 Phase 1): an unqualified REPL class reference checks the
    // session locals map first (so a local shadows the class), then delegates to
    // the shared runtime resolver. The class object construction and dynamic
    // module_name lookup now live in beamtalk_workspace:resolve_class_reference/2.

    // Should check the session locals map first (shadowing support).
    assert!(
        code.contains("call 'maps':'find'('Point', "),
        "Should check locals map for the class name first. Got:\n{code}"
    );

    // Should delegate the miss path to the shared runtime resolver.
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_class_reference'("),
        "Should delegate to resolve_class_reference on a locals miss. Got:\n{code}"
    );

    // Should pass the class name as an atom to the resolver.
    assert!(
        code.contains("'resolve_class_reference'(") && code.contains("'Point')"),
        "Should pass the class name atom to the resolver. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:1284-1330 ----

#[test]
fn test_standalone_class_reference_validates_undefined_classes() {
    // BT-215, BT-597: Test that standalone ClassReference raises class_not_found error for undefined classes
    use beamtalk_core::ast::{Expression, Identifier, Module};
    use beamtalk_core::source_analysis::Span;

    // Create expression: NonExistentClass (standalone class reference)
    let expr = Expression::ClassReference {
        name: Identifier::new("NonExistentClass", Span::new(0, 16)),
        span: Span::new(0, 16),
        package: None,
    };

    let module = Module {
        type_aliases: Vec::new(),
        expressions: vec![bare(expr)],
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 16),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_repl_expression(&module.expressions[0].expression, "repl_eval")
        .expect("codegen should succeed");

    // BT-2365 (ADR 0081 Phase 1): undefined-class validation now happens in the
    // runtime resolver (beamtalk_workspace:resolve_class_reference/2), which
    // raises the same class_not_found error. The REPL codegen emits a locals
    // check then delegates to that resolver.

    // Should check the session locals map first (shadowing support).
    assert!(
        code.contains("call 'maps':'find'('NonExistentClass', "),
        "Should check locals map for the class name first. Got:\n{code}"
    );

    // Should delegate to the shared resolver, which raises class_not_found for
    // a genuinely unknown class.
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_class_reference'(")
            && code.contains("'NonExistentClass')"),
        "Should delegate undefined-class handling to resolve_class_reference. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:2453-2486 ----

#[test]
fn test_repl_expression_spawn_uses_class_module_index() {
    // When a REPL expression like `Counter spawn` is compiled in a workspace
    // with package "getting_started", the class_module_index must be consulted
    // so the generated code calls 'bt@getting_started@counter':'spawn'()
    // instead of the heuristic fallback 'bt@counter':'spawn'().
    let src = "Counter spawn";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
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

    assert!(
        code.contains("'bt@getting_started@counter':'spawn'"),
        "spawn must use package-qualified module from class_module_index. Got:\n{code}"
    );
    assert!(
        !code.contains("'bt@counter':'spawn'"),
        "Heuristic module name must NOT appear when class_module_index is provided. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:2487-2507 ----

#[test]
fn test_repl_expression_spawn_without_index_uses_heuristic() {
    // Without class_module_index, spawn falls back to the heuristic bt@ prefix.
    let src = "Counter spawn";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();

    let code =
        generate_repl_expressions(&expressions, "repl_test_mod").expect("codegen should work");

    assert!(
        code.contains("'bt@counter':'spawn'"),
        "Without class_module_index, spawn should use heuristic bt@ prefix. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:2508-2538 ----

#[test]
fn test_repl_expression_spawn_with_args_uses_class_module_index() {
    // `Counter spawnWith: #{ value: 10 }` must also use class_module_index.
    let src = "Counter spawnWith: #{ value: 10 }";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
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

    assert!(
        code.contains("'bt@getting_started@counter':'spawn'"),
        "spawnWith: must use package-qualified module from class_module_index. Got:\n{code}"
    );
    assert!(
        !code.contains("'bt@counter':'spawn'"),
        "Heuristic module name must NOT appear when class_module_index is provided. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/dispatch.rs:3124-3164 ----

#[test]
fn test_qualified_class_reference_standalone() {
    // ADR 0070 Phase 2: `json@Parser` as a standalone expression should use
    // the class registry with the short name 'Parser' and produce the
    // display name 'json@Parser class' in the class object tuple.
    use beamtalk_core::ast::{Expression, Identifier, Module};
    use beamtalk_core::source_analysis::Span;

    let expr = Expression::ClassReference {
        name: Identifier::new("Parser", Span::new(5, 11)),
        package: Some(Identifier::new("json", Span::new(0, 4))),
        span: Span::new(0, 11),
    };

    let module = Module {
        type_aliases: Vec::new(),
        expressions: vec![bare(expr)],
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 11),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_repl_expression(&module.expressions[0].expression, "repl_eval")
        .expect("codegen should succeed");

    // Registry lookup uses the short class name
    assert!(
        code.contains("call 'beamtalk_class_registry':'whereis_class'('Parser')"),
        "Should look up 'Parser' (short name) in class registry. Got:\n{code}"
    );

    // Display name in the class object tuple includes package qualifier
    assert!(
        code.contains("'json@Parser class'"),
        "Class object tuple should use 'json@Parser class' as display name. Got:\n{code}"
    );
}

// ---- extracted from crates/beamtalk-core/src/codegen/core_erlang/tests/expressions.rs (4 string-interpolation tests) ----

#[test]
fn test_string_interpolation_simple_variable() {
    // "Hello, {name}!" — variable interpolation
    let segments = vec![
        StringSegment::Literal("Hello, ".into()),
        StringSegment::Interpolation(Expression::Identifier(Identifier::new(
            "name",
            Span::new(8, 12),
        ))),
        StringSegment::Literal("!".into()),
    ];
    let expression = Expression::StringInterpolation {
        segments,
        span: Span::new(0, 15),
    };
    let code = generate_test_expression(&expression, "test_interp").expect("codegen should work");
    // Should dispatch displayString via beamtalk_message_dispatch
    assert!(
        code.contains("'displayString'"),
        "Should dispatch displayString. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'"),
        "Should use beamtalk_message_dispatch for dispatch. Got:\n{code}"
    );
    // Binary construction with byte segments and binary variable
    assert!(
        code.contains("#<"),
        "Should contain byte segments for literal parts. Got:\n{code}"
    );
    assert!(
        code.contains("('all',8,'binary',['unsigned'|['big']])"),
        "Should contain binary variable segment. Got:\n{code}"
    );
}


#[test]
fn test_string_interpolation_multiple_expressions() {
    // "a{x}b{y}c" — multiple expression segments
    let segments = vec![
        StringSegment::Literal("a".into()),
        StringSegment::Interpolation(Expression::Identifier(Identifier::new(
            "x",
            Span::new(2, 3),
        ))),
        StringSegment::Literal("b".into()),
        StringSegment::Interpolation(Expression::Identifier(Identifier::new(
            "y",
            Span::new(5, 6),
        ))),
        StringSegment::Literal("c".into()),
    ];
    let expression = Expression::StringInterpolation {
        segments,
        span: Span::new(0, 8),
    };
    let code = generate_test_expression(&expression, "test_multi").expect("codegen should work");
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
    let segments = vec![StringSegment::Interpolation(Expression::Identifier(
        Identifier::new("name", Span::new(1, 5)),
    ))];
    let expression = Expression::StringInterpolation {
        segments,
        span: Span::new(0, 6),
    };
    let code = generate_test_expression(&expression, "test_bare").expect("codegen should work");
    assert!(
        code.contains("'displayString'"),
        "Should dispatch displayString even for bare expression. Got:\n{code}"
    );
    // Binary should contain only the variable segment
    assert!(
        code.contains("('all',8,'binary',['unsigned'|['big']])"),
        "Should contain binary variable segment. Got:\n{code}"
    );
}


#[test]
fn test_string_interpolation_integer_expression() {
    // "{42}" — integer literal in interpolation
    let segments = vec![StringSegment::Interpolation(Expression::Literal(
        Literal::Integer(42),
        Span::new(1, 3),
    ))];
    let expression = Expression::StringInterpolation {
        segments,
        span: Span::new(0, 4),
    };
    let code = generate_test_expression(&expression, "test_int").expect("codegen should work");
    // Should dispatch displayString on the integer
    assert!(
        code.contains("'displayString'"),
        "Should dispatch displayString on integer. Got:\n{code}"
    );
}


// ---- extracted from crates/beamtalk-core/src/source_analysis/parser/tests/literal_tests.rs:921-931 ----

#[test]
fn codegen_simple_match() {
    let module = parse_ok("42 match: [_ -> 99]");
    let expr = &module.expressions[0].expression;
    let result = beamtalk_repl::codegen::generate_test_expression(expr, "test_match");
    assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
    let code = result.unwrap();
    eprintln!("Generated code:\n{code}");
    assert!(code.contains("case"), "Expected case expression in: {code}");
}

// ---- extracted from crates/beamtalk-core/src/source_analysis/parser/tests/literal_tests.rs:932-942 ----

#[test]
fn codegen_match_with_arms() {
    let module = parse_ok("1 match: [1 -> \"one\"; 2 -> \"two\"; _ -> \"other\"]");
    let expr = &module.expressions[0].expression;
    let result = beamtalk_repl::codegen::generate_test_expression(expr, "test_match");
    assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
    let code = result.unwrap();
    eprintln!("Generated code:\n{code}");
    assert!(code.contains("case"), "Expected case expression in: {code}");
}

// ---- extracted from crates/beamtalk-core/src/source_analysis/parser/tests/literal_tests.rs:943-950 ----

#[test]
fn codegen_empty_match_errors() {
    let module = parse_ok("42 match: []");
    let expr = &module.expressions[0].expression;
    let result = beamtalk_repl::codegen::generate_test_expression(expr, "test_match");
    assert!(result.is_err(), "Empty match should fail codegen");
}

