// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for `gen_server` module and class-level Core Erlang code generation.
//!
//! Covers empty-module codegen, REPL workspace module generation, class
//! registration, class methods, value-subclass auto-accessors, class
//! hierarchy actor/value classification, and type-annotation writeback.

use super::*;

#[test]
fn test_generate_empty_module() {
    let module = Module::new(Vec::new(), Span::new(0, 0));
    let result = generate(&module);
    assert!(result.is_ok());
    let code = result.unwrap();
    assert!(code.contains("module 'bt_module'"));
    assert!(code.contains("attributes ['behaviour' = ['gen_server']]"));
}

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

    // Check that identifier lookup uses maps:get with State
    assert!(
        code.contains("call 'maps':'get'('x', State)"),
        "Identifier lookup should use State (aliased to Bindings). Got:\n{code}"
    );
}

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

#[test]
fn test_generate_repl_module_with_to_do_mutation() {
    use crate::ast::BlockParameter;

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
    // BT-181: Condition should read x from StateAcc, not outer scope
    assert!(
        code.contains("maps':'get'('x', StateAcc)"),
        "Condition should read x from StateAcc. Got:\n{code}"
    );
    // BT-181: Condition should be applied with StateAcc argument
    assert!(
        code.contains("apply") && code.contains("(StateAcc)"),
        "Condition should be applied with StateAcc argument. Got:\n{code}"
    );
}

#[test]
fn test_repl_multi_stmt_times_repeat_intermediate() {
    // BT-790: `x := 1. 5 timesRepeat: [x := x + 1]. x` should return 6.
    // The loop is in intermediate (non-last) position — its StateAcc must be threaded
    // to the final `x` lookup, not discarded.
    let src = "x := 1. 5 timesRepeat: [x := x + 1]. x";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
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

#[test]
fn test_repl_multi_stmt_while_true_intermediate() {
    // BT-790: `x := 0. [x < 3] whileTrue: [x := x + 1]. x` — whileTrue: in intermediate
    // position must thread its StateAcc so the final `x` lookup sees the updated value.
    let src = "x := 0. [x < 3] whileTrue: [x := x + 1]. x";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
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

#[test]
fn test_repl_multi_stmt_assignment_then_loop_then_plain() {
    // BT-790: Regression — multiple intermediate expressions including assignment + loop.
    // `count := 0. 3 timesRepeat: [count := count + 1]. count` generates state chain:
    //   State → State1 (from assignment) → State2 (from loop StateAcc) → {Result, State2}
    let src = "count := 0. 3 timesRepeat: [count := count + 1]. count";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
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

    // Check that x lookup works through State
    assert!(
        code.contains("call 'maps':'get'('x', State)"),
        "Variable x should be looked up from State. Got:\n{code}"
    );

    // Check the arithmetic operation
    assert!(
        code.contains("call 'erlang':'+'("),
        "Should have addition operation. Got:\n{code}"
    );
}

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

    // BT-800: Reads inside loop body must use StateAcc (not State) so they get
    // the accumulated value from the previous iteration.
    assert!(
        code.contains("maps':'get'('x', StateAcc)"),
        "BT-800: Read inside loop must use StateAcc to get accumulated value. Got:\n{code}"
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

    // BT-800: Final read of x must use the state produced by the loop (State2+)
    assert!(
        code.contains("maps':'get'('x', State2)"),
        "BT-800: Final x read must use loop-updated state (State2). Got:\n{code}"
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "comprehensive test covering all registration metadata"
)]
fn test_class_registration_generation() {
    // BT-218: Test that class definitions generate registration code
    use crate::ast::{ClassDefinition, Identifier, MethodDefinition, MethodKind, StateDeclaration};
    use crate::source_analysis::Span;

    // Create a Counter class with instance variables and methods
    let class = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 7)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("value", Span::new(0, 5)),
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
            type_annotation: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 10),
        }],
        methods: vec![
            MethodDefinition {
                selector: MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(42),
                    Span::new(0, 2),
                ))],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 10),
            },
            MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(42),
                    Span::new(0, 2),
                ))],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 10),
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 50),
    };

    let module = Module {
        expressions: vec![],
        classes: vec![class],
        method_definitions: Vec::new(),
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // Check that on_load attribute is present
    assert!(
        code.contains("'on_load' = [{'register_class', 0}]"),
        "Should have on_load attribute. Got:\n{code}"
    );

    // Check that register_class/0 is exported
    assert!(
        code.contains("'register_class'/0"),
        "Should export register_class/0. Got:\n{code}"
    );

    // Check that register_class/0 function exists
    assert!(
        code.contains("'register_class'/0 = fun () ->"),
        "Should generate register_class function. Got:\n{code}"
    );

    // BT-837: Check that it calls beamtalk_class_builder:register
    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState0)"),
        "Should call beamtalk_class_builder:register. Got:\n{code}"
    );

    // Check ClassBuilder state fields
    assert!(
        code.contains("'className' => 'Counter'"),
        "Should include className in builder state. Got:\n{code}"
    );
    assert!(
        code.contains("'moduleName' => 'counter'"),
        "Should include moduleName in builder state. Got:\n{code}"
    );
    assert!(
        code.contains("'superclassRef' => 'Actor'"),
        "Should include superclassRef in builder state. Got:\n{code}"
    );

    // BT-745: Check beamtalk_class module attribute for dependency sorting
    assert!(
        code.contains("'beamtalk_class' = [{'Counter', 'Actor'}]"),
        "Should include beamtalk_class attribute with class and superclass. Got:\n{code}"
    );

    // BT-1078: methodSpecs, fieldSpecs, classMethods removed from BuilderState.
    // Methods and fields now live in meta map.
    assert!(
        code.contains("'meta' => ~{"),
        "Should include meta map in builder state. Got:\n{code}"
    );
    // Check method_info contains instance methods with arity
    assert!(
        code.contains("'method_info' => ~{"),
        "Should include method_info in meta map. Got:\n{code}"
    );
    assert!(
        code.contains("'class_method_info' => ~{"),
        "Should include class_method_info in meta map. Got:\n{code}"
    );
    // Check fields in meta
    assert!(
        code.contains("'fields' => ['value']"),
        "Should include fields in meta map. Got:\n{code}"
    );

    // BT-1078: modifiers removed from BuilderState; is_sealed/is_abstract now in meta map
    assert!(
        code.contains("'is_sealed' => 'false'"),
        "Should include is_sealed in meta map. Got:\n{code}"
    );

    // Check function returns ok
    assert!(code.contains("'ok'"), "Should return 'ok'. Got:\n{code}");

    // BT-998: catch clause must re-raise, not silently swallow errors
    assert!(
        code.contains("catch <CatchType, CatchError, CatchStack> -> primop 'raw_raise'(CatchType, CatchError, CatchStack)"),
        "register_class/0 catch clause must re-raise via primop 'raw_raise' (BT-998). Got:\n{code}"
    );
}

#[test]
fn test_no_class_registration_for_empty_module() {
    // BT-218: Modules without class definitions should not have on_load or register_class
    let module = Module::new(vec![], Span::new(0, 0));
    let code = generate_module(&module, CodegenOptions::new("empty_module"))
        .expect("codegen should succeed");

    // Should NOT have on_load attribute
    assert!(
        !code.contains("'on_load'"),
        "Module without classes should not have on_load. Got:\n{code}"
    );

    // Should NOT export register_class/0
    assert!(
        !code.contains("'register_class'/0"),
        "Module without classes should not export register_class. Got:\n{code}"
    );

    // BT-745: Should NOT have beamtalk_class attribute
    assert!(
        !code.contains("'beamtalk_class'"),
        "Module without classes should not have beamtalk_class attribute. Got:\n{code}"
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn test_multiple_classes_registration() {
    // BT-218: Test that modules with multiple classes register all of them
    use crate::ast::{ClassDefinition, Identifier, StateDeclaration};
    use crate::source_analysis::Span;

    fn make_actor_class(
        name: &str,
        name_len: u32,
        field: &str,
        field_len: u32,
        span_end: u32,
    ) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, Span::new(0, name_len)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![StateDeclaration {
                name: Identifier::new(field, Span::new(0, field_len)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 10),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: Span::new(0, span_end),
        }
    }

    let module = Module {
        expressions: vec![],
        classes: vec![
            make_actor_class("Counter", 7, "value", 5, 20),
            make_actor_class("Logger", 6, "messages", 8, 30),
        ],
        method_definitions: Vec::new(),
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("multi_actors"))
        .expect("codegen should succeed");

    // Should have on_load attribute
    assert!(
        code.contains("'on_load' = [{'register_class', 0}]"),
        "Should have on_load attribute for multiple classes. Got:\n{code}"
    );

    // BT-837: Should register both classes via ClassBuilder
    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState0)"),
        "Should register Counter via ClassBuilder. Got:\n{code}"
    );
    assert!(
        code.contains("'className' => 'Counter'"),
        "Should include Counter metadata. Got:\n{code}"
    );
    // BT-1078: fieldSpecs removed from BuilderState; fields now in meta map
    assert!(
        code.contains("'fields' => ['value']"),
        "Should include Counter fields in meta. Got:\n{code}"
    );

    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState1)"),
        "Should register Logger via ClassBuilder. Got:\n{code}"
    );
    assert!(
        code.contains("'className' => 'Logger'"),
        "Should include Logger metadata. Got:\n{code}"
    );
    assert!(
        code.contains("'fields' => ['messages']"),
        "Should include Logger fields in meta. Got:\n{code}"
    );

    // Should use let-binding chain to sequence registrations
    assert!(
        code.contains("let _BuilderState0 = ~{"),
        "Should have first BuilderState binding. Got:\n{code}"
    );
    assert!(
        code.contains("let _Reg0 = case"),
        "Should have first registration with _Reg0. Got:\n{code}"
    );
    assert!(
        code.contains("let _BuilderState1 = ~{"),
        "Should have second BuilderState binding. Got:\n{code}"
    );
    assert!(
        code.contains("let _Reg1 = case"),
        "Should chain second registration with _Reg1. Got:\n{code}"
    );

    // BT-738: Final result propagates last _Reg.
    assert!(
        code.contains("in _Reg1"),
        "Should propagate last _Reg result after all registrations. Got:\n{code}"
    );

    // BT-749: Short-circuit: earlier error must propagate before executing later classes.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should short-circuit on _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error. Got:\n{code}"
    );

    // BT-745: Check beamtalk_class attribute lists both classes
    assert!(
        code.contains("'beamtalk_class' = [{'Counter', 'Actor'}, {'Logger', 'Actor'}]"),
        "Should include beamtalk_class attribute with both classes. Got:\n{code}"
    );
}

#[test]
fn test_multi_class_early_error_short_circuits() {
    // BT-749: When an earlier class (not the last) returns {error, ...} from
    // update_class (e.g. stdlib_shadowing), the error must propagate — the
    // subsequent class registrations must not mask it with 'ok'.
    //
    // We verify this by checking the generated code structure: each _RegN
    // (except the last) must be wrapped in a case that short-circuits on error.
    use crate::ast::{ClassDefinition, Identifier, StateDeclaration};
    use crate::source_analysis::Span;

    fn make_class(name: &str, name_len: u32, span_end: u32) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, Span::new(0, name_len)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![StateDeclaration {
                name: Identifier::new("x", Span::new(0, 1)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 5),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: Span::new(0, span_end),
        }
    }

    // Two classes: ShadowA (index 0), ValidB (index 1, last).
    // ValidB is fine; ShadowA would be the one shadowing stdlib.
    // The fix must ensure that if _Reg0 is {error, ...}, we never reach _Reg1.
    let module = Module {
        expressions: vec![],
        classes: vec![make_class("ShadowA", 7, 20), make_class("ValidB", 6, 30)],
        method_definitions: Vec::new(),
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("multi_shadow"))
        .expect("codegen should succeed");

    // BT-749: First class must be wrapped in a short-circuit case check.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should wrap _Reg0 in a short-circuit case. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error before executing later classes. Got:\n{code}"
    );

    // The last class's result is returned directly (no further wrapping needed).
    assert!(
        code.contains("in _Reg1"),
        "Should use _Reg1 as the final result. Got:\n{code}"
    );

    // The second class must NOT be wrapped in its own short-circuit case
    // (it is the last, so its result flows out directly).
    assert!(
        !code.contains("in case _Reg1 of"),
        "Last _Reg should not be wrapped in a short-circuit case. Got:\n{code}"
    );
}

#[test]
fn test_three_class_short_circuit_nesting() {
    // BT-749: Verify nesting correctness for N=3 classes.
    // Short-circuit cases are added for indices 0 and 1 (all except the last).
    // The last class (index 2) is returned directly with no extra wrapping.
    use crate::ast::{ClassDefinition, Identifier, StateDeclaration};
    use crate::source_analysis::Span;

    fn make_class(name: &str, name_len: u32) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, Span::new(0, name_len)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            supervisor_kind: None,
            state: vec![StateDeclaration {
                name: Identifier::new("x", Span::new(0, 1)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 5),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            backing_module: None,
            span: Span::new(0, 20),
        }
    }

    let module = Module {
        expressions: vec![],
        classes: vec![make_class("A", 1), make_class("B", 1), make_class("C", 1)],
        method_definitions: Vec::new(),
        span: Span::new(0, 60),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("three_classes"))
        .expect("codegen should succeed");

    // BT-749: Classes 0 and 1 (non-last) must have short-circuit case wrappers.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should short-circuit on _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("in case _Reg1 of"),
        "Should short-circuit on _Reg1 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr1}> when 'true' -> {'error', _RegErr1}"),
        "Should propagate _Reg1 error. Got:\n{code}"
    );

    // Class 2 (last) must be returned directly — no extra case wrapping.
    assert!(
        code.contains("in _Reg2"),
        "Should use _Reg2 as final result. Got:\n{code}"
    );
    assert!(
        !code.contains("in case _Reg2 of"),
        "Last _Reg should not be wrapped in a short-circuit case. Got:\n{code}"
    );
}

#[test]
fn test_class_method_call_generation() {
    // BT-215: Test that ClassReference message sends generate appropriate code
    // BT-490 / ADR 0019: All classes (including Transcript, Beamtalk, Workspace)
    //         use standard class dispatch via class_send
    use crate::ast::{Expression, Identifier, MessageSelector};
    use crate::source_analysis::Span;

    // Test 1: Beamtalk class uses standard class_send dispatch (no special case)
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Beamtalk", Span::new(0, 8)),
            span: Span::new(0, 8),
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

#[test]
fn test_is_actor_class_direct_actor_subclass() {
    let class = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_object_subclass_is_value_type() {
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Object", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(!CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_multi_level_inheritance() {
    // LoggingCounter extends Counter extends Actor
    // Should still be detected as actor
    let counter = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let logging_counter = ClassDefinition {
        name: Identifier::new("LoggingCounter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Counter", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    // Module with both classes; first class is LoggingCounter
    let module = Module {
        classes: vec![counter, logging_counter.clone()],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();

    // Test with LoggingCounter as the first class
    let module_lc = Module {
        classes: vec![logging_counter],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    // Build hierarchy from full module so Counter is known
    assert!(CoreErlangGenerator::is_actor_class(&module_lc, &hierarchy));
}

#[test]
fn test_is_actor_class_no_classes_defaults_to_actor() {
    let module = Module::new(Vec::new(), Span::new(0, 0));
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_unknown_superclass_defaults_to_actor() {
    // LoggingCounter extends Counter, but Counter is NOT in this module.
    // Hierarchy chain is incomplete; should default to actor (backward compat).
    let class = ClassDefinition {
        name: Identifier::new("LoggingCounter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Counter", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_collection_subclass_is_value_type() {
    // Collection extends Object (built-in), so subclasses are value types.
    let class = ClassDefinition {
        name: Identifier::new("MyList", Span::new(0, 0)),
        superclass: Some(Identifier::new("Collection", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(
        !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Collection subclass should be value type (chain reaches Object)"
    );
}

#[test]
fn test_is_actor_class_integer_subclass_is_value_type() {
    // Integer is a sealed built-in extending Object — subclass should be value type.
    // (Sealed enforcement is separate; codegen should still route correctly.)
    let class = ClassDefinition {
        name: Identifier::new("MyInt", Span::new(0, 0)),
        superclass: Some(Identifier::new("Integer", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(
        !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Integer subclass should be value type (chain reaches Object)"
    );
}

#[test]
fn test_is_actor_class_root_class_is_value_type() {
    // Root class (superclass: None → "none") should be value type, not actor.
    let class = ClassDefinition {
        name: Identifier::new("ProtoObject", Span::new(0, 0)),
        superclass: None,
        class_kind: ClassKind::Object,
        is_abstract: true,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(
        !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Root class (nil superclass) should be value type"
    );
}

#[test]
fn test_generate_with_bindings_compiles_value_type() {
    // Test that generate_with_bindings produces valid output for a value type
    let class = ClassDefinition::new(
        Identifier::new("Point", Span::new(0, 0)),
        Identifier::new("Object", Span::new(0, 0)),
        vec![StateDeclaration {
            name: Identifier::new("x", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        vec![],
        Span::new(0, 0),
    );
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let bindings = primitive_bindings::PrimitiveBindingTable::new();
    let result = generate_module(
        &module,
        CodegenOptions::new("point").with_bindings(bindings),
    );
    assert!(result.is_ok());
    let code = result.unwrap();
    assert!(code.contains("module 'point'"));
}

#[test]
fn test_generate_repl_list_reject() {
    // BT-408: reject: must generate valid Core Erlang with properly bound wrapper fun
    // The wrapper fun must be bound via `let` — not inlined in the call args,
    // because Core Erlang lambdas don't use `end` and can't be inlined in calls.
    let src = "#(1, 2, 3, 4, 5) reject: [:x | x > 2]";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
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

#[test]
fn test_class_method_rejects_field_access() {
    // BT-426: Class methods should reject instance field access
    let src = "Actor subclass: TestClass\n  state: value = 0\n\n  class broken => self.value";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("test_class_field").with_workspace_mode(true),
    );
    assert!(
        result.is_err(),
        "Should reject field access in class method"
    );
    let err = format!("{}", result.unwrap_err());
    assert!(
        err.contains("cannot access instance field"),
        "Error should mention field access. Got: {err}"
    );
}

#[test]
fn test_class_method_rejects_field_assignment() {
    // BT-426: Class methods should reject instance field mutation
    let src = "Actor subclass: TestClass\n  state: value = 0\n\n  class broken => self.value := 42";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("test_class_assign").with_workspace_mode(true),
    );
    assert!(
        result.is_err(),
        "Should reject field assignment in class method"
    );
    let err = format!("{}", result.unwrap_err());
    assert!(
        err.contains("cannot assign to instance field"),
        "Error should mention field assignment. Got: {err}"
    );
}

#[test]
fn test_value_subclass_auto_getter_exported() {
    // BT-923: `Value subclass:` auto-generates getter functions for each slot.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    assert!(result.is_ok(), "Codegen should succeed for Value subclass:");
    let code = result.unwrap();
    // Getter exports: 'x'/1 and 'y'/1
    assert!(
        code.contains("'x'/1"),
        "Should export getter 'x'/1. Got:\n{code}"
    );
    assert!(
        code.contains("'y'/1"),
        "Should export getter 'y'/1. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_auto_getter_function() {
    // BT-923: Getter body uses maps:get to read the slot from Self.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'x'/1 = fun (Self) ->"),
        "Should generate x/1 getter. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('x', Self)"),
        "x getter should use maps:get. Got:\n{code}"
    );
    assert!(
        code.contains("'y'/1 = fun (Self) ->"),
        "Should generate y/1 getter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_auto_setter_exported() {
    // BT-923: `Value subclass:` auto-generates with*: functional setters.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'withX:'/2"),
        "Should export withX:/2. Got:\n{code}"
    );
    assert!(
        code.contains("'withY:'/2"),
        "Should export withY:/2. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_auto_setter_function() {
    // BT-923: with*: setter body uses maps:put to return an updated map.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'withX:'/2 = fun (Self, NewVal) ->"),
        "Should generate withX:/2 setter. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'put'('x', NewVal, Self)"),
        "withX: setter should use maps:put. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_keyword_constructor_exported() {
    // BT-923: `Value subclass:` auto-generates an all-fields keyword constructor.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    // Keyword constructor selector for x, y → 'class_x:y:'/4
    assert!(
        code.contains("'class_x:y:'/4"),
        "Should export 'class_x:y:'/4 keyword constructor. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_keyword_constructor_function() {
    // BT-923: Keyword constructor body creates a tagged map with all slots.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'class_x:y:'/4 = fun (_ClassSelf, _ClassVars, SlotArg0, SlotArg1) ->"),
        "Should generate keyword constructor function. Got:\n{code}"
    );
    assert!(
        code.contains("'$beamtalk_class' => 'Point'"),
        "Keyword constructor should set $beamtalk_class. Got:\n{code}"
    );
    assert!(
        code.contains("'x' => SlotArg0"),
        "Keyword constructor should set x from SlotArg0. Got:\n{code}"
    );
    assert!(
        code.contains("'y' => SlotArg1"),
        "Keyword constructor should set y from SlotArg1. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_dispatch_routes_getter() {
    // BT-923: dispatch/3 must route getter selectors to auto-generated functions.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("call 'bt@point':'x'(Self)"),
        "dispatch/3 should route 'x' to getter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_dispatch_routes_setter() {
    // BT-923: dispatch/3 must route with*: selectors to auto-generated functions.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("call 'bt@point':'withX:'(Self, DispArg0)"),
        "dispatch/3 should route 'withX:' to setter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_has_method_includes_auto_methods() {
    // BT-923: has_method/1 must report true for auto-generated selectors.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'x'"),
        "has_method/1 should list 'x' getter. Got:\n{code}"
    );
    assert!(
        code.contains("'withX:'"),
        "has_method/1 should list 'withX:' setter. Got:\n{code}"
    );
}

#[test]
fn test_object_subclass_no_auto_getters() {
    // BT-923: `Object subclass:` (ClassKind::Object) must NOT generate auto-getters.
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Object", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("x", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    // Object subclass should NOT have auto-getter 'x'/1
    assert!(
        !code.contains("'x'/1 = fun (Self) ->"),
        "Object subclass should not generate auto-getter. Got:\n{code}"
    );
    // And should not have withX:/2
    assert!(
        !code.contains("'withX:'/2"),
        "Object subclass should not generate auto-setter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_user_defined_overrides_auto() {
    // BT-923: User-defined methods suppress the corresponding auto-generated method.
    let x_method = MethodDefinition {
        selector: MessageSelector::Unary("x".into()),
        parameters: vec![],
        return_type: None,
        body: vec![bare(Expression::Literal(
            Literal::Integer(99),
            Span::new(0, 0),
        ))],
        kind: MethodKind::Primary,
        is_sealed: false,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let class = ClassDefinition {
        name: Identifier::new("MyVal", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("x", Span::new(0, 0)),
            type_annotation: None,
            default_value: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        methods: vec![x_method],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@my_val"));
    let code = result.unwrap();
    // The auto-getter would produce: call 'maps':'get'('x', Self)
    // When user defines 'x', that body should NOT appear — the user's body (99) wins.
    assert!(
        !code.contains("call 'maps':'get'('x', Self)"),
        "Auto-getter body should be suppressed when user defines 'x'. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_no_slots_no_keyword_constructor() {
    // BT-923: A Value subclass with no slots produces no keyword constructor.
    let class = ClassDefinition {
        name: Identifier::new("Empty", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@empty"));
    let code = result.unwrap();
    // A class with no slots has no keyword constructor selector, so no 'class_X:'/N pattern.
    // Scan all lines for the pattern: contains 'class_' AND contains ':'/  (selector with colon)
    let has_keyword_ctor = code
        .lines()
        .any(|line| line.contains("'class_") && line.contains(":/"));
    assert!(
        !has_keyword_ctor,
        "No keyword constructor should be generated for empty Value subclass. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_class_method_slot_send_routes_to_constructor() {
    // BT-996: `ClassName slot: value` inside a class method of the same class must
    // route to the auto-generated class-side keyword constructor, not the instance getter.
    //
    // Equivalent Beamtalk:
    //   Value subclass: SchemeSymbol
    //     state: symName = ""
    //     class withName: n => SchemeSymbol symName: n
    //
    // The generated `class_withName:/3` body should call `class_symName:` (constructor),
    // NOT `symName` (instance getter).
    let class = ClassDefinition {
        name: Identifier::new("SchemeSymbol", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("symName", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(
                Literal::String("".into()),
                Span::new(0, 0),
            )),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        methods: vec![],
        class_methods: vec![MethodDefinition {
            selector: MessageSelector::Keyword(vec![KeywordPart::new(
                "withName:",
                Span::new(0, 0),
            )]),
            parameters: vec![ParameterDefinition::new(Identifier::new(
                "n",
                Span::new(0, 0),
            ))],
            body: vec![bare(Expression::MessageSend {
                receiver: Box::new(Expression::ClassReference {
                    name: Identifier::new("SchemeSymbol", Span::new(0, 0)),
                    span: Span::new(0, 0),
                }),
                selector: MessageSelector::Keyword(vec![KeywordPart::new(
                    "symName:",
                    Span::new(0, 0),
                )]),
                arguments: vec![Expression::Identifier(Identifier::new(
                    "n",
                    Span::new(0, 0),
                ))],
                is_cast: false,
                span: Span::new(0, 0),
            })],
            return_type: None,
            is_sealed: false,
            kind: MethodKind::Primary,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@scheme_symbol"));
    let code = result.unwrap();

    // Must call the class-side keyword constructor from within class_withName:
    assert!(
        code.contains("call 'bt@scheme_symbol':'class_symName:'(ClassSelf, ClassVars,"),
        "class_withName: should dispatch to class_symName: constructor. Got:\n{code}"
    );
    // The class_withName: body must not call the instance getter (symName/1) passing n as self.
    // (Note: `symName` legitimately appears in dispatch/3 for the instance getter arm — correct.)
    assert!(
        !code.contains("call 'bt@scheme_symbol':'symName'(ClassSelf")
            && !code.contains("call 'bt@scheme_symbol':'symName'(_n"),
        "class_withName: body must not call instance getter symName/1. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_writeback_inferred_return_type_appears_in_method_return_types() {
    // BT-1005: A user-defined Actor class method with no explicit return-type
    // annotation should have its inferred return type written back into the AST
    // before codegen, so the emitted BEAM module contains it in method_return_types.
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0
  getValue => value
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    // The writeback pass should have populated return_type in meta.method_info
    // with 'Integer' (inferred from the state variable type).
    // BT-1078: return types now live in meta.method_info, not methodReturnTypes.
    assert!(
        code.contains(
            "'getValue' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer', 'is_sealed' => 'false'}~"
        ),
        "meta.method_info should contain inferred return type for unannotated getValue. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_explicit_annotation_not_overwritten_by_writeback() {
    // BT-1005: An explicitly annotated method must NOT be changed by the writeback pass.
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0
  getValue -> Integer => value
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    // Explicit annotation takes precedence — still appears correctly in meta.method_info.
    assert!(
        code.contains(
            "'getValue' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer', 'is_sealed' => 'false'}~"
        ),
        "Explicitly annotated method should appear in meta.method_info. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_literal_return_type_inferred_by_writeback() {
    // BT-1005: A method returning an integer literal should have Integer inferred
    // and written back even when the class has no typed state.
    let src = "
Actor subclass: Greeter
  answer => 42
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("greeter"))
        .expect("codegen should succeed")
        .code;

    assert!(
        code.contains(
            "'answer' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer', 'is_sealed' => 'false'}~"
        ),
        "meta.method_info should contain inferred Integer for literal-returning method. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_standalone_method_writeback_infers_return_type() {
    // BT-1005: Tonel-style standalone method definitions (Counter >> getValue => ...)
    // must also have their return types inferred and written back.
    // This exercises the module.method_definitions loop in infer_method_return_types.
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0

Counter >> getValue => value
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    assert!(
        code.contains(
            "'getValue' => ~{'arity' => 0, 'param_types' => [], 'return_type' => 'Integer', 'is_sealed' => 'false'}~"
        ),
        "meta.method_info should contain inferred Integer for standalone getValue. Got:\n{code}"
    );
}

#[test]
fn test_bt1005_untyped_param_does_not_shadow_state_field_type() {
    // BT-1005: An untyped parameter with the same name as a state field must NOT
    // cause the method's return type to be inferred as the state field's type.
    // The untyped param should be Dynamic, so the method's inferred return type
    // is also Dynamic and no writeback annotation is emitted.
    let src = "
Actor subclass: Counter
  state: value :: Integer = 0
  add: value => value
";
    // `add: value` has an untyped param named `value` that shadows the `value`
    // state field. The return type should be Dynamic (not Integer).
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module_with_warnings(&module, CodegenOptions::new("counter"))
        .expect("codegen should succeed")
        .code;

    // `add:` must NOT appear in method_return_types with Integer inferred from
    // the state field — it should be absent (Dynamic = no entry).
    assert!(
        !code.contains("'add:' => 'Integer'"),
        "Untyped param `value` must not be mis-inferred as state field Integer. Got:\n{code}"
    );
}

#[test]
fn generate_module_with_pre_class_hierarchy_does_not_panic() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    use std::collections::HashMap;

    let src = "Object subclass: MyService\n  greet => \"hello\"";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _) = crate::source_analysis::parse(tokens);

    let pre_class = ClassInfo {
        name: ecow::EcoString::from("Helper"),
        superclass: Some(ecow::EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_value: false,
        state: vec![],
        state_types: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
    };

    let result = generate_module(
        &module,
        CodegenOptions::new("bt@my_service")
            .with_workspace_mode(true)
            .with_class_hierarchy(vec![pre_class]),
    );
    assert!(result.is_ok(), "generate_module should succeed: {result:?}");
}

#[test]
fn test_value_subclass_typed_fields_emit_type_alias() {
    // BT-1156: Value subclass with typed state: declarations emits '-type t()' attribute.
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![
            StateDeclaration {
                name: Identifier::new("x", Span::new(0, 0)),
                type_annotation: Some(TypeAnnotation::simple("Integer", Span::new(0, 0))),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
            StateDeclaration {
                name: Identifier::new("y", Span::new(0, 0)),
                type_annotation: Some(TypeAnnotation::simple("Integer", Span::new(0, 0))),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
        ],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'type' ="),
        "Should emit 'type' attribute. Got:\n{code}"
    );
    assert!(
        code.contains("'map_field_exact'"),
        "Type alias fields should use map_field_exact. Got:\n{code}"
    );
    assert!(
        code.contains("'$beamtalk_class'"),
        "Type alias should include $beamtalk_class tag. Got:\n{code}"
    );
    assert!(
        code.contains("'Point'"),
        "Type alias should include class name atom. Got:\n{code}"
    );
    assert!(
        code.contains("'integer'"),
        "Typed Integer fields should map to integer(). Got:\n{code}"
    );
    assert!(
        code.contains("'export_type' = [{'t', 0}]"),
        "Should emit export_type([t/0]) so other modules can reference Point:t(). Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_untyped_fields_still_emit_type_alias() {
    // BT-1156: Value subclass with untyped state: declarations also emits '-type t()'
    // using any() for untyped fields.
    let module = make_value_subclass_point(); // x and y have no type annotations
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'type' ="),
        "Should emit 'type' attribute for untyped fields too. Got:\n{code}"
    );
    assert!(
        code.contains("'any'"),
        "Untyped fields should use any(). Got:\n{code}"
    );
    assert!(
        code.contains("'export_type' = [{'t', 0}]"),
        "Should emit export_type([t/0]) so other modules can reference Point:t(). Got:\n{code}"
    );
}

#[test]
fn test_class_method_local_var_assignment_of_self_class_method() {
    // BT-1201: class method `x := self classMethod` must NOT produce `in  in`.
    // Previously generated invalid Core Erlang:
    //   let X = let _CMR = call ... in let ClassVars1 = ... in let _Unwrapped = ... in  in X
    let src = "Object subclass: Broken\n  class a =>\n    x := self b.\n    x\n\n  class b => 42";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
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
    // BT-1201 follow-up (reviewer feedback): a class var mutation (`self.cv := expr`) preceding
    // a local var assignment (`x := plainExpr`) must NOT incorrectly treat the local var RHS as
    // an open-scope expression. The stale `last_open_scope_result` from the field assignment
    // must be cleared before processing the local var's RHS.
    //
    // Pattern: class a => self.cv := 1. x := self b. x
    // Without the clear, x would be bound to the field-assignment's result var, not `self b`.
    let src = "Object subclass: CVThenLocal\n  class cv = 0\n  class a =>\n    self.cv := 1.\n    x := self b.\n    x\n\n  class b => 99";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
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

#[test]
fn test_bt1213_block_value_with_captured_mutation_actor() {
    // BT-1213: [count := count + 1] value in actor context
    // Parse from source to get a realistic AST
    // Build AST manually: Object subclass: BT1213Actor
    //   testIt => count := 0. [count := count + 1] value. count
    let s = Span::new(0, 0);
    let count_id = || Expression::Identifier(Identifier::new("count", s));

    // count := count + 1
    let add_expr = Expression::MessageSend {
        receiver: Box::new(count_id()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![Expression::Literal(Literal::Integer(1), s)],
        is_cast: false,
        span: s,
    };
    let assign = Expression::Assignment {
        target: Box::new(count_id()),
        value: Box::new(add_expr),
        span: s,
    };

    // [count := count + 1] value
    let block = Block::new(vec![], vec![bare(assign)], s);
    let block_value = Expression::MessageSend {
        receiver: Box::new(Expression::Block(block)),
        selector: MessageSelector::Unary("value".into()),
        arguments: vec![],
        is_cast: false,
        span: s,
    };

    // count := 0
    let init_count = Expression::Assignment {
        target: Box::new(count_id()),
        value: Box::new(Expression::Literal(Literal::Integer(0), s)),
        span: s,
    };

    let method = MethodDefinition::new(
        MessageSelector::Unary("testIt".into()),
        vec![],
        vec![bare(init_count), bare(block_value), bare(count_id())],
        s,
    );

    let class = ClassDefinition {
        name: Identifier::new("BT1213Actor", s),
        superclass: Some(Identifier::new("Actor", s)),
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![method],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        span: s,
    };

    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: s,
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("bt@bt1213_actor"))
        .expect("codegen should work");

    // Actor codegen should thread count through StateAcc
    assert!(
        code.contains("__local__count"),
        "Should thread count through StateAcc. Got:\n{code}"
    );
}
