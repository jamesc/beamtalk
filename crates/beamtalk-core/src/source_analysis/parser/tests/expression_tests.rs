// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for expression parsing: literals, messages, Pratt precedence, @primitive,
//! @intrinsic, and binary patterns.
use super::*;

#[test]
fn parse_integer_literal() {
    let module = parse_ok("42");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Literal(Literal::Integer(42), _) => {}
        _ => panic!("Expected integer literal"),
    }
}

#[test]
fn parse_float_literal() {
    let module = parse_ok("2.5");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Literal(Literal::Float(f), _) if (*f - 2.5_f64).abs() < 0.001 => {}
        _ => panic!("Expected float literal"),
    }
}

#[test]
fn parse_string_literal() {
    let module = parse_ok("\"hello\"");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Literal(Literal::String(s), _) if s == "hello" => {}
        _ => panic!("Expected string literal"),
    }
}

#[test]
fn parse_symbol_literal() {
    let module = parse_ok("#symbol");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Literal(Literal::Symbol(s), _) if s == "symbol" => {}
        _ => panic!("Expected symbol literal"),
    }
}

#[test]
fn parse_identifier() {
    let module = parse_ok("myVar");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Identifier(id) if id.name == "myVar" => {}
        _ => panic!("Expected identifier"),
    }
}

#[test]
fn parse_assignment() {
    let module = parse_ok("x := 42");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Assignment { target, value, .. } => {
            assert!(matches!(**target, Expression::Identifier(_)));
            assert!(matches!(
                **value,
                Expression::Literal(Literal::Integer(42), _)
            ));
        }
        _ => panic!("Expected assignment"),
    }
}

#[test]
fn parse_unary_message() {
    let module = parse_ok("3 factorial");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Unary(name),
            arguments,
            ..
        } => {
            assert!(matches!(
                **receiver,
                Expression::Literal(Literal::Integer(3), _)
            ));
            assert_eq!(name.as_str(), "factorial");
            assert!(arguments.is_empty());
        }
        _ => panic!("Expected unary message send"),
    }
}

#[test]
fn parse_binary_message() {
    let module = parse_ok("3 + 4");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Binary(op),
            arguments,
            ..
        } => {
            assert!(matches!(
                **receiver,
                Expression::Literal(Literal::Integer(3), _)
            ));
            assert_eq!(op.as_str(), "+");
            assert_eq!(arguments.len(), 1);
            assert!(matches!(
                arguments[0],
                Expression::Literal(Literal::Integer(4), _)
            ));
        }
        _ => panic!("Expected binary message send"),
    }
}

#[test]
fn parse_binary_message_with_precedence() {
    // Test: 2 + 3 * 4 should be 2 + (3 * 4) = 14
    let module = parse_ok("2 + 3 * 4");
    assert_eq!(module.expressions.len(), 1);

    // The AST should be: (2 + (3 * 4))
    match &module.expressions[0].expression {
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Binary(op),
            arguments,
            ..
        } => {
            assert!(matches!(
                **receiver,
                Expression::Literal(Literal::Integer(2), _)
            ));
            assert_eq!(op.as_str(), "+");
            assert_eq!(arguments.len(), 1);

            // The argument should be (3 * 4)
            match &arguments[0] {
                Expression::MessageSend {
                    receiver: r2,
                    selector: MessageSelector::Binary(op2),
                    arguments: args2,
                    ..
                } => {
                    assert!(matches!(**r2, Expression::Literal(Literal::Integer(3), _)));
                    assert_eq!(op2.as_str(), "*");
                    assert_eq!(args2.len(), 1);
                    assert!(matches!(
                        args2[0],
                        Expression::Literal(Literal::Integer(4), _)
                    ));
                }
                _ => panic!("Expected multiplication as right operand"),
            }
        }
        _ => panic!("Expected addition at top level"),
    }
}

#[test]
fn parse_keyword_message() {
    let module = parse_ok("array at: 1 put: \"x\"");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } => {
            assert!(matches!(**receiver, Expression::Identifier(_)));
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0].keyword.as_str(), "at:");
            assert_eq!(parts[1].keyword.as_str(), "put:");
            assert_eq!(arguments.len(), 2);
        }
        _ => panic!("Expected keyword message send"),
    }
}

#[test]
fn parse_keyword_message_multiline_iftrue_iffalse() {
    let module = parse_ok("acc isEmpty ifTrue: [cell]\n            ifFalse: [\"{acc},{cell}\"]");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } => {
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0].keyword.as_str(), "ifTrue:");
            assert_eq!(parts[1].keyword.as_str(), "ifFalse:");
            assert_eq!(arguments.len(), 2);
        }
        _ => panic!("Expected keyword message send"),
    }
}

#[test]
fn parse_keyword_message_multiline_inject_into() {
    let module = parse_ok("collection inject: 0\n             into: [:acc :each | acc + each]");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } => {
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0].keyword.as_str(), "inject:");
            assert_eq!(parts[1].keyword.as_str(), "into:");
            assert_eq!(arguments.len(), 2);
        }
        _ => panic!("Expected keyword message send"),
    }
}

#[test]
fn parse_keyword_message_multiline_to_do() {
    let module = parse_ok("1 to: 10\n  do: [:i | i]");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } => {
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0].keyword.as_str(), "to:");
            assert_eq!(parts[1].keyword.as_str(), "do:");
            assert_eq!(arguments.len(), 2);
        }
        _ => panic!("Expected keyword message send"),
    }
}

#[test]
fn parse_keyword_message_dot_terminates_not_newline() {
    // Two separate statements: `a foo: 1` then `b bar: 2`
    let module = parse_ok("a foo: 1.\nb bar: 2");
    assert_eq!(module.expressions.len(), 2);
}

#[test]
fn parse_keyword_message_multiline_paren_receiver_continuation() {
    // BT-1061: `(expr)\n  ifTrue: [...]` should be a single keyword send,
    // not two statements. The parenthesized receiver signals unambiguous
    // continuation.
    let module = parse_ok("(a > 0)\n  ifTrue: [a]");
    assert_eq!(module.expressions.len(), 1, "Should be one statement");
    match &module.expressions[0].expression {
        Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } => {
            assert_eq!(parts.len(), 1);
            assert_eq!(parts[0].keyword.as_str(), "ifTrue:");
            assert_eq!(arguments.len(), 1);
        }
        _ => panic!("Expected keyword message send"),
    }
}

#[test]
fn parse_keyword_message_multiline_paren_receiver_iftrue_iffalse() {
    // BT-1061: parenthesized receiver + multi-keyword continuation across newlines
    let module = parse_ok("(a > 0)\n  ifTrue: [a]\n  ifFalse: [0]");
    assert_eq!(module.expressions.len(), 1, "Should be one statement");
    match &module.expressions[0].expression {
        Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            arguments,
            ..
        } => {
            assert_eq!(parts.len(), 2);
            assert_eq!(parts[0].keyword.as_str(), "ifTrue:");
            assert_eq!(parts[1].keyword.as_str(), "ifFalse:");
            assert_eq!(arguments.len(), 2);
        }
        _ => panic!("Expected keyword message send"),
    }
}

#[test]
fn parse_keyword_message_multiline_complex_paren_receiver() {
    // BT-1061: the motivating case from the issue — complex parenthesized
    // boolean expression followed by ifTrue: on the next line
    let module =
        parse_ok("((step > 0 and: [x <= end]) or: [step < 0 and: [x >= end]])\n  ifTrue: [x]");
    assert_eq!(module.expressions.len(), 1, "Should be one statement");
    match &module.expressions[0].expression {
        Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            ..
        } => {
            assert_eq!(parts.len(), 1);
            assert_eq!(parts[0].keyword.as_str(), "ifTrue:");
        }
        _ => panic!("Expected keyword message send"),
    }
}

#[test]
fn parse_keyword_message_newline_keyword_continues_receiver() {
    // BT-1061: a keyword on the next line is always a continuation — keywords
    // can never validly start a new statement on their own. `x\n  ifTrue: [x]`
    // is one statement, not two.
    let module = parse_ok("x\n  ifTrue: [x]");
    assert_eq!(module.expressions.len(), 1, "Should be one statement");
    match &module.expressions[0].expression {
        Expression::MessageSend {
            selector: MessageSelector::Keyword(parts),
            ..
        } => {
            assert_eq!(parts[0].keyword.as_str(), "ifTrue:");
        }
        _ => panic!("Expected keyword message send"),
    }
}

#[test]
fn parse_keyword_message_newline_identifier_receiver_is_new_statement() {
    // The newline rule for IDENTIFIER tokens is unchanged: `x\ny foo: 1`
    // is still two statements because `y` (an identifier, not a keyword)
    // can validly start a new expression.
    let module = parse_ok("x\ny foo: 1");
    assert_eq!(module.expressions.len(), 2);
}

// ========================================================================
// Pratt Parsing Tests
// ========================================================================

#[test]
fn pratt_single_operand_expression() {
    // Single operand without any operators should return just the literal
    let module = parse_ok("42");
    assert_eq!(module.expressions.len(), 1);
    assert!(matches!(
        &module.expressions[0].expression,
        Expression::Literal(Literal::Integer(42), _)
    ));
}

#[test]
fn pratt_unknown_operator_stops_parsing() {
    // Unknown operators like `++` should stop binary parsing
    // This parses as: 1 (then `++` is unknown, stops) `++` `2`
    // The `++` gets parsed as two unary `+` messages on `2`, but since
    // there's no identifier after 1, we just get `1` and then a new expression
    // Actually, `++` is a single BinarySelector token, so this tests that
    // unknown operators don't cause errors
    let (module, diagnostics) = {
        let tokens = crate::source_analysis::lex_with_eof("1 ++ 2");
        crate::source_analysis::parse(tokens)
    };
    // The parser should handle this gracefully
    // It may produce diagnostics but shouldn't panic
    assert!(!module.expressions.is_empty());
    // The behavior is that `++` is an unknown operator, so parsing stops at 1
    // Then `++` and `2` might produce errors or be parsed separately
    // The key is that the parser doesn't crash
    let _ = diagnostics; // We don't assert specific diagnostics
}

#[test]
fn pratt_precedence_mul_over_add() {
    // 1 + 2 * 3 should parse as 1 + (2 * 3)
    let module = parse_ok("1 + 2 * 3");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Binary(op),
        arguments,
        ..
    } = &module.expressions[0].expression
    {
        // Top level should be `+`
        assert_eq!(op.as_str(), "+");
        assert!(matches!(
            **receiver,
            Expression::Literal(Literal::Integer(1), _)
        ));

        // Right side should be 2 * 3
        if let Expression::MessageSend {
            receiver: inner_recv,
            selector: MessageSelector::Binary(inner_op),
            arguments: inner_args,
            ..
        } = &arguments[0]
        {
            assert_eq!(inner_op.as_str(), "*");
            assert!(matches!(
                **inner_recv,
                Expression::Literal(Literal::Integer(2), _)
            ));
            assert!(matches!(
                inner_args[0],
                Expression::Literal(Literal::Integer(3), _)
            ));
        } else {
            panic!("Expected nested MessageSend for 2 * 3");
        }
    } else {
        panic!("Expected MessageSend for 1 + ...");
    }
}

#[test]
fn pratt_left_associativity() {
    // 1 - 2 - 3 should parse as (1 - 2) - 3
    let module = parse_ok("1 - 2 - 3");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Binary(op),
        arguments,
        ..
    } = &module.expressions[0].expression
    {
        // Top level should be `-` with `3` on right
        assert_eq!(op.as_str(), "-");
        assert!(matches!(
            arguments[0],
            Expression::Literal(Literal::Integer(3), _)
        ));

        // Left side should be 1 - 2
        if let Expression::MessageSend {
            receiver: inner_recv,
            selector: MessageSelector::Binary(inner_op),
            arguments: inner_args,
            ..
        } = &**receiver
        {
            assert_eq!(inner_op.as_str(), "-");
            assert!(matches!(
                **inner_recv,
                Expression::Literal(Literal::Integer(1), _)
            ));
            assert!(matches!(
                inner_args[0],
                Expression::Literal(Literal::Integer(2), _)
            ));
        } else {
            panic!("Expected nested MessageSend for 1 - 2");
        }
    } else {
        panic!("Expected MessageSend");
    }
}

#[test]
fn pratt_comparison_lowest_precedence() {
    // 1 + 2 < 3 * 4 should parse as (1 + 2) < (3 * 4)
    let module = parse_ok("1 + 2 < 3 * 4");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Binary(op),
        arguments,
        ..
    } = &module.expressions[0].expression
    {
        assert_eq!(op.as_str(), "<");

        // Left side: 1 + 2
        if let Expression::MessageSend {
            selector: MessageSelector::Binary(left_op),
            ..
        } = &**receiver
        {
            assert_eq!(left_op.as_str(), "+");
        } else {
            panic!("Expected 1 + 2 on left");
        }

        // Right side: 3 * 4
        if let Expression::MessageSend {
            selector: MessageSelector::Binary(right_op),
            ..
        } = &arguments[0]
        {
            assert_eq!(right_op.as_str(), "*");
        } else {
            panic!("Expected 3 * 4 on right");
        }
    } else {
        panic!("Expected comparison at top level");
    }
}

#[test]
fn pratt_all_operators() {
    // Test all supported operators parse correctly
    // Using Erlang comparison operators (ADR 0002)
    let expressions = vec![
        "1 =:= 2", "1 /= 2", "1 =/= 2", // ADR 0002: Erlang comparison operators
        "1 < 2", "1 > 2", "1 <= 2", "1 >= 2", "1 + 2", "1 - 2", "1 * 2", "1 / 2", "1 % 2",
    ];

    for expr in expressions {
        let module = parse_ok(expr);
        assert_eq!(module.expressions.len(), 1, "Failed for: {expr}");
        assert!(
            matches!(
                &module.expressions[0].expression,
                Expression::MessageSend { .. }
            ),
            "Expected MessageSend for: {expr}"
        );
    }
}

#[test]
fn parse_super_unary_message() {
    let module = parse_ok("super increment");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(name),
        arguments,
        ..
    } = &module.expressions[0].expression
    {
        assert!(matches!(&**receiver, Expression::Super(_)));
        assert_eq!(name.as_str(), "increment");
        assert_eq!(arguments.len(), 0);
    } else {
        panic!("Expected super message send");
    }
}

#[test]
fn parse_super_keyword_message() {
    let module = parse_ok("super at: 1 put: value");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        ..
    } = &module.expressions[0].expression
    {
        assert!(matches!(&**receiver, Expression::Super(_)));
        assert_eq!(parts.len(), 2);
        assert_eq!(parts[0].keyword.as_str(), "at:");
        assert_eq!(parts[1].keyword.as_str(), "put:");
        assert_eq!(arguments.len(), 2);
    } else {
        panic!("Expected super keyword message send");
    }
}

#[test]
fn parse_super_in_method_body() {
    // Test super in a method body
    let module = parse_ok(
        "Actor subclass: Counter
  increment => super increment",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1);

    let method = &class.methods[0];
    assert_eq!(method.body.len(), 1, "Method should have 1 statement");

    // Statement should be super increment
    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(name),
        ..
    } = &method.body[0].expression
    {
        assert!(matches!(&**receiver, Expression::Super(_)));
        assert_eq!(name.as_str(), "increment");
    } else {
        panic!("Expected super message send in method body");
    }
}

#[test]
fn parse_super_field_access_is_allowed() {
    // Test that super.field is currently allowed by the parser
    // (This is a known limitation - super should only be used with message sends)
    let module = parse_ok("super.value");
    assert_eq!(module.expressions.len(), 1);

    // Currently parses as FieldAccess with Super receiver
    // This is technically invalid semantically but parser allows it
    if let Expression::FieldAccess { receiver, .. } = &module.expressions[0].expression {
        assert!(
            matches!(&**receiver, Expression::Super(_)),
            "Parser allows super.field (codegen will reject it)"
        );
    } else {
        panic!("Expected field access with super receiver");
    }
}

#[test]
fn parse_super_with_cascade() {
    // Test that super with cascade parses correctly
    // Note: Codegen currently rejects this (unclear semantics), but parser allows it
    let module = parse_ok("super increment; getValue");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::Cascade {
        receiver, messages, ..
    } = &module.expressions[0].expression
    {
        // The receiver should be a MessageSend with Super
        if let Expression::MessageSend {
            receiver: inner_receiver,
            selector: MessageSelector::Unary(name),
            ..
        } = &**receiver
        {
            assert!(matches!(&**inner_receiver, Expression::Super(_)));
            assert_eq!(name.as_str(), "increment");
        } else {
            panic!("Expected super increment as cascade receiver");
        }

        assert_eq!(messages.len(), 1);
        assert_eq!(messages[0].selector.name(), "getValue");
    } else {
        panic!("Expected cascade expression");
    }
}

// ========================================================================
// @primitive parsing tests (BT-290)
// ========================================================================

#[test]
fn parse_primitive_quoted_selector() {
    let source = "Object subclass: Foo\n  + other => @primitive \"+\"";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 1);
    if let Expression::Primitive {
        name, is_quoted, ..
    } = &method.body[0].expression
    {
        assert_eq!(name.as_str(), "+");
        assert!(is_quoted);
    } else {
        panic!("Expected Primitive, got: {:?}", method.body[0]);
    }
}

#[test]
fn parse_primitive_bare_identifier() {
    let source = "Object subclass: Foo\n  new => @primitive basicNew";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 1);
    if let Expression::Primitive {
        name, is_quoted, ..
    } = &method.body[0].expression
    {
        assert_eq!(name.as_str(), "basicNew");
        assert!(!is_quoted);
    } else {
        panic!("Expected Primitive, got: {:?}", method.body[0]);
    }
}

#[test]
fn parse_primitive_in_method_body() {
    // @primitive as first expression in a method body
    let source = "Object subclass: MyInt\n  + other => @primitive \"+\"";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 1);
    assert!(matches!(
        &method.body[0].expression,
        Expression::Primitive {
            is_quoted: true,
            ..
        }
    ));
}

#[test]
fn parse_primitive_with_fallback() {
    // @primitive followed by fallback code in method body
    let source = "Object subclass: MyInt\n  abs => @primitive \"abs\". self negated";
    let module = parse_ok(source);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 2);
    assert!(matches!(
        &method.body[0].expression,
        Expression::Primitive { .. }
    ));
}

#[test]
fn parse_primitive_structural_intrinsic() {
    let source = "Object subclass: MyObj\n  new => @primitive basicNew";
    let module = parse_ok(source);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 1);
    if let Expression::Primitive {
        name, is_quoted, ..
    } = &method.body[0].expression
    {
        assert_eq!(name.as_str(), "basicNew");
        assert!(!is_quoted);
    } else {
        panic!("Expected Primitive");
    }
}

#[test]
fn parse_primitive_missing_name_error() {
    // @primitive without a name should produce an error
    let source = "Object subclass: Foo\n  abs => @primitive";
    let diagnostics = parse_err(source);
    assert!(
        !diagnostics.is_empty(),
        "Expected error for @primitive without name"
    );
    assert!(
        diagnostics[0]
            .message
            .contains("@primitive must be followed by")
    );
}

#[test]
fn parse_primitive_outside_method_body_error() {
    // @primitive at top level (outside method body) should produce an error
    let diagnostics = parse_err("@primitive \"+\"");
    assert!(
        !diagnostics.is_empty(),
        "Expected error for @primitive outside method body"
    );
    assert!(
        diagnostics[0]
            .message
            .contains("@primitive can only appear inside a method body"),
        "Got: {}",
        diagnostics[0].message
    );
}

#[test]
fn parse_primitive_inside_block_in_method_body() {
    // @primitive inside a block within a method body should still be accepted
    let source = "Object subclass: Foo\n  m => [@primitive \"+\"]";
    let module = parse_ok(source);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 1);
    // The body is a block containing the primitive
    if let Expression::Block(block) = &method.body[0].expression {
        assert_eq!(block.body.len(), 1);
        assert!(
            matches!(&block.body[0].expression, Expression::Primitive { .. }),
            "Expected Primitive inside block, got: {:?}",
            block.body[0]
        );
    } else {
        panic!("Expected Block, got: {:?}", method.body[0]);
    }
}

// ========================================================================
// @intrinsic parsing tests (BT-484)
// ========================================================================

#[test]
fn parse_intrinsic_bare_identifier() {
    // @intrinsic with bare identifier produces same AST as @primitive
    let source = "Object subclass: Foo\n  blockValue => @intrinsic blockValue";
    let module = parse_ok(source);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 1);
    match &method.body[0].expression {
        Expression::Primitive {
            name, is_quoted, ..
        } => {
            assert_eq!(name.as_ref(), "blockValue");
            assert!(!is_quoted, "bare @intrinsic should not be quoted");
        }
        other => panic!("Expected Primitive, got: {other:?}"),
    }
}

#[test]
fn parse_intrinsic_quoted_selector() {
    // @intrinsic with quoted selector produces same AST as @primitive
    let source = "Object subclass: Foo\n  size => @intrinsic \"size\"";
    let module = parse_ok(source);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 1);
    match &method.body[0].expression {
        Expression::Primitive {
            name, is_quoted, ..
        } => {
            assert_eq!(name.as_ref(), "size");
            assert!(*is_quoted, "quoted @intrinsic should be quoted");
        }
        other => panic!("Expected Primitive, got: {other:?}"),
    }
}

#[test]
fn parse_intrinsic_outside_method_body_error() {
    let diagnostics = parse_err("@intrinsic blockValue");
    assert!(
        !diagnostics.is_empty(),
        "Expected error for @intrinsic outside method body"
    );
    assert!(
        diagnostics[0]
            .message
            .contains("@intrinsic can only appear inside a method body"),
        "Got: {}",
        diagnostics[0].message
    );
}

// BT-285: Consecutive binary method definitions
#[test]
fn parse_consecutive_binary_methods() {
    let source = "Object subclass: Foo\n  + other => @primitive \"+\"\n  - other => @primitive \"-\"\n  * other => @primitive \"*\"";
    let module = parse_ok(source);
    assert_eq!(module.classes[0].methods.len(), 3);
    assert_eq!(module.classes[0].methods[0].selector.name(), "+");
    assert_eq!(module.classes[0].methods[1].selector.name(), "-");
    assert_eq!(module.classes[0].methods[2].selector.name(), "*");
}

#[test]
fn parse_binary_methods_followed_by_unary() {
    let source = "Object subclass: Foo\n  + other => @primitive \"+\"\n  - other => @primitive \"-\"\n  negated => 0";
    let module = parse_ok(source);
    assert_eq!(module.classes[0].methods.len(), 3);
    assert_eq!(module.classes[0].methods[0].selector.name(), "+");
    assert_eq!(module.classes[0].methods[1].selector.name(), "-");
    assert_eq!(module.classes[0].methods[2].selector.name(), "negated");
}

#[test]
fn parse_binary_continuation_on_same_line() {
    // Binary operator on same line should still work as expression continuation
    let source = "Object subclass: Foo\n  m => 1 + 2 - 3";
    let module = parse_ok(source);
    assert_eq!(module.classes[0].methods.len(), 1);
    assert_eq!(module.classes[0].methods[0].selector.name(), "m");
}

#[test]
fn parse_binary_continuation_on_new_line() {
    // Binary operator on new line that is NOT a method definition should
    // continue the expression (regression test for BT-285)
    let source = "Object subclass: Foo\n  m => 1\n    + 2\n  n => 3";
    let module = parse_ok(source);
    assert_eq!(module.classes[0].methods.len(), 2);
    assert_eq!(module.classes[0].methods[0].selector.name(), "m");
    assert_eq!(module.classes[0].methods[1].selector.name(), "n");
}

// ========================================================================
// Binary pattern tests (BT-663)
// ========================================================================

#[test]
fn parse_binary_pattern_simple() {
    // <<version:8, rest/binary>> in a match expression
    let source = r"
Actor subclass: Parser
  parse: data =>
    data match: [
      <<version:8, rest/binary>> -> version
    ]";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    // No errors — pattern parsed successfully
}

#[test]
fn parse_binary_pattern_wildcard_segment() {
    // <<_/binary>> — wildcard segment
    let source = r"
Actor subclass: T
  test: data =>
    data match: [
      <<_/binary>> -> #ok
    ]";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
}

#[test]
fn parse_binary_pattern_multiple_modifiers() {
    // <<val:16/signed-little>> — signed little-endian 16-bit integer
    let source = r"
Actor subclass: T
  test: data =>
    data match: [
      <<val:16/signed-little>> -> val
    ]";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
}

#[test]
fn parse_binary_pattern_utf8() {
    // <<codepoint/utf8, rest/binary>>
    let source = r"
Actor subclass: T
  test: data =>
    data match: [
      <<codepoint/utf8, rest/binary>> -> codepoint
    ]";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
}

#[test]
fn parse_binary_pattern_empty() {
    // <<>> — empty binary
    let source = r"
Actor subclass: T
  test: data =>
    data match: [
      <<>> -> #empty
    ]";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
}

#[test]
fn lex_ltlt_gtgt_tokens() {
    use crate::source_analysis::TokenKind;
    let tokens = lex_with_eof("<<");
    assert!(matches!(tokens[0].kind(), TokenKind::LtLt));
    let tokens = lex_with_eof(">>");
    assert!(matches!(tokens[0].kind(), TokenKind::GtGt));
}

#[test]
fn ltlt_not_a_binary_selector() {
    // `<<` must NOT lex as BinarySelector("<<") any more
    use crate::source_analysis::TokenKind;
    let tokens = lex_with_eof("<<");
    assert!(!matches!(tokens[0].kind(), TokenKind::BinarySelector(_)));
}

#[test]
fn gtgt_still_works_as_method_lookup() {
    // `Counter >> #increment` is a binary message send (method lookup)
    let module = parse_ok("Counter >> #increment");
    assert!(module.method_definitions.is_empty());
    assert_eq!(module.expressions.len(), 1);
}

#[test]
fn gtgt_still_works_in_standalone_method_definition() {
    // `Counter >> increment =>` is a Tonel-style standalone method definition
    let module = parse_ok("Counter >> increment => nil");
    assert_eq!(module.method_definitions.len(), 1);
    assert_eq!(
        module.method_definitions[0].class_name.name.as_str(),
        "Counter"
    );
}

#[test]
fn incomplete_trailing_gtgt() {
    // "Counter >>" is incomplete — trailing binary op / method lookup
    assert!(!is_input_complete("Counter >>"));
    assert!(!is_input_complete("Counter class >>"));
}

#[test]
fn incomplete_unclosed_binary_pattern() {
    // Unclosed `<<` should be detected as incomplete input
    assert!(!is_input_complete("data match: [<<version:8]"));
    assert!(!is_input_complete("<<version:8"));
}

#[test]
fn complete_closed_binary_pattern() {
    // Fully closed binary pattern is complete
    assert!(is_input_complete(
        r"data match: [<<version:8, _/binary>> -> version]"
    ));
}

#[test]
fn binary_segment_conflicting_type_specifiers_error() {
    // `/integer-binary` — two type specifiers: error on the second
    let diagnostics = parse_err(
        r"Actor subclass: T
  test: data =>
    data match: [<<x/integer-binary>> -> x]",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("Conflicting binary segment type specifier")),
        "Expected conflict diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn binary_segment_conflicting_endianness_specifiers_error() {
    // `/big-little` — two endianness specifiers: error on the second
    let diagnostics = parse_err(
        r"Actor subclass: T
  test: data =>
    data match: [<<x/big-little>> -> x]",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("Conflicting binary segment endianness specifier")),
        "Expected conflict diagnostic, got: {diagnostics:?}"
    );
}

#[test]
fn class_state_after_methods() {
    // state: fields should be allowed after method definitions
    let module = parse_ok(
        "Actor subclass: Counter
  increment => self.value := self.value + 1
  state: value = 0
  getValue => ^self.value",
    );
    assert_eq!(module.classes.len(), 1, "Should find 1 class");
    let class = &module.classes[0];
    assert_eq!(
        class.state.len(),
        1,
        "Should have 1 state field, got {}",
        class.state.len()
    );
    assert_eq!(
        class.methods.len(),
        2,
        "Should have 2 methods, got {}",
        class.methods.len()
    );
    assert_eq!(class.state[0].name.name.as_str(), "value");
}

#[test]
fn class_classstate_after_methods() {
    // classState: fields should be allowed after method definitions
    let module = parse_ok(
        "Actor subclass: Counter
  increment => self.count := self.count + 1
  classState: instanceCount = 0
  state: count = 0
  getCount => ^self.count",
    );
    assert_eq!(module.classes.len(), 1, "Should find 1 class");
    let class = &module.classes[0];
    assert_eq!(
        class.class_variables.len(),
        1,
        "Should have 1 classState field, got {}",
        class.class_variables.len()
    );
    assert_eq!(class.class_variables[0].name.name.as_str(), "instanceCount");
    assert_eq!(
        class.state.len(),
        1,
        "Should have 1 state field, got {}",
        class.state.len()
    );
    assert_eq!(
        class.methods.len(),
        2,
        "Should have 2 methods, got {}",
        class.methods.len()
    );
}

#[test]
fn class_members_fully_interleaved() {
    // All member types interleaved: method, classState, method, state, method
    let module = parse_ok(
        "Actor subclass: Mixed
  first => 1
  classState: cv = 0
  second => 2
  state: iv = 0
  third => 3",
    );
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 3, "Should have 3 methods");
    assert_eq!(class.state.len(), 1, "Should have 1 state field");
    assert_eq!(
        class.class_variables.len(),
        1,
        "Should have 1 classState field"
    );
}

#[test]
fn class_method_before_field_declarations() {
    // A class method (like `class serial`) before state:/field: declarations
    // should parse correctly — the `class` modifier must not confuse boundary detection.
    let module = parse_ok(
        "Object subclass: Foo
  class serial -> Boolean => true
  field: x = 0
  state: y = 1
  doSomething => self.x + self.y",
    );
    let class = &module.classes[0];
    assert_eq!(
        class.class_methods.len(),
        1,
        "Should have 1 class method (serial), got {}",
        class.class_methods.len()
    );
    assert_eq!(
        class.methods.len(),
        1,
        "Should have 1 instance method (doSomething), got {}",
        class.methods.len()
    );
    assert_eq!(
        class.state.len(),
        2,
        "Should have 2 state fields (x, y), got {}",
        class.state.len()
    );
    // Verify the class method parsed correctly
    assert!(
        class
            .class_methods
            .iter()
            .any(|m| m.selector == MessageSelector::Unary("serial".into())),
        "Should find 'serial' class method"
    );
}

#[test]
fn sealed_method_before_field_declarations() {
    // `sealed` modifier before fields — boundary detection must skip the modifier.
    let module = parse_ok(
        "Object subclass: Foo
  sealed helper => 42
  field: x = 0
  doSomething => self.x",
    );
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 2, "Should have 2 methods");
    assert_eq!(class.state.len(), 1, "Should have 1 field");
    assert!(class.methods.iter().any(|m| m.is_sealed));
}

#[test]
fn sealed_class_method_before_field_declarations() {
    // Both `sealed` and `class` modifiers before fields.
    let module = parse_ok(
        "Object subclass: Foo
  sealed class create -> Foo => Foo new
  field: x = 0
  doSomething => self.x",
    );
    let class = &module.classes[0];
    assert_eq!(class.class_methods.len(), 1, "Should have 1 class method");
    assert_eq!(class.methods.len(), 1, "Should have 1 instance method");
    assert_eq!(class.state.len(), 1, "Should have 1 field");
    assert!(class.class_methods[0].is_sealed);
}

#[test]
fn class_keyword_method_before_field_declarations() {
    // Class method with keyword selector (multi-token) before fields.
    let module = parse_ok(
        "Object subclass: Foo
  class withValue: v => Foo new
  field: x = 0
  doSomething => self.x",
    );
    let class = &module.classes[0];
    assert_eq!(class.class_methods.len(), 1, "Should have 1 class method");
    assert_eq!(class.methods.len(), 1, "Should have 1 instance method");
    assert_eq!(class.state.len(), 1, "Should have 1 field");
}

#[test]
fn class_method_before_classstate() {
    // Class method before classState: declaration.
    let module = parse_ok(
        "Actor subclass: Foo
  class serial -> Boolean => true
  classState: count = 0
  state: x = 0
  doSomething => self.x",
    );
    let class = &module.classes[0];
    assert_eq!(class.class_methods.len(), 1, "Should have 1 class method");
    assert_eq!(class.methods.len(), 1, "Should have 1 instance method");
    assert_eq!(class.class_variables.len(), 1, "Should have 1 classState");
    assert_eq!(class.state.len(), 1, "Should have 1 state field");
}

#[test]
fn multiple_class_methods_before_fields() {
    // Multiple class methods interleaved with fields.
    let module = parse_ok(
        "Object subclass: Foo
  class serial -> Boolean => true
  class create -> Foo => Foo new
  field: x = 0
  state: y = 1
  doSomething => self.x + self.y",
    );
    let class = &module.classes[0];
    assert_eq!(class.class_methods.len(), 2, "Should have 2 class methods");
    assert_eq!(class.methods.len(), 1, "Should have 1 instance method");
    assert_eq!(class.state.len(), 2, "Should have 2 fields");
}

#[test]
fn binary_segment_conflicting_signedness_specifiers_error() {
    // `/signed-unsigned` — two signedness specifiers: error on the second
    let diagnostics = parse_err(
        r"Actor subclass: T
  test: data =>
    data match: [<<x/signed-unsigned>> -> x]",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("Conflicting binary segment signedness specifier")),
        "Expected conflict diagnostic, got: {diagnostics:?}"
    );
}

// --- field: keyword tests (BT-1527) ---

#[test]
fn field_declaration_basic() {
    let source = "Object subclass: Foo\n  field: x\n";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].state.len(), 1);
    let decl = &module.classes[0].state[0];
    assert_eq!(decl.name.name, "x");
    assert_eq!(decl.declared_keyword, DeclaredKeyword::Field);
}

#[test]
fn field_declaration_with_default() {
    let source = "Object subclass: Foo\n  field: count = 0\n";
    let module = parse_ok(source);
    let decl = &module.classes[0].state[0];
    assert_eq!(decl.name.name, "count");
    assert_eq!(decl.declared_keyword, DeclaredKeyword::Field);
    assert!(decl.default_value.is_some());
}

#[test]
fn field_declaration_with_type() {
    let source = "Object subclass: Foo\n  field: count :: Integer\n";
    let module = parse_ok(source);
    let decl = &module.classes[0].state[0];
    assert_eq!(decl.name.name, "count");
    assert_eq!(decl.declared_keyword, DeclaredKeyword::Field);
    assert!(decl.type_annotation.is_some());
}

#[test]
fn field_declaration_with_type_and_default() {
    let source = "Object subclass: Foo\n  field: count :: Integer = 42\n";
    let module = parse_ok(source);
    let decl = &module.classes[0].state[0];
    assert_eq!(decl.name.name, "count");
    assert_eq!(decl.declared_keyword, DeclaredKeyword::Field);
    assert!(decl.type_annotation.is_some());
    assert!(decl.default_value.is_some());
}

#[test]
fn state_declaration_has_state_keyword() {
    let source = "Object subclass: Foo\n  state: x = 0\n";
    let module = parse_ok(source);
    let decl = &module.classes[0].state[0];
    assert_eq!(decl.declared_keyword, DeclaredKeyword::State);
}

#[test]
fn mixed_state_and_field_declarations() {
    let source = "Object subclass: Foo\n  state: x = 0\n  field: y = 1\n";
    let module = parse_ok(source);
    assert_eq!(module.classes[0].state.len(), 2);
    assert_eq!(
        module.classes[0].state[0].declared_keyword,
        DeclaredKeyword::State
    );
    assert_eq!(
        module.classes[0].state[1].declared_keyword,
        DeclaredKeyword::Field
    );
}
