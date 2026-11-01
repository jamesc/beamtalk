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
fn parse_float_literal_out_of_range_is_rejected() {
    // `1e309` overflows f64 to +inf; the parser must surface a diagnostic
    // rather than constructing a non-finite literal that codegen would emit
    // as `inf.0` (which Core Erlang cannot parse).
    use crate::source_analysis::Severity;
    let diagnostics = parse_err("1e309");
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 1, "expected one error, got: {errors:?}");
    assert!(
        errors[0].message.contains("out of range"),
        "message: {}",
        errors[0].message
    );
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
    // `(expr)\n  ifTrue: [...]` should be a single keyword send,
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
    // Parenthesized receiver + multi-keyword continuation across newlines
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
    // A complex parenthesized boolean expression followed by ifTrue: on
    // the next line
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
    // A keyword on the next line is always a continuation — keywords
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
    // Note: the parser allows it; semantic analysis rejects it (BT-4251)
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
// @primitive parsing tests
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
fn parse_primitive_bare_infers_selector() {
    // Bare @primitive (no selector string) infers the selector from the
    // enclosing method, producing the same AST as the explicit quoted form.
    let source = "Object subclass: Foo\n  size => @primitive";
    let module = parse_ok(source);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 1);
    if let Expression::Primitive {
        name, is_quoted, ..
    } = &method.body[0].expression
    {
        assert_eq!(name.as_str(), "size");
        assert!(
            is_quoted,
            "inferred selector behaves like a quoted selector"
        );
    } else {
        panic!("Expected Primitive, got: {:?}", method.body[0]);
    }
}

#[test]
fn parse_primitive_bare_infers_binary_selector() {
    // Inference works for binary selectors too.
    let source = "Object subclass: Foo\n  + other => @primitive";
    let module = parse_ok(source);
    let method = &module.classes[0].methods[0];
    if let Expression::Primitive { name, .. } = &method.body[0].expression {
        assert_eq!(name.as_str(), "+");
    } else {
        panic!("Expected Primitive, got: {:?}", method.body[0]);
    }
}

#[test]
fn parse_primitive_bare_infers_keyword_selector() {
    // Inference concatenates keyword parts: `at:put:`.
    let source = "Object subclass: Foo\n  at: i put: v => @primitive";
    let module = parse_ok(source);
    let method = &module.classes[0].methods[0];
    if let Expression::Primitive { name, .. } = &method.body[0].expression {
        assert_eq!(name.as_str(), "at:put:");
    } else {
        panic!("Expected Primitive, got: {:?}", method.body[0]);
    }
}

#[test]
fn parse_primitive_bare_with_fallback() {
    // Bare @primitive followed by fallback code on the next line infers the
    // selector and keeps the fallback as a separate statement.
    let source = "Object subclass: MyInt\n  abs => @primitive\n    self negated";
    let module = parse_ok(source);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.body.len(), 2);
    if let Expression::Primitive { name, .. } = &method.body[0].expression {
        assert_eq!(name.as_str(), "abs");
    } else {
        panic!("Expected Primitive, got: {:?}", method.body[0]);
    }
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
// @intrinsic parsing tests
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
            assert_eq!(name.as_str(), "blockValue");
            assert!(!is_quoted, "bare @intrinsic should not be quoted");
        }
        other => panic!("Expected Primitive, got: {other:?}"),
    }
}

#[test]
fn parse_intrinsic_bare_does_not_infer() {
    // Bare `@intrinsic` must still error. Intrinsic names name a
    // structural intrinsic, never the method's selector, so inference (which is
    // only correct for `@primitive`) must not apply.
    let source = "Object subclass: Foo\n  size => @intrinsic";
    let diagnostics = parse_err(source);
    assert!(
        !diagnostics.is_empty(),
        "Expected error for bare @intrinsic"
    );
    assert!(
        diagnostics[0]
            .message
            .contains("@intrinsic must be followed by"),
        "Got: {}",
        diagnostics[0].message
    );
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
            assert_eq!(name.as_str(), "size");
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

// Consecutive binary method definitions
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
    // continue the expression (regression test)
    let source = "Object subclass: Foo\n  m => 1\n    + 2\n  n => 3";
    let module = parse_ok(source);
    assert_eq!(module.classes[0].methods.len(), 2);
    assert_eq!(module.classes[0].methods[0].selector.name(), "m");
    assert_eq!(module.classes[0].methods[1].selector.name(), "n");
}

// ========================================================================
// Binary pattern tests
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
fn parse_binary_pattern_bitstring_segment() {
    // BT-4097: <<flags:3, v:5, rest/bits>> — bits/bitstring segment type
    use crate::ast::{BinarySegmentType, Pattern};
    for spec in ["bits", "bitstring"] {
        let source = format!("x match: [<<flags:3, v:5, rest/{spec}>> -> v]");
        let module = parse_ok(&source);
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match for /{spec}");
        };
        let Pattern::Binary { segments, .. } = &arms[0].pattern else {
            panic!("expected Pattern::Binary for /{spec}");
        };
        assert_eq!(
            segments[2].segment_type,
            Some(BinarySegmentType::Bitstring),
            "expected Bitstring type for /{spec}"
        );
    }
}

#[test]
fn parse_binary_pattern_literal_value() {
    // BT-4097: <<0:1, v:7, rest/binary>> — integer literal segment value
    use crate::ast::Pattern;
    let module = parse_ok("x match: [<<0:1, v:7, rest/binary>> -> v]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Binary { segments, .. } = &arms[0].pattern else {
        panic!("expected Pattern::Binary");
    };
    assert!(
        matches!(&segments[0].value, Pattern::Literal(Literal::Integer(0), _)),
        "expected integer literal 0, got: {:?}",
        segments[0].value
    );
}

#[test]
fn parse_binary_pattern_size_expression() {
    // BT-4097: <<a:(4 - q), rest/binary>> — parenthesized size expression
    use crate::ast::Pattern;
    let module = parse_ok("x match: [<<a:(4 - q), rest/binary>> -> a]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Binary { segments, .. } = &arms[0].pattern else {
        panic!("expected Pattern::Binary");
    };
    assert!(
        matches!(
            segments[0].size.as_deref(),
            Some(Expression::Parenthesized { .. })
        ),
        "expected parenthesized size expression, got: {:?}",
        segments[0].size
    );
}

#[test]
fn parse_binary_pattern_min_int_value() {
    // Review finding 2: <<-9223372036854775808:64/signed>> parses (i64::MIN).
    use crate::ast::Pattern;
    let module = parse_ok("x match: [<<-9223372036854775808:64/signed, rest/binary>> -> rest]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Binary { segments, .. } = &arms[0].pattern else {
        panic!("expected Pattern::Binary");
    };
    assert!(
        matches!(
            &segments[0].value,
            Pattern::Literal(Literal::Integer(i64::MIN), _)
        ),
        "expected i64::MIN literal, got: {:?}",
        segments[0].value
    );
}

#[test]
fn parse_binary_pattern_double_negated_min_int_is_clean_error() {
    // Review finding 2: <<- -9223372036854775808:64/signed>> exceeds
    // i64::MAX — a clean diagnostic, never a panic (or release wrap).
    use crate::source_analysis::Severity;
    let diagnostics = parse_err("x match: [<<- -9223372036854775808:64/signed>> -> 1]");
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 1, "expected one error, got: {errors:?}");
    assert!(
        errors[0].message.contains("out of range"),
        "expected out-of-range error, got: {}",
        errors[0].message
    );
}

#[test]
fn parse_binary_pattern_negative_size_is_error() {
    // Review finding 3: <<a:-5, rest/binary>> — erlc rejects negative sizes.
    use crate::source_analysis::Severity;
    let diagnostics = parse_err("x match: [<<a:-5, rest/binary>> -> a]");
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 1, "expected one error, got: {errors:?}");
    assert!(
        errors[0].message.contains("non-negative"),
        "expected non-negative error, got: {}",
        errors[0].message
    );
}

#[test]
fn parse_binary_pattern_utf8_with_size_is_error() {
    // Review finding 3: `erlc` rejects sizes on utf8 segments ("neither
    // size nor unit must be given") — Beamtalk errors instead of emitting
    // Core Erlang that `erlc` rejects with a "compiler bug" report.
    use crate::source_analysis::Severity;
    let diagnostics = parse_err("x match: [<<c:8/utf8, rest/binary>> -> c]");
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 1, "expected one error, got: {errors:?}");
    assert!(
        errors[0].message.contains("must not be given"),
        "expected must-not-be-given error, got: {}",
        errors[0].message
    );
}

#[test]
fn parse_binary_pattern_misfit_literals_warn() {
    // Review finding 3: values that cannot fit warn, erlc-style — but
    // fitting literals (including `<<-1:8/signed>>`) stay silent.
    use crate::source_analysis::Severity;
    for (source, needle) in [
        ("x match: [<<256:8, rest/binary>> -> rest]", "will not fit"),
        ("x match: [<<256, rest/binary>> -> rest]", "will not fit"),
        (
            "x match: [<<-1:8, rest/binary>> -> rest]",
            "negative value -1 will never match",
        ),
        (
            "x match: [<<65/binary>> -> 1]",
            "not of the expected type 'binary'",
        ),
        (
            "x match: [<<v:8/float, rest/binary>> -> v]",
            "bad float bit size",
        ),
    ] {
        let diagnostics = parse_err(source);
        let warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert_eq!(
            warnings.len(),
            1,
            "expected one warning for {source}, got: {warnings:?}"
        );
        assert!(
            warnings[0].message.contains(needle),
            "expected {needle} for {source}, got: {}",
            warnings[0].message
        );
    }
    for source in [
        "x match: [<<-1:8/signed, rest/binary>> -> rest]",
        "x match: [<<255:8, rest/binary>> -> rest]",
        "x match: [<<0:1, v:7, rest/binary>> -> v]",
        "x match: [<<v:64/float, rest/binary>> -> v]",
        "x match: [<<c/utf8, rest/binary>> -> c]",
    ] {
        let module = parse_ok(source);
        assert_eq!(module.expressions.len(), 1);
    }
}

#[test]
fn parse_binary_pattern_utf16_utf32() {
    // BT-4143: <<c/utf16, d/utf32, rest/binary>> — segment types
    use crate::ast::{BinarySegmentType, Pattern};
    let module = parse_ok("x match: [<<c/utf16, d/utf32, rest/binary>> -> c]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Binary { segments, .. } = &arms[0].pattern else {
        panic!("expected Pattern::Binary");
    };
    assert_eq!(segments.len(), 3);
    assert_eq!(segments[0].segment_type, Some(BinarySegmentType::Utf16));
    assert_eq!(segments[1].segment_type, Some(BinarySegmentType::Utf32));
    assert_eq!(segments[2].segment_type, Some(BinarySegmentType::Binary));
}

#[test]
fn parse_binary_pattern_utf16_with_size_is_error() {
    // BT-4143: `erlc` rejects sizes on utf16 ("neither size nor unit must
    // be given for segments of type utf8/utf16/utf32").
    use crate::source_analysis::Severity;
    for (source, needle) in [
        ("x match: [<<c:8/utf16, rest/binary>> -> c]", "utf16"),
        ("x match: [<<c:8/utf32, rest/binary>> -> c]", "utf32"),
    ] {
        let diagnostics = parse_err(source);
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert_eq!(
            errors.len(),
            1,
            "expected one error for {source}, got: {errors:?}"
        );
        assert!(
            errors[0].message.contains("must not be given"),
            "expected must-not-be-given error for {source}, got: {}",
            errors[0].message
        );
        assert!(
            errors[0].message.contains(needle),
            "expected {needle} in error for {source}, got: {}",
            errors[0].message
        );
    }
}

#[test]
fn parse_binary_pattern_unit() {
    // BT-4143: <<v:2/unit:8, rest/binary>> — attached and spaced spellings
    use crate::ast::Pattern;
    for source in [
        "x match: [<<v:2/unit:8, rest/binary>> -> v]",
        "x match: [<<v:2/unit : 8, rest/binary>> -> v]",
    ] {
        let module = parse_ok(source);
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match for {source}");
        };
        let Pattern::Binary { segments, .. } = &arms[0].pattern else {
            panic!("expected Pattern::Binary for {source}");
        };
        assert_eq!(segments[0].unit, Some(8), "expected unit 8 for {source}");
        assert_eq!(
            segments[0].segment_type, None,
            "unit-only segment keeps default type for {source}"
        );
    }
}

#[test]
fn parse_binary_pattern_unit_errors() {
    // BT-4143: `erlc` parity — a unit without a size, a unit on a utf
    // segment, and an out-of-range unit are all errors.
    use crate::source_analysis::Severity;
    for (source, needle) in [
        (
            "x match: [<<v/unit:8, rest/binary>> -> v]",
            "unless a size is specified too",
        ),
        (
            "x match: [<<c/utf16-unit:2, rest/binary>> -> c]",
            "unit must not be given",
        ),
        (
            "x match: [<<v:2/unit:0, rest/binary>> -> v]",
            "between 1 and 256",
        ),
        (
            "x match: [<<v:2/unit:8-unit:1, rest/binary>> -> v]",
            "unit already set",
        ),
    ] {
        let diagnostics = parse_err(source);
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert!(
            errors.iter().any(|d| d.message.contains(needle)),
            "expected '{needle}' error for {source}, got: {errors:?}"
        );
    }
}

#[test]
fn parse_binary_pattern_float_literal_value() {
    // BT-4143: <<1.5/float>> and <<-1.5/float>> — float literal values
    use crate::ast::Pattern;
    for (source, expected) in [
        ("x match: [<<1.5/float, rest/binary>> -> rest]", 1.5_f64),
        ("x match: [<<-1.5/float, rest/binary>> -> rest]", -1.5_f64),
    ] {
        let module = parse_ok(source);
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match for {source}");
        };
        let Pattern::Binary { segments, .. } = &arms[0].pattern else {
            panic!("expected Pattern::Binary for {source}");
        };
        assert!(
            matches!(&segments[0].value, Pattern::Literal(Literal::Float(f), _) if (*f - expected).abs() < f64::EPSILON),
            "expected float literal {expected}, got: {:?}",
            segments[0].value
        );
    }
}

#[test]
fn parse_binary_pattern_float_out_of_range_is_error() {
    // BT-4143: `1e309` overflows f64 — same "out of range" rule as
    // `parse_literal`, so the message stays in sync with it.
    use crate::source_analysis::Severity;
    let diagnostics = parse_err("x match: [<<1e309/float, rest/binary>> -> rest]");
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 1, "expected one error, got: {errors:?}");
    assert!(
        errors[0].message.contains("out of range"),
        "expected out-of-range error, got: {}",
        errors[0].message
    );
}

#[test]
fn parse_binary_pattern_string_literal_value() {
    // BT-4143: <<"hi">> — string literal values take no type or size
    use crate::ast::Pattern;
    let module = parse_ok("x match: [<<\"hi\", rest/binary>> -> rest]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Binary { segments, .. } = &arms[0].pattern else {
        panic!("expected Pattern::Binary");
    };
    assert!(
        matches!(&segments[0].value, Pattern::Literal(Literal::String(s), _) if s.as_str() == "hi"),
        "expected string literal, got: {:?}",
        segments[0].value
    );
}

#[test]
fn parse_binary_pattern_utf_string_expands() {
    // BT-4143: `<<"hi"/utf16>>` is valid — codegen expands it to one
    // segment per character, like `erlc`. Bare signedness/endianness
    // without a utf type is still an error.
    use crate::ast::{BinarySegmentType, Pattern};
    use crate::source_analysis::Severity;
    for source in [
        "x match: [<<\"hi\"/utf16, rest/binary>> -> rest]",
        "x match: [<<\"hi\"/utf32-little, rest/binary>> -> rest]",
    ] {
        let module = parse_ok(source);
        let Expression::Match { arms, .. } = &module.expressions[0].expression else {
            panic!("expected Expression::Match for {source}");
        };
        let Pattern::Binary { segments, .. } = &arms[0].pattern else {
            panic!("expected Pattern::Binary for {source}");
        };
        assert!(
            matches!(
                &segments[0].value,
                Pattern::Literal(Literal::String(s), _) if s.as_str() == "hi"
            ),
            "expected string literal value for {source}, got: {:?}",
            segments[0].value
        );
        assert!(
            matches!(
                segments[0].segment_type,
                Some(BinarySegmentType::Utf16 | BinarySegmentType::Utf32)
            ),
            "expected utf type for {source}, got: {:?}",
            segments[0].segment_type
        );
    }
    for source in [
        "x match: [<<\"hi\"/signed, rest/binary>> -> rest]",
        "x match: [<<\"hi\"/little, rest/binary>> -> rest]",
    ] {
        let diagnostics = parse_err(source);
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert_eq!(
            errors.len(),
            1,
            "expected one error for {source}, got: {errors:?}"
        );
        assert!(
            errors[0].message.contains("must not have a type or a size"),
            "expected type-or-size error for {source}, got: {}",
            errors[0].message
        );
    }
}

#[test]
fn parse_binary_pattern_unit_boundaries() {
    // BT-4143: `erlc` accepts units 1..=256.
    use crate::ast::Pattern;
    use crate::source_analysis::Severity;
    let module = parse_ok("x match: [<<v:2/unit:256, rest/binary>> -> v]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Binary { segments, .. } = &arms[0].pattern else {
        panic!("expected Pattern::Binary");
    };
    assert_eq!(segments[0].unit, Some(256));
    let diagnostics = parse_err("x match: [<<v:2/unit:257, rest/binary>> -> v]");
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        errors
            .iter()
            .any(|d| d.message.contains("between 1 and 256")),
        "expected range error, got: {errors:?}"
    );
}

#[test]
fn parse_binary_pattern_string_literal_with_type_is_error() {
    // BT-4143: `erlc` — "a literal string in a binary pattern must not
    // have a type or a size".
    use crate::source_analysis::Severity;
    for source in [
        "x match: [<<\"hi\"/binary, rest/binary>> -> rest]",
        "x match: [<<\"hi\":16, rest/binary>> -> rest]",
        // The string error takes precedence over the unit-without-size
        // error (verified against `erlc`).
        "x match: [<<\"hi\"/unit:8, rest/binary>> -> rest]",
    ] {
        let diagnostics = parse_err(source);
        let errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .collect();
        assert_eq!(
            errors.len(),
            1,
            "expected one error for {source}, got: {errors:?}"
        );
        assert!(
            errors[0].message.contains("must not have a type or a size"),
            "expected type-or-size error for {source}, got: {}",
            errors[0].message
        );
    }
}

#[test]
fn parse_binary_pattern_character_literal_value() {
    // BT-4143: <<$A>> — character literal values (the codepoint, like `erlc`)
    use crate::ast::Pattern;
    let module = parse_ok("x match: [<<$A, rest/binary>> -> rest]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected Expression::Match");
    };
    let Pattern::Binary { segments, .. } = &arms[0].pattern else {
        panic!("expected Pattern::Binary");
    };
    assert!(
        matches!(
            &segments[0].value,
            Pattern::Literal(Literal::Character('A'), _)
        ),
        "expected character literal, got: {:?}",
        segments[0].value
    );
}

#[test]
fn parse_binary_pattern_new_literal_misfits_warn() {
    // BT-4143: float/character literals of the wrong type warn, `erlc`-style.
    use crate::source_analysis::Severity;
    for (source, needle) in [
        (
            "x match: [<<1.5, rest/binary>> -> rest]",
            "not of the expected type 'integer'",
        ),
        (
            "x match: [<<1.5/utf16, rest/binary>> -> rest]",
            "not of the expected type 'utf16'",
        ),
        (
            "x match: [<<$A/binary, rest/binary>> -> rest]",
            "not of the expected type 'binary'",
        ),
        // `erlc` validates size × unit: 16×8 = 128 bits warns, and a
        // 300 that needs 9 bits warns against an effective 8.
        (
            "x match: [<<1.5:16/unit:8-float, rest/binary>> -> rest]",
            "bad float bit size",
        ),
        (
            "x match: [<<300:1/unit:8, rest/binary>> -> rest]",
            "will not fit",
        ),
    ] {
        let diagnostics = parse_err(source);
        let warnings: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .collect();
        assert_eq!(
            warnings.len(),
            1,
            "expected one warning for {source}, got: {warnings:?}"
        );
        assert!(
            warnings[0].message.contains(needle),
            "expected {needle} for {source}, got: {}",
            warnings[0].message
        );
    }
    for source in [
        "x match: [<<1.5/float, rest/binary>> -> rest]",
        "x match: [<<1.5:32/float, rest/binary>> -> rest]",
        // 4 units of 8 bits is a clean 32-bit float.
        "x match: [<<1.5:4/unit:8-float, rest/binary>> -> rest]",
        "x match: [<<$A, rest/binary>> -> rest]",
        "x match: [<<$A:16, rest/binary>> -> rest]",
        "x match: [<<65/utf16, rest/binary>> -> rest]",
        // 2 units of 8 bits hold 4660 (0x1234) exactly.
        "x match: [<<4660:2/unit:8, rest/binary>> -> v]",
        "x match: [<<\"hi\", rest/binary>> -> rest]",
        "x match: [<<\"hi\"/utf16, rest/binary>> -> rest]",
        "x match: [<<v:2/unit:8, rest/binary>> -> v]",
    ] {
        let module = parse_ok(source);
        assert_eq!(
            module.expressions.len(),
            1,
            "expected clean parse for {source}"
        );
    }
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

// --- field: keyword tests ---

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

// --- Type-annotated local variable assignment tests ---

#[test]
fn parse_annotated_assignment_simple_type() {
    let module = parse_ok("x :: Integer := 42");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Assignment {
            target,
            value,
            type_annotation,
            ..
        } => {
            assert!(matches!(**target, Expression::Identifier(ref id) if id.name == "x"));
            assert!(matches!(
                **value,
                Expression::Literal(Literal::Integer(42), _)
            ));
            let ann = type_annotation
                .as_ref()
                .expect("Expected type annotation on assignment");
            assert!(
                matches!(ann, TypeAnnotation::Simple(id) if id.name == "Integer"),
                "Expected Simple(Integer), got {ann:?}"
            );
        }
        other => panic!("Expected annotated assignment, got {other:?}"),
    }
}

#[test]
fn parse_annotated_assignment_parametric_type() {
    let module = parse_ok("r :: Result(Integer, Error) := computeSomething");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Assignment {
            type_annotation, ..
        } => {
            let ann = type_annotation.as_ref().expect("Expected type annotation");
            match ann {
                TypeAnnotation::Generic {
                    base, parameters, ..
                } => {
                    assert_eq!(base.name.as_str(), "Result");
                    assert_eq!(parameters.len(), 2);
                }
                other => panic!("Expected Generic, got {other:?}"),
            }
        }
        other => panic!("Expected assignment, got {other:?}"),
    }
}

#[test]
fn parse_annotated_assignment_union_type() {
    let module = parse_ok("x :: Integer | String := getValue");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Assignment {
            type_annotation, ..
        } => {
            let ann = type_annotation.as_ref().expect("Expected type annotation");
            match ann {
                TypeAnnotation::Union { types, .. } => {
                    assert_eq!(types.len(), 2);
                }
                other => panic!("Expected Union, got {other:?}"),
            }
        }
        other => panic!("Expected assignment, got {other:?}"),
    }
}

#[test]
fn parse_annotated_assignment_nil_union() {
    let module = parse_ok("name :: String | nil := dictionary at: \"name\"");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Assignment {
            type_annotation, ..
        } => {
            let ann = type_annotation.as_ref().expect("Expected type annotation");
            assert!(matches!(ann, TypeAnnotation::Union { types, .. } if types.len() == 2));
        }
        other => panic!("Expected assignment, got {other:?}"),
    }
}

#[test]
fn parse_plain_assignment_has_no_annotation() {
    let module = parse_ok("x := 42");
    match &module.expressions[0].expression {
        Expression::Assignment {
            type_annotation, ..
        } => {
            assert!(
                type_annotation.is_none(),
                "Plain assignment should have no type annotation"
            );
        }
        other => panic!("Expected assignment, got {other:?}"),
    }
}

#[test]
fn parse_annotated_assignment_missing_assign_op() {
    // `x :: Integer` without `:=` should emit error
    let diagnostics = parse_err("x :: Integer");
    assert!(
        !diagnostics.is_empty(),
        "Expected diagnostic for missing ':='"
    );
    // Verify the diagnostic mentions ':='
    assert!(
        diagnostics.iter().any(|d| d.message.contains(":=")),
        "Expected diagnostic about missing ':=', got: {diagnostics:?}"
    );
}

/// Bare `=` is not a valid Beamtalk operator: it has no entry in
/// `binary_binding_power`, so `x = y` never parses as an equality test.
/// Assert the parser reports a clear, targeted diagnostic rather than the
/// generic "expected expression" fallback.
#[test]
fn bare_equals_is_not_an_operator() {
    let diagnostics = parse_err("x = y");
    assert_eq!(diagnostics.len(), 1);
    let diag = &diagnostics[0];
    assert_eq!(diag.severity, Severity::Error);
    assert_eq!(
        diag.message,
        "Unexpected token: expected expression, found ="
    );
    assert_eq!(
        diag.hint.as_deref(),
        Some(
            "'=' is not an operator in Beamtalk; use '=:=' for value equality or '==' for reference equality"
        )
    );
}

/// Conformance test: the parser's own precedence-10 bucket and
/// [`is_equality_operator`] must agree for every operator this file's own
/// `binary_binding_power` table recognises — both known operators (whether
/// or not they're equality operators) and one unknown string. This is what
/// lets `is_equality_operator` be the single source for "is this one of the
/// four equality operators" everywhere else in the compiler (type checker
/// narrowing rules, the non-overridable-operator validator) without a
/// "mirrors the parser" comment: if a future edit ever adds an operator to
/// the precedence-10 bucket by hand instead of through
/// `is_equality_operator`, or vice versa, this test fails.
#[test]
fn equality_operator_set_matches_precedence_ten_bucket() {
    let known_operators = [
        "->", ">>", "==", "/=", "=:=", "=/=", "<", ">", "<=", ">=", "+", "-", "++", "*", "/", "%",
        "**",
    ];
    for op in known_operators {
        let bp = binary_binding_power(op)
            .unwrap_or_else(|| panic!("`{op}` should have a binding power"));
        let is_precedence_ten = bp.left == 10;
        assert_eq!(
            is_precedence_ten,
            is_equality_operator(op),
            "operator `{op}`: precedence-10 bucket ({is_precedence_ten}) and \
             is_equality_operator disagree"
        );
    }
    // An unknown operator is not an equality operator and has no binding power.
    assert!(binary_binding_power("~=").is_none());
    assert!(!is_equality_operator("~="));
}
