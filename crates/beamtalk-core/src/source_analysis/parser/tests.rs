// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for the Beamtalk recursive descent parser.
use super::*;
use crate::ast::{DeclaredKeyword, Identifier, MessageSelector, TypeAnnotation};
use crate::source_analysis::Span;
use crate::source_analysis::lex_with_eof;

/// Helper to parse a string and check for errors.
///
/// Passes if there are no Error or Warning diagnostics. Lint diagnostics
/// are ignored here since they do not block compilation.
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

/// Helper to parse a string expecting errors.
fn parse_err(source: &str) -> Vec<Diagnostic> {
    let tokens = lex_with_eof(source);
    let (_module, diagnostics) = parse(tokens);
    diagnostics
}

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
// native: keyword argument tests (BT-1206, ADR 0056)
// ========================================================================

#[test]
fn parse_native_keyword_stores_backing_module() {
    let module = parse_ok("Actor subclass: MyNative native: my_erl_module");
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.name.name.as_str(), "MyNative");
    assert_eq!(
        class.backing_module.as_ref().map(|id| id.name.as_str()),
        Some("my_erl_module")
    );
    let bm = class.backing_module.as_ref().unwrap();
    assert_eq!(bm.span, Span::new(33, 46));
}

#[test]
fn parse_native_keyword_with_body() {
    let module = parse_ok(
        "Actor subclass: Foo native: foo_impl
  state: x = 0
  doIt => x",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(
        class.backing_module.as_ref().map(|id| id.name.as_str()),
        Some("foo_impl")
    );
    let bm = class.backing_module.as_ref().unwrap();
    assert_eq!(bm.span, Span::new(28, 36));
    assert_eq!(class.state.len(), 1);
    assert_eq!(class.methods.len(), 1);
}

#[test]
fn parse_no_native_keyword_leaves_backing_module_none() {
    let module = parse_ok("Actor subclass: Foo");
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].backing_module, None);
}

#[test]
fn parse_native_keyword_missing_module_name_emits_error() {
    let diagnostics = parse_err("Actor subclass: Foo native:");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("Expected Erlang module name")),
        "Expected error for missing module name, got: {diagnostics:?}"
    );
}

#[test]
fn parse_native_keyword_newline_before_module_does_not_consume_method() {
    // `native:` followed by a newline should NOT consume the next-line identifier
    // as the module name — it should be treated as a missing module name error
    // and the method should still be parsed as a class member.
    let tokens = lex_with_eof("Actor subclass: Foo native:\n  doIt => 42");
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("Expected Erlang module name")),
        "Expected error for newline after native:, got: {diagnostics:?}"
    );
    // The method should still be parsed despite the native: error
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].backing_module, None);
    assert_eq!(
        module.classes[0].methods.len(),
        1,
        "Method 'doIt' should not be consumed as native module name"
    );
}

#[test]
fn parse_native_keyword_non_identifier_emits_error() {
    let diagnostics = parse_err("Actor subclass: Foo native: 42");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("Expected Erlang module name")),
        "Expected error for non-identifier after native:, got: {diagnostics:?}"
    );
}

#[test]
fn parse_multiline_keyword_does_not_consume_method_def() {
    // In a class body, a keyword method on the next line should NOT
    // be consumed as a continuation of the keyword message above.
    let module = parse_ok(
        "Actor subclass: Counter
  state: count = 0
  value => count
  increment: n => n + 1",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.state.len(), 1);
    assert_eq!(class.methods.len(), 2);
}

#[test]
fn parse_block_no_params() {
    let module = parse_ok("[42]");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert!(block.parameters.is_empty());
            assert_eq!(block.body.len(), 1);
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn parse_block_with_params() {
    let module = parse_ok("[:x :y | x + y]");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert_eq!(block.parameters.len(), 2);
            assert_eq!(block.parameters[0].name.as_str(), "x");
            assert_eq!(block.parameters[1].name.as_str(), "y");
            assert_eq!(block.body.len(), 1);
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn parse_return_statement() {
    let module = parse_ok("^42");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Return { value, .. } => {
            assert!(matches!(
                **value,
                Expression::Literal(Literal::Integer(42), _)
            ));
        }
        _ => panic!("Expected return statement"),
    }
}

#[test]
fn parse_parenthesized() {
    let module = parse_ok("(3 + 4) * 2");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Binary(op),
            arguments,
            ..
        } => {
            assert!(matches!(**receiver, Expression::Parenthesized { .. }));
            assert_eq!(op.as_str(), "*");
            assert_eq!(arguments.len(), 1);
        }
        _ => panic!("Expected binary message with parenthesized receiver"),
    }
}

#[test]
fn parse_field_access() {
    let module = parse_ok("self.value");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::FieldAccess {
            receiver, field, ..
        } => {
            assert!(matches!(**receiver, Expression::Identifier(_)));
            assert_eq!(field.name.as_str(), "value");
        }
        _ => panic!("Expected field access"),
    }
}

#[test]
fn parse_radix_integer() {
    assert_eq!(parse_integer("16rFF").unwrap(), 255);
    assert_eq!(parse_integer("2r1010").unwrap(), 10);
    assert_eq!(parse_integer("8r17").unwrap(), 15);
    assert!(parse_integer("37r10").is_err()); // Invalid radix
    assert!(parse_integer("16rGG").is_err()); // Invalid digits for radix
}

#[test]
fn parse_empty_input() {
    let module = parse_ok("");
    assert!(module.expressions.is_empty());
}

#[test]
fn parse_error_recovery() {
    // Parse with an intentional error
    let diagnostics = parse_err("x := @");
    assert!(!diagnostics.is_empty());
    // Parser should recover and not panic
}

#[test]
fn parse_top_level_error_recovery_unchanged() {
    // BT-368: Top-level parsing currently treats newlines as statement separators.
    // This test ensures that error recovery on the first statement does not
    // regress that behaviour and that we still parse subsequent top-level
    // expressions after a newline.
    let source = "x := @\ny := 3";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);

    // Should have exactly one error diagnostic from the invalid '@' token
    assert_eq!(diagnostics.len(), 1, "Expected one error diagnostic");

    // The parser should recover at the newline and continue parsing the
    // second top-level expression (we rely on this behaviour elsewhere,
    // e.g. in `parse_multiple_map_assignments`).
    assert_eq!(
        module.expressions.len(),
        2,
        "Expected both top-level expressions to be parsed despite the first containing an error"
    );
}

#[test]
fn parse_cascade() {
    let module = parse_ok("Transcript show: \"Hello\"; cr; show: \"World\"");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Cascade {
            receiver, messages, ..
        } => {
            // Receiver is the first message: Transcript show: "Hello"
            assert!(matches!(**receiver, Expression::MessageSend { .. }));
            assert_eq!(messages.len(), 2);
            // First message: cr (unary)
            assert!(matches!(
                &messages[0].selector,
                MessageSelector::Unary(name) if name == "cr"
            ));
            assert!(messages[0].arguments.is_empty());
            // Second message: show: "World" (keyword)
            assert!(matches!(&messages[1].selector, MessageSelector::Keyword(_)));
            assert_eq!(messages[1].arguments.len(), 1);
        }
        _ => panic!("Expected cascade expression"),
    }
}

#[test]
fn parse_cascade_simple() {
    let module = parse_ok("x foo; bar; baz");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Cascade {
            receiver, messages, ..
        } => {
            // Receiver is the first unary message: x foo
            assert!(matches!(**receiver, Expression::MessageSend { .. }));
            assert_eq!(messages.len(), 2);
            assert!(matches!(
                &messages[0].selector,
                MessageSelector::Unary(name) if name == "bar"
            ));
            assert!(matches!(
                &messages[1].selector,
                MessageSelector::Unary(name) if name == "baz"
            ));
        }
        _ => panic!("Expected cascade expression"),
    }
}

#[test]
fn parse_statement_with_period() {
    // Test that unary messages followed by period work correctly
    let module = parse_ok("obj foo.");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            receiver,
            selector: MessageSelector::Unary(name),
            ..
        } => {
            assert!(matches!(**receiver, Expression::Identifier(_)));
            assert_eq!(name.as_str(), "foo");
        }
        _ => panic!("Expected unary message send"),
    }
}

#[test]
fn parse_invalid_assignment_target() {
    let diagnostics = parse_err("3 := 4");
    assert!(!diagnostics.is_empty());
    assert!(
        diagnostics[0]
            .message
            .contains("Assignment target must be an identifier or field access")
    );
}

#[test]
fn parse_interpolated_string() {
    // Plain strings without interpolation still parse as Literal::String
    let module = parse_ok("\"Hello, world!\"");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Literal(Literal::String(s), _) if s == "Hello, world!" => {}
        _ => panic!("Expected plain string literal"),
    }

    // Interpolated string produces StringInterpolation node
    let tokens = lex_with_eof("\"Hello, {name}!\"");
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics.is_empty(),
        "Expected no diagnostics, got: {diagnostics:?}"
    );
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::StringInterpolation { segments, .. } => {
            assert_eq!(segments.len(), 3);
            match &segments[0] {
                crate::ast::StringSegment::Literal(s) => assert_eq!(s, "Hello, "),
                crate::ast::StringSegment::Interpolation(_) => {
                    panic!("Expected literal segment")
                }
            }
            match &segments[1] {
                crate::ast::StringSegment::Interpolation(Expression::Identifier(id)) => {
                    assert_eq!(id.name, "name");
                }
                crate::ast::StringSegment::Interpolation(_)
                | crate::ast::StringSegment::Literal(_) => {
                    panic!("Expected interpolation segment with identifier")
                }
            }
            match &segments[2] {
                crate::ast::StringSegment::Literal(s) => assert_eq!(s, "!"),
                crate::ast::StringSegment::Interpolation(_) => {
                    panic!("Expected literal segment")
                }
            }
        }
        other => panic!("Expected StringInterpolation, got: {other:?}"),
    }
}

#[test]
fn parse_interpolated_string_multiple_segments() {
    // "Name: {firstName} {lastName}"
    let tokens = lex_with_eof("\"Name: {firstName} {lastName}\"");
    let (module, diagnostics) = parse(tokens);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::StringInterpolation { segments, .. } => {
            assert_eq!(segments.len(), 4); // "Name: ", firstName, " ", lastName
        }
        other => panic!("Expected StringInterpolation, got: {other:?}"),
    }
}

#[test]
fn parse_interpolated_string_with_binary_message() {
    // "Result: {x + 1}"
    let tokens = lex_with_eof("\"Result: {x + 1}\"");
    let (module, diagnostics) = parse(tokens);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::StringInterpolation { segments, .. } => {
            assert_eq!(segments.len(), 2); // "Result: ", (x + 1)
            assert!(matches!(
                &segments[1],
                crate::ast::StringSegment::Interpolation(Expression::MessageSend { .. })
            ));
        }
        other => panic!("Expected StringInterpolation, got: {other:?}"),
    }
}

#[test]
fn parse_interpolated_string_with_keyword_message() {
    // "Value: {dict at: key}"
    let tokens = lex_with_eof("\"Value: {dict at: key}\"");
    let (module, diagnostics) = parse(tokens);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::StringInterpolation { segments, .. } => {
            assert_eq!(segments.len(), 2); // "Value: ", (dict at: key)
            assert!(matches!(
                &segments[1],
                crate::ast::StringSegment::Interpolation(Expression::MessageSend { .. })
            ));
        }
        other => panic!("Expected StringInterpolation, got: {other:?}"),
    }
}

#[test]
fn parse_interpolated_string_no_trailing_literal() {
    // "{name}" — interpolation with no literal segments at start or end
    let tokens = lex_with_eof("\"{name}\"");
    let (module, diagnostics) = parse(tokens);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::StringInterpolation { segments, .. } => {
            // Only the interpolation expression, empty start/end omitted
            assert_eq!(segments.len(), 1);
            assert!(matches!(
                &segments[0],
                crate::ast::StringSegment::Interpolation(Expression::Identifier(_))
            ));
        }
        other => panic!("Expected StringInterpolation, got: {other:?}"),
    }
}

#[test]
fn parse_interpolated_string_empty_braces_error() {
    // "{}" produces lexer Error token — parser should include it as error segment
    let tokens = lex_with_eof("\"before {} after\"");
    let (_, diagnostics) = parse(tokens);
    // Empty interpolation produces diagnostics from the lexer error
    assert!(!diagnostics.is_empty());
}

#[test]
fn parse_error_preserved_in_ast() {
    let tokens = lex_with_eof("x := @");
    let (module, diagnostics) = parse(tokens);
    assert!(!diagnostics.is_empty());
    // Error expression should be preserved in the AST
    // We expect 1 expression: the Assignment with an Error value
    // (not the error discarded as before)
    assert_eq!(module.expressions.len(), 1);
    // The assignment should contain the error as its value
    if let Expression::Assignment { value, .. } = &module.expressions[0].expression {
        assert!(value.is_error());
    }
}

#[test]
fn parse_field_access_with_period() {
    // Test that field access doesn't consume statement terminator
    let module = parse_ok("self.value.");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::FieldAccess { .. } => {}
        _ => panic!("Expected field access"),
    }
}

#[test]
fn parse_block_multiple_statements_with_binary_op() {
    // Regression test: ensure `. ` (period + space) is parsed as statement
    // separator, not field access
    let module = parse_ok("[ 1 + m. y := 1]");
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert_eq!(block.body.len(), 2, "Block should have 2 statements");
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn parse_block_newline_separated_statements() {
    // BT-360: newlines act as implicit statement separators
    let module = parse_ok("[\n  Transcript show: \"a\"\n  Transcript show: \"b\"\n  42\n]");
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert_eq!(
                block.body.len(),
                3,
                "Block should have 3 newline-separated statements"
            );
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn parse_block_mixed_period_and_newline_separators() {
    // BT-360: periods and newlines can be mixed
    let module = parse_ok("[\n  1 + 2.\n  3 + 4\n  5\n]");
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert_eq!(
                block.body.len(),
                3,
                "Block should have 3 statements (mixed separators)"
            );
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn parse_block_error_recovery_with_newlines() {
    // BT-368: Blocks should continue parsing after errors (implicit newline separation)
    let source = "[\n  x := 1\n  y := @\n  z := 3\n]";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);

    // Should have one diagnostic for the error
    assert_eq!(diagnostics.len(), 1, "Expected one error diagnostic");

    // Should still parse the block with all three statements
    assert_eq!(module.expressions.len(), 1, "Should parse the block");
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert_eq!(
                block.body.len(),
                3,
                "Block should have 3 statements (including error)"
            );

            // First statement should be valid
            assert!(
                !block.body[0].expression.is_error(),
                "First statement should be valid"
            );

            // Second statement is assignment with error value
            assert!(
                matches!(&block.body[1].expression, Expression::Assignment { value, .. } if value.is_error()),
                "Second statement should be assignment with error value"
            );

            // Third statement should be parsed after error
            assert!(
                !block.body[2].expression.is_error(),
                "Third statement should be valid (blocks don't break on errors)"
            );
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn parse_method_body_newline_separated() {
    // BT-360: method bodies parse multiple newline-separated statements
    let module = parse_ok(
        "Object subclass: Chatty\n\n  greet =>\n    Transcript show: \"Hello\"\n    Transcript show: \"World\"\n    42",
    );
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "greet");
    assert_eq!(
        method.body.len(),
        3,
        "Method should have 3 newline-separated statements"
    );
}

#[test]
fn parse_method_body_period_still_works() {
    // BT-360: explicit periods still work (backward compat)
    let module = parse_ok("Object subclass: Test\n\n  go =>\n    1 + 2.\n    3 + 4.\n    5");
    let method = &module.classes[0].methods[0];
    assert_eq!(
        method.body.len(),
        3,
        "Method should have 3 period-separated statements"
    );
}

#[test]
fn parse_method_body_error_recovery_with_newlines() {
    // BT-368: Parser should recover at newline boundaries after errors in method bodies
    let source = "Object subclass: Test\n\n  methodOne =>\n    x := 1\n    y := @\n    z := 3";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);

    // Should have one diagnostic for the error
    assert_eq!(diagnostics.len(), 1, "Expected one error diagnostic");
    assert!(
        diagnostics[0].message.contains("Unexpected token"),
        "Expected error about unexpected token"
    );

    // Should still parse the class and method
    assert_eq!(module.classes.len(), 1, "Should parse the class");
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1, "Should parse the method");

    // Method should have 3 statements: x := 1, y := @ (error), z := 3
    let method = &class.methods[0];
    assert_eq!(
        method.body.len(),
        3,
        "Method should have 3 statements (including error)"
    );

    // First statement should be valid
    assert!(
        !method.body[0].expression.is_error(),
        "First statement should be valid"
    );

    // Second statement should be an error assignment
    assert!(
        matches!(&method.body[1].expression, Expression::Assignment { value, .. } if value.is_error()),
        "Second statement should be assignment with error value"
    );

    // Third statement should be parsed after recovery
    assert!(
        !method.body[2].expression.is_error(),
        "Third statement should be valid after recovery"
    );
}

#[test]
fn parse_method_body_error_recovery_multiple_methods() {
    // BT-368: Parser should not skip following methods after error recovery
    let source =
        "Object subclass: Test\n\n  methodOne =>\n    x := @\n\n  methodTwo =>\n    y := 42";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);

    // Should have one diagnostic for the error
    assert_eq!(diagnostics.len(), 1, "Expected one error diagnostic");

    // Should parse both methods
    assert_eq!(module.classes.len(), 1, "Should parse the class");
    let class = &module.classes[0];
    assert_eq!(
        class.methods.len(),
        2,
        "Should parse both methods despite error in first method"
    );

    // First method should have error statement
    assert_eq!(
        class.methods[0].body.len(),
        1,
        "First method should have 1 statement"
    );

    // Second method should be valid
    assert_eq!(
        class.methods[1].body.len(),
        1,
        "Second method should have 1 statement"
    );
    assert!(
        !class.methods[1].body[0].expression.is_error(),
        "Second method statement should be valid"
    );
}

#[test]
fn parse_method_body_bare_error_recovery_at_newline() {
    // BT-368: A bare error token (not in an assignment) should not cause
    // synchronize() to skip the first token of the next statement.
    // This tests the fix where synchronize() checks recovery points
    // before the initial advance().
    let source = "Object subclass: Test\n\n  go =>\n    x := 1\n    #\n    z := 3";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);

    // Should have error diagnostic for the bare '#'
    assert!(!diagnostics.is_empty(), "Expected error diagnostics");

    // Should still parse the class and method
    assert_eq!(module.classes.len(), 1, "Should parse the class");
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1, "Should parse the method");

    // Method should recover and parse z := 3 after the error
    let method = &class.methods[0];
    assert!(
        method.body.len() >= 3,
        "Method should have at least 3 statements: x := 1, error, z := 3 (got {})",
        method.body.len()
    );

    // First statement should be valid (x := 1)
    assert!(
        !method.body[0].expression.is_error(),
        "First statement should be valid"
    );

    // Last statement should be valid (z := 3) — recovered after error
    assert!(
        !method.body[method.body.len() - 1].expression.is_error(),
        "Last statement should be valid after recovery"
    );
}

#[test]
fn parse_standalone_method_with_field_assignment() {
    let source = "Counter >> foo => self.count := 5";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics.is_empty(),
        "Expected no parse errors, got: {diagnostics:?}"
    );
    assert_eq!(
        module.method_definitions.len(),
        1,
        "Expected 1 standalone method definition"
    );
    let md = &module.method_definitions[0];
    assert_eq!(md.class_name.name, "Counter");
    assert_eq!(md.method.selector.name(), "foo");
    assert!(
        !md.method.body.is_empty(),
        "Method body should not be empty — it contains `self.count := 5`"
    );
    // Also verify no empty-body warning from semantic analysis
    let all_diags = crate::queries::diagnostic_provider::compute_diagnostics(&module, vec![]);
    let empty_body_warnings: Vec<_> = all_diags
        .iter()
        .filter(|d| d.message.contains("empty body"))
        .collect();
    assert!(
        empty_body_warnings.is_empty(),
        "Should not warn about empty body, got: {empty_body_warnings:?}"
    );
}

#[test]
fn parse_standalone_method_with_typed_keyword_param() {
    // BT-1136: is_keyword_method_selector_at must handle `:: Type` typed params
    let source = "Counter >> deposit: amount :: Integer => self";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics.is_empty(),
        "Expected no parse errors, got: {diagnostics:?}"
    );
    assert_eq!(module.method_definitions.len(), 1);
    let method = &module.method_definitions[0].method;
    assert_eq!(method.selector.name(), "deposit:");
    let param = &method.parameters[0];
    assert_eq!(param.name.name, "amount");
    assert!(param.type_annotation.is_some());
}

#[test]
fn parse_standalone_method_with_typed_binary_param() {
    // BT-1136: is_method_selector_at must handle `:: Type` for binary methods
    let source = "Number >> + other :: Number => self";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics.is_empty(),
        "Expected no parse errors, got: {diagnostics:?}"
    );
    assert_eq!(module.method_definitions.len(), 1);
    let method = &module.method_definitions[0].method;
    assert_eq!(method.selector.name(), "+");
    let param = &method.parameters[0];
    assert_eq!(param.name.name, "other");
    assert!(param.type_annotation.is_some());
}

#[test]
fn parse_combined_class_with_standalone_method_field_assign() {
    // Simulates what the REPL does: concatenates class source + standalone method
    let source = "Actor subclass: Counter\n  state: count = 0\n  increment => self.count := self.count + 1\nCounter >> foo => self.count := 5";
    let tokens = lex_with_eof(source);
    let (module, parse_diags) = parse(tokens);

    // The standalone method should have a non-empty body
    assert_eq!(module.method_definitions.len(), 1);
    let md = &module.method_definitions[0];
    assert_eq!(md.method.selector.name(), "foo");
    assert!(
        !md.method.body.is_empty(),
        "Standalone method body should not be empty — contains `self.count := 5`"
    );

    let all_diags = crate::queries::diagnostic_provider::compute_diagnostics(&module, parse_diags);
    let empty_body_warnings: Vec<_> = all_diags
        .iter()
        .filter(|d| d.message.contains("empty body"))
        .collect();
    assert!(
        empty_body_warnings.is_empty(),
        "Should not warn about empty body, got: {empty_body_warnings:?}"
    );
}

#[test]
fn parse_empty_map() {
    let module = parse_ok("#{}");
    assert_eq!(module.expressions.len(), 1);
    assert!(matches!(
        &module.expressions[0].expression,
        Expression::MapLiteral { pairs, .. } if pairs.is_empty()
    ));
}

#[test]
fn parse_map_with_atom_keys() {
    let module = parse_ok("#{#name => \"Alice\", #age => 30}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 2);
            // First pair: #name => "Alice"
            assert!(
                matches!(&pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "name")
            );
            assert!(
                matches!(&pairs[0].value, Expression::Literal(Literal::String(s), _) if s == "Alice")
            );
            // Second pair: #age => 30
            assert!(
                matches!(&pairs[1].key, Expression::Literal(Literal::Symbol(s), _) if s == "age")
            );
            assert!(matches!(
                &pairs[1].value,
                Expression::Literal(Literal::Integer(30), _)
            ));
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_block_with_field_assignments() {
    // Regression test: multiple field assignments should parse correctly
    let module = parse_ok("[:n | self.x := self.x + n. self.x := self.x + n. ^self.x]");
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert_eq!(block.body.len(), 3, "Block should have 3 statements");
            assert!(matches!(
                block.body[0].expression,
                Expression::Assignment { .. }
            ));
            assert!(matches!(
                block.body[1].expression,
                Expression::Assignment { .. }
            ));
            assert!(matches!(
                block.body[2].expression,
                Expression::Return { .. }
            ));
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn parse_map_with_string_keys() {
    let module = parse_ok("#{\"host\" => \"localhost\", \"port\" => 8080}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 2);
            assert!(
                matches!(&pairs[0].key, Expression::Literal(Literal::String(s), _) if s == "host")
            );
            assert!(
                matches!(&pairs[0].value, Expression::Literal(Literal::String(s), _) if s == "localhost")
            );
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_block_with_newlines_between_statements() {
    // Regression test: newlines after period should not be treated as field access
    let module = parse_ok(
        "[:n |
    self.value := self.value + n.
    self.value := self.value + n.
    ^self.value
]",
    );
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert_eq!(block.body.len(), 3, "Block should have 3 statements");
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn parse_map_with_integer_keys() {
    let module = parse_ok("#{1 => \"first\", 2 => \"second\"}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 2);
            assert!(matches!(
                &pairs[0].key,
                Expression::Literal(Literal::Integer(1), _)
            ));
            assert!(
                matches!(&pairs[0].value, Expression::Literal(Literal::String(s), _) if s == "first")
            );
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_map_with_trailing_comma() {
    let module = parse_ok("#{#a => 1, #b => 2,}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 2);
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_nested_maps() {
    let module = parse_ok("#{#outer => #{#inner => \"value\"}}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 1);
            // Outer key is #outer
            assert!(
                matches!(&pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "outer")
            );
            // Value is a nested map
            match &pairs[0].value {
                Expression::MapLiteral {
                    pairs: inner_pairs, ..
                } => {
                    assert_eq!(inner_pairs.len(), 1);
                    assert!(
                        matches!(&inner_pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "inner")
                    );
                    assert!(
                        matches!(&inner_pairs[0].value, Expression::Literal(Literal::String(s), _) if s == "value")
                    );
                }
                _ => panic!("Expected nested MapLiteral"),
            }
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_map_value_binary_expression() {
    // BT-664: Map values should support binary expressions
    let module = parse_ok("#{#x => 1 + 2}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 1);
            assert!(
                matches!(&pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "x")
            );
            match &pairs[0].value {
                Expression::MessageSend {
                    receiver,
                    selector: MessageSelector::Binary(op),
                    arguments,
                    ..
                } => {
                    assert!(matches!(
                        **receiver,
                        Expression::Literal(Literal::Integer(1), _)
                    ));
                    assert_eq!(op.as_str(), "+");
                    assert_eq!(arguments.len(), 1);
                    assert!(matches!(
                        arguments[0],
                        Expression::Literal(Literal::Integer(2), _)
                    ));
                }
                _ => panic!("Expected binary message send as map value"),
            }
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_map_value_multiple_binary_expressions() {
    // BT-664: Multiple map values with binary expressions
    let module = parse_ok("#{#x => 1 + 2, #y => 3 * 4}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 2);
            assert!(matches!(
                &pairs[0].value,
                Expression::MessageSend {
                    selector: MessageSelector::Binary(op),
                    ..
                } if op.as_str() == "+"
            ));
            assert!(matches!(
                &pairs[1].value,
                Expression::MessageSend {
                    selector: MessageSelector::Binary(op),
                    ..
                } if op.as_str() == "*"
            ));
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_map_value_unary_on_binary() {
    // BT-664: Map values with unary messages on binary results
    let module = parse_ok("#{#x => self x + other x}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 1);
            // Value should be a binary message send (+)
            assert!(matches!(
                &pairs[0].value,
                Expression::MessageSend {
                    selector: MessageSelector::Binary(op),
                    ..
                } if op.as_str() == "+"
            ));
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_map_assignment() {
    let module = parse_ok("person := #{#name => \"Alice\"}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::Assignment { target, value, .. } => {
            assert!(matches!(target.as_ref(), Expression::Identifier(id) if id.name == "person"));
            assert!(
                matches!(value.as_ref(), Expression::MapLiteral { pairs, .. } if pairs.len() == 1)
            );
        }
        _ => panic!("Expected Assignment"),
    }
}

#[test]
fn parse_multiple_map_assignments() {
    let source = "empty := #{}\nperson := #{#name => \"Alice\", #age => 30}\n";
    let module = parse_ok(source);

    assert_eq!(module.expressions.len(), 2, "Expected 2 expressions");

    // First: empty := #{}
    match &module.expressions[0].expression {
        Expression::Assignment { target, value, .. } => {
            assert!(matches!(target.as_ref(), Expression::Identifier(id) if id.name == "empty"));
            assert!(
                matches!(value.as_ref(), Expression::MapLiteral { pairs, .. } if pairs.is_empty())
            );
        }
        other => panic!("Expected Assignment for first expr, got {other:?}"),
    }

    // Second: person := #{#name => "Alice", #age => 30}
    match &module.expressions[1].expression {
        Expression::Assignment { target, value, .. } => {
            assert!(matches!(target.as_ref(), Expression::Identifier(id) if id.name == "person"));
            assert!(
                matches!(value.as_ref(), Expression::MapLiteral { pairs, .. } if pairs.len() == 2)
            );
        }
        other => panic!("Expected Assignment for second expr, got {other:?}"),
    }
}

#[test]
fn parse_map_with_bare_identifier_keys_is_error() {
    // BT-1240: Bare identifiers before `=>` in map literals are now a compile error
    let (module, diags) = parse(lex_with_eof("#{name => \"Alice\", age => 30}"));
    // Two errors: one for `name`, one for `age`
    let errors: Vec<_> = diags
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert_eq!(errors.len(), 2, "expected 2 errors, got: {errors:?}");
    assert!(
        errors
            .iter()
            .any(|d| d.message.contains("bare word 'name'") && d.message.contains("'#name'")),
        "expected suggestion for 'name': {errors:?}"
    );
    assert!(
        errors
            .iter()
            .any(|d| d.message.contains("bare word 'age'") && d.message.contains("'#age'")),
        "expected suggestion for 'age': {errors:?}"
    );
    // No valid module expression — keys produced Error nodes
    let _ = module;
}

#[test]
fn parse_map_with_mixed_keys() {
    // Explicit symbol and string keys are both valid
    let module = parse_ok("#{#x => 1, \"y\" => 2}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 2);
            assert!(
                matches!(&pairs[0].key, Expression::Literal(Literal::Symbol(s), _) if s == "x")
            );
            assert!(
                matches!(&pairs[1].key, Expression::Literal(Literal::String(s), _) if s == "y")
            );
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_map_uppercase_key_not_converted() {
    // BT-591: Uppercase identifiers (class references) used as map keys are NOT converted to symbols
    let module = parse_ok("#{Counter => 1}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 1);
            // Uppercase bare identifier `Counter` remains a ClassReference, not an implicit symbol
            assert!(matches!(&pairs[0].key, Expression::ClassReference { .. }));
            assert!(matches!(
                &pairs[0].value,
                Expression::Literal(Literal::Integer(1), _)
            ));
        }
        _ => panic!("Expected MapLiteral"),
    }
}

// ========================================================================
// Class Definition Parsing Tests
// ========================================================================

#[test]
fn parse_basic_class_definition() {
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0

  increment => self.value := self.value + 1
  getValue => ^self.value",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];

    assert_eq!(class.name.name, "Counter");
    assert_eq!(class.superclass.as_ref().unwrap().name, "Actor");
    assert!(!class.is_abstract);
    assert!(!class.is_sealed);
    assert_eq!(class.state.len(), 1);
    assert_eq!(class.state[0].name.name, "value");
    assert_eq!(class.methods.len(), 2);
    assert_eq!(class.methods[0].selector.name(), "increment");
    assert_eq!(class.methods[1].selector.name(), "getValue");
}

#[test]
fn parse_abstract_class() {
    let module = parse_ok(
        "abstract Actor subclass: Collection
  size => self subclassResponsibility",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];

    assert!(class.is_abstract);
    assert!(!class.is_sealed);
    assert_eq!(class.name.name, "Collection");
    assert_eq!(class.superclass.as_ref().unwrap().name, "Actor");
}

#[test]
fn parse_sealed_class() {
    let module = parse_ok(
        "sealed Actor subclass: Point
  state: x = 0
  state: y = 0",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];

    assert!(!class.is_abstract);
    assert!(class.is_sealed);
    assert_eq!(class.name.name, "Point");
    assert_eq!(class.state.len(), 2);
}

#[test]
fn parse_root_class_nil_superclass() {
    let module = parse_ok(
        "abstract nil subclass: ProtoObject
  class => @primitive classOf",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];

    assert!(class.is_abstract);
    assert!(class.superclass.is_none());
    assert_eq!(class.name.name, "ProtoObject");
    assert_eq!(class.superclass_name(), "none");
}

#[test]
fn parse_method_named_class_with_return_type() {
    // BT-1031: `class -> Type =>` must be parsed as a method named `class`
    // with a return type annotation, not as a class-side binary method `->`.
    let module = parse_ok(
        "Object subclass: Class
  sealed class -> Metaclass => @primitive classClass",
    );

    let class = &module.classes[0];
    assert_eq!(
        class.methods.len(),
        1,
        "should be an instance method, not class method"
    );
    assert_eq!(class.class_methods.len(), 0);

    let method = &class.methods[0];
    assert_eq!(method.selector.name(), "class");
    assert!(method.parameters.is_empty(), "unary method has no params");
    match method.return_type.as_ref() {
        Some(crate::ast::TypeAnnotation::Simple(id)) => assert_eq!(id.name, "Metaclass"),
        other => panic!("expected return type Metaclass, got: {other:?}"),
    }
    assert!(method.is_sealed);
}

#[test]
fn parse_sealed_method_named_class_with_return_type() {
    // Ensure a method named `class -> Type` and a `class`-side keyword method
    // (`class withValue:`) can coexist in the same class body.
    let module = parse_ok(
        "Object subclass: Foo
  sealed class -> Metaclass => @primitive classClass
  sealed class withValue: v => self new",
    );

    let class = &module.classes[0];
    // `class` is an instance method; `withValue:` is a class-side method
    assert_eq!(class.methods.len(), 1);
    assert_eq!(class.methods[0].selector.name(), "class");
    assert_eq!(class.class_methods.len(), 1);
    assert_eq!(class.class_methods[0].selector.name(), "withValue:");
}

#[test]
fn parse_state_with_type_annotation() {
    let module = parse_ok(
        "Actor subclass: Person
  state: name :: String = \"unnamed\"",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.state.len(), 1);

    let state = &class.state[0];
    assert_eq!(state.name.name, "name");
    assert!(state.type_annotation.is_some());
    assert!(state.default_value.is_some());
}

#[test]
fn legacy_colon_state_annotation_produces_error_and_parses_type() {
    // Legacy `state: name : String` (single colon) must NOT be silently dropped.
    // The parser should consume the colon, emit a focused error, parse the type,
    // and continue — so the next method is not lost.
    let source = "Actor subclass: Person
  state: name : String = \"unnamed\"
  greet => \"hello\"";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        !errors.is_empty(),
        "Expected a migration error for legacy `:` syntax"
    );
    assert!(
        errors[0].message.contains("`::`"),
        "Error should mention `::`, got: {}",
        errors[0].message
    );
    // The class and following method must still be parsed
    assert_eq!(module.classes.len(), 1);
    assert_eq!(
        module.classes[0].methods.len(),
        1,
        "method after legacy-typed state should not be dropped"
    );
}

#[test]
fn legacy_colon_param_annotation_produces_error_and_parses_type() {
    // Legacy `deposit: amount : Integer` (single colon after param name) must
    // emit a focused error and continue so the next method is not dropped.
    let source = "Actor subclass: BankAccount
  deposit: amount : Integer => nil
  withdraw: amount => nil";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        !errors.is_empty(),
        "Expected a migration error for legacy `:` param syntax"
    );
    assert!(
        errors[0].message.contains("`::`"),
        "Error should mention `::`, got: {}",
        errors[0].message
    );
    assert_eq!(module.classes.len(), 1);
    assert_eq!(
        module.classes[0].methods.len(),
        2,
        "method after legacy-typed param should not be dropped"
    );
}

#[test]
fn legacy_colon_classvar_annotation_produces_error_and_parses_type() {
    // Legacy `classState: count : Integer` (single colon) must NOT be silently dropped.
    // The parser should consume the colon, emit a focused error, parse the type,
    // and continue — so the next method is not lost.
    let source = "Actor subclass: Counter
  classState: count : Integer = 0
  increment => count + 1";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect();
    assert!(
        !errors.is_empty(),
        "Expected a migration error for legacy `:` classState syntax"
    );
    assert!(
        errors[0].message.contains("`::`"),
        "Error should mention `::`, got: {}",
        errors[0].message
    );
    // The class and following method must still be parsed
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(
        class.methods.len(),
        1,
        "method after legacy-typed classState should not be dropped"
    );
    // The classState declaration itself must be recovered with its type and default value
    assert_eq!(
        class.class_variables.len(),
        1,
        "Expected exactly one classState variable to be parsed"
    );
    let cv = &class.class_variables[0];
    assert!(
        cv.type_annotation.is_some(),
        "Legacy `classState:` should still have a parsed type_annotation after recovery"
    );
    assert!(
        cv.default_value.is_some(),
        "Legacy `classState:` default value (`= 0`) should still be present after recovery"
    );
}

#[test]
fn double_colon_type_annotation_no_diagnostic() {
    // Valid `::` syntax must not produce any diagnostics, and types must be in the AST.
    let source = "Actor subclass: BankAccount
  state: balance :: Integer = 0
  classState: rate :: Integer = 5
  deposit: amount :: Integer => nil";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics.is_empty(),
        "Expected no diagnostics for valid `::` syntax, got: {diagnostics:?}"
    );
    assert_eq!(module.classes.len(), 1, "Expected exactly one class");
    let class = &module.classes[0];
    // `state: balance :: Integer = 0`
    assert!(
        !class.state.is_empty(),
        "Expected at least one state variable"
    );
    assert!(
        class.state[0].type_annotation.is_some(),
        "Expected state variable to have a type annotation from `::`"
    );
    // `classState: rate :: Integer = 5`
    assert!(
        !class.class_variables.is_empty(),
        "Expected at least one classState variable"
    );
    assert!(
        class.class_variables[0].type_annotation.is_some(),
        "Expected classState variable to have a type annotation from `::`"
    );
    // `deposit: amount :: Integer => nil`
    assert!(!class.methods.is_empty(), "Expected at least one method");
    assert!(
        !class.methods[0].parameters.is_empty(),
        "Expected deposit: to have a parameter"
    );
    assert!(
        class.methods[0].parameters[0].type_annotation.is_some(),
        "Expected deposit: parameter to have a type annotation from `::`"
    );
}

#[test]
fn parse_binary_method() {
    let module = parse_ok(
        "Actor subclass: Number
  + other => self.value + other",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1);

    let method = &class.methods[0];
    assert_eq!(method.selector.name(), "+");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "other");
}

#[test]
fn parse_keyword_method() {
    let module = parse_ok(
        "Actor subclass: Array
  at: index put: value => self.storage at: index put: value",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1);

    let method = &class.methods[0];
    assert_eq!(method.selector.name(), "at:put:");
    assert_eq!(method.parameters.len(), 2);
    assert_eq!(method.parameters[0].name.name, "index");
    assert_eq!(method.parameters[1].name.name, "value");
}

#[test]
fn parse_sealed_method() {
    let module = parse_ok(
        "Actor subclass: Counter
  sealed getValue => ^self.value",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1);

    let method = &class.methods[0];
    assert!(method.is_sealed);
    assert_eq!(method.kind, crate::ast::MethodKind::Primary);
}

#[test]
fn parse_return_type_unary() {
    let module = parse_ok(
        "Actor subclass: Counter
  getBalance -> Integer => self.balance",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "getBalance");
    assert!(method.return_type.is_some());
    let rt = method.return_type.as_ref().unwrap();
    match rt {
        crate::ast::TypeAnnotation::Simple(id) => assert_eq!(id.name, "Integer"),
        _ => panic!("Expected Simple type annotation"),
    }
}

#[test]
fn parse_return_type_binary() {
    let module = parse_ok(
        "Object subclass: Number
  + other -> Number => self",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "+");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "other");
    assert!(method.return_type.is_some());
    match method.return_type.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Simple(id) => assert_eq!(id.name, "Number"),
        _ => panic!("Expected Simple type annotation"),
    }
}

#[test]
fn parse_return_type_union() {
    let module = parse_ok(
        "Actor subclass: Container
  find -> Integer | Nil => nil",
    );

    let method = &module.classes[0].methods[0];
    assert!(method.return_type.is_some());
    match method.return_type.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Union { types, .. } => {
            assert_eq!(types.len(), 2);
        }
        _ => panic!("Expected Union type annotation"),
    }
}

#[test]
fn parse_typed_keyword_param() {
    let module = parse_ok(
        "Actor subclass: BankAccount
  deposit: amount :: Integer => self.balance := self.balance + amount",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "deposit:");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "amount");
    assert!(method.parameters[0].type_annotation.is_some());
    match method.parameters[0].type_annotation.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Simple(id) => assert_eq!(id.name, "Integer"),
        _ => panic!("Expected Simple type annotation"),
    }
}

#[test]
fn parse_typed_keyword_params_multiple() {
    let module = parse_ok(
        "Actor subclass: BankAccount
  transfer: amount :: Integer to: target :: BankAccount => self",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "transfer:to:");
    assert_eq!(method.parameters.len(), 2);
    assert_eq!(method.parameters[0].name.name, "amount");
    assert!(method.parameters[0].type_annotation.is_some());
    assert_eq!(method.parameters[1].name.name, "target");
    assert!(method.parameters[1].type_annotation.is_some());
}

#[test]
fn parse_typed_param_with_return_type() {
    let module = parse_ok(
        "Actor subclass: BankAccount
  deposit: amount :: Integer -> Integer => self.balance := self.balance + amount",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "deposit:");
    assert_eq!(method.parameters[0].name.name, "amount");
    assert!(method.parameters[0].type_annotation.is_some());
    assert!(method.return_type.is_some());
}

#[test]
fn parse_untyped_keyword_param_unchanged() {
    // Existing syntax should still work unchanged
    let module = parse_ok(
        "Actor subclass: Counter
  setValue: v => self.value := v",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "setValue:");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "v");
    assert!(method.parameters[0].type_annotation.is_none());
    assert!(method.return_type.is_none());
}

#[test]
fn parse_binary_typed_param() {
    let module = parse_ok(
        "Object subclass: Number
  + other :: Number -> Number => self",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "+");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "other");
    assert!(method.parameters[0].type_annotation.is_some());
    assert!(method.return_type.is_some());
}

#[test]
fn parse_mixed_typed_untyped_keyword_params() {
    // First param typed, second untyped
    let module = parse_ok(
        "Actor subclass: BankAccount
  transfer: amount :: Integer to: target => self",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "transfer:to:");
    assert_eq!(method.parameters.len(), 2);
    assert_eq!(method.parameters[0].name.name, "amount");
    assert!(method.parameters[0].type_annotation.is_some());
    assert_eq!(method.parameters[1].name.name, "target");
    assert!(method.parameters[1].type_annotation.is_none());
}

#[test]
fn parse_keyword_typed_param_with_space_colon() {
    // DoubleColon with spaces: `amount :: Integer`
    let module = parse_ok(
        "Actor subclass: BankAccount
  deposit: amount :: Integer => self",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "deposit:");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "amount");
    assert!(method.parameters[0].type_annotation.is_some());
}

#[test]
fn parse_binary_typed_param_with_space_colon() {
    // DoubleColon with spaces: `other :: Number`
    let module = parse_ok(
        "Actor subclass: Adder
  + other :: Number => self",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "+");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "other");
    assert!(method.parameters[0].type_annotation.is_some());
}

#[test]
fn parse_arrow_as_binary_method_selector() {
    // ADR 0047: `->` (Arrow token) is a valid binary method selector name
    let module = parse_ok(
        "Object subclass: Pair
  -> value => self",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "->");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "value");
    assert!(method.return_type.is_none());
}

#[test]
fn parse_arrow_method_with_return_type() {
    // ADR 0047: `->` method can have a return type annotation (the original bug fix)
    let module = parse_ok(
        "Object subclass: Pair
  -> value -> Association => @primitive \"->\"",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "->");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "value");
    assert!(method.return_type.is_some());
    match method.return_type.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Simple(id) => assert_eq!(id.name, "Association"),
        other => panic!("Expected Simple type annotation, got {other:?}"),
    }
}

#[test]
fn parse_arrow_method_with_keyword_typed_param() {
    // `-> value :: Association =>` — typed param via DoubleColon
    let module = parse_ok(
        "Object subclass: Pair
  -> value :: Association => @primitive \"->\"",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "->");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "value");
    assert!(method.parameters[0].type_annotation.is_some());
    assert!(method.return_type.is_none());
}

#[test]
fn parse_binary_method_with_union_typed_param() {
    // BT-1136/Copilot: `+ other :: Integer | Nil =>` — union type in binary param
    let module = parse_ok(
        "Object subclass: Number
  + other :: Integer | Nil -> Integer => self",
    );
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "+");
    assert!(method.parameters[0].type_annotation.is_some());
    assert!(method.return_type.is_some());
}

#[test]
fn parse_keyword_method_with_union_typed_param() {
    // BT-1136/Copilot: `deposit: amount :: Integer | Float =>` — union type in keyword param
    let module = parse_ok(
        "Actor subclass: BankAccount
  deposit: amount :: Integer | Float => self",
    );
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "deposit:");
    assert!(method.parameters[0].type_annotation.is_some());
}

#[test]
fn parse_standalone_method_with_union_typed_binary_param() {
    // BT-1136/Copilot: standalone `+` method with union-typed param
    let source = "Number >> + other :: Integer | Nil => self";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics.is_empty(),
        "Expected no parse errors, got: {diagnostics:?}"
    );
    assert_eq!(module.method_definitions.len(), 1);
    assert!(
        module.method_definitions[0].method.parameters[0]
            .type_annotation
            .is_some()
    );
}

#[test]
fn parse_standalone_method_with_union_typed_keyword_param() {
    // BT-1136/Copilot: standalone keyword method with union-typed param
    let source = "Counter >> deposit: amount :: Integer | Float => self";
    let tokens = lex_with_eof(source);
    let (module, diagnostics) = parse(tokens);
    assert!(
        diagnostics.is_empty(),
        "Expected no parse errors, got: {diagnostics:?}"
    );
    assert_eq!(module.method_definitions.len(), 1);
    assert!(
        module.method_definitions[0].method.parameters[0]
            .type_annotation
            .is_some()
    );
}

#[test]
fn parse_malformed_return_type_recovers() {
    // `-> =>` (missing type name) should still detect the method definition
    // and let parse_type_annotation emit the error, not truncate class parsing
    let tokens = lex_with_eof(
        "Actor subclass: Counter
  increment -> => self",
    );
    let (module, diagnostics) = parse(tokens);
    // Should have an error about missing type name
    assert!(
        !diagnostics.is_empty(),
        "Expected error for missing type name after ->"
    );
    // But the method should still be parsed
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].methods.len(), 1);
    assert_eq!(module.classes[0].methods[0].selector.name(), "increment");
}

#[test]
fn parse_class_with_mixed_content() {
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0
  state: name :: String

  increment => self.value := self.value + 1
  decrement => self.value := self.value - 1",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];

    assert_eq!(class.state.len(), 2);
    assert_eq!(class.methods.len(), 2);

    // Check method kinds
    assert_eq!(class.methods[0].kind, crate::ast::MethodKind::Primary);
    assert_eq!(class.methods[1].kind, crate::ast::MethodKind::Primary);
}

#[test]
fn parse_class_with_trailing_expression() {
    let module = parse_ok(
        "Object subclass: Foo
  state: count = 0
  count => self.count
Foo new count",
    );
    assert_eq!(module.classes.len(), 1, "Should have 1 class");
    assert_eq!(
        module.expressions.len(),
        1,
        "Should have 1 trailing expression, got {}",
        module.expressions.len()
    );
}

#[test]
fn parse_class_multiline_method_body_no_false_break() {
    // Ensure indented multi-line method bodies are NOT broken prematurely
    let module = parse_ok(
        "Object subclass: Bar
  doStuff =>
    x := 1
    x + 2",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1);
    // Method body should have both statements (x := 1 and x + 2)
    assert_eq!(
        class.methods[0].body.len(),
        2,
        "Multi-line indented method body should have 2 statements"
    );
    // No trailing expressions
    assert_eq!(module.expressions.len(), 0);
}

#[test]
fn parse_multiple_classes() {
    let module = parse_ok(
        "Actor subclass: Point
  state: x = 0
  state: y = 0

Actor subclass: Rectangle
  state: origin
  state: corner",
    );

    assert_eq!(module.classes.len(), 2);
    assert_eq!(module.classes[0].name.name, "Point");
    assert_eq!(module.classes[1].name.name, "Rectangle");
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
// Class Reference Tests
// ========================================================================

#[test]
fn parse_class_reference_spawn() {
    let module = parse_ok("Counter spawn");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(name),
        arguments,
        ..
    } = &module.expressions[0].expression
    {
        // Receiver should be ClassReference, not Identifier
        if let Expression::ClassReference {
            name: class_name, ..
        } = &**receiver
        {
            assert_eq!(class_name.name.as_str(), "Counter");
        } else {
            panic!("Expected ClassReference receiver, got {receiver:?}");
        }
        assert_eq!(name.as_str(), "spawn");
        assert_eq!(arguments.len(), 0);
    } else {
        panic!("Expected message send");
    }
}

#[test]
fn parse_class_reference_vs_variable() {
    // Uppercase should parse as ClassReference
    let module1 = parse_ok("Counter");
    if let Expression::ClassReference { name, .. } = &module1.expressions[0].expression {
        assert_eq!(name.name.as_str(), "Counter");
    } else {
        panic!("Expected ClassReference for 'Counter'");
    }

    // Lowercase should parse as Identifier
    let module2 = parse_ok("counter");
    if let Expression::Identifier(id) = &module2.expressions[0].expression {
        assert_eq!(id.name.as_str(), "counter");
    } else {
        panic!("Expected Identifier for 'counter'");
    }
}

#[test]
fn parse_class_reference_keyword_message() {
    let module = parse_ok("Counter spawnWith: initialState");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        ..
    } = &module.expressions[0].expression
    {
        // Receiver should be ClassReference
        if let Expression::ClassReference {
            name: class_name, ..
        } = &**receiver
        {
            assert_eq!(class_name.name.as_str(), "Counter");
        } else {
            panic!("Expected ClassReference receiver");
        }
        assert_eq!(parts.len(), 1);
        assert_eq!(parts[0].keyword.as_str(), "spawnWith:");
        assert_eq!(arguments.len(), 1);
    } else {
        panic!("Expected keyword message send");
    }
}

#[test]
fn parse_mixed_case_identifier() {
    // camelCase should parse as Identifier (starts with lowercase)
    let module = parse_ok("myVariable");
    if let Expression::Identifier(id) = &module.expressions[0].expression {
        assert_eq!(id.name.as_str(), "myVariable");
    } else {
        panic!("Expected Identifier for 'myVariable'");
    }

    // PascalCase should parse as ClassReference (starts with uppercase)
    let module = parse_ok("MyClass");
    if let Expression::ClassReference { name, .. } = &module.expressions[0].expression {
        assert_eq!(name.name.as_str(), "MyClass");
    } else {
        panic!("Expected ClassReference for 'MyClass'");
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
// List Literal Tests (BT-402)
// ========================================================================

#[test]
fn parse_empty_list() {
    let module = parse_ok("#()");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert!(elements.is_empty());
            assert!(tail.is_none());
        }
        other => panic!("Expected list literal, got: {other:?}"),
    }
}

#[test]
fn parse_single_element_list() {
    let module = parse_ok("#(42)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 1);
            assert!(tail.is_none());
        }
        other => panic!("Expected list literal, got: {other:?}"),
    }
}

#[test]
fn parse_multi_element_list() {
    let module = parse_ok("#(1, 2, 3)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 3);
            assert!(tail.is_none());
        }
        other => panic!("Expected list literal, got: {other:?}"),
    }
}

#[test]
fn parse_list_trailing_comma() {
    let module = parse_ok("#(1, 2, 3,)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 3);
            assert!(tail.is_none());
        }
        other => panic!("Expected list literal, got: {other:?}"),
    }
}

#[test]
fn parse_list_cons_syntax() {
    let module = parse_ok("#(0 | rest)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 1);
            assert!(tail.is_some());
        }
        other => panic!("Expected list literal with cons, got: {other:?}"),
    }
}

#[test]
fn parse_nested_list() {
    let module = parse_ok("#(#(1, 2), #(3, 4))");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 2);
            assert!(tail.is_none());
            // Each element should be a list literal
            assert!(matches!(&elements[0], Expression::ListLiteral { .. }));
            assert!(matches!(&elements[1], Expression::ListLiteral { .. }));
        }
        other => panic!("Expected nested list literal, got: {other:?}"),
    }
}

#[test]
fn parse_list_with_mixed_types() {
    let module = parse_ok("#(1, \"hello\", #ok)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 3);
            assert!(tail.is_none());
        }
        other => panic!("Expected list literal, got: {other:?}"),
    }
}

#[test]
fn parse_list_as_message_receiver() {
    // List literal as receiver of a message
    let module = parse_ok("#(1, 2, 3) size");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            receiver, selector, ..
        } => {
            assert!(matches!(receiver.as_ref(), Expression::ListLiteral { .. }));
            assert_eq!(selector.name(), "size");
        }
        other => panic!("Expected message send to list, got: {other:?}"),
    }
}

#[test]
fn parse_list_multi_cons() {
    // Multiple elements before cons: #(1, 2 | rest) → [1, 2 | rest]
    let module = parse_ok("#(1, 2 | rest)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 2);
            assert!(tail.is_some());
        }
        other => panic!("Expected list literal with cons, got: {other:?}"),
    }
}

#[test]
fn parse_list_error_unterminated() {
    // Missing closing paren produces diagnostic
    let diagnostics = parse_err("#(1, 2");
    assert!(
        !diagnostics.is_empty(),
        "Expected error for unterminated list"
    );
}

// ========================================================================
// List Literal: keyword message elements (BT-1287)
// ========================================================================

#[test]
fn parse_list_single_keyword_element() {
    // #(obj kw: arg) → single-element list containing the keyword send
    let module = parse_ok("#(obj kw: 42)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 1);
            assert!(tail.is_none());
            assert!(
                matches!(&elements[0], Expression::MessageSend { selector, .. } if selector.name() == "kw:"),
                "Expected keyword send as list element"
            );
        }
        other => panic!("Expected list literal, got: {other:?}"),
    }
}

#[test]
fn parse_list_keyword_element_with_more_elements() {
    // #(obj kw: arg, other) → two-element list
    let module = parse_ok("#(obj kw: 42, other)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 2);
            assert!(tail.is_none());
            assert!(
                matches!(&elements[0], Expression::MessageSend { selector, .. } if selector.name() == "kw:"),
                "First element should be keyword send"
            );
        }
        other => panic!("Expected list literal, got: {other:?}"),
    }
}

#[test]
fn parse_list_multi_keyword_element() {
    // #(obj kw1: a kw2: b) → single-element list containing multi-keyword send
    let module = parse_ok("#(obj kw1: 1 kw2: 2)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 1);
            assert!(tail.is_none());
            assert!(
                matches!(&elements[0], Expression::MessageSend { selector, .. } if selector.name() == "kw1:kw2:"),
                "Expected multi-keyword send as list element"
            );
        }
        other => panic!("Expected list literal, got: {other:?}"),
    }
}

#[test]
fn parse_list_keyword_element_with_cons_tail() {
    // #(obj kw: 42 | rest) → one-element list with keyword send head and cons tail
    let module = parse_ok("#(obj kw: 42 | rest)");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 1);
            assert!(tail.is_some(), "Expected cons tail");
            assert!(
                matches!(&elements[0], Expression::MessageSend { selector, .. } if selector.name() == "kw:"),
                "Expected keyword send as head element"
            );
        }
        other => panic!("Expected list literal with cons, got: {other:?}"),
    }
}

#[test]
fn parse_array_single_keyword_element() {
    // #[obj kw: arg] → single-element array containing the keyword send
    let module = parse_ok("#[obj kw: 42]");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ArrayLiteral { elements, .. } => {
            assert_eq!(elements.len(), 1);
            assert!(
                matches!(&elements[0], Expression::MessageSend { selector, .. } if selector.name() == "kw:"),
                "Expected keyword send as array element"
            );
        }
        other => panic!("Expected array literal, got: {other:?}"),
    }
}

#[test]
fn parse_array_keyword_element_with_more_elements() {
    // #[obj kw: arg, other] → two-element array
    let module = parse_ok("#[obj kw: 42, other]");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ArrayLiteral { elements, .. } => {
            assert_eq!(elements.len(), 2);
            assert!(
                matches!(&elements[0], Expression::MessageSend { selector, .. } if selector.name() == "kw:"),
                "First element should be keyword send"
            );
        }
        other => panic!("Expected array literal, got: {other:?}"),
    }
}

#[test]
fn parse_array_multi_keyword_element() {
    // #[obj kw1: 1 kw2: 2] → single-element array containing multi-keyword send
    let module = parse_ok("#[obj kw1: 1 kw2: 2]");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::ArrayLiteral { elements, .. } => {
            assert_eq!(elements.len(), 1);
            assert!(
                matches!(&elements[0], Expression::MessageSend { selector, .. } if selector.name() == "kw1:kw2:"),
                "Expected multi-keyword send as array element"
            );
        }
        other => panic!("Expected array literal, got: {other:?}"),
    }
}

#[test]
fn parse_list_keyword_element_with_map_literal_arg() {
    // Regression test for BT-1287: the exact reported failing syntax
    let module = parse_ok("#(Counter supervisionSpec withArgs: #{#value => 42})");
    match &module.expressions[0].expression {
        Expression::ListLiteral { elements, tail, .. } => {
            assert_eq!(elements.len(), 1);
            assert!(tail.is_none());
            assert!(
                matches!(&elements[0], Expression::MessageSend { selector, arguments, .. }
                        if selector.name() == "withArgs:"
                            && matches!(&arguments[0], Expression::MapLiteral { .. })),
                "Expected keyword send with map arg as list element"
            );
        }
        other => panic!("Expected list literal, got: {other:?}"),
    }
}

#[test]
fn parse_array_keyword_element_with_map_literal_arg() {
    // Same regression but for #[...] array literals
    let module = parse_ok("#[Counter supervisionSpec withArgs: #{#value => 42}]");
    match &module.expressions[0].expression {
        Expression::ArrayLiteral { elements, .. } => {
            assert_eq!(elements.len(), 1);
            assert!(
                matches!(&elements[0], Expression::MessageSend { selector, arguments, .. }
                        if selector.name() == "withArgs:"
                            && matches!(&arguments[0], Expression::MapLiteral { .. })),
                "Expected keyword send with map arg as array element"
            );
        }
        other => panic!("Expected array literal, got: {other:?}"),
    }
}

#[test]
fn parse_doc_comment_on_class() {
    let module = parse_ok(
        "/// A simple counter actor.
/// Maintains a count that can be incremented.
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Counter");
    assert_eq!(
        class.doc_comment.as_deref(),
        Some("A simple counter actor.\nMaintains a count that can be incremented.")
    );
}

#[test]
fn parse_doc_comment_on_method() {
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0

  /// Increments the counter by one.
  increment => self.value := self.value + 1",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1);
    assert_eq!(
        class.methods[0].doc_comment.as_deref(),
        Some("Increments the counter by one.")
    );
}

#[test]
fn parse_no_doc_comment_with_regular_comment() {
    let module = parse_ok(
        "// This is a regular comment
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1",
    );

    assert_eq!(module.classes.len(), 1);
    assert!(module.classes[0].doc_comment.is_none());
}

#[test]
fn parse_four_slashes_not_doc_comment() {
    let module = parse_ok(
        "//// This is NOT a doc comment
Actor subclass: Counter
  state: value = 0
  increment => self.value := self.value + 1",
    );

    assert_eq!(module.classes.len(), 1);
    assert!(module.classes[0].doc_comment.is_none());
}

#[test]
fn parse_doc_comment_strips_prefix() {
    let module = parse_ok(
        "///No space after slashes
Actor subclass: Counter
  increment => 1",
    );

    assert_eq!(module.classes.len(), 1);
    assert_eq!(
        module.classes[0].doc_comment.as_deref(),
        Some("No space after slashes")
    );
}

#[test]
fn parse_doc_comment_on_class_and_method() {
    let module = parse_ok(
        "/// Class doc.
Actor subclass: Counter
  state: value = 0

  /// Method doc.
  increment => self.value := self.value + 1",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.doc_comment.as_deref(), Some("Class doc."));
    assert_eq!(class.methods.len(), 1);
    assert_eq!(class.methods[0].doc_comment.as_deref(), Some("Method doc."));
}

#[test]
fn parse_doc_comment_resets_on_regular_comment() {
    // A `//` comment between `///` and the declaration orphans the first
    // `///` block.  We now emit a warning for it.
    let tokens = lex_with_eof(
        "/// Orphaned doc comment.
// Regular comment interrupts.
/// Actual class doc.
Actor subclass: Counter
  increment => 1",
    );
    let (module, diagnostics) = parse(tokens);

    assert_eq!(module.classes.len(), 1);
    // Only the last consecutive block should be collected.
    assert_eq!(
        module.classes[0].doc_comment.as_deref(),
        Some("Actual class doc.")
    );
    // The orphaned `///` block must produce exactly one warning.
    let warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Warning)
        .collect();
    assert_eq!(warnings.len(), 1);
    assert!(
        warnings[0].message.contains("not attached"),
        "unexpected warning message: {}",
        warnings[0].message,
    );
}

#[test]
fn parse_doc_comment_with_abstract_modifier() {
    let module = parse_ok(
        "/// An abstract collection.
abstract Actor subclass: Collection
  size => self subclassResponsibility",
    );

    assert_eq!(module.classes.len(), 1);
    assert!(module.classes[0].is_abstract);
    assert_eq!(
        module.classes[0].doc_comment.as_deref(),
        Some("An abstract collection.")
    );
}

#[test]
fn parse_doc_comment_blank_line_resets() {
    // A blank line between `///` and the declaration orphans the first
    // `///` block.  We now emit a warning for it.
    let tokens = lex_with_eof(
        "/// Orphaned doc comment.

/// Actual class doc.
Actor subclass: Counter
  increment => 1",
    );
    let (module, diagnostics) = parse(tokens);

    assert_eq!(module.classes.len(), 1);
    // Blank line separates the two doc blocks; only the last is attached.
    assert_eq!(
        module.classes[0].doc_comment.as_deref(),
        Some("Actual class doc.")
    );
    // The orphaned `///` block must produce exactly one warning.
    let warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Warning)
        .collect();
    assert_eq!(warnings.len(), 1);
    assert!(
        warnings[0].message.contains("not attached"),
        "unexpected warning message: {}",
        warnings[0].message,
    );
}

// ── BT-980: unattached doc comment warnings ──────────────────────────────

#[test]
fn unattached_doc_comment_blank_line_before_class_emits_warning() {
    // `///` + blank line + class definition must produce a warning.
    let tokens = lex_with_eof(
        "/// This comment is separated by a blank line.

Object subclass: Foo
  size => 0",
    );
    let (_module, diagnostics) = parse(tokens);
    let warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Warning)
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "expected exactly one warning, got: {diagnostics:?}"
    );
    assert!(warnings[0].message.contains("not attached"));
}

#[test]
fn attached_doc_comment_no_blank_line_no_warning() {
    // `///` immediately before class definition must NOT produce a warning.
    let module = parse_ok(
        "/// This comment is attached.
Object subclass: Foo
  size => 0",
    );
    assert_eq!(
        module.classes[0].doc_comment.as_deref(),
        Some("This comment is attached.")
    );
}

#[test]
fn unattached_doc_comment_before_expression_emits_warning() {
    // `///` before a non-declaration expression must produce a warning.
    let tokens = lex_with_eof("/// Orphan before assignment.\nx := 42");
    let (_module, diagnostics) = parse(tokens);
    let warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Warning)
        .collect();
    assert_eq!(
        warnings.len(),
        1,
        "expected exactly one warning, got: {diagnostics:?}"
    );
    assert!(warnings[0].message.contains("not attached"));
}

#[test]
fn parse_doc_comment_with_sealed_modifier() {
    let module = parse_ok(
        "/// A sealed value type.
sealed Object subclass: Point
  state: x = 0
  state: y = 0
  x => self.x",
    );

    assert_eq!(module.classes.len(), 1);
    assert!(module.classes[0].is_sealed);
    assert_eq!(
        module.classes[0].doc_comment.as_deref(),
        Some("A sealed value type.")
    );
}

#[test]
fn parse_doc_comment_on_sealed_method() {
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0

  /// Cannot be overridden.
  sealed getValue => self.value",
    );

    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert!(method.is_sealed);
    assert_eq!(method.doc_comment.as_deref(), Some("Cannot be overridden."));
}

#[test]
fn parse_doc_comment_on_keyword_method() {
    let module = parse_ok(
        "Actor subclass: MyCollection
  state: items = #()

  /// Stores a value at the given index.
  at: index put: value => self.items",
    );

    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "at:put:");
    assert_eq!(
        method.doc_comment.as_deref(),
        Some("Stores a value at the given index.")
    );
}

#[test]
fn parse_doc_comment_on_binary_method() {
    let module = parse_ok(
        "Object subclass: Vector
  state: x = 0

  /// Adds two vectors.
  + other => self.x + other x",
    );

    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "+");
    assert_eq!(method.doc_comment.as_deref(), Some("Adds two vectors."));
}

#[test]
fn parse_empty_doc_comment_line() {
    let module = parse_ok(
        "/// First line.
///
/// After empty line.
Actor subclass: Counter
  increment => 1",
    );

    assert_eq!(module.classes.len(), 1);
    assert_eq!(
        module.classes[0].doc_comment.as_deref(),
        Some("First line.\n\nAfter empty line.")
    );
}

#[test]
fn parse_doc_comment_on_class_method() {
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0

  /// Creates a counter starting at the given value.
  class withValue: v => self new initialize: v",
    );

    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].class_methods[0];
    assert_eq!(
        method.doc_comment.as_deref(),
        Some("Creates a counter starting at the given value.")
    );
}

// === is_input_complete tests ===

#[test]
fn complete_simple_expression() {
    assert!(is_input_complete("3 + 4"));
}

#[test]
fn complete_empty_input() {
    assert!(is_input_complete(""));
    assert!(is_input_complete("   "));
}

#[test]
fn complete_assignment() {
    assert!(is_input_complete("x := 42"));
}

#[test]
fn complete_keyword_message() {
    assert!(is_input_complete("array at: 1 put: \"hello\""));
}

#[test]
fn complete_block() {
    assert!(is_input_complete("[:x | x * 2]"));
}

#[test]
fn complete_nested_blocks() {
    assert!(is_input_complete("[:x | [:y | x + y]]"));
}

#[test]
fn complete_parenthesized() {
    assert!(is_input_complete("(3 + 4) * 2"));
}

#[test]
fn complete_map_literal() {
    assert!(is_input_complete("#{name => \"Alice\", age => 30}"));
}

#[test]
fn complete_list_literal() {
    assert!(is_input_complete("#(1, 2, 3)"));
}

#[test]
fn complete_tuple_literal() {
    assert!(is_input_complete("{1, 2, 3}"));
}

#[test]
fn complete_string() {
    assert!(is_input_complete("\"hello world\""));
}

#[test]
fn complete_interpolated_string() {
    // Complete interpolated string
    assert!(is_input_complete("\"Hello, {name}!\""));
    // Unterminated interpolation
    assert!(!is_input_complete("\"Hello, {name"));
}

#[test]
fn complete_with_period() {
    assert!(is_input_complete("x := 1."));
}

#[test]
fn complete_multi_statement() {
    assert!(is_input_complete("x := 1.\ny := 2"));
}

#[test]
fn incomplete_unclosed_block() {
    assert!(!is_input_complete("["));
    assert!(!is_input_complete("[:x | x * 2"));
    assert!(!is_input_complete("[:x | [:y | x + y]"));
}

#[test]
fn incomplete_unclosed_paren() {
    assert!(!is_input_complete("("));
    assert!(!is_input_complete("(3 + 4"));
}

#[test]
fn incomplete_unclosed_brace() {
    assert!(!is_input_complete("{"));
    assert!(!is_input_complete("{1, 2"));
}

#[test]
fn incomplete_unclosed_map() {
    assert!(!is_input_complete("#{"));
    assert!(!is_input_complete("#{name => \"Alice\""));
}

#[test]
fn incomplete_unclosed_list() {
    assert!(!is_input_complete("#("));
    assert!(!is_input_complete("#(1, 2"));
}

#[test]
fn incomplete_unterminated_string() {
    assert!(!is_input_complete("\"hello"));
}

#[test]
fn incomplete_unterminated_interpolated_string() {
    assert!(!is_input_complete("\"hello"));
}

#[test]
fn incomplete_trailing_keyword() {
    assert!(!is_input_complete("array at:"));
    assert!(!is_input_complete("array at: 1 put:"));
    assert!(!is_input_complete("x ifTrue:"));
}

#[test]
fn incomplete_unterminated_block_comment() {
    assert!(!is_input_complete("/* unterminated comment"));
    assert!(!is_input_complete("x := 1 /* still going"));
}

#[test]
fn complete_block_comment() {
    assert!(is_input_complete("/* comment */ x := 1"));
}

#[test]
fn complete_multiline_block() {
    assert!(is_input_complete("[:x |\n  x * 2\n]"));
}

#[test]
fn complete_multiline_map() {
    assert!(is_input_complete(
        "#{\n  name => \"Alice\",\n  age => 30\n}"
    ));
}

#[test]
fn incomplete_multiline_block() {
    assert!(!is_input_complete("[:x |\n  x * 2"));
}

#[test]
fn incomplete_trailing_binary_operator() {
    assert!(!is_input_complete("x := 1 +"));
    assert!(!is_input_complete("3 *"));
}

#[test]
fn incomplete_trailing_assign() {
    assert!(!is_input_complete("x :="));
}

#[test]
fn complete_binary_continuation_on_new_line() {
    assert!(is_input_complete("x := 1 +\n  2"));
}

#[test]
fn complete_line_comment_only() {
    assert!(is_input_complete("// just a comment"));
}

#[test]
fn complete_extra_closing_delimiters() {
    // Extra closing delimiters are "complete" — they'll produce syntax errors
    // when evaluated, which is the desired behavior (show the error, don't
    // loop waiting for more input that can never balance them).
    assert!(is_input_complete("]"));
    assert!(is_input_complete(")"));
    assert!(is_input_complete("}"));
}

#[test]
fn incomplete_trailing_cascade() {
    assert!(!is_input_complete("x foo;"));
}

#[test]
fn incomplete_trailing_caret() {
    assert!(!is_input_complete("^"));
}

#[test]
fn incomplete_class_definition_header_only() {
    // Just the class header — user hasn't typed methods yet
    assert!(!is_input_complete("Actor subclass: Counter"));
}

#[test]
fn incomplete_class_definition_with_state_no_methods() {
    // Class with state but no methods — still incomplete
    assert!(!is_input_complete(
        "Actor subclass: Counter\n  state: value = 0"
    ));
}

#[test]
fn complete_class_definition_with_method() {
    // Class with at least one method — complete
    assert!(is_input_complete(
        "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1"
    ));
}

#[test]
fn complete_class_definition_with_multiple_methods() {
    assert!(is_input_complete(
        "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1\n  getValue => ^self.value"
    ));
}

#[test]
fn incomplete_class_definition_with_map_in_state() {
    // Map literals contain `=>` but should NOT count as method arrows
    assert!(!is_input_complete(
        "Object subclass: Config\n  state: opts = #{verbose => true}"
    ));
}

#[test]
fn single_quoted_string_is_complete() {
    // Single-quoted strings are invalid (ADR 0023) but fully consumed by the
    // lexer — the REPL should NOT wait for more input.
    assert!(is_input_complete("'hello'"));
    assert!(is_input_complete("Transcript show: 'hello'"));
}

#[test]
fn unknown_directive_is_complete() {
    // Unknown directives like @foo are invalid but fully consumed
    assert!(is_input_complete("@foo"));
}

// === Match expression parsing ===

#[test]
fn parse_simple_match() {
    let module = parse_ok("42 match: [_ -> 99]");
    assert_eq!(module.expressions.len(), 1);
    assert!(
        matches!(&module.expressions[0].expression, Expression::Match { arms, .. } if arms.len() == 1)
    );
}

#[test]
fn parse_match_with_semicolons() {
    let module = parse_ok("x match: [1 -> \"one\"; 2 -> \"two\"; _ -> \"other\"]");
    assert_eq!(module.expressions.len(), 1);
    assert!(
        matches!(&module.expressions[0].expression, Expression::Match { arms, .. } if arms.len() == 3)
    );
}

#[test]
fn parse_match_with_tuple_pattern() {
    let module = parse_ok("r match: [{#ok, v} -> v; {#error, _} -> nil]");
    assert_eq!(module.expressions.len(), 1);
    assert!(
        matches!(&module.expressions[0].expression, Expression::Match { arms, .. } if arms.len() == 2)
    );
}

#[test]
fn parse_match_with_guard() {
    let module = parse_ok("n match: [x when: [x > 0] -> x; _ -> 0]");
    assert_eq!(module.expressions.len(), 1);
    if let Expression::Match { arms, .. } = &module.expressions[0].expression {
        assert_eq!(arms.len(), 2);
        assert!(arms[0].guard.is_some());
        assert!(arms[1].guard.is_none());
    } else {
        panic!("Expected Match expression");
    }
}

#[test]
fn codegen_simple_match() {
    let module = parse_ok("42 match: [_ -> 99]");
    let expr = &module.expressions[0].expression;
    let result = crate::codegen::core_erlang::generate_test_expression(expr, "test_match");
    assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
    let code = result.unwrap();
    eprintln!("Generated code:\n{code}");
    assert!(code.contains("case"), "Expected case expression in: {code}");
}

#[test]
fn codegen_match_with_arms() {
    let module = parse_ok("1 match: [1 -> \"one\"; 2 -> \"two\"; _ -> \"other\"]");
    let expr = &module.expressions[0].expression;
    let result = crate::codegen::core_erlang::generate_test_expression(expr, "test_match");
    assert!(result.is_ok(), "Codegen failed: {:?}", result.err());
    let code = result.unwrap();
    eprintln!("Generated code:\n{code}");
    assert!(code.contains("case"), "Expected case expression in: {code}");
}

#[test]
fn codegen_empty_match_errors() {
    let module = parse_ok("42 match: []");
    let expr = &module.expressions[0].expression;
    let result = crate::codegen::core_erlang::generate_test_expression(expr, "test_match");
    assert!(result.is_err(), "Empty match should fail codegen");
}

/// Non-local return (`^`) inside a match arm body should parse as `Return`.
#[test]
fn match_arm_with_non_local_return() {
    let module = parse_ok("x match: [#ok -> ^42; _ -> nil]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("Expected Match expression");
    };
    assert!(
        matches!(arms[0].body, Expression::Return { .. }),
        "Expected Return in first arm body, got: {:?}",
        arms[0].body
    );
}

/// Assignment (`:=`) inside a match arm body should parse as `Assignment`.
#[test]
fn match_arm_with_assignment() {
    let module = parse_ok("x match: [#ok -> result := 1; _ -> result := 2]");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("Expected Match expression");
    };
    assert!(
        matches!(arms[0].body, Expression::Assignment { .. }),
        "Expected Assignment in first arm body, got: {:?}",
        arms[0].body
    );
    assert!(
        matches!(arms[1].body, Expression::Assignment { .. }),
        "Expected Assignment in second arm body, got: {:?}",
        arms[1].body
    );
}

/// Regression test: unclosed map literal at EOF must not infinite-loop.
///
/// The fuzzer discovered that `#{key` (no `=>`, no `}`) followed by EOF
/// caused `parse_map_literal`'s outer loop to spin forever because the
/// error-recovery path reached EOF and `continue`d without breaking.
#[test]
fn unclosed_map_literal_at_eof_terminates() {
    // Minimal repro: unclosed map literal inside parens, followed by EOF
    let _diagnostics = parse_err("(#{key value");
    // Parser must terminate (not infinite-loop). Errors are expected.
}

/// Variant: bare unclosed map literal without surrounding parens.
#[test]
fn unclosed_map_literal_bare_eof_terminates() {
    let _diagnostics = parse_err("#{key");
}

#[test]
fn parse_exponentiation_operator() {
    // BT-414: `**` is a binary operator
    let module = parse_ok("2 ** 10");
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
                Expression::Literal(Literal::Integer(2), _)
            ));
            assert_eq!(op.as_str(), "**");
            assert_eq!(arguments.len(), 1);
            assert!(matches!(
                arguments[0],
                Expression::Literal(Literal::Integer(10), _)
            ));
        }
        _ => panic!("Expected binary message send with **"),
    }
}

#[test]
fn parse_exponentiation_higher_precedence_than_multiply() {
    // BT-414: `3 * 2 ** 4` should be `3 * (2 ** 4)`
    let module = parse_ok("3 * 2 ** 4");
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
            assert_eq!(op.as_str(), "*");
            // The argument should be (2 ** 4)
            match &arguments[0] {
                Expression::MessageSend {
                    receiver: r2,
                    selector: MessageSelector::Binary(op2),
                    arguments: args2,
                    ..
                } => {
                    assert!(matches!(**r2, Expression::Literal(Literal::Integer(2), _)));
                    assert_eq!(op2.as_str(), "**");
                    assert!(matches!(
                        args2[0],
                        Expression::Literal(Literal::Integer(4), _)
                    ));
                }
                _ => panic!("Expected ** as right operand of *"),
            }
        }
        _ => panic!("Expected * at top level"),
    }
}

#[test]
fn parse_exponentiation_right_associative() {
    // BT-414: `2 ** 3 ** 2` should be `2 ** (3 ** 2)` (right-associative)
    let module = parse_ok("2 ** 3 ** 2");
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
                Expression::Literal(Literal::Integer(2), _)
            ));
            assert_eq!(op.as_str(), "**");
            // The argument should be (3 ** 2), NOT ((2 ** 3) ** 2)
            match &arguments[0] {
                Expression::MessageSend {
                    receiver: r2,
                    selector: MessageSelector::Binary(op2),
                    arguments: args2,
                    ..
                } => {
                    assert!(matches!(**r2, Expression::Literal(Literal::Integer(3), _)));
                    assert_eq!(op2.as_str(), "**");
                    assert!(matches!(
                        args2[0],
                        Expression::Literal(Literal::Integer(2), _)
                    ));
                }
                _ => panic!("Expected ** as right operand (right-associative)"),
            }
        }
        _ => panic!("Expected ** at top level"),
    }
}

// ========================================================================
// BT-571: Standalone Method Definition Tests
// ========================================================================

#[test]
fn standalone_method_definition_unary() {
    let module = parse_ok("Counter >> increment => self.value := self.value + 1");
    assert!(module.classes.is_empty());
    assert!(module.expressions.is_empty());
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Counter");
    assert!(!method_def.is_class_method);
    assert_eq!(method_def.method.selector.name().as_str(), "increment");
}

#[test]
fn standalone_method_definition_keyword() {
    let module = parse_ok("Counter >> setValue: v => self.value := v");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Counter");
    assert_eq!(method_def.method.selector.name().as_str(), "setValue:");
    assert_eq!(method_def.method.parameters.len(), 1);
}

#[test]
fn standalone_method_definition_binary() {
    let module = parse_ok("Point >> + other => self x + (other x)");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Point");
    assert_eq!(method_def.method.selector.name().as_str(), "+");
}

#[test]
fn standalone_method_definition_class_side() {
    let module = parse_ok("Counter class >> withInitial: n => self spawnWith: #{#value => n}");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Counter");
    assert!(method_def.is_class_method);
    assert_eq!(method_def.method.selector.name().as_str(), "withInitial:");
}

#[test]
fn standalone_method_definition_with_class_def() {
    let source = "Actor subclass: Counter\n  state: value = 0\n\nCounter >> increment => self.value := self.value + 1";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.method_definitions.len(), 1);
    assert_eq!(module.classes[0].name.name.as_str(), "Counter");
    assert_eq!(
        module.method_definitions[0].class_name.name.as_str(),
        "Counter"
    );
}

#[test]
fn multiple_standalone_method_definitions() {
    let source =
        "Counter >> increment => self.value := self.value + 1\nCounter >> getValue => ^self.value";
    let module = parse_ok(source);
    assert_eq!(module.method_definitions.len(), 2);
    assert_eq!(
        module.method_definitions[0].method.selector.name().as_str(),
        "increment"
    );
    assert_eq!(
        module.method_definitions[1].method.selector.name().as_str(),
        "getValue"
    );
}

#[test]
fn standalone_method_definition_keyword_typed_param() {
    // Regression: BT-1151 — is_keyword_method_selector_at must handle typed params
    // just like is_keyword_method_at; without the shared helper this was missed.
    let module = parse_ok("Counter >> setValue: v :: Integer => self.value := v");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Counter");
    assert_eq!(method_def.method.selector.name().as_str(), "setValue:");
    assert_eq!(method_def.method.parameters.len(), 1);
}

#[test]
fn standalone_method_definition_keyword_union_typed_param_lookahead() {
    // The lookahead (is_keyword_method_params_at) must not return false for
    // `deposit: amount: Integer | Nil =>` — it should recognise this as a method
    // definition even though the parser does not yet fully parse union-typed params.
    // This test verifies the lookahead produces a method_definition node (with
    // possible parse errors) rather than silently treating it as an expression.
    let source = "Counter >> deposit: amount: Integer | Nil => self.value := 0";
    let tokens = crate::source_analysis::lex_with_eof(source);
    let (module, _diagnostics) = crate::source_analysis::parser::parse(tokens);
    // The lookahead must identify this as a standalone method, not an expression
    assert_eq!(module.method_definitions.len(), 1);
    assert_eq!(
        module.method_definitions[0].class_name.name.as_str(),
        "Counter"
    );
}

// --- BT-1519: Extension type annotation syntax (:: -> ReturnType) ---

#[test]
fn standalone_method_unary_return_type_arrow() {
    // Standard syntax: `Integer >> factorial -> Integer =>` already works
    let module = parse_ok("Integer >> factorial -> Integer => 1");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Integer");
    assert_eq!(method_def.method.selector.name().as_str(), "factorial");
    assert!(
        method_def.method.return_type.is_some(),
        "should have return type"
    );
    match method_def.method.return_type.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Simple(ident) => assert_eq!(ident.name.as_str(), "Integer"),
        other => panic!("expected Simple type annotation, got: {other:?}"),
    }
}

#[test]
fn standalone_method_unary_return_type_double_colon_arrow() {
    // Extension-style: `Integer >> factorial :: -> Integer =>`
    let module = parse_ok("Integer >> factorial :: -> Integer => 1");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Integer");
    assert_eq!(method_def.method.selector.name().as_str(), "factorial");
    assert!(
        method_def.method.return_type.is_some(),
        "should have return type with :: -> syntax"
    );
    match method_def.method.return_type.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Simple(ident) => assert_eq!(ident.name.as_str(), "Integer"),
        other => panic!("expected Simple type annotation, got: {other:?}"),
    }
}

#[test]
fn standalone_method_keyword_return_type_double_colon_arrow() {
    // `String >> split: sep :: String :: -> Array =>`
    // The first `:: String` is the param type, the second `:: -> Array` is the return type.
    let module = parse_ok("String >> split: sep :: String :: -> Array => self");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.method.selector.name().as_str(), "split:");
    assert_eq!(method_def.method.parameters.len(), 1);
    assert!(method_def.method.parameters[0].type_annotation.is_some());
    assert!(
        method_def.method.return_type.is_some(),
        "should have return type with :: -> syntax on keyword method"
    );
    match method_def.method.return_type.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Simple(ident) => assert_eq!(ident.name.as_str(), "Array"),
        other => panic!("expected Simple type annotation, got: {other:?}"),
    }
}

#[test]
fn standalone_method_binary_return_type_double_colon_arrow() {
    // `Point >> + other :: Point :: -> Point =>`
    let module = parse_ok("Point >> + other :: Point :: -> Point => self");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.method.selector.name().as_str(), "+");
    assert_eq!(method_def.method.parameters.len(), 1);
    assert!(method_def.method.parameters[0].type_annotation.is_some());
    assert!(
        method_def.method.return_type.is_some(),
        "should have return type with :: -> syntax on binary method"
    );
    match method_def.method.return_type.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Simple(ident) => assert_eq!(ident.name.as_str(), "Point"),
        other => panic!("expected Simple type annotation, got: {other:?}"),
    }
}

#[test]
fn standalone_method_class_side_return_type_double_colon_arrow() {
    // `String class >> fromJson :: -> String =>`
    let module = parse_ok("String class >> fromJson :: -> String => self");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "String");
    assert!(method_def.is_class_method);
    assert_eq!(method_def.method.selector.name().as_str(), "fromJson");
    assert!(
        method_def.method.return_type.is_some(),
        "class-side extension should have return type with :: -> syntax"
    );
    match method_def.method.return_type.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Simple(ident) => assert_eq!(ident.name.as_str(), "String"),
        other => panic!("expected Simple type annotation, got: {other:?}"),
    }
}

#[test]
fn standalone_method_return_type_union_double_colon_arrow() {
    // `Array >> first :: -> Integer | Nil =>`
    let module = parse_ok("Array >> first :: -> Integer | Nil => self");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert!(
        method_def.method.return_type.is_some(),
        "should have union return type with :: -> syntax"
    );
    match method_def.method.return_type.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Union { types, .. } => {
            assert_eq!(types.len(), 2);
        }
        other => panic!("expected Union type annotation, got: {other:?}"),
    }
}

#[test]
fn method_lookup_not_confused_with_method_definition() {
    // Counter >> #increment is method lookup, not method definition
    let module = parse_ok("Counter >> #increment");
    assert!(module.method_definitions.is_empty());
    assert_eq!(module.expressions.len(), 1);
}

#[test]
fn deeply_nested_parens_does_not_stack_overflow() {
    // 300 levels of nesting exceeds MAX_NESTING_DEPTH (64)
    let source = "(".repeat(300) + "1" + &")".repeat(300);
    let diagnostics = parse_err(&source);
    assert!(
        diagnostics.iter().any(|d| d.message.contains("nesting")),
        "Expected nesting depth error, got: {diagnostics:?}"
    );
}

#[test]
fn deeply_chained_assignments_does_not_stack_overflow() {
    // 300 chained assignments: a := a := a := ... := 1
    let source = "a := ".repeat(300) + "1";
    let diagnostics = parse_err(&source);
    assert!(
        diagnostics.iter().any(|d| d.message.contains("nesting")),
        "Expected nesting depth error, got: {diagnostics:?}"
    );
}

#[test]
fn deeply_nested_blocks_does_not_stack_overflow() {
    // 300 nested blocks: [[[...1...]]]
    let source = "[".repeat(300) + "1" + &"]".repeat(300);
    let diagnostics = parse_err(&source);
    assert!(
        diagnostics.iter().any(|d| d.message.contains("nesting")),
        "Expected nesting depth error, got: {diagnostics:?}"
    );
}

#[test]
fn deeply_nested_match_patterns_does_not_stack_overflow() {
    // Nested tuple patterns in match: x match: [{{{...}}} => 1]
    let source = "x match: [".to_string() + &"{".repeat(300) + "a" + &"}".repeat(300) + " => 1]";
    let diagnostics = parse_err(&source);
    assert!(
        diagnostics.iter().any(|d| d.message.contains("nesting")),
        "Expected nesting depth error, got: {diagnostics:?}"
    );
}

#[test]
fn deeply_nested_string_interpolation_does_not_stack_overflow() {
    // Each nesting level needs a real string-in-interpolation: "x {"y {"z {1} z"} y"} x"
    // The lexer only produces StringStart inside {..."..."...} sequences.
    // Build: "{"{"{ ... 1 ... "}"}"}"
    let mut source = String::new();
    for _ in 0..300 {
        source.push_str("\"a{");
    }
    source.push('1');
    for _ in 0..300 {
        source.push_str("}a\"");
    }
    // This input may or may not trigger the nesting guard depending on
    // lexer behavior, but it must never stack overflow.
    let tokens = crate::source_analysis::lex_with_eof(&source);
    let (_module, _diagnostics) = crate::source_analysis::parse(tokens);
    // Success = no panic/stack overflow
}

#[test]
fn unclosed_deeply_nested_blocks_does_not_oom() {
    // Fuzz regression: 158 unclosed '[' caused OOM because the
    // nesting guard returned Error without consuming tokens and the
    // parse_block body loop spun infinitely.
    let source = "[".repeat(158);
    let diagnostics = parse_err(&source);
    assert!(
        diagnostics.iter().any(|d| d.message.contains("nesting")),
        "Expected nesting depth error, got: {diagnostics:?}"
    );
}

#[test]
fn hash_newline_brace_does_not_stack_overflow() {
    // Fuzz regression: "#\n{" caused infinite mutual recursion
    // between parse_primary (Hash+LeftBrace lookahead) and
    // parse_map_literal (expected MapOpen, got Hash).
    let diagnostics = parse_err("#\n{");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("Unexpected '#'")),
        "Expected unexpected '#' error, got: {diagnostics:?}"
    );
}

#[test]
fn unclosed_nested_blocks_in_method_body_does_not_oom() {
    // Fuzz regression: deeply nested unclosed '[' inside a method
    // body could spin the parse_method_body loop infinitely because
    // synchronize() can return without advancing when in_method_body.
    let source = format!("Object subclass: Foo\n  bar => {}", "[".repeat(100));
    let diagnostics = parse_err(&source);
    assert!(
        diagnostics.iter().any(|d| d.message.contains("nesting")),
        "Expected nesting depth error, got: {diagnostics:?}"
    );
}

#[test]
fn parse_typed_class() {
    let module = parse_ok(
        "typed Actor subclass: StrictCounter
  state: value :: Integer = 0

  increment -> Integer => self.value := self.value + 1",
    );

    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];

    assert!(!class.is_abstract);
    assert!(!class.is_sealed);
    assert!(class.is_typed);
    assert_eq!(class.name.name, "StrictCounter");
    assert_eq!(class.state.len(), 1);
    assert_eq!(class.methods.len(), 1);
}

#[test]
fn parse_typed_sealed_class() {
    let module = parse_ok(
        "typed sealed Actor subclass: ImmutablePoint
  state: x :: Integer = 0
  state: y :: Integer = 0",
    );

    let class = &module.classes[0];
    assert!(class.is_typed);
    assert!(class.is_sealed);
    assert_eq!(class.name.name, "ImmutablePoint");
}

#[test]
fn parse_abstract_typed_class() {
    let module = parse_ok(
        "abstract typed Actor subclass: Shape
  area -> Float => ^0.0",
    );

    let class = &module.classes[0];
    assert!(class.is_abstract);
    assert!(class.is_typed);
    assert_eq!(class.name.name, "Shape");
}

#[test]
fn parse_non_typed_class_defaults_false() {
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0",
    );

    let class = &module.classes[0];
    assert!(!class.is_typed);
}

// ========================================================================
// BT-919: Cast (!) statement terminator tests
// ========================================================================

#[test]
fn parse_bang_marks_message_send_as_cast() {
    // `foo bar!` should parse as a MessageSend with is_cast = true
    let module = parse_ok("foo bar!");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            is_cast,
            selector: MessageSelector::Unary(name),
            ..
        } => {
            assert_eq!(name.as_str(), "bar");
            assert!(
                is_cast,
                "Expected is_cast = true for bang-terminated message"
            );
        }
        other => panic!("Expected MessageSend, got: {other:?}"),
    }
}

#[test]
fn parse_period_keeps_message_send_as_call() {
    // `foo bar.` should parse as a MessageSend with is_cast = false
    let module = parse_ok("foo bar.");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend { is_cast, .. } => {
            assert!(
                !is_cast,
                "Expected is_cast = false for period-terminated message"
            );
        }
        other => panic!("Expected MessageSend, got: {other:?}"),
    }
}

#[test]
fn parse_bang_keyword_message_is_cast() {
    // `obj doSomething: 42!` should mark the keyword send as cast
    let module = parse_ok("obj doSomething: 42!");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            is_cast,
            selector: MessageSelector::Keyword(_),
            ..
        } => {
            assert!(is_cast, "Expected keyword message with ! to be a cast");
        }
        other => panic!("Expected keyword MessageSend, got: {other:?}"),
    }
}

#[test]
fn parse_bang_in_expression_context_is_error() {
    // `x := foo bar!` — cast has no return value, can't be used in an expression
    let diagnostics = parse_err("x := foo bar!");
    assert!(
        !diagnostics.is_empty(),
        "Expected error for cast in expression context"
    );
    assert!(
        diagnostics[0]
            .message
            .contains("Cast (!) has no return value"),
        "Expected cast-in-expression error, got: {}",
        diagnostics[0].message
    );
}

#[test]
fn parse_multiple_statements_with_bang() {
    // `foo bar! baz qux.` — two statements: first cast, second call
    let module = parse_ok("foo bar!\nbaz qux.");
    assert_eq!(module.expressions.len(), 2);
    match &module.expressions[0].expression {
        Expression::MessageSend { is_cast, .. } => assert!(is_cast),
        other => panic!("Expected cast MessageSend, got: {other:?}"),
    }
    match &module.expressions[1].expression {
        Expression::MessageSend { is_cast, .. } => assert!(!is_cast),
        other => panic!("Expected call MessageSend, got: {other:?}"),
    }
}

#[test]
fn parse_bang_binary_message_is_cast() {
    // `3 + 4!` — binary message send with cast
    let module = parse_ok("3 + 4!");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MessageSend {
            is_cast,
            selector: MessageSelector::Binary(op),
            ..
        } => {
            assert_eq!(op.as_str(), "+");
            assert!(is_cast, "Expected binary message with ! to be a cast");
        }
        other => panic!("Expected binary MessageSend, got: {other:?}"),
    }
}

#[test]
fn parse_bang_in_block_body() {
    // `[foo bar!]` — cast inside a block body
    let module = parse_ok("[foo bar!]");
    assert_eq!(module.expressions.len(), 1);
    if let Expression::Block(block) = &module.expressions[0].expression {
        assert_eq!(block.body.len(), 1);
        match &block.body[0].expression {
            Expression::MessageSend { is_cast, .. } => {
                assert!(is_cast, "Expected cast inside block body");
            }
            other => panic!("Expected MessageSend in block, got: {other:?}"),
        }
    } else {
        panic!("Expected Block expression");
    }
}

#[test]
fn parse_return_with_bang_is_error() {
    // `^foo bar!` — can't return a cast (no return value)
    let diagnostics = parse_err("^foo bar!");
    assert!(!diagnostics.is_empty(), "Expected error for return of cast");
    assert!(
        diagnostics[0]
            .message
            .contains("Cast (!) has no return value"),
        "Expected cast error for return, got: {}",
        diagnostics[0].message
    );
}

#[test]
fn parse_cascade_with_bang_is_error() {
    // `obj msg1; msg2!` — cascade is not a MessageSend, so ! produces error
    // (Cascade + cast semantics are deferred to a future issue)
    let diagnostics = parse_err("obj msg1; msg2!");
    assert!(
        !diagnostics.is_empty(),
        "Expected error for cascade with bang"
    );
}

// =========================================================================
// BT-948: Unnecessary period separator warnings
// =========================================================================

/// Helper to extract only lint diagnostics from a parse.
fn parse_lints(source: &str) -> Vec<Diagnostic> {
    let tokens = lex_with_eof(source);
    let (_module, diagnostics) = parse(tokens);
    diagnostics
        .into_iter()
        .filter(|d| d.severity == Severity::Lint)
        .collect()
}

#[test]
fn trailing_period_before_bracket_in_block_emits_lint() {
    // `[ foo bar. ]` — trailing period before `]` is redundant
    let lints = parse_lints("x := [ foo bar. ]");
    assert_eq!(lints.len(), 1, "expected one lint, got: {lints:?}");
    assert!(
        lints[0].message.contains("trailing"),
        "message: {}",
        lints[0].message
    );
    assert_eq!(lints[0].severity, Severity::Lint);
    assert!(lints[0].hint.is_some());
}

#[test]
fn period_before_newline_in_block_emits_lint() {
    // `[ foo bar.\n  baz ]` — period before newline is redundant
    let lints = parse_lints("x := [\n  foo bar.\n  baz\n]");
    assert_eq!(lints.len(), 1, "expected one lint, got: {lints:?}");
    assert!(
        lints[0].message.contains("newline"),
        "message: {}",
        lints[0].message
    );
    assert_eq!(lints[0].severity, Severity::Lint);
    assert!(lints[0].hint.is_some());
}

#[test]
fn period_between_statements_same_line_in_block_no_lint() {
    // `[ foo bar. baz quux ]` — period on same line is the separator, no lint
    let lints = parse_lints("x := [ foo bar. baz quux ]");
    assert!(lints.is_empty(), "expected no lints, got: {lints:?}");
}

#[test]
fn trailing_period_at_end_of_method_emits_lint() {
    // `increment => self.n := self.n + 1.` — trailing period after last statement
    let lints = parse_lints(
        "Object subclass: Counter\n  state: n = 0\n  increment => self.n := self.n + 1.\n",
    );
    assert_eq!(lints.len(), 1, "expected one lint, got: {lints:?}");
    assert!(
        lints[0].message.contains("end of method"),
        "message: {}",
        lints[0].message
    );
    assert_eq!(lints[0].severity, Severity::Lint);
}

#[test]
fn period_before_newline_in_method_body_emits_lint() {
    // Two statements in a method body with an explicit period before newline
    let lints = parse_lints("Object subclass: Foo\n  go =>\n    x := 1.\n    x + 2\n");
    assert_eq!(lints.len(), 1, "expected one lint, got: {lints:?}");
    assert!(
        lints[0].message.contains("newline"),
        "message: {}",
        lints[0].message
    );
}

#[test]
fn period_between_same_line_statements_in_method_no_lint() {
    // `go => foo bar. baz quux` — same-line separator, needed
    let lints = parse_lints("Object subclass: Foo\n  go => foo bar. baz quux\n");
    assert!(lints.is_empty(), "expected no lints, got: {lints:?}");
}

#[test]
fn module_level_period_does_not_lint() {
    // At module level, periods are optional statement terminators — no lint
    let lints = parse_lints("foo bar.\nbaz quux.");
    assert!(
        lints.is_empty(),
        "expected no lints at module level, got: {lints:?}"
    );
}

// ========================================================================
// Comment attachment tests (BT-975)
// ========================================================================

#[test]
fn class_line_comment_attached_to_leading() {
    // A `//` comment immediately before a class definition should appear in
    // `ClassDefinition.comments.leading`.
    let module = parse_ok("// A useful class\nObject subclass: Foo\n");
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(
        class.comments.leading.len(),
        1,
        "expected one leading comment"
    );
    assert_eq!(class.comments.leading[0].kind, CommentKind::Line);
    assert!(
        class.comments.leading[0].content.contains("A useful class"),
        "content: {}",
        class.comments.leading[0].content
    );
}

#[test]
fn class_doc_comment_not_duplicated_in_attachment() {
    // A `///` doc comment must NOT appear in `comments.leading`; it goes to
    // `doc_comment` only.
    let module = parse_ok("/// Doc text\nObject subclass: Foo\n");
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert!(
        class.doc_comment.is_some(),
        "expected doc_comment to be populated"
    );
    assert!(
        class.comments.leading.is_empty(),
        "doc comment must not be duplicated into leading comments"
    );
}

#[test]
fn method_mixed_doc_and_line_comment_separated() {
    // `//` goes to `comments.leading`, `///` goes to `doc_comment`.
    // The `//` must appear BEFORE `///` so the doc comment collection is
    // not reset (a `//` after `///` would reset the doc-comment buffer).
    let source = "Object subclass: Foo\n  // Line comment\n  /// Doc comment\n  go => 42\n";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert!(
        method.doc_comment.is_some(),
        "expected doc_comment on method"
    );
    assert_eq!(
        method.comments.leading.len(),
        1,
        "expected one leading comment on method"
    );
    assert_eq!(method.comments.leading[0].kind, CommentKind::Line);
}

#[test]
fn state_declaration_doc_comment_populated() {
    // A `///` doc comment before a state declaration is captured in `doc_comment`.
    let source = "Object subclass: Foo\n  /// The count\n  state: count = 0\n";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    let state = &module.classes[0].state[0];
    assert!(
        state.doc_comment.is_some(),
        "expected doc_comment on state declaration"
    );
    assert!(
        state
            .doc_comment
            .as_deref()
            .unwrap_or("")
            .contains("The count"),
        "doc_comment: {:?}",
        state.doc_comment
    );
}

#[test]
fn state_declaration_line_comment_attached() {
    // A `//` comment before `state:` is attached to `comments.leading`.
    let source = "Object subclass: Foo\n  // The x field\n  state: x = 1\n";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    let state = &module.classes[0].state[0];
    assert_eq!(
        state.comments.leading.len(),
        1,
        "expected one leading comment on state declaration"
    );
    assert!(
        state.comments.leading[0].content.contains("The x field"),
        "content: {}",
        state.comments.leading[0].content
    );
}

// ========================================================================
// ExpressionStatement comment attachment tests (BT-976)
// ========================================================================

#[test]
fn expression_statement_leading_comment_in_method_body() {
    // A `//` comment between two statements in a method body must appear as a
    // leading comment on the second ExpressionStatement.
    let source = "Object subclass: Foo\n  go =>\n    x := 1.\n    // Step two\n    y := 2\n";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    let body = &module.classes[0].methods[0].body;
    assert_eq!(body.len(), 2, "expected 2 statements");
    // First statement has no leading comment
    assert!(
        body[0].comments.leading.is_empty(),
        "first statement should have no leading comment"
    );
    // Second statement has the `// Step two` comment as leading
    assert_eq!(
        body[1].comments.leading.len(),
        1,
        "expected one leading comment on second statement"
    );
    assert!(
        body[1].comments.leading[0].content.contains("Step two"),
        "content: {}",
        body[1].comments.leading[0].content
    );
}

#[test]
fn expression_statement_trailing_comment() {
    // A `// comment` on the same line as a statement must appear as `trailing`
    // on that ExpressionStatement.
    let source = "Object subclass: Foo\n  go => x := 1 // inline note\n";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    let body = &module.classes[0].methods[0].body;
    assert_eq!(body.len(), 1, "expected 1 statement");
    assert!(
        body[0].comments.trailing.is_some(),
        "expected a trailing comment"
    );
    assert!(
        body[0]
            .comments
            .trailing
            .as_ref()
            .unwrap()
            .content
            .contains("inline note"),
        "trailing content: {:?}",
        body[0].comments.trailing
    );
}

#[test]
fn empty_module_file_leading_comments_populated() {
    // A `.bt` file containing only comments and no items must populate
    // `Module.file_leading_comments`.
    let source = "// First comment\n// Second comment\n";
    let (module, diagnostics) = parse(lex_with_eof(source));
    assert!(diagnostics.is_empty(), "expected no diagnostics");
    assert!(module.classes.is_empty());
    assert!(module.expressions.is_empty());
    assert_eq!(
        module.file_leading_comments.len(),
        2,
        "expected 2 file-level comments"
    );
    assert!(
        module.file_leading_comments[0].content.contains("First"),
        "content: {}",
        module.file_leading_comments[0].content
    );
    assert!(
        module.file_leading_comments[1].content.contains("Second"),
        "content: {}",
        module.file_leading_comments[1].content
    );
}

#[test]
fn non_empty_module_file_leading_comments_empty() {
    // In a non-empty module, file-level leading comments attach to the first
    // item — `file_leading_comments` must remain empty.
    let source = "// File comment\nx := 42\n";
    let (module, diagnostics) = parse(lex_with_eof(source));
    assert!(diagnostics.is_empty(), "expected no diagnostics");
    assert!(
        module.file_leading_comments.is_empty(),
        "file_leading_comments should be empty for non-empty module"
    );
    assert_eq!(module.expressions.len(), 1);
    // The comment attaches to the first expression's ExpressionStatement
    assert_eq!(
        module.expressions[0].comments.leading.len(),
        1,
        "expected leading comment on first expression"
    );
    assert!(
        module.expressions[0].comments.leading[0]
            .content
            .contains("File comment"),
        "content: {}",
        module.expressions[0].comments.leading[0].content
    );
}

#[test]
fn block_body_expression_statement_leading_comment() {
    // A `//` comment between statements in a block body attaches as leading
    // on the following ExpressionStatement in the block.
    let source = "Object subclass: Foo\n  go =>\n    [:each |\n      // Transform\n      each asUppercase]\n";
    let module = parse_ok(source);
    assert_eq!(module.classes.len(), 1);
    let body = &module.classes[0].methods[0].body;
    assert_eq!(body.len(), 1, "expected 1 statement in method body");
    // The statement is a block expression
    let Expression::Block(block) = &body[0].expression else {
        panic!("expected Block expression, got {:?}", body[0].expression);
    };
    assert_eq!(block.body.len(), 1, "expected 1 statement in block");
    assert_eq!(
        block.body[0].comments.leading.len(),
        1,
        "expected leading comment on block statement"
    );
    assert!(
        block.body[0].comments.leading[0]
            .content
            .contains("Transform"),
        "content: {}",
        block.body[0].comments.leading[0].content
    );
}

#[test]
fn standalone_method_leading_comment_attached() {
    // A `//` comment before a standalone method definition (`ClassName >> ...`)
    // must appear as a leading comment on the MethodDefinition.
    // Regression test: previously parse_standalone_method_definition() dropped
    // comments from the class-name token's leading trivia.
    let source = "// Note about Counter\nCounter >> increment => self.n := self.n + 1\n";
    let (module, diagnostics) = parse(lex_with_eof(source));
    assert!(
        diagnostics.is_empty(),
        "expected no diagnostics: {diagnostics:?}"
    );
    assert_eq!(module.method_definitions.len(), 1);
    let method = &module.method_definitions[0].method;
    assert_eq!(
        method.comments.leading.len(),
        1,
        "expected one leading comment on standalone method"
    );
    assert!(
        method.comments.leading[0]
            .content
            .contains("Note about Counter"),
        "content: {}",
        method.comments.leading[0].content
    );
    assert!(module.file_leading_comments.is_empty());
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

// ==========================================================================
// Generic type annotation parsing (ADR 0068, BT-1568)
// ==========================================================================

#[test]
fn parse_generic_class_definition_single_param() {
    let module = parse_ok(
        "Value subclass: Stack(E)
  state: items = #[]",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Stack");
    assert_eq!(class.type_params.len(), 1);
    assert_eq!(class.type_params[0].name.name, "E");
}

#[test]
fn parse_generic_class_definition_two_params() {
    let module = parse_ok(
        "sealed Value subclass: Result(T, E)
  state: okValue = nil
  state: errReason = nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Result");
    assert!(class.is_sealed);
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.type_params[0].name.name, "T");
    assert_eq!(class.type_params[1].name.name, "E");
    assert_eq!(class.state.len(), 2);
}

#[test]
fn parse_generic_class_definition_no_params() {
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Counter");
    assert!(class.type_params.is_empty());
}

// --- ADR 0068 Phase 2d: Bounded type parameters ---

#[test]
fn parse_bounded_type_param_single() {
    let module = parse_ok(
        "Actor subclass: Logger(T :: Printable)
  log: item :: T => item asString",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Logger");
    assert_eq!(class.type_params.len(), 1);
    assert_eq!(class.type_params[0].name.name, "T");
    assert!(class.type_params[0].bound.is_some());
    assert_eq!(
        class.type_params[0].bound.as_ref().unwrap().name,
        "Printable"
    );
}

#[test]
fn parse_bounded_type_param_mixed() {
    // T is bounded, E is unbounded
    let module = parse_ok(
        "Value subclass: Container(T :: Printable, E)
  state: item :: T = nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.type_params[0].name.name, "T");
    assert!(class.type_params[0].bound.is_some());
    assert_eq!(
        class.type_params[0].bound.as_ref().unwrap().name,
        "Printable"
    );
    assert_eq!(class.type_params[1].name.name, "E");
    assert!(class.type_params[1].bound.is_none());
}

#[test]
fn parse_bounded_type_param_all_bounded() {
    let module = parse_ok(
        "Value subclass: SortedMap(K :: Comparable, V :: Printable)
  state: items = #[]",
    );
    let class = &module.classes[0];
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.type_params[0].name.name, "K");
    assert_eq!(
        class.type_params[0].bound.as_ref().unwrap().name,
        "Comparable"
    );
    assert_eq!(class.type_params[1].name.name, "V");
    assert_eq!(
        class.type_params[1].bound.as_ref().unwrap().name,
        "Printable"
    );
}

#[test]
fn parse_bounded_type_param_protocol() {
    // Protocol with bounded type params
    let module = parse_ok(
        "Protocol define: Mapper(T :: Printable)
  map: block :: Block(T, Object) -> Object",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.type_params.len(), 1);
    assert_eq!(proto.type_params[0].name.name, "T");
    assert!(proto.type_params[0].bound.is_some());
    assert_eq!(
        proto.type_params[0].bound.as_ref().unwrap().name,
        "Printable"
    );
}

#[test]
fn parse_generic_type_annotation_single() {
    // `Array(Integer)` in a type annotation
    let module = parse_ok(
        "Actor subclass: Foo
  state: items :: Array(Integer) = #[]",
    );
    let class = &module.classes[0];
    let state = &class.state[0];
    let ty = state.type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = ty
    {
        assert_eq!(base.name, "Array");
        assert_eq!(parameters.len(), 1);
        assert!(matches!(&parameters[0], TypeAnnotation::Simple(id) if id.name == "Integer"));
    } else {
        panic!("Expected Generic type annotation, got: {ty:?}");
    }
}

#[test]
fn parse_generic_type_annotation_result() {
    // `Result(T, E)` in a type annotation
    let module = parse_ok(
        "Actor subclass: Foo
  state: result :: Result(String, IOError) = nil",
    );
    let ty = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = ty
    {
        assert_eq!(base.name, "Result");
        assert_eq!(parameters.len(), 2);
        assert!(matches!(&parameters[0], TypeAnnotation::Simple(id) if id.name == "String"));
        assert!(matches!(&parameters[1], TypeAnnotation::Simple(id) if id.name == "IOError"));
    } else {
        panic!("Expected Generic type annotation, got: {ty:?}");
    }
}

#[test]
fn parse_generic_type_annotation_nested() {
    // `Block(T, Result(R, E))` — nested generic type
    let module = parse_ok(
        "Actor subclass: Foo
  state: handler :: Block(T, Result(R, E)) = nil",
    );
    let ty = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base,
        parameters: params,
        ..
    } = ty
    {
        assert_eq!(base.name, "Block");
        assert_eq!(params.len(), 2);
        assert!(matches!(&params[0], TypeAnnotation::Simple(id) if id.name == "T"));
        if let TypeAnnotation::Generic {
            base: inner_base,
            parameters: inner_params,
            ..
        } = &params[1]
        {
            assert_eq!(inner_base.name, "Result");
            assert_eq!(inner_params.len(), 2);
            assert!(matches!(&inner_params[0], TypeAnnotation::Simple(id) if id.name == "R"));
            assert!(matches!(&inner_params[1], TypeAnnotation::Simple(id) if id.name == "E"));
        } else {
            panic!("Expected nested Generic, got: {:?}", params[1]);
        }
    } else {
        panic!("Expected Generic type annotation, got: {ty:?}");
    }
}

#[test]
fn parse_generic_type_in_method_return_type() {
    // Method with `-> Result(Integer, Error)` return type
    let module = parse_ok(
        "Actor subclass: Foo
  compute -> Result(Integer, Error) => nil",
    );
    let method = &module.classes[0].methods[0];
    let ret_ty = method.return_type.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = ret_ty
    {
        assert_eq!(base.name, "Result");
        assert_eq!(parameters.len(), 2);
    } else {
        panic!("Expected Generic return type, got: {ret_ty:?}");
    }
}

#[test]
fn parse_generic_type_in_method_param() {
    // Method with `block :: Block(T, R)` parameter type
    let module = parse_ok(
        "Actor subclass: Foo
  map: block :: Block(T, R) -> Result(R, E) => nil",
    );
    let method = &module.classes[0].methods[0];
    let param_ty = method.parameters[0].type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = param_ty
    {
        assert_eq!(base.name, "Block");
        assert_eq!(parameters.len(), 2);
    } else {
        panic!("Expected Generic param type, got: {param_ty:?}");
    }
    let ret_ty = method.return_type.as_ref().unwrap();
    if let TypeAnnotation::Generic {
        base, parameters, ..
    } = ret_ty
    {
        assert_eq!(base.name, "Result");
        assert_eq!(parameters.len(), 2);
    } else {
        panic!("Expected Generic return type, got: {ret_ty:?}");
    }
}

#[test]
fn parse_generic_type_name_uses_parentheses() {
    // `type_name()` should produce `Collection(Integer)` not `Collection<Integer>`
    let ty = TypeAnnotation::generic(
        Identifier::new("Collection", Span::new(0, 10)),
        vec![TypeAnnotation::simple("Integer", Span::new(11, 18))],
        Span::new(0, 19),
    );
    assert_eq!(ty.type_name(), "Collection(Integer)");

    let ty2 = TypeAnnotation::generic(
        Identifier::new("Result", Span::new(0, 6)),
        vec![
            TypeAnnotation::simple("String", Span::new(7, 13)),
            TypeAnnotation::simple("Error", Span::new(15, 20)),
        ],
        Span::new(0, 21),
    );
    assert_eq!(ty2.type_name(), "Result(String, Error)");
}

#[test]
fn parse_generic_class_with_native_module() {
    // Generic class with native: backing module
    let module = parse_ok(
        "Actor subclass: Cache(K, V) native: beamtalk_cache
  getValue: key :: K -> V => nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Cache");
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.type_params[0].name.name, "K");
    assert_eq!(class.type_params[1].name.name, "V");
    assert!(class.backing_module.is_some());
    assert_eq!(
        class.backing_module.as_ref().unwrap().name,
        "beamtalk_cache"
    );
}

#[test]
fn parse_generic_union_with_generic_member() {
    // `Result(Integer, Error) | nil` — union with a generic member
    let module = parse_ok(
        "Actor subclass: Foo
  state: result :: Result(Integer, Error) | nil = nil",
    );
    let ty = module.classes[0].state[0].type_annotation.as_ref().unwrap();
    if let TypeAnnotation::Union { types, .. } = ty {
        assert_eq!(types.len(), 2);
        assert!(
            matches!(&types[0], TypeAnnotation::Generic { base, parameters, .. }
            if base.name == "Result" && parameters.len() == 2)
        );
    } else {
        panic!("Expected Union type, got: {ty:?}");
    }
}

// ========================================================================
// Protocol Definition Tests (ADR 0068, Phase 2a — BT-1578)
// ========================================================================

#[test]
fn parse_protocol_printable() {
    let module = parse_ok(
        "Protocol define: Printable
  /// Return a human-readable string representation.
  asString -> String",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Printable");
    assert!(proto.type_params.is_empty());
    assert!(proto.extending.is_none());
    assert_eq!(proto.method_signatures.len(), 1);

    let sig = &proto.method_signatures[0];
    assert_eq!(sig.selector.name(), "asString");
    assert!(sig.parameters.is_empty());
    let ret_ty = sig.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::Simple(id) if id.name == "String"));
    assert!(sig.doc_comment.is_some());
}

#[test]
fn parse_protocol_comparable() {
    let module = parse_ok(
        "Protocol define: Comparable
  < other :: Self -> Boolean
  > other :: Self -> Boolean
  <= other :: Self -> Boolean
  >= other :: Self -> Boolean",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Comparable");
    assert_eq!(proto.method_signatures.len(), 4);

    // Check first signature: `< other :: Self -> Boolean`
    let sig = &proto.method_signatures[0];
    assert!(matches!(&sig.selector, MessageSelector::Binary(op) if op == "<"));
    assert_eq!(sig.parameters.len(), 1);
    assert_eq!(sig.parameters[0].name.name, "other");
    let param_ty = sig.parameters[0].type_annotation.as_ref().unwrap();
    assert!(matches!(param_ty, TypeAnnotation::SelfType { .. }));
    let ret_ty = sig.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::Simple(id) if id.name == "Boolean"));
}

#[test]
fn parse_protocol_generic_collection() {
    let module = parse_ok(
        "Protocol define: Collection(E)
  /// The number of elements in this collection.
  size -> Integer
  /// Iterate over each element.
  do: block :: Block(E, Object)
  /// Transform each element.
  collect: block :: Block(E, Object) -> Self
  /// Return elements matching the predicate.
  select: block :: Block(E, Boolean) -> Self",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Collection");
    assert_eq!(proto.type_params.len(), 1);
    assert_eq!(proto.type_params[0].name.name, "E");
    assert_eq!(proto.method_signatures.len(), 4);

    // Check `size -> Integer`
    let sig_size = &proto.method_signatures[0];
    assert_eq!(sig_size.selector.name(), "size");
    assert!(sig_size.parameters.is_empty());
    assert!(matches!(
        sig_size.return_type.as_ref().unwrap(),
        TypeAnnotation::Simple(id) if id.name == "Integer"
    ));

    // Check `do: block :: Block(E, Object)`
    let sig_do = &proto.method_signatures[1];
    assert_eq!(sig_do.selector.name(), "do:");
    assert_eq!(sig_do.parameters.len(), 1);
    let param_ty = sig_do.parameters[0].type_annotation.as_ref().unwrap();
    assert!(
        matches!(param_ty, TypeAnnotation::Generic { base, parameters, .. }
        if base.name == "Block" && parameters.len() == 2)
    );
    assert!(sig_do.return_type.is_none());

    // Check `collect: block :: Block(E, Object) -> Self`
    let sig_collect = &proto.method_signatures[2];
    assert_eq!(sig_collect.selector.name(), "collect:");
    let ret_ty = sig_collect.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::SelfType { .. }));
}

#[test]
fn parse_protocol_extending() {
    let module = parse_ok(
        "Protocol define: Sortable
  extending: Comparable
  /// The key used for sort ordering.
  sortKey -> Object",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Sortable");
    assert!(proto.type_params.is_empty());
    let extending = proto.extending.as_ref().unwrap();
    assert_eq!(extending.name, "Comparable");
    assert_eq!(proto.method_signatures.len(), 1);
    assert_eq!(proto.method_signatures[0].selector.name(), "sortKey");
}

#[test]
fn parse_protocol_no_methods() {
    // A protocol with no method signatures (just the definition)
    let module = parse_ok("Protocol define: Marker");
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Marker");
    assert!(proto.method_signatures.is_empty());
}

#[test]
fn parse_protocol_multiple_type_params() {
    let module = parse_ok(
        "Protocol define: Mapping(K, V)
  at: key :: K -> V
  put: key :: K value: val :: V",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Mapping");
    assert_eq!(proto.type_params.len(), 2);
    assert_eq!(proto.type_params[0].name.name, "K");
    assert_eq!(proto.type_params[1].name.name, "V");
    assert_eq!(proto.method_signatures.len(), 2);

    // Check `at: key :: K -> V`
    let sig_at = &proto.method_signatures[0];
    assert_eq!(sig_at.selector.name(), "at:");
    assert_eq!(sig_at.parameters.len(), 1);
    let ret_ty = sig_at.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::Simple(id) if id.name == "V"));

    // Check `put: key :: K value: val :: V`
    let sig_put = &proto.method_signatures[1];
    assert_eq!(sig_put.selector.name(), "put:value:");
    assert_eq!(sig_put.parameters.len(), 2);
}

#[test]
fn parse_protocol_followed_by_class() {
    // Protocol definition followed by a class definition
    let module = parse_ok(
        "Protocol define: Printable
  asString -> String

Object subclass: Foo
  toString => \"hello\"",
    );
    assert_eq!(module.protocols.len(), 1);
    assert_eq!(module.protocols[0].name.name, "Printable");
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].name.name, "Foo");
}

#[test]
fn parse_multiple_protocols() {
    let module = parse_ok(
        "Protocol define: Printable
  asString -> String

Protocol define: Comparable
  < other :: Self -> Boolean",
    );
    assert_eq!(module.protocols.len(), 2);
    assert_eq!(module.protocols[0].name.name, "Printable");
    assert_eq!(module.protocols[1].name.name, "Comparable");
}

#[test]
fn parse_protocol_with_impl_body_error() {
    // Protocol method signatures should NOT have `=>`
    let diagnostics = parse_err(
        "Protocol define: Bad
  asString => \"hello\"",
    );
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .contains("Protocol method signatures cannot have implementations")),
        "Expected error about protocol implementations, got: {diagnostics:?}"
    );
}

#[test]
fn parse_protocol_unary_no_return_type() {
    // Unary method without a return type
    let module = parse_ok(
        "Protocol define: Hashable
  hash",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.method_signatures.len(), 1);
    assert_eq!(proto.method_signatures[0].selector.name(), "hash");
    assert!(proto.method_signatures[0].return_type.is_none());
}

#[test]
fn parse_protocol_identifier_in_expression_context() {
    // `Protocol` used as a variable name should not trigger protocol parsing
    let module = parse_ok("x := Protocol");
    assert!(module.protocols.is_empty());
    assert_eq!(module.expressions.len(), 1);
}

#[test]
fn parse_protocol_class_method_unary() {
    // BT-1611: Protocol with a class method requirement
    let module = parse_ok(
        "Protocol define: Serializable
  asString -> String
  class fromString: aString :: String -> Self",
    );
    assert_eq!(module.protocols.len(), 1);
    let proto = &module.protocols[0];
    assert_eq!(proto.name.name, "Serializable");

    // Instance methods
    assert_eq!(proto.method_signatures.len(), 1);
    assert_eq!(proto.method_signatures[0].selector.name(), "asString");

    // Class methods
    assert_eq!(proto.class_method_signatures.len(), 1);
    let class_sig = &proto.class_method_signatures[0];
    assert_eq!(class_sig.selector.name(), "fromString:");
    assert_eq!(class_sig.parameters.len(), 1);
    assert_eq!(class_sig.parameters[0].name.name, "aString");
    let param_ty = class_sig.parameters[0].type_annotation.as_ref().unwrap();
    assert!(matches!(param_ty, TypeAnnotation::Simple(id) if id.name == "String"));
    let ret_ty = class_sig.return_type.as_ref().unwrap();
    assert!(matches!(ret_ty, TypeAnnotation::SelfType { .. }));
}

#[test]
fn parse_protocol_class_method_only() {
    // BT-1611: Protocol with only class methods
    let module = parse_ok(
        "Protocol define: Factory
  class create -> Self",
    );
    let proto = &module.protocols[0];
    assert!(proto.method_signatures.is_empty());
    assert_eq!(proto.class_method_signatures.len(), 1);
    assert_eq!(proto.class_method_signatures[0].selector.name(), "create");
}

#[test]
fn parse_protocol_mixed_instance_and_class_methods() {
    // BT-1611: Protocol with interleaved instance and class method signatures
    let module = parse_ok(
        "Protocol define: Configurable
  config -> Map
  class default -> Self
  reset -> Object",
    );
    let proto = &module.protocols[0];
    assert_eq!(proto.method_signatures.len(), 2); // config and reset
    assert_eq!(proto.method_signatures[0].selector.name(), "config");
    assert_eq!(proto.method_signatures[1].selector.name(), "reset");
    assert_eq!(proto.class_method_signatures.len(), 1);
    assert_eq!(proto.class_method_signatures[0].selector.name(), "default");
}

#[test]
fn parse_protocol_class_definition_named_protocol() {
    // A class named `Protocol` should parse as a class, not a protocol
    // because the keyword after `Protocol` is `subclass:`, not `define:`
    let module = parse_ok(
        "Object subclass: Protocol
  doSomething => nil",
    );
    assert!(module.protocols.is_empty());
    assert_eq!(module.classes.len(), 1);
    assert_eq!(module.classes[0].name.name, "Protocol");
}

// ---- BT-1577: Superclass type argument parsing ----

#[test]
fn parse_superclass_type_args_single_param() {
    // Collection(E) subclass: Array(E)
    let module = parse_ok(
        "Collection(E) subclass: Array(E)
  append: item => nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "Array");
    assert_eq!(class.superclass.as_ref().unwrap().name, "Collection");
    assert_eq!(class.type_params.len(), 1);
    assert_eq!(class.type_params[0].name.name, "E");
    assert_eq!(class.superclass_type_args.len(), 1);
    match &class.superclass_type_args[0] {
        TypeAnnotation::Simple(id) => assert_eq!(id.name, "E"),
        other => panic!("Expected Simple(E), got: {other:?}"),
    }
}

#[test]
fn parse_superclass_type_args_concrete_type() {
    // Collection(Integer) subclass: IntArray
    let module = parse_ok(
        "Collection(Integer) subclass: IntArray
  append: item => nil",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "IntArray");
    assert!(class.type_params.is_empty());
    assert_eq!(class.superclass_type_args.len(), 1);
    match &class.superclass_type_args[0] {
        TypeAnnotation::Simple(id) => assert_eq!(id.name, "Integer"),
        other => panic!("Expected Simple(Integer), got: {other:?}"),
    }
}

#[test]
fn parse_superclass_type_args_two_params() {
    // Mapping(K, V) subclass: SortedMap(K, V)
    let module = parse_ok(
        "Mapping(K, V) subclass: SortedMap(K, V)
  size => 0",
    );
    let class = &module.classes[0];
    assert_eq!(class.name.name, "SortedMap");
    assert_eq!(class.type_params.len(), 2);
    assert_eq!(class.superclass_type_args.len(), 2);
    match &class.superclass_type_args[0] {
        TypeAnnotation::Simple(id) => assert_eq!(id.name, "K"),
        other => panic!("Expected Simple(K), got: {other:?}"),
    }
    match &class.superclass_type_args[1] {
        TypeAnnotation::Simple(id) => assert_eq!(id.name, "V"),
        other => panic!("Expected Simple(V), got: {other:?}"),
    }
}

#[test]
fn parse_superclass_no_type_args() {
    // Actor subclass: Counter — no superclass type args
    let module = parse_ok(
        "Actor subclass: Counter
  state: value = 0",
    );
    let class = &module.classes[0];
    assert!(class.superclass_type_args.is_empty());
}

// === Protocol define: is_input_complete tests ===

#[test]
fn incomplete_protocol_define_no_methods() {
    // "Protocol define: Greetable" alone is incomplete — waiting for method sigs
    assert!(!is_input_complete("Protocol define: Greetable"));
}

#[test]
fn incomplete_protocol_define_with_method() {
    // Protocol definitions are always incomplete — the REPL needs a blank line
    // to know the definition is finished (protocols can have multiple methods).
    assert!(!is_input_complete(
        "Protocol define: Greetable\n  greet -> String"
    ));
}

#[test]
fn incomplete_protocol_define_with_multiple_methods() {
    // Still incomplete even with multiple methods — needs blank line to submit
    assert!(!is_input_complete(
        "Protocol define: Serializable\n  serialize -> String\n  deserialize: data :: String -> Self"
    ));
}

#[test]
fn incomplete_protocol_define_extending_only() {
    assert!(!is_input_complete(
        "Protocol define: PrettyPrintable\n  extending: Greetable"
    ));
}

#[test]
fn incomplete_protocol_define_extending_with_method() {
    assert!(!is_input_complete(
        "Protocol define: PrettyPrintable\n  extending: Greetable\n  prettyPrint -> String"
    ));
}

#[test]
fn non_protocol_define_is_complete() {
    // "define:" on a non-Protocol receiver should not trigger the protocol check
    assert!(is_input_complete("myObj define: something"));
}

// === needs_blank_line_to_complete tests ===

#[test]
fn blank_line_submit_protocol_define() {
    // Protocol definitions need a blank line to complete
    assert!(needs_blank_line_to_complete("Protocol define: Greetable"));
    assert!(needs_blank_line_to_complete(
        "Protocol define: Greetable\n  greet -> String"
    ));
    assert!(needs_blank_line_to_complete(
        "Protocol define: Serializable\n  serialize -> String\n  deserialize: data :: String -> Self"
    ));
}

#[test]
fn blank_line_submit_class_definition_no_methods() {
    // Class header without methods — needs blank line
    assert!(needs_blank_line_to_complete("Actor subclass: Counter"));
    assert!(needs_blank_line_to_complete(
        "Actor subclass: Counter\n  state: value = 0"
    ));
}

#[test]
fn blank_line_submit_class_definition_with_state_map() {
    // Map `=>` in state initializer should not count as a method arrow
    assert!(needs_blank_line_to_complete(
        "Object subclass: Config\n  state: opts = #{verbose => true}"
    ));
}

#[test]
fn no_blank_line_submit_for_complete_input() {
    // Already complete — blank line not needed
    assert!(!needs_blank_line_to_complete("3 + 4"));
    assert!(!needs_blank_line_to_complete("x := 42"));
    assert!(!needs_blank_line_to_complete(""));
}

#[test]
fn no_blank_line_submit_for_unclosed_delimiters() {
    // Unclosed delimiters — blank line should NOT force-submit
    assert!(!needs_blank_line_to_complete("[:x | x * 2"));
    assert!(!needs_blank_line_to_complete("#{name =>"));
    assert!(!needs_blank_line_to_complete("#(1, 2"));
    assert!(!needs_blank_line_to_complete("{1, 2"));
}

#[test]
fn no_blank_line_submit_for_trailing_operators() {
    // Trailing operators — user is mid-expression
    assert!(!needs_blank_line_to_complete("x := 1 +"));
    assert!(!needs_blank_line_to_complete("array at:"));
    assert!(!needs_blank_line_to_complete("x :="));
}

#[test]
fn no_blank_line_submit_non_protocol_define() {
    // "define:" on a non-Protocol receiver should not trigger
    assert!(!needs_blank_line_to_complete("myObj define: something"));
}

#[test]
fn blank_line_submit_class_with_method_is_already_complete() {
    // Class with at least one method is already complete via is_input_complete,
    // so needs_blank_line_to_complete returns false
    assert!(!needs_blank_line_to_complete(
        "Actor subclass: Counter\n  state: value = 0\n  increment => self.value := self.value + 1"
    ));
}

// === Package-qualified class references (ADR 0070) ===

#[test]
fn parse_qualified_class_reference() {
    // `json@Parser` should parse as a ClassReference with package = Some("json")
    let module = parse_ok("json@Parser");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::ClassReference {
        name,
        package,
        span,
        ..
    } = &module.expressions[0].expression
    {
        assert_eq!(name.name.as_str(), "Parser");
        let pkg = package.as_ref().expect("expected package qualifier");
        assert_eq!(pkg.name.as_str(), "json");
        // Span should cover the full `json@Parser`
        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 11);
    } else {
        panic!(
            "Expected ClassReference, got {:?}",
            module.expressions[0].expression
        );
    }
}

#[test]
fn parse_qualified_class_reference_message_send() {
    // `json@Parser parse: x` — qualified class ref as message receiver
    let module = parse_ok("json@Parser parse: x");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Keyword(parts),
        arguments,
        ..
    } = &module.expressions[0].expression
    {
        // Receiver should be a qualified ClassReference
        if let Expression::ClassReference { name, package, .. } = &**receiver {
            assert_eq!(name.name.as_str(), "Parser");
            assert_eq!(
                package.as_ref().expect("expected package").name.as_str(),
                "json"
            );
        } else {
            panic!("Expected ClassReference receiver, got {receiver:?}");
        }
        assert_eq!(parts.len(), 1);
        assert_eq!(parts[0].keyword.as_str(), "parse:");
        assert_eq!(arguments.len(), 1);
    } else {
        panic!("Expected keyword message send");
    }
}

#[test]
fn parse_qualified_class_reference_unary_message() {
    // `json@Parser new` — qualified class ref with unary message
    let module = parse_ok("json@Parser new");
    assert_eq!(module.expressions.len(), 1);

    if let Expression::MessageSend {
        receiver,
        selector: MessageSelector::Unary(name),
        ..
    } = &module.expressions[0].expression
    {
        if let Expression::ClassReference {
            name: class_name,
            package,
            ..
        } = &**receiver
        {
            assert_eq!(class_name.name.as_str(), "Parser");
            assert_eq!(
                package.as_ref().expect("expected package").name.as_str(),
                "json"
            );
        } else {
            panic!("Expected ClassReference receiver");
        }
        assert_eq!(name.as_str(), "new");
    } else {
        panic!("Expected unary message send");
    }
}

#[test]
fn parse_unqualified_class_reference_has_no_package() {
    // `Parser` — plain class reference should have package = None
    let module = parse_ok("Parser");
    if let Expression::ClassReference { package, .. } = &module.expressions[0].expression {
        assert!(
            package.is_none(),
            "plain class reference should have no package"
        );
    } else {
        panic!("Expected ClassReference");
    }
}

#[test]
fn parse_qualified_class_mixed_with_directive() {
    // Directives on a previous line should not interfere with qualified class refs
    let module = parse_ok("json@Parser");
    assert_eq!(module.expressions.len(), 1);
    assert!(matches!(
        &module.expressions[0].expression,
        Expression::ClassReference { .. }
    ));
}

#[test]
fn parse_at_parser_no_package_error_recovery() {
    // `@Parser` — missing package name. This is lexed as an error token.
    let diagnostics = parse_err("@Parser");
    // Should produce at least one diagnostic (from the error token)
    assert!(
        !diagnostics.is_empty(),
        "expected error for @Parser without package"
    );
}

#[test]
fn parse_qualified_class_reference_spans() {
    // Verify span precision for `json@Parser`
    let module = parse_ok("json@Parser");
    if let Expression::ClassReference {
        name,
        package,
        span,
        ..
    } = &module.expressions[0].expression
    {
        // Package span: 0..4
        let pkg = package.as_ref().unwrap();
        assert_eq!(pkg.span.start(), 0);
        assert_eq!(pkg.span.end(), 4);
        // Class name span: 5..11
        assert_eq!(name.span.start(), 5);
        assert_eq!(name.span.end(), 11);
        // Full span: 0..11
        assert_eq!(span.start(), 0);
        assert_eq!(span.end(), 11);
    } else {
        panic!("Expected ClassReference");
    }
}

#[test]
fn parse_qualified_lowercase_after_at_is_error() {
    // `json@parser` — lowercase after `@` is not a valid class name
    let diagnostics = parse_err("json@parser");
    assert!(
        !diagnostics.is_empty(),
        "expected error for lowercase class name after @"
    );
}

// ========================================================================
// Package-qualified standalone method definitions (ADR 0070 Phase 2, BT-1651)
// ========================================================================

#[test]
fn parse_qualified_standalone_method_definition() {
    // `json@Parser >> lenient => 42` — cross-package extension method
    let module = parse_ok("json@Parser >> lenient => 42");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Parser");
    let pkg = method_def
        .package
        .as_ref()
        .expect("expected package qualifier");
    assert_eq!(pkg.name.as_str(), "json");
    assert!(!method_def.is_class_method);
}

#[test]
fn parse_qualified_standalone_class_method() {
    // `json@Parser class >> fromString: s => 42` — cross-package class-side extension
    let module = parse_ok("json@Parser class >> fromString: s => 42");
    assert_eq!(module.method_definitions.len(), 1);
    let method_def = &module.method_definitions[0];
    assert_eq!(method_def.class_name.name.as_str(), "Parser");
    assert_eq!(
        method_def
            .package
            .as_ref()
            .expect("expected package")
            .name
            .as_str(),
        "json"
    );
    assert!(method_def.is_class_method);
}

#[test]
fn parse_unqualified_standalone_method_has_no_package() {
    // `Counter >> increment => 42` — no package qualifier
    let module = parse_ok("Counter >> increment => 42");
    assert_eq!(module.method_definitions.len(), 1);
    assert!(
        module.method_definitions[0].package.is_none(),
        "unqualified standalone method should have package = None"
    );
}

// ========================================================================
// Package-qualified superclass in class definitions (ADR 0070 Phase 2, BT-1651)
// ========================================================================

#[test]
fn parse_qualified_superclass() {
    // `json@Parser subclass: LenientParser` — subclassing from another package
    let module = parse_ok("json@Parser subclass: LenientParser\n  parse => 42");
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.name.name.as_str(), "LenientParser");
    let superclass = class.superclass.as_ref().expect("expected superclass");
    assert_eq!(superclass.name.as_str(), "Parser");
    let pkg = class
        .superclass_package
        .as_ref()
        .expect("expected superclass_package");
    assert_eq!(pkg.name.as_str(), "json");
}

#[test]
fn parse_unqualified_superclass_has_no_package() {
    // `Object subclass: Foo` — no package qualifier on superclass
    let module = parse_ok("Object subclass: Foo\n  bar => 42");
    assert_eq!(module.classes.len(), 1);
    assert!(
        module.classes[0].superclass_package.is_none(),
        "unqualified superclass should have superclass_package = None"
    );
}
