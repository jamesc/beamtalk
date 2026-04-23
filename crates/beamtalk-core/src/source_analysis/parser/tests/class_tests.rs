// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for class definition parsing, native keyword arguments, class references,
//! and visibility modifiers.
use super::*;

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
fn parse_block_with_typed_param_simple() {
    // BT-2043: `[:b :: Dictionary | b isNil]` — the `:: Type` annotation is
    // consumed (and currently discarded). Prior to the fix the parser left
    // `::` in place, producing cascading errors that also corrupted the
    // surrounding method body and caused a spurious "Unused variable"
    // warning for the assignment receiving the block.
    let module = parse_ok("[:b :: Dictionary | b isNil]");
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert_eq!(block.parameters.len(), 1);
            assert_eq!(block.parameters[0].name.as_str(), "b");
            assert_eq!(block.body.len(), 1);
        }
        _ => panic!("Expected block"),
    }
}

#[test]
fn parse_block_with_typed_params_multiple() {
    // BT-2043: multiple typed block parameters.
    let module = parse_ok("[:x :: Integer :y :: Integer | x + y]");
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
fn parse_block_with_typed_param_generic() {
    // BT-2043: Generic types such as `List(Integer)` should not confuse the
    // block parser, since the parens are balanced inside the type annotation.
    let module = parse_ok("[:xs :: List(Integer) | xs size]");
    match &module.expressions[0].expression {
        Expression::Block(block) => {
            assert_eq!(block.parameters.len(), 1);
            assert_eq!(block.parameters[0].name.as_str(), "xs");
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

#[test]
fn parse_map_value_keyword_message() {
    // BT-1854: Nested keyword messages with map literal arguments inside outer map literals
    let module =
        parse_ok("#{#session => SessionInfo new: #{#turnCount => 0}, #tokens => TokenUsage new}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 2);
            // First value is a keyword message: SessionInfo new: #{#turnCount => 0}
            match &pairs[0].value {
                Expression::MessageSend {
                    receiver,
                    selector: MessageSelector::Keyword(keywords),
                    arguments,
                    ..
                } => {
                    assert!(
                        matches!(receiver.as_ref(), Expression::ClassReference { name, .. } if name.name.as_str() == "SessionInfo")
                    );
                    assert_eq!(keywords.len(), 1);
                    assert_eq!(keywords[0].keyword.as_str(), "new:");
                    assert_eq!(arguments.len(), 1);
                    assert!(
                        matches!(&arguments[0], Expression::MapLiteral { pairs, .. } if pairs.len() == 1)
                    );
                }
                _ => panic!("Expected keyword MessageSend for first value"),
            }
            // Second value is a unary message: TokenUsage new
            assert!(matches!(
                &pairs[1].value,
                Expression::MessageSend {
                    selector: MessageSelector::Unary(_),
                    ..
                }
            ));
        }
        _ => panic!("Expected MapLiteral"),
    }
}

#[test]
fn parse_map_value_multi_keyword_message() {
    // BT-1854: Multi-keyword messages as map values
    let module = parse_ok("#{#key => Foo from: 1 to: 2}");
    assert_eq!(module.expressions.len(), 1);
    match &module.expressions[0].expression {
        Expression::MapLiteral { pairs, .. } => {
            assert_eq!(pairs.len(), 1);
            match &pairs[0].value {
                Expression::MessageSend {
                    selector: MessageSelector::Keyword(keywords),
                    arguments,
                    ..
                } => {
                    assert_eq!(keywords.len(), 2);
                    assert_eq!(keywords[0].keyword.as_str(), "from:");
                    assert_eq!(keywords[1].keyword.as_str(), "to:");
                    assert_eq!(arguments.len(), 2);
                }
                _ => panic!("Expected keyword MessageSend"),
            }
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
fn parse_gtgt_as_binary_method_selector() {
    // BT-1735: `>>` (GtGt token) is a valid binary method selector in class bodies
    let module = parse_ok(
        "Object subclass: Behaviour
  sealed >> aSelector :: Symbol -> CompiledMethod => @primitive \"methodLookup\"",
    );

    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), ">>");
    assert_eq!(method.parameters.len(), 1);
    assert_eq!(method.parameters[0].name.name, "aSelector");
    assert!(method.parameters[0].type_annotation.is_some());
    assert!(method.return_type.is_some());
    assert!(method.is_sealed);
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
fn parse_multi_keyword_method_with_union_typed_last_param() {
    // BT-1944: Multi-keyword method with `:: Integer | Nil` on last param
    let module = parse_ok(
        "Actor subclass: WorkerPool
  executeActivity: act selector: sel args: a timeout: t :: Integer | Nil =>
    self",
    );
    let method = &module.classes[0].methods[0];
    assert_eq!(
        method.selector.name(),
        "executeActivity:selector:args:timeout:"
    );
    assert_eq!(method.parameters.len(), 4);
    // First 3 params should be untyped
    assert!(method.parameters[0].type_annotation.is_none());
    assert!(method.parameters[1].type_annotation.is_none());
    assert!(method.parameters[2].type_annotation.is_none());
    // Last param should have union type annotation
    assert!(
        method.parameters[3].type_annotation.is_some(),
        "timeout param should have type annotation"
    );
    match method.parameters[3].type_annotation.as_ref().unwrap() {
        crate::ast::TypeAnnotation::Union { types, .. } => {
            assert_eq!(types.len(), 2, "should be a 2-member union");
        }
        other => panic!("expected Union type annotation, got: {other:?}"),
    }
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
// BT-1698: `internal` modifier for classes and methods (ADR 0071)
// ========================================================================

#[test]
fn parse_internal_class() {
    let module = parse_ok(
        "internal Object subclass: Foo
  bar => 42",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert!(class.is_internal);
    assert!(!class.is_abstract);
    assert!(!class.is_sealed);
    assert_eq!(class.name.name, "Foo");
    assert_eq!(class.superclass.as_ref().unwrap().name, "Object");
}

#[test]
fn parse_internal_abstract_class() {
    let module = parse_ok(
        "internal abstract Object subclass: Bar
  baz => self subclassResponsibility",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert!(class.is_internal);
    assert!(class.is_abstract);
    assert_eq!(class.name.name, "Bar");
}

#[test]
fn parse_abstract_internal_class() {
    // Modifiers can appear in any order
    let module = parse_ok(
        "abstract internal Object subclass: Baz
  qux => self subclassResponsibility",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert!(class.is_internal);
    assert!(class.is_abstract);
    assert_eq!(class.name.name, "Baz");
}

#[test]
fn parse_internal_sealed_class() {
    let module = parse_ok(
        "internal sealed Object subclass: Impl
  state: x = 0",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert!(class.is_internal);
    assert!(class.is_sealed);
    assert_eq!(class.name.name, "Impl");
}

#[test]
fn parse_internal_typed_class() {
    let module = parse_ok(
        "internal typed Object subclass: TypedImpl
  state: x :: Integer = 0",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert!(class.is_internal);
    assert!(class.is_typed);
    assert_eq!(class.name.name, "TypedImpl");
}

#[test]
fn parse_non_internal_class_defaults_false() {
    let module = parse_ok(
        "Object subclass: Pub
  foo => 1",
    );
    assert_eq!(module.classes.len(), 1);
    assert!(!module.classes[0].is_internal);
}

#[test]
fn parse_internal_method_unary() {
    // `internal foo => ...` — `internal` is a modifier, `foo` is the method name
    let module = parse_ok(
        "Object subclass: Svc
  internal foo => 42",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1);
    let method = &class.methods[0];
    assert_eq!(method.selector.name(), "foo");
    assert!(method.is_internal);
}

#[test]
fn parse_internal_method_keyword() {
    // `internal foo: x => ...`
    let module = parse_ok(
        "Object subclass: Svc
  internal buildHeaders: req => req",
    );
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "buildHeaders:");
    assert!(method.is_internal);
}

#[test]
fn parse_method_named_internal() {
    // A bare `internal => ...` defines a unary method named `internal`, NOT a modifier.
    let module = parse_ok(
        "Object subclass: Svc
  internal => 42",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.methods.len(), 1);
    let method = &class.methods[0];
    assert_eq!(method.selector.name(), "internal");
    assert!(
        !method.is_internal,
        "bare `internal =>` should not set is_internal"
    );
}

#[test]
fn parse_method_named_internal_with_return_type() {
    // `internal -> Type =>` is a method named `internal` with a return type, not a modifier.
    let module = parse_ok(
        "Object subclass: Svc
  internal -> Boolean => true",
    );
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "internal");
    assert!(!method.is_internal);
    assert!(method.return_type.is_some());
}

#[test]
fn parse_sealed_internal_method() {
    // Modifiers can combine: `sealed internal foo => ...`
    let module = parse_ok(
        "Object subclass: Svc
  sealed internal foo => 42",
    );
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "foo");
    assert!(method.is_sealed);
    assert!(method.is_internal);
}

#[test]
fn parse_internal_sealed_method() {
    // Reverse order: `internal sealed foo => ...`
    let module = parse_ok(
        "Object subclass: Svc
  internal sealed foo => 42",
    );
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "foo");
    assert!(method.is_sealed);
    assert!(method.is_internal);
}

#[test]
fn parse_non_internal_method_defaults_false() {
    let module = parse_ok(
        "Object subclass: Svc
  foo => 42",
    );
    assert_eq!(module.classes.len(), 1);
    assert!(!module.classes[0].methods[0].is_internal);
}

#[test]
fn parse_internal_class_method() {
    // `internal class foo => ...` — internal class-side method
    let module = parse_ok(
        "Object subclass: Svc
  internal class create => self new",
    );
    assert_eq!(module.classes.len(), 1);
    let class = &module.classes[0];
    assert_eq!(class.class_methods.len(), 1);
    let method = &class.class_methods[0];
    assert_eq!(method.selector.name(), "create");
    assert!(method.is_internal);
}

#[test]
fn parse_internal_standalone_method() {
    // `Foo >> internal bar => ...` — standalone method with internal modifier
    let module = parse_ok("Foo >> internal bar => 42");
    assert_eq!(module.method_definitions.len(), 1);
    let standalone = &module.method_definitions[0];
    assert_eq!(standalone.class_name.name, "Foo");
    assert_eq!(standalone.method.selector.name(), "bar");
    assert!(standalone.method.is_internal);
}

#[test]
fn parse_standalone_method_named_internal() {
    // `Foo >> internal => 42` — standalone method named `internal`, not a modifier
    let module = parse_ok("Foo >> internal => 42");
    assert_eq!(module.method_definitions.len(), 1);
    let standalone = &module.method_definitions[0];
    assert_eq!(standalone.method.selector.name(), "internal");
    assert!(!standalone.method.is_internal);
}

#[test]
fn parse_internal_binary_method() {
    // `internal + other => ...` — internal binary method
    let module = parse_ok(
        "Object subclass: Svc
  internal + other => 42",
    );
    assert_eq!(module.classes.len(), 1);
    let method = &module.classes[0].methods[0];
    assert_eq!(method.selector.name(), "+");
    assert!(method.is_internal);
}
