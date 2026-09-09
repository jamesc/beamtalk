// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Assignment/return/message-send unparsing, synthesized methods, blank-line
//! preservation, method-body comment indentation, class-side method
//! prefix placement, file trailing comments, match-expression formatting, and
//! map-literal formatting.

use super::common::*;

// --- Assignment ---

#[test]
fn assignment_expression() {
    let expr = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("x", span()))),
        value: Box::new(Expression::Literal(Literal::Integer(5), span())),
        type_annotation: None,
        span: span(),
    };
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "x := 5");
}

#[test]
fn annotated_assignment_expression() {
    let expr = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("x", span()))),
        value: Box::new(Expression::Literal(Literal::Integer(5), span())),
        type_annotation: Some(TypeAnnotation::simple("Integer", span())),
        span: span(),
    };
    assert_eq!(
        unparse_expression(&expr).to_pretty_string(),
        "x :: Integer := 5"
    );
}

#[test]
fn annotated_assignment_union_type() {
    let expr = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("name", span()))),
        value: Box::new(Expression::Identifier(Identifier::new("val", span()))),
        type_annotation: Some(TypeAnnotation::union(
            vec![
                TypeAnnotation::simple("String", span()),
                TypeAnnotation::simple("nil", span()),
            ],
            span(),
        )),
        span: span(),
    };
    assert_eq!(
        unparse_expression(&expr).to_pretty_string(),
        "name :: String | nil := val"
    );
}

#[test]
fn annotated_assignment_generic_type() {
    let expr = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("r", span()))),
        value: Box::new(Expression::Identifier(Identifier::new("compute", span()))),
        type_annotation: Some(TypeAnnotation::generic(
            Identifier::new("Result", span()),
            vec![
                TypeAnnotation::simple("Integer", span()),
                TypeAnnotation::simple("Error", span()),
            ],
            span(),
        )),
        span: span(),
    };
    assert_eq!(
        unparse_expression(&expr).to_pretty_string(),
        "r :: Result(Integer, Error) := compute"
    );
}

// --- Return ---

#[test]
fn return_expression() {
    let expr = Expression::Return {
        value: Box::new(Expression::Identifier(Identifier::new("x", span()))),
        span: span(),
    };
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "^x");
}

// --- Message send ---

#[test]
fn unary_message_send() {
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new("coll", span()))),
        selector: MessageSelector::Unary("size".into()),
        arguments: Vec::new(),
        is_cast: false,
        span: span(),
    };
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "coll size");
}

#[test]
fn binary_message_send() {
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::Literal(Literal::Integer(3), span())),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![Expression::Literal(Literal::Integer(4), span())],
        is_cast: false,
        span: span(),
    };
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "3 + 4");
}

#[test]
fn keyword_message_send() {
    use crate::ast::KeywordPart;
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::Identifier(Identifier::new("arr", span()))),
        selector: MessageSelector::Keyword(vec![
            KeywordPart::new("at:", span()),
            KeywordPart::new("put:", span()),
        ]),
        arguments: vec![
            Expression::Literal(Literal::Integer(1), span()),
            Expression::Literal(Literal::String("x".into()), span()),
        ],
        is_cast: false,
        span: span(),
    };
    assert_eq!(
        unparse_expression(&expr).to_pretty_string(),
        "arr at: 1 put: \"x\""
    );
}

// --- Synthesized method (no source text) ---

#[test]
fn synthesized_method_produces_valid_source() {
    // Synthesized method — built from data with no source text, Span::default()
    let method = MethodDefinition::new(
        MessageSelector::Keyword(vec![crate::ast::KeywordPart::new(
            "withValue:",
            Span::default(),
        )]),
        vec![ParameterDefinition::new(Identifier::new(
            "v",
            Span::default(),
        ))],
        vec![ExpressionStatement::bare(Expression::Identifier(
            Identifier::new("v", Span::default()),
        ))],
        Span::default(),
    );
    let source = unparse_method(&method);
    // Single-expr body is inline; must be valid-looking source
    assert_eq!(source, "withValue: v => v");
}

#[test]
fn method_with_leading_comment_in_body() {
    // Method where a body expression has a leading comment — forces multi-line output
    let body_stmt = ExpressionStatement {
        comments: CommentAttachment {
            leading: vec![Comment::line("the result", span())],
            trailing: None,
            leading_blank_line: false,
            blank_line_after_comments: false,
        },
        expression: Expression::Identifier(Identifier::new("x", span())),
        preceding_blank_line: false,
    };
    let method = MethodDefinition::new(
        MessageSelector::Unary("compute".into()),
        Vec::new(),
        vec![body_stmt],
        span(),
    );
    let output = unparse_method_definition(&method).to_pretty_string();
    // Leading comment in body forces multi-line, comment indented with body
    assert_eq!(output, "compute =>\n  // the result\n  x");
}

// --- Blank line preservation ---

#[test]
fn method_body_blank_line_preserved() {
    let body = vec![
        ExpressionStatement::bare(Expression::Literal(Literal::Integer(1), span())),
        ExpressionStatement {
            comments: CommentAttachment::default(),
            expression: Expression::Literal(Literal::Integer(2), span()),
            preceding_blank_line: true,
        },
        ExpressionStatement::bare(Expression::Literal(Literal::Integer(3), span())),
    ];
    let method = MethodDefinition::new(
        MessageSelector::Unary("doStuff".into()),
        Vec::new(),
        body,
        span(),
    );
    let output = unparse_method_definition(&method).to_pretty_string();
    // Blank line before `2` but not before `3`
    assert_eq!(output, "doStuff =>\n  1\n\n  2\n  3");
}

#[test]
fn method_body_no_blank_lines() {
    let body = vec![
        ExpressionStatement::bare(Expression::Literal(Literal::Integer(1), span())),
        ExpressionStatement::bare(Expression::Literal(Literal::Integer(2), span())),
    ];
    let method = MethodDefinition::new(
        MessageSelector::Unary("doStuff".into()),
        Vec::new(),
        body,
        span(),
    );
    let output = unparse_method_definition(&method).to_pretty_string();
    assert_eq!(output, "doStuff =>\n  1\n  2");
}

// --- Method body comment indentation ---

#[test]
fn method_body_leading_comment_indented_with_body() {
    // A single expression with a leading comment forces multi-line.
    // The comment must be indented at the same level as the expression.
    let body_stmt = ExpressionStatement {
        comments: CommentAttachment {
            leading: vec![Comment::line("do the thing", span())],
            trailing: None,
            leading_blank_line: false,
            blank_line_after_comments: false,
        },
        expression: Expression::Identifier(Identifier::new("x", span())),
        preceding_blank_line: false,
    };
    let method = MethodDefinition::new(
        MessageSelector::Unary("compute".into()),
        Vec::new(),
        vec![body_stmt],
        span(),
    );
    let output = unparse_method_definition(&method).to_pretty_string();
    assert_eq!(output, "compute =>\n  // do the thing\n  x");
}

#[test]
fn method_body_multiple_leading_comments_indented() {
    let body_stmt = ExpressionStatement {
        comments: CommentAttachment {
            leading: vec![
                Comment::line("first comment", span()),
                Comment::line("second comment", span()),
            ],
            trailing: None,
            leading_blank_line: false,
            blank_line_after_comments: false,
        },
        expression: Expression::Literal(Literal::Integer(42), span()),
        preceding_blank_line: false,
    };
    let method = MethodDefinition::new(
        MessageSelector::Unary("run".into()),
        Vec::new(),
        vec![body_stmt],
        span(),
    );
    let output = unparse_method_definition(&method).to_pretty_string();
    assert_eq!(
        output,
        "run =>\n  // first comment\n  // second comment\n  42"
    );
}

#[test]
fn method_body_blank_line_has_no_trailing_whitespace() {
    let body = vec![
        ExpressionStatement::bare(Expression::Literal(Literal::Integer(1), span())),
        ExpressionStatement {
            comments: CommentAttachment::default(),
            expression: Expression::Literal(Literal::Integer(2), span()),
            preceding_blank_line: true,
        },
    ];
    let method = MethodDefinition::new(
        MessageSelector::Unary("doStuff".into()),
        Vec::new(),
        body,
        span(),
    );
    let output = unparse_method_definition(&method).to_pretty_string();
    // The blank line between 1 and 2 must be truly empty (no trailing spaces).
    for (i, line_text) in output.lines().enumerate() {
        assert_eq!(
            line_text,
            line_text.trim_end(),
            "line {i} has trailing whitespace: {line_text:?}"
        );
    }
}

// --- Class-side method prefix placement ---

#[test]
fn class_method_doc_comment_before_class_keyword() {
    // The `class` keyword must appear on the signature line, not before the doc comment.
    let source = "Object subclass: Foo\n  /// Doc for bar\n  class bar => 1\n";
    let module = parse_source(source);
    let output = unparse_module(&module);
    // Doc comment should come first, then "class bar =>"
    assert!(
        output.contains("/// Doc for bar\n  class bar => 1"),
        "expected doc comment before 'class' keyword in: {output}"
    );
}

#[test]
fn state_declaration_doc_comment_round_trips() {
    // A `///` doc comment before `state:` must survive a parse → unparse round-trip.
    let source = "Object subclass: Foo\n  /// The current count.\n  state: count = 0\n";
    let module = parse_source(source);
    let output = unparse_module(&module);
    assert!(
        output.contains("  /// The current count.\n  state: count = 0"),
        "expected indented doc comment before state: in: {output}"
    );
}

// --- File trailing comments ---

#[test]
fn file_trailing_comments_preserved() {
    let source =
        "Object subclass: Foo\n  bar => 1\n\n// trailing comment 1\n// trailing comment 2\n";
    let module = parse_source(source);
    assert_eq!(
        module.file_trailing_comments.len(),
        2,
        "expected 2 trailing comments, got: {:?}",
        module.file_trailing_comments
    );
    let output = unparse_module(&module);
    assert!(
        output.contains("// trailing comment 1"),
        "missing trailing comment 1 in: {output}"
    );
    assert!(
        output.contains("// trailing comment 2"),
        "missing trailing comment 2 in: {output}"
    );
}

// --- Match expression formatting ---

#[test]
fn match_single_arm_inline() {
    let source = "Actor subclass: A\n  m => x match: [1 -> \"one\"]";
    let module = parse_source(source);
    let output = unparse_module(&module);
    assert!(
        output.contains("x match: [1 -> \"one\"]"),
        "single arm should be inline: {output}"
    );
}

#[test]
fn match_multi_arm_one_per_line() {
    let source = "Actor subclass: A\n  m => x match: [1 -> \"one\"; 2 -> \"two\"; _ -> \"other\"]";
    let module = parse_source(source);
    let output = unparse_module(&module);
    // Each arm should be on its own line, indented inside the brackets
    assert!(
        output.contains("x match: [\n"),
        "multi-arm match should break after opening bracket: {output}"
    );
    assert!(
        output.contains("1 -> \"one\";\n"),
        "first arm should end with semicolon: {output}"
    );
    assert!(
        output.contains("_ -> \"other\"\n"),
        "last arm should not have semicolon: {output}"
    );
}

#[test]
fn match_nil_pattern_round_trips() {
    // ADR 0107 Phase A: `nil` pattern round-trips through parse + unparse.
    let source = "Actor subclass: A\n  m => x match: [nil -> \"none\"; _ -> \"other\"]";
    let module = parse_source(source);
    let output = unparse_module(&module);
    assert!(
        output.contains("nil -> \"none\""),
        "nil pattern should round-trip: {output}"
    );
}

#[test]
fn match_type_pattern_round_trips() {
    // ADR 0107 Phase A: `binding :: ClassName` type pattern round-trips
    // through parse + unparse.
    let source = "Actor subclass: A\n  m => x match: [path :: String -> path; _ -> \"other\"]";
    let module = parse_source(source);
    let output = unparse_module(&module);
    assert!(
        output.contains("path :: String -> path"),
        "type pattern should round-trip: {output}"
    );
}

#[test]
fn cascade_short_stays_inline() {
    let source = "Actor subclass: A\n  m => self foo; bar; baz";
    let module = parse_source(source);
    let output = unparse_module(&module);
    assert!(
        output.contains("self foo; bar; baz"),
        "short cascade should stay inline: {output}"
    );
}

#[test]
fn cascade_multi_message_one_per_line() {
    let source = "Actor subclass: A\n  m =>\n    self assert: txn amount equals: 500; assert: txn from equals: \"Alice\"; assert: txn to equals: \"Bob\"";
    let module = parse_source(source);
    let output = unparse_module(&module);
    // Each cascade message should be on its own line
    assert!(
        output.contains("assert: txn amount equals: 500;\n"),
        "first cascade message should end with semicolon+newline: {output}"
    );
    assert!(
        output.contains("assert: txn from equals: \"Alice\";\n"),
        "middle cascade message should end with semicolon+newline: {output}"
    );
    assert!(
        output.contains("assert: txn to equals: \"Bob\""),
        "last cascade message should not have trailing semicolon: {output}"
    );
}

// --- Map literal formatting ---

#[test]
fn map_literal_short_stays_inline() {
    let source = "Actor subclass: A\n  m => #{#a => 1, #b => 2}";
    let module = parse_source(source);
    let output = unparse_module(&module);
    assert!(
        output.contains("#{#a => 1, #b => 2}"),
        "short map should stay inline: {output}"
    );
}

#[test]
fn map_literal_long_breaks() {
    let source = "Actor subclass: A\n  m => #{#name => \"Alice\", #age => 30, #city => \"Wonderland\", #country => \"Fantasy\"}";
    let module = parse_source(source);
    let output = unparse_module(&module);
    assert!(
        output.contains("#{\n"),
        "long map should break to multi-line: {output}"
    );
}
