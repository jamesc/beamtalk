// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `format_source`, `matchExhaustive:` round-tripping, difference/intersection/
//! grouped type-annotation unparsing, comment unparsing, expression-statement
//! comments, method definitions, and literal round-trips (moved from
//! `unparse/mod.rs` per BT-3450).

use super::common::*;

// --- format_source ---

#[test]
fn format_source_parse_error_returns_none() {
    // Source with a parse error must return None.
    let result = format_source("@@@invalid beamtalk source@@@");
    assert!(result.is_none(), "parse error should return None");
}

#[test]
fn format_source_valid_source_is_formatted() {
    // Valid source must be returned formatted (with trailing newline).
    let source = "Object subclass: Foo\n  bar => 42\n";
    let result = format_source(source);
    assert!(result.is_some(), "valid source should return Some");
    let formatted = result.unwrap();
    assert!(formatted.ends_with('\n'), "output must end with newline");
    assert!(
        formatted.contains("bar => 42"),
        "formatted output must contain the method"
    );
}

#[test]
fn format_source_is_idempotent() {
    // format_source(format_source(s).unwrap()).unwrap() == format_source(s).unwrap()
    let source = "Actor subclass: Counter\n  state: value = 0\n\n  getValue => self.value\n";
    let pass1 = format_source(source).expect("pass1 should succeed");
    let pass2 = format_source(&pass1).expect("pass2 should succeed");
    assert_eq!(pass1, pass2, "format_source must be idempotent");
}

// --- `matchExhaustive:` unparse round-trip (BT-2763 / ADR 0106; moved
// from `source_analysis::parser::expressions`, BT-3346 / ADR 0117 Phase 4) ---

#[test]
fn match_exhaustive_unparse_round_trips_keyword() {
    let formatted =
        format_source("x matchExhaustive: [#a -> 1; #b -> 2]").expect("formatting should succeed");
    assert!(
        formatted.contains("matchExhaustive:"),
        "unparse output should preserve matchExhaustive: keyword, got: {formatted}"
    );
    assert!(
        !formatted.contains(" match: ["),
        "unparse output must not downgrade matchExhaustive: to match:, got: {formatted}"
    );
}

#[test]
fn plain_match_unparse_does_not_gain_exhaustive_keyword() {
    let formatted =
        format_source("x match: [#a -> 1; #b -> 2]").expect("formatting should succeed");
    assert!(
        formatted.contains(" match: ["),
        "plain match: should round-trip unchanged, got: {formatted}"
    );
    assert!(
        !formatted.contains("matchExhaustive:"),
        "plain match: must not gain the exhaustive keyword, got: {formatted}"
    );
}

// --- Difference type annotation unparsing (BT-2742) ---

#[test]
fn difference_type_annotation_unparses() {
    // `Symbol \ #foo` unparses back to `Symbol \ #foo`.
    let ann = crate::ast::TypeAnnotation::difference(
        crate::ast::TypeAnnotation::simple("Symbol", span()),
        crate::ast::TypeAnnotation::singleton("foo", span()),
        span(),
    );
    assert_eq!(
        unparse_type_annotation(&ann).to_pretty_string(),
        "Symbol \\ #foo"
    );
}

#[test]
fn difference_return_type_round_trips_through_format_source() {
    // Parse → unparse must preserve `Symbol \ #foo` in a method return type.
    let source = "Object subclass: Foo\n  narrow -> Symbol \\ #foo => #bar\n";
    let formatted = format_source(source).expect("should format");
    assert!(
        formatted.contains("Symbol \\ #foo"),
        "difference type must survive round-trip, got: {formatted}"
    );
}

// --- Intersection type annotation unparsing (ADR 0068 §Protocol
// Composition, ADR 0102 §1/§3, BT-2743) ---

#[test]
fn intersection_type_annotation_unparses() {
    // `Printable & Comparable` unparses back to `Printable & Comparable`.
    let ann = crate::ast::TypeAnnotation::intersection(
        crate::ast::TypeAnnotation::simple("Printable", span()),
        crate::ast::TypeAnnotation::simple("Comparable", span()),
        span(),
    );
    assert_eq!(
        unparse_type_annotation(&ann).to_pretty_string(),
        "Printable & Comparable"
    );
}

#[test]
fn intersection_return_type_round_trips_through_format_source() {
    // Parse → unparse must preserve `Collection(Object) & Comparable` in a
    // method return type.
    let source = "Object subclass: Foo\n  narrow -> Collection(Object) & Comparable => self\n";
    let formatted = format_source(source).expect("should format");
    assert!(
        formatted.contains("Collection(Object) & Comparable"),
        "intersection type must survive round-trip, got: {formatted}"
    );
}

// --- Grouping parentheses in type annotations (BT-2760) ---

#[test]
fn grouped_difference_operands_unparse_with_parens() {
    // `Difference { base: Intersection }` must re-derive the grouping
    // parens — the bare form `A & B \ #c` is the mixed-operator parse
    // error (ADR 0102 §3).
    let ann = crate::ast::TypeAnnotation::difference(
        crate::ast::TypeAnnotation::intersection(
            crate::ast::TypeAnnotation::simple("A", span()),
            crate::ast::TypeAnnotation::simple("B", span()),
            span(),
        ),
        crate::ast::TypeAnnotation::singleton("c", span()),
        span(),
    );
    assert_eq!(
        unparse_type_annotation(&ann).to_pretty_string(),
        "(A & B) \\ #c"
    );
}

#[test]
fn grouped_union_excluded_unparses_with_parens() {
    // `Symbol \ (#a | #b)` — a union excluded operand keeps its parens
    // (`\` binds tighter than `|`).
    let ann = crate::ast::TypeAnnotation::difference(
        crate::ast::TypeAnnotation::simple("Symbol", span()),
        crate::ast::TypeAnnotation::union(
            vec![
                crate::ast::TypeAnnotation::singleton("a", span()),
                crate::ast::TypeAnnotation::singleton("b", span()),
            ],
            span(),
        ),
        span(),
    );
    assert_eq!(
        unparse_type_annotation(&ann).to_pretty_string(),
        "Symbol \\ (#a | #b)"
    );
}

#[test]
fn right_nested_difference_unparses_with_parens() {
    // `\` is left-associative, so a right-nested difference must keep
    // its parens: `Symbol \ (#a \ #b)`, not `Symbol \ #a \ #b`.
    let ann = crate::ast::TypeAnnotation::difference(
        crate::ast::TypeAnnotation::simple("Symbol", span()),
        crate::ast::TypeAnnotation::difference(
            crate::ast::TypeAnnotation::singleton("a", span()),
            crate::ast::TypeAnnotation::singleton("b", span()),
            span(),
        ),
        span(),
    );
    assert_eq!(
        unparse_type_annotation(&ann).to_pretty_string(),
        "Symbol \\ (#a \\ #b)"
    );
}

#[test]
fn grouped_mixed_types_round_trip_through_format_source() {
    // Parse → unparse preserves the parenthesised mixed forms (BT-2760),
    // and the formatter is idempotent on them.
    for (source, expected) in [
        (
            "Object subclass: Foo\n  narrow -> (A & B) \\ #c => self\n",
            "(A & B) \\ #c",
        ),
        (
            "Object subclass: Foo\n  narrow -> A & (B \\ #c) => self\n",
            "A & (B \\ #c)",
        ),
        (
            "Object subclass: Foo\n  narrow -> Symbol \\ (#a | #b) => #c\n",
            "Symbol \\ (#a | #b)",
        ),
        (
            "Object subclass: Foo\n  narrow -> Integer | (Symbol \\ #foo) => 1\n",
            // The group is redundant (`\` already binds tighter than
            // `|`), so the formatter drops it.
            "Integer | Symbol \\ #foo",
        ),
    ] {
        let formatted = format_source(source).expect("should format");
        assert!(
            formatted.contains(expected),
            "grouped type must survive round-trip: expected `{expected}` in: {formatted}"
        );
        let reformatted = format_source(&formatted).expect("should reformat");
        assert_eq!(formatted, reformatted, "format must be idempotent");
    }
}

// --- Comment unparsing ---

#[test]
fn line_comment_document() {
    let c = Comment::line("hello world", span());
    assert_eq!(unparse_comment(&c).to_pretty_string(), "// hello world");
}

#[test]
fn block_comment_document() {
    let c = Comment::block("block text", span());
    assert_eq!(unparse_comment(&c).to_pretty_string(), "/* block text */");
}

// --- Expression statement with comments ---

#[test]
fn expression_statement_no_comments() {
    let stmt = ExpressionStatement::bare(Expression::Literal(Literal::Integer(42), span()));
    assert_eq!(unparse_expression_statement(&stmt).to_pretty_string(), "42");
}

#[test]
fn expression_statement_leading_comment() {
    let stmt = ExpressionStatement {
        comments: CommentAttachment {
            leading: vec![Comment::line("This is 42", span())],
            trailing: None,
            leading_blank_line: false,
            blank_line_after_comments: false,
        },
        expression: Expression::Literal(Literal::Integer(42), span()),
        preceding_blank_line: false,
    };
    let output = unparse_expression_statement(&stmt).to_pretty_string();
    assert_eq!(output, "// This is 42\n42");
}

#[test]
fn expression_statement_trailing_comment() {
    let stmt = ExpressionStatement {
        comments: CommentAttachment {
            leading: Vec::new(),
            trailing: Some(Comment::line("trailing", span())),
            leading_blank_line: false,
            blank_line_after_comments: false,
        },
        expression: Expression::Literal(Literal::Integer(1), span()),
        preceding_blank_line: false,
    };
    let output = unparse_expression_statement(&stmt).to_pretty_string();
    assert_eq!(output, "1  // trailing");
}

#[test]
fn expression_statement_both_comments() {
    let stmt = ExpressionStatement {
        comments: CommentAttachment {
            leading: vec![Comment::line("before", span())],
            trailing: Some(Comment::line("after", span())),
            leading_blank_line: false,
            blank_line_after_comments: false,
        },
        expression: Expression::Literal(Literal::Integer(99), span()),
        preceding_blank_line: false,
    };
    let output = unparse_expression_statement(&stmt).to_pretty_string();
    assert_eq!(output, "// before\n99  // after");
}

// --- Method definition ---

#[test]
fn method_unary_single_expr_inline() {
    let method = MethodDefinition::new(
        MessageSelector::Unary("increment".into()),
        Vec::new(),
        vec![ExpressionStatement::bare(Expression::Literal(
            Literal::Integer(1),
            span(),
        ))],
        span(),
    );
    let output = unparse_method_definition(&method).to_pretty_string();
    // Single-expression body goes on same line
    assert_eq!(output, "increment => 1");
}

#[test]
fn method_with_leading_comment_single_expr() {
    let mut method = MethodDefinition::new(
        MessageSelector::Unary("getValue".into()),
        Vec::new(),
        vec![ExpressionStatement::bare(Expression::Literal(
            Literal::Integer(0),
            span(),
        ))],
        span(),
    );
    method.comments = CommentAttachment {
        leading: vec![Comment::line("Returns the current value", span())],
        trailing: None,
        leading_blank_line: false,
        blank_line_after_comments: false,
    };
    let output = unparse_method_definition(&method).to_pretty_string();
    assert_eq!(output, "// Returns the current value\ngetValue => 0");
}

#[test]
fn method_with_doc_comment() {
    let mut method = MethodDefinition::new(
        MessageSelector::Unary("size".into()),
        Vec::new(),
        vec![ExpressionStatement::bare(Expression::Identifier(
            Identifier::new("n", span()),
        ))],
        span(),
    );
    method.doc_comment = Some("Returns the size.".into());
    let output = unparse_method_definition(&method).to_pretty_string();
    assert_eq!(output, "/// Returns the size.\nsize => n");
}

#[test]
fn state_declaration_with_doc_comment() {
    let mut state = StateDeclaration::new(Identifier::new("count", span()), span());
    state.doc_comment = Some("The current count.".into());
    let output = unparse_state_declaration(&state).to_pretty_string();
    assert_eq!(output, "/// The current count.\nstate: count");
}

#[test]
fn field_declaration_unparse() {
    use crate::ast::DeclaredKeyword;
    let mut decl = StateDeclaration::new(Identifier::new("x", span()), span());
    decl.declared_keyword = DeclaredKeyword::Field;
    let output = unparse_state_declaration(&decl).to_pretty_string();
    assert_eq!(output, "field: x");
}

#[test]
fn field_declaration_round_trip() {
    let source = "Object subclass: Foo\n  field: count = 0\n";
    let module = parse_source(source);
    let output = unparse_module(&module);
    assert!(
        output.contains("field: count = 0"),
        "expected field: in output, got: {output}"
    );
}

#[test]
fn state_declaration_with_multiline_doc_comment() {
    let mut state = StateDeclaration::new(Identifier::new("timeout", span()), span());
    state.doc_comment = Some("Timeout in milliseconds.\n\nDefaults to 30000.".into());
    let output = unparse_state_declaration(&state).to_pretty_string();
    assert_eq!(
        output,
        "/// Timeout in milliseconds.\n///\n/// Defaults to 30000.\nstate: timeout"
    );
}

#[test]
fn method_multi_expr_on_new_lines() {
    let method = MethodDefinition::new(
        MessageSelector::Unary("doTwoThings".into()),
        Vec::new(),
        vec![
            ExpressionStatement::bare(Expression::Literal(Literal::Integer(1), span())),
            ExpressionStatement::bare(Expression::Literal(Literal::Integer(2), span())),
        ],
        span(),
    );
    let output = unparse_method_definition(&method).to_pretty_string();
    assert_eq!(output, "doTwoThings =>\n  1\n  2");
}

// --- Literal round-trip ---

#[test]
fn integer_literal() {
    let expr = Expression::Literal(Literal::Integer(42), span());
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "42");
}

#[test]
fn string_literal_with_escape() {
    let expr = Expression::Literal(Literal::String("it's".into()), span());
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "\"it's\"");
}

#[test]
fn string_literal_with_embedded_double_quotes() {
    // Bare " in the AST (from doubled-delimiter unescaping) gets re-escaped as ""
    let expr = Expression::Literal(Literal::String("say \"hello\"".into()), span());
    assert_eq!(
        unparse_expression(&expr).to_pretty_string(),
        "\"say \"\"hello\"\"\""
    );
}

#[test]
fn symbol_literal_simple() {
    let expr = Expression::Literal(Literal::Symbol("ok".into()), span());
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "#ok");
}

#[test]
fn symbol_literal_with_space_needs_quoting() {
    let expr = Expression::Literal(Literal::Symbol("with space".into()), span());
    assert_eq!(
        unparse_expression(&expr).to_pretty_string(),
        "#'with space'"
    );
}

#[test]
fn float_literal() {
    let expr = Expression::Literal(Literal::Float(1.5), span());
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "1.5");
}

#[test]
fn character_literal_plain() {
    let expr = Expression::Literal(Literal::Character('A'), span());
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "$A");
}

#[test]
fn character_literal_newline() {
    let expr = Expression::Literal(Literal::Character('\n'), span());
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "$\\n");
}

#[test]
fn character_literal_tab() {
    let expr = Expression::Literal(Literal::Character('\t'), span());
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "$\\t");
}

#[test]
fn character_literal_backslash() {
    let expr = Expression::Literal(Literal::Character('\\'), span());
    assert_eq!(unparse_expression(&expr).to_pretty_string(), "$\\\\");
}
