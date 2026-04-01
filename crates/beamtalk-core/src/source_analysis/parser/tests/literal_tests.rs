// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for list literal parsing and keyword message elements in list literals.
use super::*;

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
