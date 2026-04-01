// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for standalone method definitions, cast (!) terminators, period separator
//! warnings, and comment attachment.
use super::*;

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
