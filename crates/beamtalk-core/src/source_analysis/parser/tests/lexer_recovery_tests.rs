// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Lexer and parser recovery and literal edge cases (BT-5182).
//!
//! An unterminated string, or a stray `/*`, must not swallow the rest of the
//! file, top-level recovery must stop at the next definition, and the number,
//! BOM, CRLF and escape edge cases must lex as one literal (or one named
//! error). Each test parses whole files the way the compiler and the LSP do,
//! so "the classes after the error are still there" is checked on the
//! module those tools index.

use super::*;
use crate::ast::{Expression, Literal, Module, Pattern};
use crate::source_analysis::{Trivia, is_input_complete, lex};

/// Parses `source` as a file: the module and its diagnostics.
fn parse_file(source: &str) -> (Module, Vec<Diagnostic>) {
    parse(lex_with_eof(source))
}

/// The names of the classes in `module`, in source order.
fn class_names(module: &Module) -> Vec<&str> {
    module
        .classes
        .iter()
        .map(|class| class.name.name.as_str())
        .collect()
}

/// The error diagnostics in `diagnostics`.
fn errors(diagnostics: &[Diagnostic]) -> Vec<&Diagnostic> {
    diagnostics
        .iter()
        .filter(|d| d.severity == Severity::Error)
        .collect()
}

/// The value of the single top-level expression in `source`, a literal.
fn literal_of(source: &str) -> Literal {
    let (module, diagnostics) = parse_file(source);
    assert!(
        errors(&diagnostics).is_empty(),
        "{source:?} should parse cleanly, got {diagnostics:?}"
    );
    match &module.expressions[0].expression {
        Expression::Literal(literal, _) => literal.clone(),
        other => panic!("{source:?} should be one literal, got {other:?}"),
    }
}

// ============================================================================
// Unterminated strings
// ============================================================================

#[test]
fn unterminated_string_keeps_later_classes() {
    let source = "Object subclass: A\n  \
                  greet => \"hello\n\n\
                  Object subclass: B\n  \
                  size => 1\n";
    let (module, diagnostics) = parse_file(source);
    assert_eq!(class_names(&module), ["A", "B"]);
    let errors = errors(&diagnostics);
    assert_eq!(errors.len(), 1, "one error for the one typo: {errors:?}");
    assert_eq!(
        errors[0].message,
        "string literal starting at line 2, column 12 is not closed before the definition \
         on line 4: add a closing \""
    );
}

#[test]
fn unterminated_string_diagnostic_names_construct_not_token_kind() {
    let source = "x := \"abc\nObject subclass: B\n  size => 1\n";
    let (_, diagnostics) = parse_file(source);
    let errors = errors(&diagnostics);
    assert_eq!(errors.len(), 1, "{errors:?}");
    let message = &errors[0].message;
    assert!(
        message.starts_with(
            "string literal starting at line 1, column 6 is not closed before the definition on line 2"
        ),
        "{message}"
    );
    assert!(!message.contains("<error"), "token kind leaked: {message}");
    assert!(
        !message.contains("subclass"),
        "rest of file leaked: {message}"
    );
    // The span covers the opening line only.
    assert_eq!(errors[0].span, Span::new(5, 9));
}

#[test]
fn unterminated_string_with_later_quotes_keeps_later_classes() {
    // The missing quote pairs the later quotes up wrongly inside `A`, but no
    // string may run over `B`'s header, so `B` and `C` still parse.
    let source = "Object subclass: A\n  \
                  one => \"oops\n  \
                  two => \"fine\"\n  \
                  three => \"also fine\"\n\n\
                  Object subclass: B\n  \
                  four => \"b\"\n\n\
                  Object subclass: C\n  \
                  five => 5\n";
    let (module, _) = parse_file(source);
    let names = class_names(&module);
    assert!(
        names.ends_with(&["B", "C"]),
        "B and C should survive the typo in A, got {names:?}"
    );
    let b = &module.classes[names.len() - 2];
    assert_eq!(b.methods.len(), 1);
    assert_eq!(b.methods[0].selector.name(), "four");
}

#[test]
fn unterminated_string_stops_at_protocol_header() {
    let source = "x := \"abc\n\nProtocol define: Greeter\n  greet\n";
    let (module, _) = parse_file(source);
    assert_eq!(module.protocols.len(), 1);
    assert_eq!(module.protocols[0].name.name, "Greeter");
}

#[test]
fn unterminated_interpolation_keeps_later_classes() {
    let source = "Object subclass: A\n  \
                  greet => \"hello {name\n\n\
                  Object subclass: B\n  \
                  size => 1\n";
    let (module, diagnostics) = parse_file(source);
    assert_eq!(class_names(&module), ["A", "B"]);
    assert!(
        errors(&diagnostics)
            .iter()
            .any(|d| d.message.starts_with("string literal starting at line 2")),
        "{diagnostics:?}"
    );
}

#[test]
fn unterminated_string_with_unknown_escape_does_not_swallow_file() {
    let source = "x := \"a\\q\nObject subclass: B\n  size => 1\n";
    let (module, diagnostics) = parse_file(source);
    assert_eq!(class_names(&module), ["B"]);
    assert!(
        errors(&diagnostics)
            .iter()
            .any(|d| d.message.starts_with("string literal starting at line 1")),
        "the open string outranks its unknown escape: {diagnostics:?}"
    );
}

#[test]
fn unknown_escape_in_unterminated_multi_line_string_reports_the_string() {
    // The string outranks the escape: more input can still close it. Spans
    // must stay in order (no token re-lexes the escape after a rewind).
    let source = "x := \"abc\nde\\q f";
    let tokens = lex(source);
    for pair in tokens.windows(2) {
        assert!(
            pair[0].span().end() <= pair[1].span().start(),
            "spans out of order: {tokens:?}"
        );
    }
    assert!(
        tokens.iter().any(
            |t| matches!(t.kind(), TokenKind::Error(m) if m.starts_with("unterminated string literal"))
        ),
        "{tokens:?}"
    );
    assert!(!is_input_complete(source));
}

#[test]
fn multi_line_string_still_lexes_as_one_string() {
    // Recovery only applies to a string with no closing quote.
    let tokens = lex("x := \"line one\nline two\"\ny := 1");
    assert!(
        tokens
            .iter()
            .any(|t| *t.kind() == TokenKind::String("line one\nline two".into())),
        "{tokens:?}"
    );
}

#[test]
fn string_cannot_span_a_class_header() {
    // A string still open at a definition header is unterminated even though
    // a quote follows the header.
    let source = "x := \"abc\nObject subclass: B\n  size => \"z\"\n";
    let (module, diagnostics) = parse_file(source);
    assert_eq!(class_names(&module), ["B"]);
    assert_eq!(errors(&diagnostics).len(), 1, "{diagnostics:?}");
}

#[test]
fn line_and_column_are_counted_in_the_file_for_multibyte_text() {
    let source = "// héllo\ny := \"é\" ++ \"abc\n";
    let (_, diagnostics) = parse_file(source);
    assert!(
        errors(&diagnostics)
            .iter()
            .any(|d| d.message.contains("starting at line 2, column 13")),
        "{diagnostics:?}"
    );
}

// ============================================================================
// Block comments are not supported (BT-5229)
// ============================================================================

const BLOCK_COMMENT_ERROR: &str = "block comments are not supported; use `//` on each line";

#[test]
fn stray_block_comment_opener_reports_one_diagnostic_and_keeps_later_classes() {
    let source = "Object subclass: A\n  \
                  /* never closed\n  \
                  greet => 1\n\n\
                  Object subclass: B\n  \
                  size => 1\n";
    let (module, diagnostics) = parse_file(source);
    assert_eq!(class_names(&module), ["A", "B"]);
    let errors = errors(&diagnostics);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(errors[0].message, BLOCK_COMMENT_ERROR);
    // The diagnostic covers the `/*` to the end of its own line, no further.
    let start = source.find("/*").expect("opener");
    let end = start + "/* never closed".len();
    assert_eq!(errors[0].span.start() as usize, start);
    assert_eq!(errors[0].span.end() as usize, end);
}

#[test]
fn stray_block_comment_opener_skips_only_its_own_line() {
    let (module, diagnostics) = parse_file("x := 1\n/* never closed\ny := 2\n");
    let errors = errors(&diagnostics);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(errors[0].message, BLOCK_COMMENT_ERROR);
    assert_eq!(module.expressions.len(), 2, "`y := 2` is still parsed");
}

#[test]
fn closed_block_comment_is_still_a_diagnostic() {
    let source = "/* note */\nObject subclass: New\n  y => 2\n";
    let (module, diagnostics) = parse_file(source);
    let errors = errors(&diagnostics);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert_eq!(errors[0].message, BLOCK_COMMENT_ERROR);
    assert_eq!(class_names(&module), ["New"]);
}

#[test]
fn commented_out_class_in_block_comment_does_not_hide_later_classes() {
    let source = "/*\nObject subclass: Old\n  x => 1\n*/\nObject subclass: New\n  y => 2\n";
    let (module, diagnostics) = parse_file(source);
    assert!(
        errors(&diagnostics)
            .iter()
            .any(|d| d.message == BLOCK_COMMENT_ERROR),
        "{diagnostics:?}"
    );
    assert!(
        class_names(&module).contains(&"New"),
        "{:?}",
        class_names(&module)
    );
}

// ============================================================================
// Top-level recovery
// ============================================================================

#[test]
fn stray_token_before_class_header_does_not_skip_the_class() {
    let source = ")\nObject subclass: Foo\n  bar => 1\n";
    let (module, diagnostics) = parse_file(source);
    assert_eq!(class_names(&module), ["Foo"]);
    assert_eq!(module.classes[0].methods.len(), 1);
    assert_eq!(errors(&diagnostics).len(), 1, "no cascade: {diagnostics:?}");
}

#[test]
fn top_level_error_recovers_at_later_class_header() {
    let source = "x := ) ) foo\nbar baz\nObject subclass: Foo\n  bar => 1\n";
    let (module, _) = parse_file(source);
    assert_eq!(class_names(&module), ["Foo"]);
    assert_eq!(module.classes[0].methods.len(), 1);
}

#[test]
fn top_level_error_recovers_at_protocol_header() {
    let source = "]\nProtocol define: Greeter\n  greet\n";
    let (module, diagnostics) = parse_file(source);
    assert_eq!(module.protocols.len(), 1);
    assert_eq!(errors(&diagnostics).len(), 1, "{diagnostics:?}");
}

#[test]
fn top_level_error_recovers_at_modified_class_header() {
    let source = "}\nsealed Object subclass: Foo\n  bar => 1\n";
    let (module, _) = parse_file(source);
    assert_eq!(class_names(&module), ["Foo"]);
}

// ============================================================================
// Numbers
// ============================================================================

#[test]
fn negative_radix_is_one_literal() {
    let tokens = lex("-16rFF");
    assert_eq!(tokens.len(), 1, "{tokens:?}");
    assert_eq!(*tokens[0].kind(), TokenKind::Integer("-16rFF".into()));
    assert_eq!(literal_of("-16rFF"), Literal::Integer(-255));
    assert_eq!(literal_of("-2r1010"), Literal::Integer(-10));
}

#[test]
fn negative_radix_after_an_operand_is_still_one_literal() {
    // As with `a -5`, a `-` glued to the digits belongs to the literal.
    let tokens = lex("a -16rFF");
    assert_eq!(tokens.len(), 2, "{tokens:?}");
    assert_eq!(*tokens[1].kind(), TokenKind::Integer("-16rFF".into()));
}

#[test]
fn integer_literals_of_any_size_parse() {
    assert_eq!(
        literal_of("100000000000000000000"),
        Literal::BigInteger("100000000000000000000".into())
    );
    assert_eq!(
        literal_of("-100000000000000000000"),
        Literal::BigInteger("-100000000000000000000".into())
    );
    // 2^72 - 1 in hex
    assert_eq!(
        literal_of("16rFFFFFFFFFFFFFFFFFF"),
        Literal::BigInteger("4722366482869645213695".into())
    );
    assert_eq!(
        literal_of("-36rZZZZZZZZZZZZZZZZ"),
        Literal::BigInteger("-7958661109946400884391935".into())
    );
}

#[test]
fn integer_literal_length_is_capped() {
    let at_limit = "9".repeat(10_000);
    assert!(matches!(literal_of(&at_limit), Literal::BigInteger(digits) if digits == at_limit));
    let (_, diagnostics) = parse_file(&"9".repeat(10_001));
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message == "Integer literal is too long (10001 characters, maximum 10000)"),
        "{diagnostics:?}"
    );
}

#[test]
fn integer_literals_at_the_i64_boundary_stay_small() {
    assert_eq!(
        literal_of("9223372036854775807"),
        Literal::Integer(i64::MAX)
    );
    assert_eq!(
        literal_of("-9223372036854775808"),
        Literal::Integer(i64::MIN)
    );
    assert_eq!(
        literal_of("9223372036854775808"),
        Literal::BigInteger("9223372036854775808".into())
    );
    assert_eq!(
        literal_of("-9223372036854775809"),
        Literal::BigInteger("-9223372036854775809".into())
    );
    // Leading zeros are not part of the canonical value.
    assert_eq!(literal_of("000000000000000000000042"), Literal::Integer(42));
    assert_eq!(literal_of("-0"), Literal::Integer(0));
}

#[test]
fn negated_big_integer_patterns_normalise() {
    let (module, diagnostics) =
        parse_file("x match: [- 9223372036854775808 -> 1; - 100000000000000000000 -> 2; _ -> 3]");
    assert!(errors(&diagnostics).is_empty(), "{diagnostics:?}");
    let Expression::Match { arms, .. } = &module.expressions[0].expression else {
        panic!("expected a match expression");
    };
    assert!(matches!(
        arms[0].pattern,
        Pattern::Literal(Literal::Integer(i64::MIN), _)
    ));
    assert!(matches!(
        &arms[1].pattern,
        Pattern::Literal(Literal::BigInteger(digits), _) if digits == "-100000000000000000000"
    ));
}

#[test]
fn literal_negation_round_trips() {
    for value in [0, 1, -1, i64::MAX, i64::MIN] {
        let literal = Literal::Integer(value);
        assert_eq!(literal.clone().negated().negated(), literal, "{value}");
    }
    assert_eq!(
        Literal::Integer(i64::MIN).negated(),
        Literal::BigInteger("9223372036854775808".into())
    );
    assert_eq!(
        Literal::BigInteger("9223372036854775808".into()).negated(),
        Literal::Integer(i64::MIN)
    );
}

#[test]
fn exponent_without_digits_is_a_named_error() {
    for source in ["1e", "2.5e", "3e+", "-4e-"] {
        let tokens = lex(source);
        assert_eq!(tokens.len(), 1, "{source:?}: {tokens:?}");
        match tokens[0].kind() {
            TokenKind::Error(message) => assert!(
                message.starts_with("float exponent in `") && message.contains("has no digits"),
                "{source:?}: {message}"
            ),
            other => panic!("{source:?} should be an error token, got {other:?}"),
        }
    }
    let (_, diagnostics) = parse_file("x := 1e");
    assert!(
        diagnostics.iter().any(|d| d
            .message
            .starts_with("float exponent in `1e` has no digits")),
        "{diagnostics:?}"
    );
}

#[test]
fn radix_without_digits_is_a_named_error() {
    let (_, diagnostics) = parse_file("x := 16r");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.starts_with("radix integer `16r` has no digits")),
        "{diagnostics:?}"
    );
    // A digit invalid for the radix names the radix and the digits.
    let (_, diagnostics) = parse_file("x := 10rABC");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message == "Invalid digit for radix 10: ABC"),
        "{diagnostics:?}"
    );
}

// ============================================================================
// BOM and CRLF
// ============================================================================

#[test]
fn byte_order_mark_is_skipped() {
    let source = "\u{FEFF}Object subclass: Foo\n  bar => 1\n";
    let (module, diagnostics) = parse_file(source);
    assert!(diagnostics.is_empty(), "{diagnostics:?}");
    assert_eq!(class_names(&module), ["Foo"]);
    // Spans stay offsets into the original text: the BOM is 3 bytes.
    let tokens = lex(source);
    assert_eq!(tokens[0].span(), Span::new(3, 9));
    assert!(tokens[0].leading_trivia().is_empty());
}

#[test]
fn byte_order_mark_takes_no_column() {
    let (_, diagnostics) = parse_file("\u{FEFF}x := \"abc");
    assert!(
        diagnostics
            .iter()
            .any(|d| d.message.contains("starting at line 1, column 6")),
        "{diagnostics:?}"
    );
}

#[test]
fn byte_order_mark_only_file_is_empty() {
    let tokens = lex_with_eof("\u{FEFF}");
    assert_eq!(tokens.len(), 1);
    assert!(tokens[0].kind().is_eof());
}

#[test]
fn crlf_is_not_part_of_comment_text() {
    let source = "// line comment\r\n/// doc comment\r\nx := 1 // trailing\r\ny := 2\r\n";
    let comments: Vec<String> = lex_with_eof(source)
        .iter()
        .flat_map(|t| t.leading_trivia().iter().chain(t.trailing_trivia()))
        .filter(|trivia| matches!(trivia, Trivia::LineComment(_) | Trivia::DocComment(_)))
        .map(|trivia| trivia.as_str().to_string())
        .collect();
    assert_eq!(
        comments,
        ["// line comment", "/// doc comment", "// trailing"]
    );
}

#[test]
fn crlf_file_with_unterminated_string_recovers_at_line_end() {
    let source = "x := \"abc\r\nObject subclass: B\r\n  size => 1\r\n";
    let (module, diagnostics) = parse_file(source);
    assert_eq!(class_names(&module), ["B"]);
    let errors = errors(&diagnostics);
    assert_eq!(errors.len(), 1, "{errors:?}");
    // The `\r` of the CRLF line ending is not part of the error's span.
    assert_eq!(errors[0].span, Span::new(5, 9));
}

#[test]
fn lone_carriage_return_inside_a_comment_does_not_end_it() {
    let tokens = lex_with_eof("// a\rb\nx");
    let comment = tokens[0].leading_trivia()[0].as_str();
    assert_eq!(comment, "// a\rb");
}

// ============================================================================
// Escape tables
// ============================================================================

#[test]
fn character_and_string_escapes_agree() {
    for escape in ['r', 'n', 't', '\\', '"', '0', '{', '}'] {
        let character = lex(&format!("$\\{escape}"));
        let string = lex(&format!("\"\\{escape}\""));
        let TokenKind::String(text) = string[0].kind() else {
            panic!("\"\\{escape}\" should be a string, got {string:?}");
        };
        let mut chars = text.chars();
        let expected = chars.next().expect("one character");
        assert!(chars.next().is_none(), "\"\\{escape}\" is one character");
        assert_eq!(
            *character[0].kind(),
            TokenKind::Character(expected),
            "$\\{escape} must mean what \"\\{escape}\" means"
        );
    }
}

#[test]
fn character_nul_escape_is_nul() {
    assert_eq!(*lex("$\\0")[0].kind(), TokenKind::Character('\0'));
}

#[test]
fn unknown_character_escape_is_an_error_like_a_string_escape() {
    for escape in ['q', '\'', '(', 'x'] {
        let character = lex(&format!("$\\{escape}"));
        let string = lex(&format!("\"\\{escape}\""));
        for (what, tokens) in [("character", &character), ("string", &string)] {
            assert!(
                matches!(tokens[0].kind(), TokenKind::Error(m) if m.starts_with("unknown escape")),
                "{what} escape \\{escape} should be an unknown escape, got {tokens:?}"
            );
        }
    }
    // The character error is a complete-but-invalid diagnostic, not a
    // request for more input.
    assert!(is_input_complete("$\\q"));
}

// ============================================================================
// REPL completeness
// ============================================================================

#[test]
fn unterminated_string_is_still_incomplete_input() {
    assert!(!is_input_complete("x := \"abc"));
    assert!(!is_input_complete("x := \"abc {y"));
    assert!(!is_input_complete("x := \"abc\\"));
    // A stray `/*` is complete but invalid (BT-5229): more input cannot fix it.
    assert!(is_input_complete("x := 1 /* note"));
    assert!(is_input_complete("x := \"abc\ndef\""));
}

#[test]
fn string_cut_off_by_a_definition_is_complete_but_invalid_input() {
    // No later line can close a string the lexer stops at a definition
    // header, so the REPL must report the error, not wait for more input.
    let source = "s := \"doc\nObject subclass: Foo\n  bar => 1";
    assert!(is_input_complete(source));
    // A class header without methods still waits, but a blank line submits.
    assert!(crate::source_analysis::needs_blank_line_to_complete(
        "s := \"doc\nObject subclass: Foo"
    ));
    let (_, diagnostics) = parse_file(source);
    assert!(
        diagnostics.iter().any(|d| d.message
            == "string literal starting at line 1, column 6 is not closed before the definition \
                on line 2: add a closing \""),
        "{diagnostics:?}"
    );
}

#[test]
fn number_errors_are_complete_but_invalid_input() {
    assert!(is_input_complete("x := 1e"));
    assert!(is_input_complete("x := 16r"));
}

// ============================================================================
// Fuzz seeds
// ============================================================================

/// The BT-5182 fuzz seeds parse with the recovery they were written for, so a
/// seed that stops exercising its case fails here rather than going stale.
#[test]
fn fuzz_seeds_recover() {
    let corpus = beamtalk_workspace::cargo_workspace_root!().join("fuzz/corpus/parse_arbitrary");
    let read = |name: &str| {
        let path = corpus.join(name);
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()))
    };

    let (module, _) = parse_file(&read("037_unterminated_string.bt"));
    assert!(
        class_names(&module).ends_with(&["B", "C"]),
        "{:?}",
        class_names(&module)
    );

    let (module, _) = parse_file(&read("038_unterminated_comment.bt"));
    assert_eq!(class_names(&module), ["A", "B"]);
    assert_eq!(module.protocols.len(), 1);

    let source = read("039_bom_crlf_numbers.bt");
    assert!(source.starts_with('\u{FEFF}'), "seed keeps its BOM");
    assert!(source.contains("\r\n"), "seed keeps its CRLF line endings");
    let (module, _) = parse_file(&source);
    assert!(matches!(
        &module.expressions[0].expression,
        Expression::Assignment { value, .. }
            if matches!(**value, Expression::Literal(Literal::Integer(-255), _))
    ));
}

// ============================================================================
// Definition-header detection
// ============================================================================

/// The parser's verdict on `line` as a line of a file: does it start a class
/// or protocol definition at column 1? (The parser itself has no notion of
/// column; the lexer only stops at headers in column 1, where every
/// definition in a file starts.)
fn parser_sees_definition(line: &str) -> bool {
    let parser = Parser::new(lex_with_eof(line));
    !line.starts_with([' ', '\t'])
        && (parser.is_at_class_definition() || parser.is_at_protocol_definition())
}

/// The lexer's textual header check (used where no tokens exist yet) and the
/// parser's token check must agree on every line, or an unterminated string
/// could stop at a line the parser does not treat as a definition (or run
/// over one it does).
#[test]
fn definition_header_agrees_with_parser() {
    use crate::source_analysis::lexer::is_definition_header;

    // The shared fixture the Erlang mirror (`beamtalk_repl_editor_context:
    // is_definition_header/1`) also runs.
    let root = beamtalk_workspace::cargo_workspace_root!();
    let path =
        root.join("runtime/apps/beamtalk_workspace/test/fixtures/definition_header_corpus.json");
    let raw = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("read corpus {}: {e}", path.display()));
    let cases: Vec<serde_json::Value> = serde_json::from_str(&raw).expect("corpus is a JSON array");
    assert!(!cases.is_empty(), "corpus must have cases");
    for case in &cases {
        let line = case["line"].as_str().expect("line is a string");
        let expected = case["header"].as_bool().expect("header is a bool");
        let why = case["why"].as_str().unwrap_or("");
        assert_eq!(
            is_definition_header(line),
            expected,
            "lexer on {line:?} ({why})"
        );
        assert_eq!(
            parser_sees_definition(line),
            expected,
            "parser on {line:?} ({why})"
        );
    }

    let mut checked = 0;
    for dir in ["stdlib/src", "stdlib/test", "examples"] {
        let mut stack = vec![root.join(dir)];
        while let Some(path) = stack.pop() {
            let Ok(entries) = std::fs::read_dir(&path) else {
                continue;
            };
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    stack.push(path);
                } else if path.extension().is_some_and(|ext| ext == "bt") {
                    let source = std::fs::read_to_string(&path).unwrap_or_default();
                    for line in source.lines() {
                        assert_eq!(
                            is_definition_header(line),
                            parser_sees_definition(line),
                            "lexer and parser disagree on {line:?} in {}",
                            path.display()
                        );
                        checked += 1;
                    }
                }
            }
        }
    }
    assert!(checked > 10_000, "corpus too small: {checked} lines");
}
