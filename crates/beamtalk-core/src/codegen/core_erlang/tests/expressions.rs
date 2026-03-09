// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for expression-level Core Erlang code generation.
//!
//! Covers literal codegen (integers, floats, symbols, strings, maps), binary
//! operators, variable naming, string interpolation, type erasure, and
//! state-variable field access patterns.

use super::*;

#[test]
fn test_generate_literal_integer() {
    let generator = CoreErlangGenerator::new("test");
    let lit = Literal::Integer(42);
    let doc = generator.generate_literal(&lit).unwrap();
    assert_eq!(doc.to_pretty_string(), "42");
}

#[test]
fn test_generate_literal_float() {
    let generator = CoreErlangGenerator::new("test");
    let lit = Literal::Float(2.5);
    let doc = generator.generate_literal(&lit).unwrap();
    assert_eq!(doc.to_pretty_string(), "2.5");
}

#[test]
fn test_generate_literal_symbol() {
    let generator = CoreErlangGenerator::new("test");
    let lit = Literal::Symbol("ok".into());
    let doc = generator.generate_literal(&lit).unwrap();
    assert_eq!(doc.to_pretty_string(), "'ok'");
}

#[test]
fn test_generate_literal_string() {
    let generator = CoreErlangGenerator::new("test");
    let lit = Literal::String("hello".into());
    let doc = generator.generate_literal(&lit).unwrap();
    // Core Erlang binary syntax: #{segment, ...}#
    // Each segment is #<charcode>(8,1,'integer',['unsigned'|['big']])
    assert_eq!(
        doc.to_pretty_string(),
        "#\
{#<104>(8,1,'integer',['unsigned'|['big']]),\
#<101>(8,1,'integer',['unsigned'|['big']]),\
#<108>(8,1,'integer',['unsigned'|['big']]),\
#<108>(8,1,'integer',['unsigned'|['big']]),\
#<111>(8,1,'integer',['unsigned'|['big']])}#",
        "String 'hello' should generate correct Core Erlang binary literal"
    );
}

#[test]
fn test_generate_binary_op_addition() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(3), Span::new(0, 1));
    let right = vec![Expression::Literal(Literal::Integer(4), Span::new(4, 5))];
    let doc = generator.generate_binary_op("+", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    // Literal operands are provably non-future: no maybe_await wrapping needed.
    assert_eq!(output, "call 'erlang':'+'(3, 4)");
}

#[test]
fn test_generate_binary_op_no_wrap_for_literals() {
    // Literals (integer, float, symbol, string) are never futures:
    // is_definitely_sync should suppress maybe_await for all literal types.
    let mut generator = CoreErlangGenerator::new("test");
    for (op, l, r) in [
        (
            "+",
            Expression::Literal(Literal::Integer(1), Span::new(0, 1)),
            Expression::Literal(Literal::Integer(2), Span::new(2, 3)),
        ),
        (
            "<",
            Expression::Literal(Literal::Integer(0), Span::new(0, 1)),
            Expression::Literal(Literal::Float(1.0), Span::new(2, 4)),
        ),
    ] {
        let doc = generator.generate_binary_op(op, &l, &[r]).unwrap();
        let output = doc.to_pretty_string();
        assert!(
            !output.contains("maybe_await"),
            "literal operands should not be wrapped in maybe_await; got: {output}"
        );
    }
}

#[test]
fn test_generate_binary_op_wraps_non_literals() {
    use crate::ast::Identifier;
    // Identifiers (other than self) may be futures: must still be wrapped.
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Identifier(Identifier {
        name: "x".into(),
        span: Span::new(0, 1),
    });
    let right = vec![Expression::Literal(Literal::Integer(1), Span::new(4, 5))];
    let doc = generator.generate_binary_op("+", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("maybe_await"),
        "non-literal left operand must be wrapped; got: {output}"
    );
    // Right operand is a literal: should NOT be wrapped.
    assert!(
        !output.ends_with("maybe_await'(1))"),
        "literal right operand should not be wrapped; got: {output}"
    );
}

#[test]
fn test_generate_string_concatenation_operator() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::String("Hello".into()), Span::new(0, 7));
    let right = vec![Expression::Literal(
        Literal::String(" World".into()),
        Span::new(11, 19),
    )];
    let doc = generator.generate_binary_op("++", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("iolist_to_binary"),
        "Should use iolist_to_binary for string concatenation. Got: {output}",
    );
    assert!(
        output.contains("binary_to_list"),
        "Should convert binaries to lists first. Got: {output}",
    );
}

#[test]
fn test_generate_strict_equality_operator() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
    let right = vec![Expression::Literal(Literal::Integer(42), Span::new(6, 8))];
    let doc = generator.generate_binary_op("=:=", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'erlang':'=:='"),
        "Should use strict equality =:=. Got: {output}",
    );
}

#[test]
fn test_generate_loose_inequality_operator() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
    let right = vec![Expression::Literal(Literal::Integer(99), Span::new(7, 9))];
    let doc = generator.generate_binary_op("/=", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'erlang':'/='"),
        "Should use loose inequality /= (negation of ==). Got: {output}",
    );
}

#[test]
fn test_generate_loose_equality_operator() {
    let mut generator = CoreErlangGenerator::new("test");
    let left = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let right = vec![Expression::Literal(Literal::Integer(5), Span::new(6, 7))];
    let doc = generator.generate_binary_op("==", &left, &right).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'erlang':'=='"),
        "Should use loose equality ==. Got: {output}",
    );
    // Literal operands are provably non-future: no maybe_await wrapping needed.
    assert_eq!(output, "call 'erlang':'=='(5, 5)");
}

#[test]
fn test_fresh_var_generation() {
    let mut generator = CoreErlangGenerator::new("test");
    let var1 = generator.fresh_var("temp");
    let var2 = generator.fresh_var("temp");
    assert_ne!(var1, var2);
    assert!(var1.starts_with("_temp"));
    assert!(var2.starts_with("_temp"));
}

#[test]
fn test_class_name_derived_from_module() {
    let generator = CoreErlangGenerator::new("my_counter_actor");
    assert_eq!(generator.class_name(), "MyCounterActor");

    let generator = CoreErlangGenerator::new("simple");
    assert_eq!(generator.class_name(), "Simple");
}

#[test]
fn test_class_name_from_identity_overrides_module() {
    let mut generator = CoreErlangGenerator::new("bt@stdlib@string");
    generator.class_identity = Some(util::ClassIdentity::new("String"));
    assert_eq!(generator.class_name(), "String");
}

#[test]
fn test_class_name_to_module_name() {
    // Single word
    assert_eq!(util::to_module_name("Counter"), "counter");

    // Multi-word CamelCase
    assert_eq!(util::to_module_name("MyCounterActor"), "my_counter_actor");

    // With acronyms
    assert_eq!(util::to_module_name("HTTPRouter"), "httprouter");

    // Mixed case
    assert_eq!(util::to_module_name("HTTPSConnection"), "httpsconnection");
}

#[test]
fn test_generated_core_erlang_compiles() {
    use std::fs;
    use std::process::Command;

    // Test a self-contained Core Erlang module to verify syntax is valid
    // This specifically tests that:
    // 1. Empty map syntax ~{}~ compiles correctly
    // 2. The overall Core Erlang structure is valid
    // Full gen_server integration is tested in integration tests
    let core_erlang = r"module 'test_module' ['get_methods'/0, 'simple_fun'/0]
  attributes []

'get_methods'/0 = fun () ->
    ~{}~

'simple_fun'/0 = fun () ->
    42

end
";

    // Write to temporary file
    let temp_dir = std::env::temp_dir();
    let core_file = temp_dir.join("test_module.core");
    fs::write(&core_file, core_erlang).expect("should write core erlang file");

    // Try to compile with erlc
    let output = Command::new("erlc")
        .arg("+from_core")
        .arg(&core_file)
        .current_dir(&temp_dir)
        .output();

    // Clean up
    let _ = fs::remove_file(&core_file);
    let beam_file = temp_dir.join("test_module.beam");
    let _ = fs::remove_file(&beam_file);

    // Check compilation result
    match output {
        Ok(output) => {
            assert!(
                output.status.success(),
                "erlc compilation failed:\nstdout: {}\nstderr: {}\nGenerated code:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
                core_erlang
            );
        }
        Err(_) => {
            // erlc not available, skip test
            println!("Skipping test - erlc not installed in CI environment");
        }
    }
}

#[test]
fn test_string_literal_core_erlang_compiles() {
    use std::fs;
    use std::process::Command;

    // Test that string literals compile correctly through the full pipeline
    // This tests the new binary syntax: #{#<value>(8,1,'integer',['unsigned'|['big']]),...}#
    let core_erlang = r"module 'test_string' ['get_greeting'/0]
  attributes []

'get_greeting'/0 = fun () ->
    #{#<104>(8,1,'integer',['unsigned'|['big']]),#<105>(8,1,'integer',['unsigned'|['big']])}#

end
";

    // Write to temporary file
    let temp_dir = std::env::temp_dir();
    let core_file = temp_dir.join("test_string.core");
    fs::write(&core_file, core_erlang).expect("should write core erlang file");

    // Try to compile with erlc
    let output = Command::new("erlc")
        .arg("+from_core")
        .arg(&core_file)
        .current_dir(&temp_dir)
        .output();

    // Clean up
    let _ = fs::remove_file(&core_file);
    let beam_file = temp_dir.join("test_string.beam");
    let _ = fs::remove_file(&beam_file);

    // Check compilation result
    match output {
        Ok(output) => {
            assert!(
                output.status.success(),
                "erlc compilation of string literal failed:\nstdout: {}\nstderr: {}\nGenerated code:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
                core_erlang
            );
        }
        Err(_) => {
            // erlc not available, skip test
            println!("Skipping test - erlc not installed in CI environment");
        }
    }
}

#[test]
fn test_temp_vars_dont_shadow_user_identifiers() {
    let mut generator = CoreErlangGenerator::new("test");

    // First, generate a whileTrue: which creates internal _Loop, _Cond, _Body temps
    let condition_block = Block::new(
        vec![],
        vec![bare(Expression::Identifier(Identifier::new(
            "false",
            Span::new(1, 6),
        )))],
        Span::new(0, 7),
    );
    let body_block = Block::new(vec![], vec![], Span::new(8, 10));

    let receiver = Expression::Block(condition_block);
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", Span::new(7, 17))]);
    let arguments = vec![Expression::Block(body_block)];

    let _doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();

    // generate_message_send registers temp vars as a side effect;
    // the document output is discarded since we only need the scope state.

    // Now push a scope with a user variable named "Loop"
    generator.push_scope();
    let user_loop_var = generator.fresh_var("Loop");

    // Access the "Loop" identifier - it should resolve to the user's binding
    let loop_id = Identifier::new("Loop", Span::new(0, 4));
    let doc = generator.generate_identifier(&loop_id).unwrap();
    let output = doc.to_pretty_string();

    // The identifier should resolve to the user's variable, not an internal temp
    assert!(
        output.contains(&user_loop_var),
        "User identifier 'Loop' should resolve to user's binding {user_loop_var}, got: {output}"
    );

    generator.pop_scope();
}

#[test]
fn test_generate_empty_map_literal() {
    let mut generator = CoreErlangGenerator::new("test");
    let pairs = vec![];
    let doc = generator.generate_map_literal(&pairs).unwrap();
    assert_eq!(doc.to_pretty_string().trim(), "~{}~");
}

#[test]
fn test_generate_map_literal_with_atoms() {
    let mut generator = CoreErlangGenerator::new("test");

    let pairs = vec![
        MapPair::new(
            Expression::Literal(Literal::Symbol("name".into()), Span::new(2, 7)),
            Expression::Literal(Literal::String("Alice".into()), Span::new(11, 18)),
            Span::new(2, 18),
        ),
        MapPair::new(
            Expression::Literal(Literal::Symbol("age".into()), Span::new(20, 24)),
            Expression::Literal(Literal::Integer(30), Span::new(28, 30)),
            Span::new(20, 30),
        ),
    ];

    let doc = generator.generate_map_literal(&pairs).unwrap();
    let output = doc.to_pretty_string();
    // Symbols become atoms in Core Erlang
    assert!(
        output.contains("'name'"),
        "Output should contain 'name': {output}",
    );
    // Strings are represented as binaries with character codes
    assert!(
        output.contains("#<65>"),
        "Output should contain character code for 'A': {output}",
    );
    assert!(
        output.contains("'age'"),
        "Output should contain 'age': {output}",
    );
    assert!(output.contains("30"), "Output should contain 30: {output}",);
}

#[test]
fn test_generate_map_literal_compiles() {
    use std::fs;
    use std::process::Command;

    let pairs = vec![MapPair::new(
        Expression::Literal(Literal::Symbol("key".into()), Span::new(2, 6)),
        Expression::Literal(Literal::String("value".into()), Span::new(10, 17)),
        Span::new(2, 17),
    )];

    let map_expr = Expression::MapLiteral {
        pairs,
        span: Span::new(0, 19),
    };

    let code = generate_repl_expression(&map_expr, "test_map_lit").expect("codegen should succeed");

    // Verify the generated Core Erlang contains the map literal syntax
    assert!(
        code.contains("~{"),
        "Should contain Core Erlang map syntax ~{{"
    );
    // Symbols become atoms in Core Erlang
    assert!(code.contains("'key'"));
    // Strings are represented as binaries with character codes
    assert!(
        code.contains("#<"),
        "String should be represented as binary"
    );

    // Try to compile with erlc if available
    let temp_dir = std::env::temp_dir();
    let core_file = temp_dir.join("test_map_lit.core");
    if let Ok(()) = fs::write(&core_file, &code) {
        let output = Command::new("erlc")
            .arg("+from_core")
            .arg(&core_file)
            .current_dir(&temp_dir)
            .output();

        // Clean up
        let _ = fs::remove_file(&core_file);
        let beam_file = temp_dir.join("test_map_lit.beam");
        let _ = fs::remove_file(&beam_file);

        // Check compilation result if erlc is available
        if let Ok(output) = output {
            assert!(
                output.status.success(),
                "erlc compilation of map literal failed:\nstdout: {}\nstderr: {}\nGenerated code:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
                code
            );
        }
    }
}

#[test]
fn test_dictionary_at_on_identifier() {
    // BT-296: Dictionary methods now go through runtime dispatch
    // BT-430: person at: #name -> beamtalk_message_dispatch:send(person, 'at:', ['name'])
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();
    generator.bind_var("person", "Person");

    let receiver = Expression::Identifier(Identifier::new("person", Span::new(0, 6)));
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("at:", Span::new(7, 10))]);
    let arguments = vec![Expression::Literal(
        Literal::Symbol("name".into()),
        Span::new(11, 16),
    )];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    // BT-430: Unified dispatch
    assert!(
        output.contains("beamtalk_message_dispatch") && output.contains("'at:'"),
        "Should generate unified dispatch for at:. Got: {output}"
    );
}

#[test]
fn test_dictionary_at_put_on_identifier() {
    // BT-296: Dictionary methods now go through runtime dispatch
    // BT-430: person at: #age put: 31 -> beamtalk_message_dispatch:send(person, 'at:put:', ['age', 31])
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();
    generator.bind_var("person", "Person");

    let receiver = Expression::Identifier(Identifier::new("person", Span::new(0, 6)));
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("at:", Span::new(7, 10)),
        KeywordPart::new("put:", Span::new(14, 18)),
    ]);
    let arguments = vec![
        Expression::Literal(Literal::Symbol("age".into()), Span::new(11, 14)),
        Expression::Literal(Literal::Integer(31), Span::new(19, 21)),
    ];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("beamtalk_message_dispatch") && output.contains("'at:put:'"),
        "Should generate unified dispatch for at:put:. Got: {output}"
    );
}

#[test]
fn test_dictionary_size_on_identifier() {
    // BT-296: Dictionary methods now go through runtime dispatch
    // BT-430: person size -> beamtalk_message_dispatch:send(person, 'size', [])
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();
    generator.bind_var("person", "Person");

    let receiver = Expression::Identifier(Identifier::new("person", Span::new(0, 6)));
    let selector = MessageSelector::Unary("size".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("beamtalk_message_dispatch") && output.contains("'size'"),
        "Should generate unified dispatch for size. Got: {output}"
    );
}

#[test]
fn test_string_interpolation_simple_variable() {
    // "Hello, {name}!" — variable interpolation
    let segments = vec![
        StringSegment::Literal("Hello, ".into()),
        StringSegment::Interpolation(Expression::Identifier(Identifier::new(
            "name",
            Span::new(8, 12),
        ))),
        StringSegment::Literal("!".into()),
    ];
    let expression = Expression::StringInterpolation {
        segments,
        span: Span::new(0, 15),
    };
    let code = generate_test_expression(&expression, "test_interp").expect("codegen should work");
    // Should dispatch displayString via beamtalk_message_dispatch
    assert!(
        code.contains("'displayString'"),
        "Should dispatch displayString. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'"),
        "Should use beamtalk_message_dispatch for dispatch. Got:\n{code}"
    );
    // Binary construction with byte segments and binary variable
    assert!(
        code.contains("#<"),
        "Should contain byte segments for literal parts. Got:\n{code}"
    );
    assert!(
        code.contains("('all',8,'binary',['unsigned'|['big']])"),
        "Should contain binary variable segment. Got:\n{code}"
    );
}

#[test]
fn test_string_interpolation_no_interpolation() {
    // Plain string — compiles as regular string literal (zero overhead)
    let lit = Literal::String("Hello, World!".into());
    let generator = CoreErlangGenerator::new("test");
    let doc = generator.generate_literal(&lit).unwrap();
    let code = doc.to_pretty_string();
    // Should be a plain binary literal, no dispatch
    assert!(
        !code.contains("displayString"),
        "Plain string should NOT dispatch displayString. Got:\n{code}"
    );
    assert!(
        code.starts_with("#{"),
        "Plain string should be binary literal. Got:\n{code}"
    );
}

#[test]
fn test_string_interpolation_multiple_expressions() {
    // "a{x}b{y}c" — multiple expression segments
    let segments = vec![
        StringSegment::Literal("a".into()),
        StringSegment::Interpolation(Expression::Identifier(Identifier::new(
            "x",
            Span::new(2, 3),
        ))),
        StringSegment::Literal("b".into()),
        StringSegment::Interpolation(Expression::Identifier(Identifier::new(
            "y",
            Span::new(5, 6),
        ))),
        StringSegment::Literal("c".into()),
    ];
    let expression = Expression::StringInterpolation {
        segments,
        span: Span::new(0, 8),
    };
    let code = generate_test_expression(&expression, "test_multi").expect("codegen should work");
    // Should have two displayString dispatches
    let dispatch_count = code.matches("'displayString'").count();
    assert_eq!(
        dispatch_count, 2,
        "Should have 2 displayString dispatches. Got {dispatch_count}:\n{code}"
    );
}

#[test]
fn test_string_interpolation_only_expression() {
    // "{name}" — only an interpolation, no literal segments
    let segments = vec![StringSegment::Interpolation(Expression::Identifier(
        Identifier::new("name", Span::new(1, 5)),
    ))];
    let expression = Expression::StringInterpolation {
        segments,
        span: Span::new(0, 6),
    };
    let code = generate_test_expression(&expression, "test_bare").expect("codegen should work");
    assert!(
        code.contains("'displayString'"),
        "Should dispatch displayString even for bare expression. Got:\n{code}"
    );
    // Binary should contain only the variable segment
    assert!(
        code.contains("('all',8,'binary',['unsigned'|['big']])"),
        "Should contain binary variable segment. Got:\n{code}"
    );
}

#[test]
fn test_string_interpolation_integer_expression() {
    // "{42}" — integer literal in interpolation
    let segments = vec![StringSegment::Interpolation(Expression::Literal(
        Literal::Integer(42),
        Span::new(1, 3),
    ))];
    let expression = Expression::StringInterpolation {
        segments,
        span: Span::new(0, 4),
    };
    let code = generate_test_expression(&expression, "test_int").expect("codegen should work");
    // Should dispatch displayString on the integer
    assert!(
        code.contains("'displayString'"),
        "Should dispatch displayString on integer. Got:\n{code}"
    );
}

#[test]
fn test_as_type_erasure() {
    // `x asType: Integer` should erase to just the receiver `x`
    let mut generator = CoreErlangGenerator::new("test");
    let receiver = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("asType:", Span::new(2, 9))]);
    let arguments = vec![Expression::ClassReference {
        name: Identifier::new("Integer", Span::new(10, 17)),
        span: Span::new(10, 17),
    }];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    // asType: should be erased — no dispatch, no beamtalk_message_dispatch
    assert!(
        !output.contains("beamtalk_message_dispatch"),
        "asType: should be erased, not dispatched. Got: {output}"
    );
    assert!(
        !output.contains("asType"),
        "asType: should not appear in generated code. Got: {output}"
    );
    // The output should just be the variable reference
    assert!(
        output.contains('x') || output.contains("_x"),
        "asType: should generate only the receiver expression. Got: {output}"
    );
}

#[test]
fn test_binary_pattern_integer_segment() {
    // <<version:8>> — single integer segment, default signedness/endianness
    let mut generator = CoreErlangGenerator::new("test");
    let seg = BinarySegment {
        value: Pattern::Variable(Identifier::new("version", Span::new(0, 7))),
        size: Some(Box::new(Expression::Literal(
            Literal::Integer(8),
            Span::new(8, 9),
        ))),
        segment_type: Some(BinarySegmentType::Integer),
        signedness: None,
        endianness: None,
        unit: None,
        span: Span::new(0, 9),
    };
    let doc = generator.generate_binary_segment(&seg).unwrap();
    let output = doc.to_pretty_string();
    // Must contain: #<Version>(8,1,'integer',['unsigned'|['big']])
    assert!(
        output.contains("#<"),
        "Expected #< in binary segment, got: {output}"
    );
    assert!(
        output.contains("'integer'"),
        "Expected 'integer' type, got: {output}"
    );
    assert!(
        output.contains("'unsigned'"),
        "Expected 'unsigned' signedness, got: {output}"
    );
    assert!(
        output.contains("'big'"),
        "Expected 'big' endianness, got: {output}"
    );
}

#[test]
fn test_binary_pattern_binary_segment() {
    // <<rest/binary>> — binary/rest segment with 'all' size and unit 8
    let mut generator = CoreErlangGenerator::new("test");
    let seg = BinarySegment {
        value: Pattern::Variable(Identifier::new("rest", Span::new(0, 4))),
        size: None,
        segment_type: Some(BinarySegmentType::Binary),
        signedness: None,
        endianness: None,
        unit: None,
        span: Span::new(0, 4),
    };
    let doc = generator.generate_binary_segment(&seg).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'all'"),
        "Expected 'all' size, got: {output}"
    );
    assert!(
        output.contains("'binary'"),
        "Expected 'binary' type, got: {output}"
    );
}

#[test]
fn test_binary_pattern_utf8_segment() {
    // <<codepoint/utf8>> — UTF-8 segment with 'undefined' size and unit
    let mut generator = CoreErlangGenerator::new("test");
    let seg = BinarySegment {
        value: Pattern::Variable(Identifier::new("cp", Span::new(0, 2))),
        size: None,
        segment_type: Some(BinarySegmentType::Utf8),
        signedness: None,
        endianness: None,
        unit: None,
        span: Span::new(0, 2),
    };
    let doc = generator.generate_binary_segment(&seg).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'undefined'"),
        "UTF-8 segment should have 'undefined' size, got: {output}"
    );
    assert!(
        output.contains("'utf8'"),
        "Expected 'utf8' type, got: {output}"
    );
}

#[test]
fn test_binary_pattern_signed_little_segment() {
    // <<val:16/signed-little>> — signed little-endian integer
    let mut generator = CoreErlangGenerator::new("test");
    let seg = BinarySegment {
        value: Pattern::Variable(Identifier::new("val", Span::new(0, 3))),
        size: Some(Box::new(Expression::Literal(
            Literal::Integer(16),
            Span::new(4, 6),
        ))),
        segment_type: Some(BinarySegmentType::Integer),
        signedness: Some(BinarySignedness::Signed),
        endianness: Some(BinaryEndianness::Little),
        unit: None,
        span: Span::new(0, 6),
    };
    let doc = generator.generate_binary_segment(&seg).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'signed'"),
        "Expected 'signed', got: {output}"
    );
    assert!(
        output.contains("'little'"),
        "Expected 'little', got: {output}"
    );
}

#[test]
fn test_binary_pattern_whole_pattern() {
    // <<version:8, rest/binary>> — full binary pattern
    let mut generator = CoreErlangGenerator::new("test");
    let pattern = Pattern::Binary {
        segments: vec![
            BinarySegment {
                value: Pattern::Variable(Identifier::new("version", Span::new(0, 7))),
                size: Some(Box::new(Expression::Literal(
                    Literal::Integer(8),
                    Span::new(8, 9),
                ))),
                segment_type: Some(BinarySegmentType::Integer),
                signedness: None,
                endianness: None,
                unit: None,
                span: Span::new(0, 9),
            },
            BinarySegment {
                value: Pattern::Variable(Identifier::new("rest", Span::new(11, 15))),
                size: None,
                segment_type: Some(BinarySegmentType::Binary),
                signedness: None,
                endianness: None,
                unit: None,
                span: Span::new(11, 15),
            },
        ],
        span: Span::new(0, 17),
    };
    let doc = generator.generate_pattern(&pattern).unwrap();
    let output = doc.to_pretty_string();
    // Should wrap in #{...}#
    assert!(
        output.starts_with("#{"),
        "Expected #{{ prefix, got: {output}"
    );
    assert!(output.ends_with("}#"), "Expected }}# suffix, got: {output}");
    // Should contain both segments
    assert!(
        output.contains("'integer'"),
        "Expected integer segment, got: {output}"
    );
    assert!(
        output.contains("'binary'"),
        "Expected binary segment, got: {output}"
    );
}
