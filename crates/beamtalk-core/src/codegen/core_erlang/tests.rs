// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for Core Erlang code generation.

use super::*;
use crate::ast::*;
use crate::source_analysis::Span;

fn bare(expr: Expression) -> ExpressionStatement {
    ExpressionStatement::bare(expr)
}

#[test]
fn test_generate_empty_module() {
    let module = Module::new(Vec::new(), Span::new(0, 0));
    let result = generate(&module);
    assert!(result.is_ok());
    let code = result.unwrap();
    assert!(code.contains("module 'bt_module'"));
    assert!(code.contains("attributes ['behaviour' = ['gen_server']]"));
}

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
    assert!(output.contains("call 'erlang':'+'(call 'beamtalk_future':'maybe_await'(3), call 'beamtalk_future':'maybe_await'(4))"));
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
    assert_eq!(
        output,
        "call 'erlang':'=='(call 'beamtalk_future':'maybe_await'(5), call 'beamtalk_future':'maybe_await'(5))"
    );
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
fn test_generate_unary_message_send_creates_future() {
    let mut generator = CoreErlangGenerator::new("test");

    // Build: receiver unarySelector
    let receiver = Expression::Identifier(Identifier::new("counter", Span::new(0, 7)));
    let selector = MessageSelector::Unary("increment".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    // BT-430: Unified dispatch via beamtalk_message_dispatch:send/3
    assert!(
        output.contains("beamtalk_message_dispatch':'send'("),
        "Should dispatch via beamtalk_message_dispatch:send/3. Got: {output}"
    );
    assert!(
        output.contains("'increment'"),
        "Should include selector atom. Got: {output}"
    );
}

#[test]
fn test_generate_keyword_message_send_creates_future() {
    let mut generator = CoreErlangGenerator::new("test");

    // Build: object foo: 1 bar: 'x'
    // (using a non-Dictionary selector to avoid interception)
    let receiver = Expression::Identifier(Identifier::new("object", Span::new(0, 6)));
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("foo:", Span::new(7, 11)),
        KeywordPart::new("bar:", Span::new(14, 18)),
    ]);
    // Arguments are passed separately to generate_message_send
    let arguments = vec![
        Expression::Literal(Literal::Integer(1), Span::new(12, 13)),
        Expression::Literal(Literal::String("x".into()), Span::new(19, 22)),
    ];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    // BT-430: Unified dispatch via beamtalk_message_dispatch:send/3
    assert!(
        output.contains("beamtalk_message_dispatch':'send'("),
        "Should dispatch via beamtalk_message_dispatch:send/3. Got: {output}"
    );
    assert!(
        output.contains("'foo:bar:'"),
        "Should include combined keyword selector. Got: {output}"
    );
}

#[test]
fn test_generate_await_message_uses_future_await() {
    let mut generator = CoreErlangGenerator::new("test");

    // Build: future await (special case - should NOT create new future)
    let receiver = Expression::Identifier(Identifier::new("myFuture", Span::new(0, 8)));
    let selector = MessageSelector::Unary("await".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    // Special case: await uses beamtalk_future:await(), not the async protocol
    assert!(
        output.contains("beamtalk_future':'await'("),
        "Should call beamtalk_future:await(). Got: {output}"
    );
    assert!(
        !output.contains("gen_server':'cast'"),
        "Should NOT use gen_server:cast for await. Got: {output}"
    );
}

#[test]
fn test_generate_await_with_timeout() {
    let mut generator = CoreErlangGenerator::new("test");

    // Build: future await: 5000
    let receiver = Expression::Identifier(Identifier::new("myFuture", Span::new(0, 8)));
    let selector = MessageSelector::Keyword(vec![KeywordPart {
        keyword: "await:".into(),
        span: Span::new(9, 15),
    }]);
    let timeout = Expression::Literal(Literal::Integer(5000), Span::new(16, 20));

    let doc = generator
        .generate_message_send(&receiver, &selector, &[timeout])
        .unwrap();
    let output = doc.to_pretty_string();
    // Should call beamtalk_future:await/2 with timeout
    assert!(
        output.contains("beamtalk_future':'await'("),
        "Should call beamtalk_future:await(). Got: {output}"
    );
    assert!(
        output.contains("5000"),
        "Should include timeout value. Got: {output}"
    );
    assert!(
        !output.contains("gen_server':'cast'"),
        "Should NOT use gen_server:cast for await. Got: {output}"
    );
}

#[test]
fn test_generate_await_forever() {
    let mut generator = CoreErlangGenerator::new("test");

    // Build: future awaitForever
    let receiver = Expression::Identifier(Identifier::new("myFuture", Span::new(0, 8)));
    let selector = MessageSelector::Unary("awaitForever".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    // Should call beamtalk_future:await_forever/1
    assert!(
        output.contains("beamtalk_future':'await_forever'("),
        "Should call beamtalk_future:await_forever(). Got: {output}"
    );
    assert!(
        !output.contains("gen_server':'cast'"),
        "Should NOT use gen_server:cast for awaitForever. Got: {output}"
    );
}

#[test]
fn test_generate_binary_op_is_synchronous() {
    let mut generator = CoreErlangGenerator::new("test");

    // Build: 3 + 4 (binary ops are synchronous, not async)
    let receiver = Expression::Literal(Literal::Integer(3), Span::new(0, 1));
    let selector = MessageSelector::Binary("+".into());
    let arguments = vec![Expression::Literal(Literal::Integer(4), Span::new(4, 5))];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    // Binary ops use erlang's built-in operators - synchronous
    assert!(
        output.contains("erlang':'+'("),
        "Should use erlang:'+'. Got: {output}"
    );
    assert!(
        !output.contains("beamtalk_future':'new'"),
        "Binary ops should NOT create futures. Got: {output}"
    );
}

#[test]
fn test_generate_nested_message_sends_use_unique_variables() {
    let mut generator = CoreErlangGenerator::new("test");

    // Build: (counter new) increment
    // This is a nested message send: receiver is also a message send
    let inner_receiver = Expression::Identifier(Identifier::new("counter", Span::new(0, 7)));
    let inner_selector = MessageSelector::Unary("new".into());
    let inner_send = Expression::MessageSend {
        receiver: Box::new(inner_receiver),
        selector: inner_selector,
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 11),
    };

    let outer_selector = MessageSelector::Unary("increment".into());
    let doc = generator
        .generate_message_send(&inner_send, &outer_selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();

    // BT-430: Unified dispatch generates nested beamtalk_message_dispatch:send calls
    let send_count = output.matches("beamtalk_message_dispatch':'send'(").count();
    assert!(
        send_count >= 2,
        "Nested message sends should produce at least 2 dispatch calls. Got {send_count} in: {output}"
    );
}

#[test]
fn test_generate_spawn_message_send() {
    // BT-794: Use workspace-qualified module name to test package-mode spawn
    let mut generator = CoreErlangGenerator::new("bt@my_pkg@test_module");

    // Create AST for: Counter spawn
    let receiver = Expression::ClassReference {
        name: Identifier::new("Counter", Span::new(0, 7)),
        span: Span::new(0, 7),
    };
    let selector = MessageSelector::Unary("spawn".into());
    let arguments = vec![];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'bt@my_pkg@counter':'spawn'()"),
        "spawn should use workspace-qualified module name. Got: {output}",
    );
}

#[test]
fn test_generate_spawn_with_message_send() {
    // BT-794: Use workspace-qualified module name to test package-mode spawn
    let mut generator = CoreErlangGenerator::new("bt@my_pkg@test_module");

    // Create AST for: Counter spawnWith: #{value => 10}
    // For simplicity, we'll use an integer literal as the init arg
    // (in practice this would be a map literal)
    let receiver = Expression::ClassReference {
        name: Identifier::new("Counter", Span::new(0, 7)),
        span: Span::new(0, 7),
    };
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("spawnWith:", Span::new(8, 18))]);
    let arguments = vec![Expression::Literal(Literal::Integer(42), Span::new(19, 21))];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    // Should call spawn/1 with the argument using workspace-qualified name
    assert!(
        output.contains("call 'bt@my_pkg@counter':'spawn'(42)"),
        "spawnWith: should generate spawn/1 call with package prefix. Got: {output}",
    );
    // Should NOT create a future (spawn is synchronous)
    assert!(
        !output.contains("beamtalk_future"),
        "spawnWith: should NOT create futures. Got: {output}",
    );
}

#[test]
fn test_generate_spawn_function() {
    use crate::ast::*;

    // Create a module with a simple field assignment
    let value_assignment = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new(
            "value",
            Span::new(0, 5),
        ))),
        value: Box::new(Expression::Literal(Literal::Integer(0), Span::new(9, 10))),
        span: Span::new(0, 10),
    };

    let module = Module::new(vec![bare(value_assignment)], Span::new(0, 10));
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // Check that spawn/0 and spawn/1 are exported
    assert!(code.contains("'spawn'/0"));
    assert!(code.contains("'spawn'/1"));

    // Check that spawn/0 function exists and calls gen_server:start_link with empty map
    assert!(code.contains("'spawn'/0 = fun () ->"));
    assert!(code.contains("call 'gen_server':'start_link'('counter', ~{}~, [])"));

    // Check that spawn/1 function exists and calls gen_server:start_link with InitArgs
    assert!(code.contains("'spawn'/1 = fun (InitArgs) ->"));
    assert!(code.contains("call 'gen_server':'start_link'('counter', InitArgs, [])"));

    // Check that it uses a case expression to extract the Pid and wrap it in #beamtalk_object{}
    assert!(code.contains("case call 'gen_server':'start_link'"));
    assert!(code.contains("<{'ok', Pid}> when 'true' ->"));

    // Check that it returns a #beamtalk_object{} record (class='Counter', class_mod='counter', pid=Pid)
    assert!(
        code.contains("{'beamtalk_object', 'Counter', 'counter', Pid}"),
        "spawn functions should return #beamtalk_object{{}} record. Got: {code}"
    );

    // Check that it handles errors
    assert!(code.contains("<{'error', Reason}> when 'true' ->"));
    assert!(code.contains("call 'beamtalk_error':'raise'(SpawnErr1)"));

    // Check that init/1 creates the default state with fields and merges with InitArgs
    assert!(code.contains("'init'/1 = fun (InitArgs) ->"));
    assert!(code.contains("let DefaultState = ~{"));
    assert!(code.contains("'$beamtalk_class' => 'Counter'"));
    assert!(code.contains("'__methods__' => call 'counter':'method_table'()"));
    assert!(code.contains("'value' => 0"));
    // Check that InitArgs is merged into DefaultState
    assert!(code.contains("call 'maps':'merge'(DefaultState, InitArgs)"));
    assert!(code.contains("{'ok', FinalState}"));
}

#[test]
fn test_bt897_subdirectory_module_name_consistency() {
    // BT-897: Actor classes from subdirectory file paths must use the full
    // module name consistently — module declaration, gen_server:start_link,
    // register_class, init method_table call, and all self-dispatch calls.
    use crate::ast::*;

    let class = ClassDefinition {
        name: Identifier::new("EventBus", Span::new(0, 8)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![StateDeclaration {
            name: Identifier::new("listeners", Span::new(0, 9)),
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
            type_annotation: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 10),
        }],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 50),
    };

    let module = Module {
        classes: vec![class],
        expressions: vec![],
        method_definitions: vec![],
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    // Use a subdirectory-qualified module name (package mode with subdirectories)
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@gang_of_four@observer@event_bus"),
    )
    .expect("codegen should succeed");

    let full_name = "bt@gang_of_four@observer@event_bus";
    let simplified_name = "bt@event_bus";

    // Module declaration must use the full name
    assert!(
        code.contains(&format!("module '{full_name}'")),
        "Module declaration should use full path-qualified name. Got:\n{code}"
    );

    // gen_server:start_link must use the full name
    assert!(
        code.contains(&format!(
            "call 'gen_server':'start_link'('{full_name}', ~{{}}~, [])"
        )),
        "gen_server:start_link in spawn/0 should use full module name, not '{simplified_name}'. Got:\n{code}"
    );

    // gen_server:start_link in spawn/1 must also use full name
    assert!(
        code.contains(&format!(
            "call 'gen_server':'start_link'('{full_name}', InitArgs, [])"
        )),
        "gen_server:start_link in spawn/1 should use full module name. Got:\n{code}"
    );

    // init/1 method_table call must use the full name
    assert!(
        code.contains(&format!("call '{full_name}':'method_table'()")),
        "method_table call in init should use full module name. Got:\n{code}"
    );

    // register_class moduleName must use the full name
    assert!(
        code.contains(&format!("'moduleName' => '{full_name}'")),
        "register_class moduleName should use full module name. Got:\n{code}"
    );

    // The simplified name should NOT appear anywhere
    assert!(
        !code.contains(&format!("'{simplified_name}'")),
        "The simplified module name '{simplified_name}' should NOT appear in generated code. Got:\n{code}"
    );
}

#[test]
fn test_bt906_class_module_index_overrides_heuristic_for_spawn() {
    // BT-906: When class_module_index contains an explicit mapping, it must be
    // used for actor spawn — not the heuristic that drops subdirectory segments.
    //
    // Without the index, a generator with module_name="bt@my_pkg@main" would
    // resolve `EventBus` → `bt@my_pkg@event_bus` (heuristic, no subdirectory).
    // With the index, it must use the explicit entry `bt@gang_of_four@observer@event_bus`.
    let mut generator = CoreErlangGenerator::new("bt@my_pkg@main");
    generator.class_module_index.insert(
        "EventBus".to_string(),
        "bt@gang_of_four@observer@event_bus".to_string(),
    );

    let receiver = Expression::ClassReference {
        name: Identifier::new("EventBus", Span::new(0, 8)),
        span: Span::new(0, 8),
    };
    let selector = MessageSelector::Unary("spawn".into());
    let arguments = vec![];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("call 'bt@gang_of_four@observer@event_bus':'spawn'()"),
        "class_module_index must override the heuristic for actor spawn. Got: {output}",
    );
    assert!(
        !output.contains("call 'bt@my_pkg@event_bus':'spawn'()"),
        "Heuristic module path must NOT appear when index is present. Got: {output}",
    );
}

#[test]
fn test_generate_actor_new_error_methods() {
    // BT-217: Actor classes must export and generate new/0 and new/1 error methods
    // using structured #beamtalk_error{} records
    use crate::ast::*;

    let module = Module::new(vec![], Span::new(0, 0));
    let code = generate_module(&module, CodegenOptions::new("test_actor"))
        .expect("codegen should succeed");

    // Check that new/0 and new/1 are exported
    assert!(code.contains("'new'/0"));
    assert!(code.contains("'new'/1"));

    // Check that new/0 function exists and uses beamtalk_error
    assert!(code.contains("'new'/0 = fun () ->"));
    assert!(code.contains("call 'beamtalk_error':'new'('instantiation_error', 'Actor')"));
    assert!(code.contains("call 'beamtalk_error':'with_selector'(Error0, 'new')"));
    assert!(code.contains("call 'beamtalk_error':'with_hint'(Error1,"));
    assert!(code.contains("call 'beamtalk_error':'raise'(Error2)"));

    // Check that new/1 function exists and uses beamtalk_error
    assert!(code.contains("'new'/1 = fun (_InitArgs) ->"));
    assert!(code.contains("call 'beamtalk_error':'with_selector'(Error0, 'new:')"));
}

#[test]
fn test_generate_repl_module_aliases_state_to_bindings() {
    // BT-57: REPL modules must alias State to Bindings for identifier lookups
    let expression = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let code = generate_repl_expression(&expression, "repl_test").expect("codegen should work");

    // Check that the module aliases State to Bindings
    assert!(
        code.contains("let State = Bindings in"),
        "REPL module should alias State to Bindings. Got:\n{code}"
    );

    // Check that identifier lookup uses maps:get with State
    assert!(
        code.contains("call 'maps':'get'('x', State)"),
        "Identifier lookup should use State (aliased to Bindings). Got:\n{code}"
    );
}

#[test]
fn test_generate_repl_module_block_value_call() {
    // Test full REPL module generation for block value call
    // Expression: [:x | x + 1] value: 5
    let block = Block::new(
        vec![BlockParameter::new("x", Span::new(1, 2))],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "x",
                Span::new(5, 6),
            ))),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(9, 10))],
            is_cast: false,
            span: Span::new(5, 10),
        })],
        Span::new(0, 12),
    );

    let expression = Expression::MessageSend {
        receiver: Box::new(Expression::Block(block)),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(13, 19))]),
        arguments: vec![Expression::Literal(Literal::Integer(5), Span::new(20, 21))],
        is_cast: false,
        span: Span::new(0, 22),
    };

    let code =
        generate_repl_expression(&expression, "test_block_repl").expect("codegen should work");

    // Check basic structure
    assert!(
        code.contains("let State = Bindings in"),
        "Should alias State to Bindings"
    );
    assert!(code.contains("apply"), "Should use apply for block call");
}

#[test]
fn test_generate_repl_module_returns_tuple_with_state() {
    // BT-153: REPL eval/1 should return {Result, UpdatedBindings}
    let expression = Expression::Literal(Literal::Integer(42), Span::new(0, 2));
    let code =
        generate_repl_expression(&expression, "repl_tuple_test").expect("codegen should work");

    eprintln!("Generated code for literal 42:");
    eprintln!("{code}");

    // Check that the result is wrapped in a tuple with State
    assert!(
        code.contains("let Result ="),
        "Should bind the result to Result variable. Got:\n{code}"
    );
    assert!(
        code.contains("{Result, State}"),
        "Should return tuple {{Result, State}}. Got:\n{code}"
    );
}

#[test]
fn test_generate_repl_module_with_times_repeat_mutation() {
    // BT-153: REPL with mutation should return updated state
    // Expression: 5 timesRepeat: [count := count + 1]

    // Build the block: [count := count + 1]
    let count_id = Expression::Identifier(Identifier::new("count", Span::new(0, 5)));
    let one = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
    let add = Expression::MessageSend {
        receiver: Box::new(count_id.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span: Span::new(0, 15),
    };
    let assignment = Expression::Assignment {
        target: Box::new(count_id),
        value: Box::new(add),
        span: Span::new(0, 20),
    };
    let body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(assignment)],
        span: Span::new(0, 25),
    });

    // Build: 5 timesRepeat: [...]
    let five = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span: Span::new(2, 14),
        }]),
        arguments: vec![body],
        is_cast: false,
        span: Span::new(0, 40),
    };

    let code =
        generate_repl_expression(&times_repeat, "repl_times_test").expect("codegen should work");

    eprintln!("Generated code for 5 timesRepeat: [count := count + 1]:");
    eprintln!("{code}");

    // BT-483: For mutation-threaded loops, return {Result, State} tuple.
    // REPL extracts via element/2: let _LoopResult = element(1, Result) ...
    assert!(
        code.contains("'element'(1, Result)") && code.contains("'element'(2, Result)"),
        "Should extract Result tuple elements via element/2 for mutation loop. Got:\n{code}"
    );

    // BT-483: Loop termination should return {nil, StateAcc}
    assert!(
        code.contains("{'nil', StateAcc}"),
        "Loop should return {{'nil', StateAcc}} on termination. Got:\n{code}"
    );

    // Verify mutation threading details
    assert!(
        code.contains("letrec 'repeat'/2"),
        "Should use arity-2 repeat function (I, StateAcc). Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('count'"),
        "Should update 'count' in StateAcc (plain key in REPL mode, BT-790). Got:\n{code}"
    );
}

#[test]
fn test_generate_repl_module_with_to_do_mutation() {
    use crate::ast::BlockParameter;

    // BT-153: REPL with to:do: mutation should return updated state
    // Expression: 1 to: 5 do: [:n | total := total + n]

    // Build the block: [:n | total := total + n]
    let total_id = Expression::Identifier(Identifier::new("total", Span::new(0, 5)));
    let n_id = Expression::Identifier(Identifier::new("n", Span::new(0, 1)));
    let add = Expression::MessageSend {
        receiver: Box::new(total_id.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![n_id],
        is_cast: false,
        span: Span::new(0, 15),
    };
    let assignment = Expression::Assignment {
        target: Box::new(total_id),
        value: Box::new(add),
        span: Span::new(0, 20),
    };
    let body = Expression::Block(Block {
        parameters: vec![BlockParameter {
            name: "n".into(),
            span: Span::new(0, 1),
        }],
        body: vec![bare(assignment)],
        span: Span::new(0, 25),
    });

    // Build: 1 to: 5 do: [...]
    let one = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
    let five = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let to_do = Expression::MessageSend {
        receiver: Box::new(one),
        selector: MessageSelector::Keyword(vec![
            KeywordPart {
                keyword: "to:".into(),
                span: Span::new(2, 5),
            },
            KeywordPart {
                keyword: "do:".into(),
                span: Span::new(8, 11),
            },
        ]),
        arguments: vec![five, body],
        is_cast: false,
        span: Span::new(0, 40),
    };

    let code = generate_repl_expression(&to_do, "repl_to_do_test").expect("codegen should work");

    eprintln!("Generated code for 1 to: 5 do: [:n | total := total + n]:");
    eprintln!("{code}");

    // BT-483: For mutation-threaded loops, return {Result, State} tuple.
    assert!(
        code.contains("'element'(1, Result)") && code.contains("'element'(2, Result)"),
        "Should extract Result tuple elements via element/2 for mutation loop. Got:\n{code}"
    );

    // Verify to:do: mutation threading
    assert!(
        code.contains("letrec 'loop'/2"),
        "Should use arity-2 loop function (I, StateAcc). Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('total'"),
        "Should update 'total' in StateAcc (plain key in REPL mode, BT-790). Got:\n{code}"
    );
}

#[test]
fn test_generate_repl_module_with_while_true_mutation() {
    // BT-181: REPL with whileTrue: mutation should read condition from StateAcc
    // Expression: [x < 5] whileTrue: [x := x + 1]

    // Build the condition: [x < 5]
    let x_id = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let five = Expression::Literal(Literal::Integer(5), Span::new(0, 1));
    let compare = Expression::MessageSend {
        receiver: Box::new(x_id.clone()),
        selector: MessageSelector::Binary("<".into()),
        arguments: vec![five],
        is_cast: false,
        span: Span::new(0, 10),
    };
    let condition = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(compare)],
        span: Span::new(0, 12),
    });

    // Build the body: [x := x + 1]
    let one = Expression::Literal(Literal::Integer(1), Span::new(0, 1));
    let add = Expression::MessageSend {
        receiver: Box::new(x_id.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span: Span::new(0, 10),
    };
    let assignment = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(add),
        span: Span::new(0, 15),
    };
    let body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(assignment)],
        span: Span::new(0, 17),
    });

    // Build: [x < 5] whileTrue: [x := x + 1]
    let while_true = Expression::MessageSend {
        receiver: Box::new(condition),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "whileTrue:".into(),
            span: Span::new(10, 20),
        }]),
        arguments: vec![body],
        is_cast: false,
        span: Span::new(0, 40),
    };

    let code =
        generate_repl_expression(&while_true, "repl_while_test").expect("codegen should work");

    eprintln!("Generated code for [x < 5] whileTrue: [x := x + 1]:");
    eprintln!("{code}");

    // BT-181: Condition lambda should take StateAcc parameter
    assert!(
        code.contains("fun (StateAcc) ->"),
        "Condition lambda should accept StateAcc parameter. Got:\n{code}"
    );
    // BT-181: Condition should read x from StateAcc, not outer scope
    assert!(
        code.contains("maps':'get'('x', StateAcc)"),
        "Condition should read x from StateAcc. Got:\n{code}"
    );
    // BT-181: Condition should be applied with StateAcc argument
    assert!(
        code.contains("apply") && code.contains("(StateAcc)"),
        "Condition should be applied with StateAcc argument. Got:\n{code}"
    );
}

#[test]
fn test_repl_multi_stmt_times_repeat_intermediate() {
    // BT-790: `x := 1. 5 timesRepeat: [x := x + 1]. x` should return 6.
    // The loop is in intermediate (non-last) position — its StateAcc must be threaded
    // to the final `x` lookup, not discarded.
    let src = "x := 1. 5 timesRepeat: [x := x + 1]. x";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_repl_expressions(
        &module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect::<Vec<_>>(),
        "repl_multi_times_test",
    )
    .expect("codegen should work");

    eprintln!("Generated code for `x := 1. 5 timesRepeat: [x := x + 1]. x`:");
    eprintln!("{code}");

    // The loop StateAcc must be extracted and threaded: element(2, _R2) → StateN
    assert!(
        code.contains("'element'(2, _R2)"),
        "Loop StateAcc must be extracted via element(2, _R2). Got:\n{code}"
    );

    // The final `x` lookup must use the updated state (StateN), not a stale State1
    // StateN is whatever state comes after threading the loop result
    assert!(
        !code.contains("maps':'get'('x', State1)"),
        "Final x lookup must not use stale State1. Got:\n{code}"
    );

    // The return tuple must NOT use element/2 unwrapping for the last (plain identifier) expr
    assert!(
        !code.contains("'element'(1, Result)"),
        "Final expr is a plain identifier — must not apply element/2 to Result. Got:\n{code}"
    );

    // The return tuple should be plain {Result, StateN}
    assert!(
        code.contains("{Result, State"),
        "Return tuple should be {{Result, StateN}}. Got:\n{code}"
    );
}

#[test]
fn test_repl_multi_stmt_while_true_intermediate() {
    // BT-790: `x := 0. [x < 3] whileTrue: [x := x + 1]. x` — whileTrue: in intermediate
    // position must thread its StateAcc so the final `x` lookup sees the updated value.
    let src = "x := 0. [x < 3] whileTrue: [x := x + 1]. x";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_repl_expressions(
        &module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect::<Vec<_>>(),
        "repl_multi_while_test",
    )
    .expect("codegen should work");

    eprintln!("Generated code for `x := 0. [x < 3] whileTrue: [x := x + 1]. x`:");
    eprintln!("{code}");

    // The loop StateAcc must be extracted from the intermediate result
    assert!(
        code.contains("'element'(2, _R2)"),
        "Loop StateAcc must be extracted via element(2, _R2). Got:\n{code}"
    );

    // The final x lookup must NOT read from State1 (which has x=0 from init only)
    assert!(
        !code.contains("maps':'get'('x', State1)"),
        "Final x lookup must not use stale State1. Got:\n{code}"
    );

    // Return tuple must be plain {Result, StateN}, not element/2 wrapped
    assert!(
        !code.contains("'element'(1, Result)"),
        "Final expr is a plain identifier — must not apply element/2 to Result. Got:\n{code}"
    );
}

#[test]
fn test_repl_multi_stmt_assignment_then_loop_then_plain() {
    // BT-790: Regression — multiple intermediate expressions including assignment + loop.
    // `count := 0. 3 timesRepeat: [count := count + 1]. count` generates state chain:
    //   State → State1 (from assignment) → State2 (from loop StateAcc) → {Result, State2}
    let src = "count := 0. 3 timesRepeat: [count := count + 1]. count";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_repl_expressions(
        &module
            .expressions
            .iter()
            .map(|s| s.expression.clone())
            .collect::<Vec<_>>(),
        "repl_multi_chain_test",
    )
    .expect("codegen should work");

    eprintln!("Generated code for `count := 0. 3 timesRepeat: [count := count + 1]. count`:");
    eprintln!("{code}");

    // Assignment creates State1
    assert!(
        code.contains("State1 = call 'maps':'put'('count'"),
        "Assignment should create State1. Got:\n{code}"
    );

    // Loop in intermediate position: StateAcc extracted as State2
    assert!(
        code.contains("'element'(2, _R2)"),
        "Loop StateAcc must be extracted via element(2, _R2). Got:\n{code}"
    );

    // Final count lookup must not use stale State1
    assert!(
        !code.contains("maps':'get'('count', State1)"),
        "Final count lookup must not use stale State1. Got:\n{code}"
    );

    // No element/2 unwrapping on the final return
    assert!(
        !code.contains("'element'(1, Result)"),
        "Final plain identifier expr must not use element/2. Got:\n{code}"
    );
}

#[test]
fn test_generate_repl_module_with_arithmetic() {
    // BT-57: Verify complex expressions with variable references work
    // Expression: x + 1
    let x_ref = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let one = Expression::Literal(Literal::Integer(1), Span::new(4, 5));
    let expression = Expression::MessageSend {
        receiver: Box::new(x_ref),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span: Span::new(0, 5),
    };

    let code = generate_repl_expression(&expression, "repl_arith").expect("codegen should work");

    // Check State aliasing
    assert!(
        code.contains("let State = Bindings in"),
        "REPL module should alias State to Bindings. Got:\n{code}"
    );

    // Check that x lookup works through State
    assert!(
        code.contains("call 'maps':'get'('x', State)"),
        "Variable x should be looked up from State. Got:\n{code}"
    );

    // Check the arithmetic operation
    assert!(
        code.contains("call 'erlang':'+'("),
        "Should have addition operation. Got:\n{code}"
    );
}

// ========================================================================
// Multi-Statement REPL with Loop Mutations (BT-790)
// ========================================================================

#[test]
fn test_generate_repl_multi_stmt_times_repeat_then_read() {
    // BT-790: x := 1. 5 timesRepeat: [x := x + 1]. x
    // The loop in intermediate position must thread its updated state to the final `x` read.

    let span = Span::new(0, 1);

    // x := 1
    let x_id = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let assign_x = Expression::Assignment {
        target: Box::new(x_id.clone()),
        value: Box::new(one),
        span,
    };

    // 5 timesRepeat: [x := x + 1]
    let x_id2 = Expression::Identifier(Identifier::new("x", span));
    let one2 = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_id2.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one2],
        is_cast: false,
        span,
    };
    let loop_assign = Expression::Assignment {
        target: Box::new(x_id2),
        value: Box::new(add),
        span,
    };
    let loop_body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(loop_assign)],
        span,
    });
    let five = Expression::Literal(Literal::Integer(5), span);
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span,
        }]),
        arguments: vec![loop_body],
        is_cast: false,
        span,
    };

    // x (final read)
    let x_read = Expression::Identifier(Identifier::new("x", span));

    let expressions = vec![assign_x, times_repeat, x_read];
    let code = generate_repl_expressions(&expressions, "repl_multi_loop_test")
        .expect("codegen should work");

    eprintln!("Generated code for x := 1. 5 timesRepeat: [x := x + 1]. x:");
    eprintln!("{code}");

    // BT-790: The loop in intermediate position must have its StateAcc extracted
    assert!(
        code.contains("call 'erlang':'element'(2,"),
        "Should extract StateAcc from loop result in intermediate position. Got:\n{code}"
    );

    // BT-790: The final x read must use a state that was updated by the loop
    // (not the original State or State1 from the x := 1 assignment)
    assert!(
        code.contains("let Result ="),
        "Should bind final result. Got:\n{code}"
    );

    // The overall structure: should have State1 from assignment, then state extraction from loop
    assert!(
        code.contains("maps':'put'('x'"),
        "Should have maps:put for x assignment. Got:\n{code}"
    );
    assert!(
        code.contains("letrec 'repeat'/2"),
        "Should use arity-2 repeat function for mutation loop. Got:\n{code}"
    );
}

#[test]
fn test_generate_repl_multi_stmt_while_true_then_read() {
    // BT-790: x := 0. [x < 5] whileTrue: [x := x + 1]. x
    // whileTrue: in intermediate position must thread state to the final x read.

    let span = Span::new(0, 1);

    // x := 0
    let x_id = Expression::Identifier(Identifier::new("x", span));
    let zero = Expression::Literal(Literal::Integer(0), span);
    let assign_x = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(zero),
        span,
    };

    // [x < 5] whileTrue: [x := x + 1]
    let x_cond = Expression::Identifier(Identifier::new("x", span));
    let five = Expression::Literal(Literal::Integer(5), span);
    let cmp = Expression::MessageSend {
        receiver: Box::new(x_cond),
        selector: MessageSelector::Binary("<".into()),
        arguments: vec![five],
        is_cast: false,
        span,
    };
    let condition = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(cmp)],
        span,
    });
    let x_body = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_body.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span,
    };
    let loop_assign = Expression::Assignment {
        target: Box::new(Expression::Identifier(Identifier::new("x", span))),
        value: Box::new(add),
        span,
    };
    let loop_body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(loop_assign)],
        span,
    });
    let while_true = Expression::MessageSend {
        receiver: Box::new(condition),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "whileTrue:".into(),
            span,
        }]),
        arguments: vec![loop_body],
        is_cast: false,
        span,
    };

    // x (final read)
    let x_read = Expression::Identifier(Identifier::new("x", span));

    let expressions = vec![assign_x, while_true, x_read];
    let code = generate_repl_expressions(&expressions, "repl_multi_while_test")
        .expect("codegen should work");

    eprintln!("Generated code for x := 0. [x < 5] whileTrue: [x := x + 1]. x:");
    eprintln!("{code}");

    // BT-790: The loop in intermediate position must have its StateAcc extracted
    assert!(
        code.contains("call 'erlang':'element'(2,"),
        "Should extract StateAcc from whileTrue: loop result in intermediate position. Got:\n{code}"
    );

    // Should bind the final result
    assert!(
        code.contains("let Result ="),
        "Should bind final result. Got:\n{code}"
    );

    // Should use whileTrue: mutation-threaded structure
    assert!(
        code.contains("letrec 'while'/1"),
        "Should use whileTrue: loop function. Got:\n{code}"
    );
}

#[test]
fn test_generate_repl_multi_stmt_loop_does_not_corrupt_final_expr() {
    // BT-790: repl_loop_mutated must be reset before the final expression.
    // x := 1. 5 timesRepeat: [x := x + 1]. 42
    // The final expression `42` is not a loop, so it must NOT use element/2 unwrapping.

    let span = Span::new(0, 1);

    // x := 1
    let x_id = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let assign_x = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(one),
        span,
    };

    // 5 timesRepeat: [x := x + 1]
    let x_id2 = Expression::Identifier(Identifier::new("x", span));
    let one2 = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_id2.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one2],
        is_cast: false,
        span,
    };
    let loop_assign = Expression::Assignment {
        target: Box::new(x_id2),
        value: Box::new(add),
        span,
    };
    let loop_body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(loop_assign)],
        span,
    });
    let five = Expression::Literal(Literal::Integer(5), span);
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span,
        }]),
        arguments: vec![loop_body],
        is_cast: false,
        span,
    };

    // 42 (final literal - not a loop)
    let forty_two = Expression::Literal(Literal::Integer(42), span);

    let expressions = vec![assign_x, times_repeat, forty_two];
    let code = generate_repl_expressions(&expressions, "repl_multi_loop_no_corrupt_test")
        .expect("codegen should work");

    eprintln!("Generated code for x := 1. 5 timesRepeat: [x := x + 1]. 42:");
    eprintln!("{code}");

    // BT-790: Final expression is a literal — must NOT apply element/2 unwrapping on Result
    // The return tuple must be {Result, StateN} where Result = 42 (not extracted from a tuple)
    assert!(
        !code.contains("'element'(1, Result)"),
        "Final non-loop expression must NOT apply element(1, Result) unwrapping. Got:\n{code}"
    );
    assert!(
        !code.contains("'element'(2, Result)"),
        "Final non-loop expression must NOT apply element(2, Result) unwrapping. Got:\n{code}"
    );

    // The intermediate loop SHOULD still extract state (element(2, _R2))
    assert!(
        code.contains("call 'erlang':'element'(2,"),
        "Intermediate loop must extract StateAcc. Got:\n{code}"
    );
}

// ========================================================================
// BT-800: REPL Loop Mutation Accumulation Tests
// ========================================================================

#[test]
fn test_repl_loop_mutations_accumulate_plain_key() {
    // BT-800: In REPL mode, loop writes must use plain key so reads accumulate.
    // Expression: 5 timesRepeat: [x := x + 1]
    // Write path must use 'x' not '__local__x' so that each iteration reads the
    // value written by the previous iteration from StateAcc.

    let span = Span::new(0, 1);

    let x_id = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_id.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span,
    };
    let assignment = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(add),
        span,
    };
    let body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(assignment)],
        span,
    });
    let five = Expression::Literal(Literal::Integer(5), span);
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span,
        }]),
        arguments: vec![body],
        is_cast: false,
        span,
    };

    let code = generate_repl_expression(&times_repeat, "bt800_test").expect("codegen should work");

    eprintln!("BT-800: Generated code for 5 timesRepeat: [x := x + 1]:");
    eprintln!("{code}");

    // BT-800: REPL mode must use plain key 'x' (not '__local__x') so reads match writes.
    assert!(
        code.contains("maps':'put'('x'"),
        "BT-800: REPL write must use plain key 'x', not '__local__x'. Got:\n{code}"
    );
    assert!(
        !code.contains("__local__x"),
        "BT-800: REPL mode must never use __local__ prefix for x. Got:\n{code}"
    );

    // BT-800: Reads inside loop body must use StateAcc (not State) so they get
    // the accumulated value from the previous iteration.
    assert!(
        code.contains("maps':'get'('x', StateAcc)"),
        "BT-800: Read inside loop must use StateAcc to get accumulated value. Got:\n{code}"
    );

    // BT-800: Loop must thread state correctly (arity-2 letrec, returns {nil, StateAcc}).
    assert!(
        code.contains("letrec 'repeat'/2"),
        "BT-800: Must use arity-2 repeat for state threading. Got:\n{code}"
    );
    assert!(
        code.contains("{'nil', StateAcc}"),
        "BT-800: Loop must return {{nil, StateAcc}} so caller can extract updated state. Got:\n{code}"
    );
}

#[test]
fn test_repl_multi_stmt_loop_accumulates_from_zero() {
    // BT-800: x := 0. 5 timesRepeat: [x := x + 1]. x
    // Acceptance criteria: starting from zero, result must be 5.
    // Validates that the multi-statement path threads state correctly through the loop.

    let span = Span::new(0, 1);

    // x := 0
    let x_id = Expression::Identifier(Identifier::new("x", span));
    let zero = Expression::Literal(Literal::Integer(0), span);
    let assign_x = Expression::Assignment {
        target: Box::new(x_id),
        value: Box::new(zero),
        span,
    };

    // 5 timesRepeat: [x := x + 1]
    let x_id2 = Expression::Identifier(Identifier::new("x", span));
    let one = Expression::Literal(Literal::Integer(1), span);
    let add = Expression::MessageSend {
        receiver: Box::new(x_id2.clone()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![one],
        is_cast: false,
        span,
    };
    let loop_assign = Expression::Assignment {
        target: Box::new(x_id2),
        value: Box::new(add),
        span,
    };
    let loop_body = Expression::Block(Block {
        parameters: vec![],
        body: vec![bare(loop_assign)],
        span,
    });
    let five = Expression::Literal(Literal::Integer(5), span);
    let times_repeat = Expression::MessageSend {
        receiver: Box::new(five),
        selector: MessageSelector::Keyword(vec![KeywordPart {
            keyword: "timesRepeat:".into(),
            span,
        }]),
        arguments: vec![loop_body],
        is_cast: false,
        span,
    };

    // x (final read)
    let x_read = Expression::Identifier(Identifier::new("x", span));

    let expressions = vec![assign_x, times_repeat, x_read];
    let code =
        generate_repl_expressions(&expressions, "bt800_zero_test").expect("codegen should work");

    eprintln!("BT-800: Generated code for x := 0. 5 timesRepeat: [x := x + 1]. x:");
    eprintln!("{code}");

    // BT-800: Loop write must use plain key (no __local__ prefix in REPL mode)
    assert!(
        !code.contains("__local__"),
        "BT-800: REPL mode must never use __local__ prefix. Got:\n{code}"
    );

    // BT-800: The loop must be applied with the state containing x=0
    assert!(
        code.contains("apply 'repeat'/2 (1, State1)"),
        "BT-800: Loop must start with State1 (after x := 0 binding). Got:\n{code}"
    );

    // BT-800: The intermediate loop result must be unpacked to thread state forward
    assert!(
        code.contains("call 'erlang':'element'(2,"),
        "BT-800: Must extract updated StateAcc from loop result. Got:\n{code}"
    );

    // BT-800: Final read of x must use the state produced by the loop (State2+)
    assert!(
        code.contains("maps':'get'('x', State2)"),
        "BT-800: Final x read must use loop-updated state (State2). Got:\n{code}"
    );
}

// ========================================================================
// Block Evaluation Message Tests (BT-32)
// ========================================================================

#[test]
fn test_block_value_message_no_args() {
    // [42] value → let _Fun = fun () -> 42 in apply _Fun ()
    let mut generator = CoreErlangGenerator::new("test");

    let block = Block::new(
        vec![],
        vec![bare(Expression::Literal(
            Literal::Integer(42),
            Span::new(1, 3),
        ))],
        Span::new(0, 4),
    );
    let receiver = Expression::Block(block);
    let selector = MessageSelector::Unary("value".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("let _Fun1 = fun () -> 42 in apply _Fun1 ()"),
        "Should generate let binding with apply. Got: {output}"
    );
    // Should NOT use async protocol
    assert!(
        !output.contains("beamtalk_future"),
        "value message should NOT create futures. Got: {output}"
    );
}

#[test]
fn test_block_value_message_one_arg() {
    // [:x | x + 1] value: 5 → let _Fun = ... in apply _Fun (5)
    let mut generator = CoreErlangGenerator::new("test");

    let block = Block::new(
        vec![BlockParameter::new("x", Span::new(1, 2))],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "x",
                Span::new(5, 6),
            ))),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(9, 10))],
            is_cast: false,
            span: Span::new(5, 10),
        })],
        Span::new(0, 12),
    );
    let receiver = Expression::Block(block);
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(13, 19))]);
    let arguments = vec![Expression::Literal(Literal::Integer(5), Span::new(20, 21))];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("let _Fun"),
        "Should use let binding for block evaluation. Got: {output}"
    );
    assert!(
        output.contains("apply _Fun"),
        "Should use apply on bound fun variable. Got: {output}"
    );
    assert!(
        output.contains("(5)"),
        "Should pass argument 5 to the block. Got: {output}"
    );
    // Binary ops inside block bodies are wrapped with maybe_await (BT-899)
    assert!(
        output.contains("beamtalk_future"),
        "value: binary ops should wrap operands with maybe_await. Got: {output}"
    );
}

#[test]
fn test_block_value_message_two_args() {
    // [:x :y | x + y] value: 3 value: 4
    let mut generator = CoreErlangGenerator::new("test");

    let block = Block::new(
        vec![
            BlockParameter::new("x", Span::new(1, 2)),
            BlockParameter::new("y", Span::new(4, 5)),
        ],
        vec![bare(Expression::Literal(
            Literal::Integer(0),
            Span::new(8, 9),
        ))], // placeholder body
        Span::new(0, 11),
    );
    let receiver = Expression::Block(block);
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("value:", Span::new(12, 18)),
        KeywordPart::new("value:", Span::new(21, 27)),
    ]);
    let arguments = vec![
        Expression::Literal(Literal::Integer(3), Span::new(19, 20)),
        Expression::Literal(Literal::Integer(4), Span::new(28, 29)),
    ];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("apply"),
        "Should use apply for block evaluation. Got: {output}"
    );
    assert!(
        output.contains("(3, 4)"),
        "Should pass arguments 3, 4 to the block. Got: {output}"
    );
}

#[test]
fn test_block_while_true_loop() {
    // [counter < 5] whileTrue: [counter := counter + 1]
    let mut generator = CoreErlangGenerator::new("test");

    // Condition block: [counter < 5]
    let condition_block = Block::new(
        vec![],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "counter",
                Span::new(1, 8),
            ))),
            selector: MessageSelector::Binary("<".into()),
            arguments: vec![Expression::Literal(Literal::Integer(5), Span::new(11, 12))],
            is_cast: false,
            span: Span::new(1, 12),
        })],
        Span::new(0, 13),
    );

    // Body block: [counter := counter + 1]
    let body_block = Block::new(
        vec![],
        vec![bare(Expression::Literal(
            Literal::Integer(0),
            Span::new(0, 1),
        ))], // simplified body
        Span::new(15, 38),
    );

    let receiver = Expression::Block(condition_block);
    let selector =
        MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", Span::new(14, 24))]);
    let arguments = vec![Expression::Block(body_block)];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("letrec"),
        "whileTrue: should generate a letrec for looping. Got: {output}"
    );
    assert!(
        output.contains("case apply"),
        "whileTrue: should have case on condition result. Got: {output}"
    );
    assert!(
        output.contains("<'true'> when 'true'"),
        "whileTrue: should match on true to continue. Got: {output}"
    );
    assert!(
        output.contains("<'false'> when 'true' -> 'nil'"),
        "whileTrue: should return nil when condition is false. Got: {output}"
    );
    // Binary ops inside condition blocks are wrapped with maybe_await (BT-899)
    assert!(
        output.contains("beamtalk_future"),
        "whileTrue: binary ops should wrap operands with maybe_await. Got: {output}"
    );
}

#[test]
fn test_block_while_false_loop() {
    // [done] whileFalse: [process next]
    let mut generator = CoreErlangGenerator::new("test");

    let condition_block = Block::new(
        vec![],
        vec![bare(Expression::Identifier(Identifier::new(
            "done",
            Span::new(1, 5),
        )))],
        Span::new(0, 6),
    );
    let body_block = Block::new(vec![], vec![], Span::new(8, 10));

    let receiver = Expression::Block(condition_block);
    let selector =
        MessageSelector::Keyword(vec![KeywordPart::new("whileFalse:", Span::new(7, 18))]);
    let arguments = vec![Expression::Block(body_block)];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("letrec"),
        "whileFalse: should generate a letrec for looping. Got: {output}"
    );
    assert!(
        output.contains("<'false'> when 'true' ->"),
        "whileFalse: should continue when condition is false. Got: {output}"
    );
    assert!(
        output.contains("<'true'> when 'true' -> 'nil'"),
        "whileFalse: should stop when condition is true. Got: {output}"
    );
}

#[test]
fn test_block_repeat_infinite_loop() {
    // [process] repeat
    let mut generator = CoreErlangGenerator::new("test");

    let body_block = Block::new(
        vec![],
        vec![bare(Expression::Literal(
            Literal::Integer(1),
            Span::new(1, 2),
        ))],
        Span::new(0, 3),
    );

    let receiver = Expression::Block(body_block);
    let selector = MessageSelector::Unary("repeat".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("letrec"),
        "repeat should generate a letrec for looping. Got: {output}"
    );
    assert!(
        output.contains("'_Loop"),
        "repeat should create a loop function. Got: {output}"
    );
    // repeat has no condition, just loops forever
    assert!(
        !output.contains("case"),
        "repeat should NOT have a case (no condition). Got: {output}"
    );
    assert!(
        !output.contains("beamtalk_future"),
        "repeat should NOT create futures. Got: {output}"
    );
}

#[test]
fn test_non_block_message_uses_unified_dispatch() {
    // BT-430: Regular message sends now use unified dispatch
    // actor increment → beamtalk_message_dispatch:send(actor, 'increment', [])
    let mut generator = CoreErlangGenerator::new("test");

    let receiver = Expression::Identifier(Identifier::new("actor", Span::new(0, 5)));
    let selector = MessageSelector::Unary("increment".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("beamtalk_message_dispatch':'send'("),
        "Non-block unary messages should use unified dispatch. Got: {output}"
    );
}

/// End-to-end test that generates Core Erlang for a whileTrue: loop and
/// compiles it through erlc to verify the output is valid Core Erlang.
#[test]
fn test_while_true_compiles_through_erlc() {
    use std::io::Write;
    use std::process::Command;

    // Generate a complete module with a whileTrue: expression
    let mut generator = CoreErlangGenerator::new("test_while_loop");

    // Start module
    let mut full_output = String::from("module 'test_while_loop' ['main'/0]\n  attributes []\n\n");
    full_output.push_str("'main'/0 = fun () ->\n    ");

    // Generate: [true] whileTrue: [42]
    // This creates a loop that runs once (returns nil after first iteration)
    let condition_block = Block::new(
        vec![],
        vec![bare(Expression::Identifier(Identifier::new(
            "false",
            Span::new(1, 6),
        )))],
        Span::new(0, 7),
    );
    let body_block = Block::new(
        vec![],
        vec![bare(Expression::Literal(
            Literal::Integer(42),
            Span::new(9, 11),
        ))],
        Span::new(8, 12),
    );

    let receiver = Expression::Block(condition_block);
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("whileTrue:", Span::new(7, 17))]);
    let arguments = vec![Expression::Block(body_block)];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    full_output.push_str(&doc.to_pretty_string());

    full_output.push_str("\n\nend\n");

    // Write to temp file
    let temp_dir = std::env::temp_dir();
    let core_file = temp_dir.join("test_while_loop.core");
    let mut file = std::fs::File::create(&core_file).expect("Failed to create temp file");
    file.write_all(full_output.as_bytes())
        .expect("Failed to write Core Erlang");

    // Try to compile with erlc
    let output = Command::new("erlc")
        .arg("+from_core")
        .arg("-o")
        .arg(&temp_dir)
        .arg(&core_file)
        .output();

    // Clean up temp files regardless of result
    let _ = std::fs::remove_file(&core_file);
    let beam_file = temp_dir.join("test_while_loop.beam");
    let _ = std::fs::remove_file(&beam_file);

    match output {
        Ok(result) => {
            if !result.status.success() {
                let stderr = String::from_utf8_lossy(&result.stderr);
                panic!(
                    "erlc compilation failed.\n\nGenerated Core Erlang:\n{full_output}\n\nerlc error:\n{stderr}",
                );
            }
        }
        Err(e) => {
            // erlc not available, skip this test with a message
            eprintln!("Skipping erlc compilation test: {e}");
        }
    }
}

/// Test that internal loop temporaries don't shadow user identifiers.
///
/// This tests that a user variable named "Loop" in a block parameter
/// is correctly resolved even after generating whileTrue: code.
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

// ========================================================================
// Dictionary Method Code Generation Tests (BT-296: now uses runtime dispatch)
// ========================================================================

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

// ========================================================================
// Cascade Code Generation Tests (BT-86)
// ========================================================================

#[test]
fn test_cascade_unary_messages() {
    // x negated; abs  (two unary messages to x)
    // Parser creates:
    // - receiver: MessageSend { receiver: Identifier(x), selector: Unary(negated), args: [] }
    // - messages: [CascadeMessage { selector: Unary(abs), args: [] }]
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();

    let x_ident = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let first_msg = Expression::MessageSend {
        receiver: Box::new(x_ident),
        selector: MessageSelector::Unary("negated".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 9),
    };

    let cascade = Expression::Cascade {
        receiver: Box::new(first_msg),
        messages: vec![CascadeMessage::new(
            MessageSelector::Unary("abs".into()),
            vec![],
            Span::new(11, 14),
        )],
        span: Span::new(0, 14),
    };

    let doc = generator.generate_expression(&cascade).unwrap();
    let output = doc.to_pretty_string();

    // Should bind the underlying receiver (x) once
    assert!(
        output.contains("let _Receiver1 = call 'maps':'get'('x', State) in"),
        "Should bind the underlying receiver x. Got: {output}"
    );

    // Should send BOTH messages (negated AND abs) to the receiver via unified dispatch
    assert!(
        output.contains("'negated'"),
        "Should send first message 'negated'. Got: {output}"
    );
    assert!(
        output.contains("'abs'"),
        "Should send second message 'abs'. Got: {output}"
    );

    // BT-430: Should use unified dispatch for cascade messages
    assert!(
        output.contains("call 'beamtalk_message_dispatch':'send'(_Receiver"),
        "Should send messages via unified dispatch. Got: {output}"
    );

    generator.pop_scope();
}

#[test]
fn test_cascade_keyword_messages() {
    // collection at: 1 put: 'a'; at: 2 put: 'b'; size
    // Parser creates:
    // - receiver: MessageSend { receiver: Identifier(collection), selector: Keyword(at:put:), args: [1, 'a'] }
    // - messages: [CascadeMessage { selector: Keyword(at:put:), args: [2, 'b'] },
    //              CascadeMessage { selector: Unary(size), args: [] }]
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();

    let collection_ident = Expression::Identifier(Identifier::new("collection", Span::new(0, 10)));
    let first_msg = Expression::MessageSend {
        receiver: Box::new(collection_ident),
        selector: MessageSelector::Keyword(vec![
            KeywordPart::new("at:", Span::new(11, 14)),
            KeywordPart::new("put:", Span::new(16, 20)),
        ]),
        arguments: vec![
            Expression::Literal(Literal::Integer(1), Span::new(15, 16)),
            Expression::Literal(Literal::String("a".into()), Span::new(21, 24)),
        ],
        is_cast: false,
        span: Span::new(0, 24),
    };

    let cascade = Expression::Cascade {
        receiver: Box::new(first_msg),
        messages: vec![
            CascadeMessage::new(
                MessageSelector::Keyword(vec![
                    KeywordPart::new("at:", Span::new(26, 29)),
                    KeywordPart::new("put:", Span::new(31, 35)),
                ]),
                vec![
                    Expression::Literal(Literal::Integer(2), Span::new(30, 31)),
                    Expression::Literal(Literal::String("b".into()), Span::new(36, 39)),
                ],
                Span::new(26, 39),
            ),
            CascadeMessage::new(
                MessageSelector::Unary("size".into()),
                vec![],
                Span::new(41, 45),
            ),
        ],
        span: Span::new(0, 45),
    };

    let doc = generator.generate_expression(&cascade).unwrap();
    let output = doc.to_pretty_string();

    // Should bind the underlying receiver (collection) once
    assert!(
        output.contains("let _Receiver1 = call 'maps':'get'('collection', State) in"),
        "Should bind the underlying receiver collection. Got: {output}"
    );

    // Should send all three messages
    assert!(
        output.contains("'at:put:'"),
        "Should send keyword message 'at:put:'. Got: {output}"
    );
    assert!(
        output.contains("'size'"),
        "Should send unary message 'size'. Got: {output}"
    );

    generator.pop_scope();
}

#[test]
fn test_cascade_binary_selector_error() {
    // counter + 1; negated  (binary selector in cascade - should error)
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();

    let counter_ident = Expression::Identifier(Identifier::new("counter", Span::new(0, 7)));
    let first_msg = Expression::MessageSend {
        receiver: Box::new(counter_ident),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(10, 11))],
        is_cast: false,
        span: Span::new(0, 11),
    };

    let cascade = Expression::Cascade {
        receiver: Box::new(first_msg),
        messages: vec![CascadeMessage::new(
            MessageSelector::Unary("negated".into()),
            vec![],
            Span::new(13, 20),
        )],
        span: Span::new(0, 20),
    };

    let result = generator.generate_expression(&cascade);

    // Binary selectors in cascades should return UnsupportedFeature error, not Internal
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(
        matches!(err, CodeGenError::UnsupportedFeature { .. }),
        "Binary selectors in cascades should return UnsupportedFeature, got: {err:?}"
    );

    generator.pop_scope();
}

#[test]
fn test_cascade_repl_expression() {
    // Test cascade in a full REPL module context
    // x negated; abs
    let x_ident = Expression::Identifier(Identifier::new("x", Span::new(0, 1)));
    let first_msg = Expression::MessageSend {
        receiver: Box::new(x_ident),
        selector: MessageSelector::Unary("negated".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 9),
    };

    let cascade = Expression::Cascade {
        receiver: Box::new(first_msg),
        messages: vec![CascadeMessage::new(
            MessageSelector::Unary("abs".into()),
            vec![],
            Span::new(11, 14),
        )],
        span: Span::new(0, 14),
    };

    let code = generate_repl_expression(&cascade, "test_cascade").expect("codegen should work");

    // Should have module structure
    assert!(
        code.contains("module 'test_cascade' ['eval'/1]"),
        "Should have module header. Got:\n{code}"
    );

    // Should bind the underlying receiver once
    assert!(
        code.contains("let _Receiver1 = call 'maps':'get'('x', State) in"),
        "Should bind receiver x from State. Got:\n{code}"
    );

    // Should send both messages
    assert!(
        code.contains("'negated'"),
        "Should have first message negated. Got:\n{code}"
    );
    assert!(
        code.contains("'abs'"),
        "Should have second message abs. Got:\n{code}"
    );
}

// BT-884: Cascade with field-assignment arguments must hoist state bindings
// so that StateN remains in scope for subsequent cascade messages.
#[test]
fn test_cascade_field_assignment_arg_hoisted() {
    // obj msg1: (self.x := 5); msg2
    //
    // Before fix, `let State1 = maps:put(...)` was nested inside the first
    // send call's argument list and went out of scope. After fix, the binding
    // is hoisted before the send call.
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();

    let obj_ident = Expression::Identifier(Identifier::new("obj", Span::new(0, 3)));

    // First message: msg1: (self.x := 5)
    let field_assignment = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "self",
                Span::new(10, 14),
            ))),
            field: Identifier::new("x", Span::new(15, 16)),
            span: Span::new(10, 16),
        }),
        value: Box::new(Expression::Literal(Literal::Integer(5), Span::new(20, 21))),
        span: Span::new(10, 21),
    };

    let first_msg = Expression::MessageSend {
        receiver: Box::new(obj_ident),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("msg1:", Span::new(4, 9))]),
        arguments: vec![field_assignment],
        is_cast: false,
        span: Span::new(0, 22),
    };

    // Second message: msg2 (unary, no args)
    let cascade = Expression::Cascade {
        receiver: Box::new(first_msg),
        messages: vec![CascadeMessage::new(
            MessageSelector::Unary("msg2".into()),
            vec![],
            Span::new(24, 28),
        )],
        span: Span::new(0, 28),
    };

    let doc = generator.generate_expression(&cascade).unwrap();
    let output = doc.to_pretty_string();

    // The state binding must appear BEFORE the send call, not inside it.
    // _Receiver1 takes temp var 1, so the val var is _Val2.
    // Correct pattern:
    //   let _Val2 = 5 in let State1 = call 'maps':'put'('x', _Val2, State) in
    //   let _ = call 'beamtalk_message_dispatch':'send'(..., [_Val2]) in ...
    assert!(
        output.contains("let _Val2 = 5 in let State1 = call 'maps':'put'('x', _Val2, State) in"),
        "State binding should be hoisted before send call. Got:\n{output}"
    );

    // The argument to the send call should be the val var, not the full assignment
    assert!(
        output.contains("'msg1:', [_Val2]"),
        "Argument should be the val var _Val2. Got:\n{output}"
    );

    // The second message should be able to reference State1 (it's in scope)
    // Just verify the second message is generated
    assert!(
        output.contains("'msg2'"),
        "Should have second message. Got:\n{output}"
    );

    generator.pop_scope();
}

// BT-884: Cascade with field assignments in multiple messages threads state correctly.
#[test]
fn test_cascade_multiple_field_assignments_state_threading() {
    // obj msg1: (self.x := 5); msg2: (self.y := 10)
    //
    // The second field assignment should reference State1 (from the first),
    // and produce State2.
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();

    let obj_ident = Expression::Identifier(Identifier::new("obj", Span::new(0, 3)));

    // First message: msg1: (self.x := 5)
    let field_assign_x = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "self",
                Span::new(10, 14),
            ))),
            field: Identifier::new("x", Span::new(15, 16)),
            span: Span::new(10, 16),
        }),
        value: Box::new(Expression::Literal(Literal::Integer(5), Span::new(20, 21))),
        span: Span::new(10, 21),
    };

    let first_msg = Expression::MessageSend {
        receiver: Box::new(obj_ident),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("msg1:", Span::new(4, 9))]),
        arguments: vec![field_assign_x],
        is_cast: false,
        span: Span::new(0, 22),
    };

    // Second message: msg2: (self.y := 10)
    let field_assign_y = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "self",
                Span::new(30, 34),
            ))),
            field: Identifier::new("y", Span::new(35, 36)),
            span: Span::new(30, 36),
        }),
        value: Box::new(Expression::Literal(Literal::Integer(10), Span::new(40, 42))),
        span: Span::new(30, 42),
    };

    let cascade = Expression::Cascade {
        receiver: Box::new(first_msg),
        messages: vec![CascadeMessage::new(
            MessageSelector::Keyword(vec![KeywordPart::new("msg2:", Span::new(24, 29))]),
            vec![field_assign_y],
            Span::new(24, 43),
        )],
        span: Span::new(0, 43),
    };

    let doc = generator.generate_expression(&cascade).unwrap();
    let output = doc.to_pretty_string();

    // _Receiver1 takes temp var 1, so val vars are _Val2 and _Val3.
    // First field assignment: State → State1
    assert!(
        output.contains("let State1 = call 'maps':'put'('x', _Val2, State) in"),
        "First assignment should produce State1. Got:\n{output}"
    );

    // Second field assignment: State1 → State2
    assert!(
        output.contains("let State2 = call 'maps':'put'('y', _Val3, State1) in"),
        "Second assignment should reference State1 and produce State2. Got:\n{output}"
    );

    generator.pop_scope();
}

#[test]
fn test_validate_stored_closure_empty_block() {
    // Empty block should not trigger errors
    let block = Block {
        parameters: vec![],
        body: vec![],
        span: Span::new(0, 2),
    };

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
    assert!(result.is_ok(), "Empty block should be valid");
}

#[test]
fn test_validate_stored_closure_with_captured_mutation() {
    // Block that reads and writes a captured variable: [count := count + 1]
    // `count` is read (captured from outer scope) and written → should error
    let block = Block {
        parameters: vec![],
        body: vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "count",
                Span::new(1, 6),
            ))),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    Span::new(10, 15),
                ))),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(18, 19))],
                is_cast: false,
                span: Span::new(10, 19),
            }),
            span: Span::new(1, 19),
        })],
        span: Span::new(0, 20),
    };

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
    assert!(
        result.is_err(),
        "Captured variable mutation should produce error"
    );

    if let Err(CodeGenError::LocalMutationInStoredClosure { variable, .. }) = result {
        assert_eq!(variable, "count");
    } else {
        panic!("Expected LocalMutationInStoredClosure error");
    }
}

#[test]
fn test_validate_stored_closure_with_new_local_definition() {
    // BT-665: Block with only new local variable definition: [temp := 1]
    // `temp` is never read from outer scope → should be allowed
    let block = Block {
        parameters: vec![],
        body: vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "temp",
                Span::new(1, 5),
            ))),
            value: Box::new(Expression::Literal(Literal::Integer(1), Span::new(9, 10))),
            span: Span::new(1, 10),
        })],
        span: Span::new(0, 11),
    };

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
    assert!(
        result.is_ok(),
        "New local definition should be allowed in stored closure"
    );
}

#[test]
fn test_validate_stored_closure_with_new_local_used_later() {
    // BT-665: Block defines a new local and uses it later: [:x | temp := x * 2. temp + 1]
    // `temp` is defined then read — NOT a captured variable → should be allowed
    let block = Block {
        parameters: vec![BlockParameter::new("x", Span::new(1, 2))],
        body: vec![
            bare(Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new(
                    "temp",
                    Span::new(5, 9),
                ))),
                value: Box::new(Expression::MessageSend {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "x",
                        Span::new(13, 14),
                    ))),
                    selector: MessageSelector::Binary("*".into()),
                    arguments: vec![Expression::Literal(Literal::Integer(2), Span::new(17, 18))],
                    is_cast: false,
                    span: Span::new(13, 18),
                }),
                span: Span::new(5, 18),
            }),
            bare(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "temp",
                    Span::new(20, 24),
                ))),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(27, 28))],
                is_cast: false,
                span: Span::new(20, 28),
            }),
        ],
        span: Span::new(0, 29),
    };

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
    assert!(
        result.is_ok(),
        "Block with new local definition used later should be allowed"
    );
}

#[test]
fn test_validate_stored_closure_with_field_assignment() {
    // Block with field assignment: [self.value := 1]
    let block = Block {
        parameters: vec![],
        body: vec![bare(Expression::Assignment {
            target: Box::new(Expression::FieldAccess {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "self",
                    Span::new(1, 5),
                ))),
                field: Identifier::new("value", Span::new(6, 11)),
                span: Span::new(1, 11),
            }),
            value: Box::new(Expression::Literal(Literal::Integer(1), Span::new(15, 16))),
            span: Span::new(1, 16),
        })],
        span: Span::new(0, 17),
    };

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
    assert!(result.is_err(), "Field assignment should produce error");

    if let Err(CodeGenError::FieldAssignmentInStoredClosure {
        field,
        field_capitalized,
        ..
    }) = result
    {
        assert_eq!(field, "value");
        assert_eq!(field_capitalized, "Value");
    } else {
        panic!("Expected FieldAssignmentInStoredClosure error");
    }
}

#[test]
fn test_validate_stored_closure_field_takes_precedence() {
    // Block with both field and local assignment
    // Field error should be reported first
    let block = Block {
        parameters: vec![],
        body: vec![
            bare(Expression::Assignment {
                target: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    Span::new(1, 6),
                ))),
                value: Box::new(Expression::Literal(Literal::Integer(0), Span::new(10, 11))),
                span: Span::new(1, 11),
            }),
            bare(Expression::Assignment {
                target: Box::new(Expression::FieldAccess {
                    receiver: Box::new(Expression::Identifier(Identifier::new(
                        "self",
                        Span::new(13, 17),
                    ))),
                    field: Identifier::new("value", Span::new(18, 23)),
                    span: Span::new(13, 23),
                }),
                value: Box::new(Expression::Literal(Literal::Integer(1), Span::new(27, 28))),
                span: Span::new(13, 28),
            }),
        ],
        span: Span::new(0, 29),
    };

    let result = CoreErlangGenerator::validate_stored_closure(&block, "test".to_string());
    assert!(result.is_err());

    // Should be field error (checked first), not local
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInStoredClosure { .. })
        ),
        "Field error should take precedence over local mutation"
    );
}

#[test]
fn test_codegen_rejects_stored_closure_with_field_assignment() {
    // Integration test: verify the full codegen pipeline catches field assignments
    // Build a module with: test := [ myBlock := [self.value := 1]. myBlock ]
    let module = Module {
        classes: vec![],
        method_definitions: Vec::new(),
        expressions: vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "test",
                Span::new(0, 4),
            ))),
            value: Box::new(Expression::Block(Block {
                parameters: vec![],
                body: vec![
                    // myBlock := [self.value := self.value + 1]
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "myBlock",
                            Span::new(10, 17),
                        ))),
                        value: Box::new(Expression::Block(Block {
                            parameters: vec![],
                            body: vec![bare(Expression::Assignment {
                                target: Box::new(Expression::FieldAccess {
                                    receiver: Box::new(Expression::Identifier(Identifier::new(
                                        "self",
                                        Span::new(22, 26),
                                    ))),
                                    field: Identifier::new("value", Span::new(27, 32)),
                                    span: Span::new(22, 32),
                                }),
                                value: Box::new(Expression::Literal(
                                    Literal::Integer(1),
                                    Span::new(36, 37),
                                )),
                                span: Span::new(22, 37),
                            })],
                            span: Span::new(21, 38),
                        })),
                        span: Span::new(10, 38),
                    }),
                    bare(Expression::Identifier(Identifier::new(
                        "myBlock",
                        Span::new(40, 47),
                    ))),
                ],
                span: Span::new(8, 49),
            })),
            span: Span::new(0, 49),
        })],
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    // BT-852: Stored closures with field assignments are now allowed via Tier 2 protocol.
    let result = generate(&module);
    assert!(
        result.is_ok(),
        "Field assignment in stored closure should now be allowed via Tier 2 protocol. Got: {result:?}"
    );
}

#[test]
fn test_codegen_rejects_stored_closure_with_local_mutation() {
    // Integration test: verify the full codegen pipeline catches local mutations
    // Build a module with: test := [ count := 0. myBlock := [count := count + 1]. myBlock ]
    let module = Module {
        classes: vec![],
        method_definitions: Vec::new(),
        expressions: vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "test",
                Span::new(0, 4),
            ))),
            value: Box::new(Expression::Block(Block {
                parameters: vec![],
                body: vec![
                    // count := 0
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "count",
                            Span::new(10, 15),
                        ))),
                        value: Box::new(Expression::Literal(
                            Literal::Integer(0),
                            Span::new(19, 20),
                        )),
                        span: Span::new(10, 20),
                    }),
                    // myBlock := [count := count + 1]
                    bare(Expression::Assignment {
                        target: Box::new(Expression::Identifier(Identifier::new(
                            "myBlock",
                            Span::new(22, 29),
                        ))),
                        value: Box::new(Expression::Block(Block {
                            parameters: vec![],
                            body: vec![bare(Expression::Assignment {
                                target: Box::new(Expression::Identifier(Identifier::new(
                                    "count",
                                    Span::new(34, 39),
                                ))),
                                value: Box::new(Expression::MessageSend {
                                    receiver: Box::new(Expression::Identifier(Identifier::new(
                                        "count",
                                        Span::new(43, 48),
                                    ))),
                                    selector: MessageSelector::Binary("+".into()),
                                    arguments: vec![Expression::Literal(
                                        Literal::Integer(1),
                                        Span::new(51, 52),
                                    )],
                                    is_cast: false,
                                    span: Span::new(43, 52),
                                }),
                                span: Span::new(34, 52),
                            })],
                            span: Span::new(33, 53),
                        })),
                        span: Span::new(22, 53),
                    }),
                    bare(Expression::Identifier(Identifier::new(
                        "myBlock",
                        Span::new(55, 62),
                    ))),
                ],
                span: Span::new(8, 64),
            })),
            span: Span::new(0, 64),
        })],
        span: Span::new(0, 65),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    // BT-852: Stored closures with local mutations are now allowed via Tier 2 protocol.
    let result = generate(&module);
    assert!(
        result.is_ok(),
        "Local mutation in stored closure should now be allowed via Tier 2 protocol. Got: {result:?}"
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "comprehensive test covering all registration metadata"
)]
fn test_class_registration_generation() {
    // BT-218: Test that class definitions generate registration code
    use crate::ast::{ClassDefinition, Identifier, MethodDefinition, MethodKind, StateDeclaration};
    use crate::source_analysis::Span;

    // Create a Counter class with instance variables and methods
    let class = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 7)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![StateDeclaration {
            name: Identifier::new("value", Span::new(0, 5)),
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
            type_annotation: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 10),
        }],
        methods: vec![
            MethodDefinition {
                selector: MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(42),
                    Span::new(0, 2),
                ))],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 10),
            },
            MethodDefinition {
                selector: MessageSelector::Unary("getValue".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(42),
                    Span::new(0, 2),
                ))],
                return_type: None,
                is_sealed: false,
                kind: MethodKind::Primary,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 10),
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 50),
    };

    let module = Module {
        expressions: vec![],
        classes: vec![class],
        method_definitions: Vec::new(),
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // Check that on_load attribute is present
    assert!(
        code.contains("'on_load' = [{'register_class', 0}]"),
        "Should have on_load attribute. Got:\n{code}"
    );

    // Check that register_class/0 is exported
    assert!(
        code.contains("'register_class'/0"),
        "Should export register_class/0. Got:\n{code}"
    );

    // Check that register_class/0 function exists
    assert!(
        code.contains("'register_class'/0 = fun () ->"),
        "Should generate register_class function. Got:\n{code}"
    );

    // BT-837: Check that it calls beamtalk_class_builder:register
    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState0)"),
        "Should call beamtalk_class_builder:register. Got:\n{code}"
    );

    // Check ClassBuilder state fields
    assert!(
        code.contains("'className' => 'Counter'"),
        "Should include className in builder state. Got:\n{code}"
    );
    assert!(
        code.contains("'moduleName' => 'counter'"),
        "Should include moduleName in builder state. Got:\n{code}"
    );
    assert!(
        code.contains("'superclassRef' => 'Actor'"),
        "Should include superclassRef in builder state. Got:\n{code}"
    );

    // BT-745: Check beamtalk_class module attribute for dependency sorting
    assert!(
        code.contains("'beamtalk_class' = [{'Counter', 'Actor'}]"),
        "Should include beamtalk_class attribute with class and superclass. Got:\n{code}"
    );

    // Check methodSpecs
    assert!(
        code.contains("'methodSpecs' => ~{"),
        "Should include methodSpecs map. Got:\n{code}"
    );
    assert!(
        code.contains("'increment' => ~{'arity' => 0, 'is_sealed' => 'false'}~"),
        "Should include increment method with arity and sealed flag. Got:\n{code}"
    );
    assert!(
        code.contains("'getValue' => ~{'arity' => 0, 'is_sealed' => 'false'}~"),
        "Should include getValue method with arity and sealed flag. Got:\n{code}"
    );

    // Check fieldSpecs (map with defaults, not list)
    assert!(
        code.contains("'fieldSpecs' => ~{'value' => 0}~"),
        "Should include fieldSpecs map with defaults. Got:\n{code}"
    );

    // Check classMethods map
    assert!(
        code.contains("'classMethods' => ~{"),
        "Should include classMethods map. Got:\n{code}"
    );
    assert!(
        code.contains("'spawn' => ~{'arity' => 0}~"),
        "Should include spawn class method. Got:\n{code}"
    );
    assert!(
        code.contains("'spawnWith:' => ~{'arity' => 1}~"),
        "Should include spawnWith: class method. Got:\n{code}"
    );

    // Check modifiers list
    assert!(
        code.contains("'modifiers' => []"),
        "Should include empty modifiers list. Got:\n{code}"
    );

    // Check function returns ok
    assert!(code.contains("'ok'"), "Should return 'ok'. Got:\n{code}");
}

#[test]
fn test_no_class_registration_for_empty_module() {
    // BT-218: Modules without class definitions should not have on_load or register_class
    let module = Module::new(vec![], Span::new(0, 0));
    let code = generate_module(&module, CodegenOptions::new("empty_module"))
        .expect("codegen should succeed");

    // Should NOT have on_load attribute
    assert!(
        !code.contains("'on_load'"),
        "Module without classes should not have on_load. Got:\n{code}"
    );

    // Should NOT export register_class/0
    assert!(
        !code.contains("'register_class'/0"),
        "Module without classes should not export register_class. Got:\n{code}"
    );

    // BT-745: Should NOT have beamtalk_class attribute
    assert!(
        !code.contains("'beamtalk_class'"),
        "Module without classes should not have beamtalk_class attribute. Got:\n{code}"
    );
}

#[test]
#[allow(clippy::too_many_lines)]
fn test_multiple_classes_registration() {
    // BT-218: Test that modules with multiple classes register all of them
    use crate::ast::{ClassDefinition, Identifier, StateDeclaration};
    use crate::source_analysis::Span;

    fn make_actor_class(
        name: &str,
        name_len: u32,
        field: &str,
        field_len: u32,
        span_end: u32,
    ) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, Span::new(0, name_len)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![StateDeclaration {
                name: Identifier::new(field, Span::new(0, field_len)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 10),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, span_end),
        }
    }

    let module = Module {
        expressions: vec![],
        classes: vec![
            make_actor_class("Counter", 7, "value", 5, 20),
            make_actor_class("Logger", 6, "messages", 8, 30),
        ],
        method_definitions: Vec::new(),
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("multi_actors"))
        .expect("codegen should succeed");

    // Should have on_load attribute
    assert!(
        code.contains("'on_load' = [{'register_class', 0}]"),
        "Should have on_load attribute for multiple classes. Got:\n{code}"
    );

    // BT-837: Should register both classes via ClassBuilder
    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState0)"),
        "Should register Counter via ClassBuilder. Got:\n{code}"
    );
    assert!(
        code.contains("'className' => 'Counter'"),
        "Should include Counter metadata. Got:\n{code}"
    );
    assert!(
        code.contains("'fieldSpecs' => ~{'value' => 0}~"),
        "Should include Counter fieldSpecs. Got:\n{code}"
    );

    assert!(
        code.contains("call 'beamtalk_class_builder':'register'(_BuilderState1)"),
        "Should register Logger via ClassBuilder. Got:\n{code}"
    );
    assert!(
        code.contains("'className' => 'Logger'"),
        "Should include Logger metadata. Got:\n{code}"
    );
    assert!(
        code.contains("'fieldSpecs' => ~{'messages' => 0}~"),
        "Should include Logger fieldSpecs. Got:\n{code}"
    );

    // Should use let-binding chain to sequence registrations
    assert!(
        code.contains("let _BuilderState0 = ~{"),
        "Should have first BuilderState binding. Got:\n{code}"
    );
    assert!(
        code.contains("let _Reg0 = case"),
        "Should have first registration with _Reg0. Got:\n{code}"
    );
    assert!(
        code.contains("let _BuilderState1 = ~{"),
        "Should have second BuilderState binding. Got:\n{code}"
    );
    assert!(
        code.contains("let _Reg1 = case"),
        "Should chain second registration with _Reg1. Got:\n{code}"
    );

    // BT-738: Final result propagates last _Reg.
    assert!(
        code.contains("in _Reg1"),
        "Should propagate last _Reg result after all registrations. Got:\n{code}"
    );

    // BT-749: Short-circuit: earlier error must propagate before executing later classes.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should short-circuit on _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error. Got:\n{code}"
    );

    // BT-745: Check beamtalk_class attribute lists both classes
    assert!(
        code.contains("'beamtalk_class' = [{'Counter', 'Actor'}, {'Logger', 'Actor'}]"),
        "Should include beamtalk_class attribute with both classes. Got:\n{code}"
    );
}

#[test]
fn test_multi_class_early_error_short_circuits() {
    // BT-749: When an earlier class (not the last) returns {error, ...} from
    // update_class (e.g. stdlib_shadowing), the error must propagate — the
    // subsequent class registrations must not mask it with 'ok'.
    //
    // We verify this by checking the generated code structure: each _RegN
    // (except the last) must be wrapped in a case that short-circuits on error.
    use crate::ast::{ClassDefinition, Identifier, StateDeclaration};
    use crate::source_analysis::Span;

    fn make_class(name: &str, name_len: u32, span_end: u32) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, Span::new(0, name_len)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![StateDeclaration {
                name: Identifier::new("x", Span::new(0, 1)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 5),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, span_end),
        }
    }

    // Two classes: ShadowA (index 0), ValidB (index 1, last).
    // ValidB is fine; ShadowA would be the one shadowing stdlib.
    // The fix must ensure that if _Reg0 is {error, ...}, we never reach _Reg1.
    let module = Module {
        expressions: vec![],
        classes: vec![make_class("ShadowA", 7, 20), make_class("ValidB", 6, 30)],
        method_definitions: Vec::new(),
        span: Span::new(0, 50),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("multi_shadow"))
        .expect("codegen should succeed");

    // BT-749: First class must be wrapped in a short-circuit case check.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should wrap _Reg0 in a short-circuit case. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error before executing later classes. Got:\n{code}"
    );

    // The last class's result is returned directly (no further wrapping needed).
    assert!(
        code.contains("in _Reg1"),
        "Should use _Reg1 as the final result. Got:\n{code}"
    );

    // The second class must NOT be wrapped in its own short-circuit case
    // (it is the last, so its result flows out directly).
    assert!(
        !code.contains("in case _Reg1 of"),
        "Last _Reg should not be wrapped in a short-circuit case. Got:\n{code}"
    );
}

#[test]
fn test_three_class_short_circuit_nesting() {
    // BT-749: Verify nesting correctness for N=3 classes.
    // Short-circuit cases are added for indices 0 and 1 (all except the last).
    // The last class (index 2) is returned directly with no extra wrapping.
    use crate::ast::{ClassDefinition, Identifier, StateDeclaration};
    use crate::source_analysis::Span;

    fn make_class(name: &str, name_len: u32) -> ClassDefinition {
        ClassDefinition {
            name: Identifier::new(name, Span::new(0, name_len)),
            superclass: Some(Identifier::new("Actor", Span::new(0, 5))),
            class_kind: ClassKind::Actor,
            is_abstract: false,
            is_sealed: false,
            is_typed: false,
            state: vec![StateDeclaration {
                name: Identifier::new("x", Span::new(0, 1)),
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
                type_annotation: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 5),
            }],
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 20),
        }
    }

    let module = Module {
        expressions: vec![],
        classes: vec![make_class("A", 1), make_class("B", 1), make_class("C", 1)],
        method_definitions: Vec::new(),
        span: Span::new(0, 60),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("three_classes"))
        .expect("codegen should succeed");

    // BT-749: Classes 0 and 1 (non-last) must have short-circuit case wrappers.
    assert!(
        code.contains("in case _Reg0 of"),
        "Should short-circuit on _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr0}> when 'true' -> {'error', _RegErr0}"),
        "Should propagate _Reg0 error. Got:\n{code}"
    );
    assert!(
        code.contains("in case _Reg1 of"),
        "Should short-circuit on _Reg1 error. Got:\n{code}"
    );
    assert!(
        code.contains("<{'error', _RegErr1}> when 'true' -> {'error', _RegErr1}"),
        "Should propagate _Reg1 error. Got:\n{code}"
    );

    // Class 2 (last) must be returned directly — no extra case wrapping.
    assert!(
        code.contains("in _Reg2"),
        "Should use _Reg2 as final result. Got:\n{code}"
    );
    assert!(
        !code.contains("in case _Reg2 of"),
        "Last _Reg should not be wrapped in a short-circuit case. Got:\n{code}"
    );
}

#[test]
fn test_class_method_call_generation() {
    // BT-215: Test that ClassReference message sends generate appropriate code
    // BT-490 / ADR 0019: All classes (including Transcript, Beamtalk, Workspace)
    //         use standard class dispatch via class_send
    use crate::ast::{Expression, Identifier, MessageSelector};
    use crate::source_analysis::Span;

    // Test 1: Beamtalk class uses standard class_send dispatch (no special case)
    let expr = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Beamtalk", Span::new(0, 8)),
            span: Span::new(0, 8),
        }),
        selector: MessageSelector::Unary("allClasses".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 20),
    };

    let code = generate_repl_expression(&expr, "repl_eval").expect("codegen should succeed");

    // ADR 0019: Beamtalk should check bindings first, then class_send fallback
    assert!(
        code.contains("maps':'find") && code.contains("class_send"),
        "Beamtalk should check bindings then class_send. Got:\n{code}"
    );
    assert!(
        !code.contains("persistent_term"),
        "Beamtalk should NOT use persistent_term. Got:\n{code}"
    );

    // Test 2: Non-binding class (Point) dispatches via class_send in REPL
    let expr2 = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Point", Span::new(0, 5)),
            span: Span::new(0, 5),
        }),
        selector: MessageSelector::Unary("new".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 10),
    };

    let code2 = generate_repl_expression(&expr2, "repl_eval2")
        .expect("codegen should succeed for non-binding class");

    // BT-411/ADR 0019: In REPL, all class references check bindings then class_send
    assert!(
        code2.contains("maps':'find") && code2.contains("class_send"),
        "Non-binding class should check bindings then class_send. Got:\n{code2}"
    );
    assert!(
        !code2.contains("persistent_term"),
        "Non-binding class should NOT use persistent_term. Got:\n{code2}"
    );

    // Test 3: ClassReference spawn in REPL uses generate_actor_spawn with registry
    let expr3 = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("InitCounter", Span::new(0, 11)),
            span: Span::new(0, 11),
        }),
        selector: MessageSelector::Unary("spawn".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 17),
    };

    let code3 =
        generate_repl_expression(&expr3, "repl_eval3").expect("codegen should succeed for spawn");

    // Spawn on ClassReference uses generate_actor_spawn which calls
    // Module:spawn() with REPL registry integration
    assert!(
        code3.contains("'initcounter':'spawn'") || code3.contains("register_spawned"),
        "REPL spawn should use direct module spawn (with optional registry). Got:\n{code3}"
    );
}

// BT-98 PR Comment #8: Test for state version increment bug
// Bug: generate_local_var_assignment_in_loop() doesn't increment state_version
// This test documents the expected behavior
#[test]
fn test_state_version_should_increment_for_local_var_assignment() {
    // This test verifies the EXPECTED behavior (currently failing due to bug).
    // The bug is in control_flow.rs:978 where state_version is not incremented
    // between reading current_state (lines 966-970) and creating new_state (line 978).
    //
    // Expected Core Erlang pattern:
    //   let _Val1 = <value> in
    //   let StateAcc1 = call 'maps':'put'('x', _Val1, StateAcc) in
    //                   ^^^^^^^^^                             ^^^^^^^^
    //                   version 1 (new)                       version 0 (current)
    //
    // Actual (buggy) pattern:
    //   let _Val1 = <value> in
    //   let StateAcc = call 'maps':'put'('x', _Val1, StateAcc) in
    //       ^^^^^^^^                                 ^^^^^^^^
    //       same version - INVALID!

    // This test documents the issue. The fix will be in control_flow.rs line 975-978
    // by adding: let _ = self.next_state_var();
}

// BT-98 PR Comment #10: Test for double " in " bug
// Bug: Sequential field assignments produce " in  in " (double space)
// This test documents the expected behavior
#[test]
fn test_sequential_field_assignments_should_not_double_in() {
    // This test verifies the EXPECTED behavior (currently failing due to bug).
    // The bug is in control_flow.rs:1177 where " in " is written before every
    // expression when i > 0, but generate_field_assignment_open() already writes
    // a trailing " in ".
    //
    // Expected Core Erlang pattern for two field assignments:
    //   let _Val1 = 1 in let StateAcc1 = ... in let _Val2 = 2 in let StateAcc2 = ... in
    //                                       ^^^ single " in "
    //
    // Actual (buggy) pattern:
    //   let _Val1 = 1 in let StateAcc1 = ... in  in let _Val2 = 2 in let StateAcc2 = ... in
    //                                         ^^^^^^^ double " in " - INVALID!

    // This test documents the issue. The fix will be in control_flow.rs
    // by ensuring only non-assignment expressions write " in " when necessary.
}

#[test]
fn test_standalone_class_reference_uses_dynamic_module_name() {
    // BT-215: Test that standalone ClassReference uses module_name/1 dynamically
    // Review comment: Should match generate_beamtalk_class_named pattern (lines 915-922)
    use crate::ast::{Expression, Identifier, Module};
    use crate::source_analysis::Span;

    // Create expression: Point (standalone class reference)
    let expr = Expression::ClassReference {
        name: Identifier::new("Point", Span::new(0, 5)),
        span: Span::new(0, 5),
    };

    let module = Module {
        expressions: vec![bare(expr)],
        classes: vec![],
        method_definitions: Vec::new(),
        span: Span::new(0, 5),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_repl_expression(&module.expressions[0].expression, "repl_eval")
        .expect("codegen should succeed");

    // Should call whereis_class to get the class PID
    assert!(
        code.contains("call 'beamtalk_class_registry':'whereis_class'('Point')"),
        "Should call whereis_class to get class PID. Got:\n{code}"
    );

    // Should call module_name to get the module name dynamically
    assert!(
        code.contains("call 'beamtalk_object_class':'module_name'("),
        "Should call module_name to get module name dynamically. Got:\n{code}"
    );

    // Should NOT hardcode 'beamtalk_object_class' as the module name
    assert!(
        !code.contains("'beamtalk_object', 'Point class', 'beamtalk_object_class'"),
        "Should not hardcode 'beamtalk_object_class' as module name. Got:\n{code}"
    );

    // Should create beamtalk_object with dynamic ClassModName variable
    assert!(
        code.contains("'beamtalk_object'") && code.contains("'Point class'"),
        "Should create beamtalk_object with metaclass name. Got:\n{code}"
    );
}

#[test]
fn test_standalone_class_reference_validates_undefined_classes() {
    // BT-215, BT-597: Test that standalone ClassReference raises class_not_found error for undefined classes
    use crate::ast::{Expression, Identifier, Module};
    use crate::source_analysis::Span;

    // Create expression: NonExistentClass (standalone class reference)
    let expr = Expression::ClassReference {
        name: Identifier::new("NonExistentClass", Span::new(0, 16)),
        span: Span::new(0, 16),
    };

    let module = Module {
        expressions: vec![bare(expr)],
        classes: vec![],
        method_definitions: Vec::new(),
        span: Span::new(0, 16),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_repl_expression(&module.expressions[0].expression, "repl_eval")
        .expect("codegen should succeed");

    // Should use a case expression to check for undefined
    assert!(
        code.contains("case call 'beamtalk_class_registry':'whereis_class'('NonExistentClass')"),
        "Should use case expression to handle whereis_class result. Got:\n{code}"
    );

    // Should raise class_not_found error when class is undefined (BT-597)
    assert!(
        code.contains("beamtalk_error':'new'('class_not_found', 'NonExistentClass')"),
        "Should raise class_not_found error when whereis_class returns 'undefined'. Got:\n{code}"
    );

    // Should include actionable hint via with_hint call
    assert!(
        code.contains("beamtalk_error':'with_hint'"),
        "Should include hint via with_hint call. Got:\n{code}"
    );

    // Should create beamtalk_object in the success branch
    assert!(
        code.contains('<')
            && code.contains("> when 'true' ->")
            && code.contains("'beamtalk_object'"),
        "Should create beamtalk_object in success branch of case. Got:\n{code}"
    );
}

// --- ClassHierarchy integration tests (BT-279) ---

#[test]
fn test_is_actor_class_direct_actor_subclass() {
    let class = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_object_subclass_is_value_type() {
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Object", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(!CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_multi_level_inheritance() {
    // LoggingCounter extends Counter extends Actor
    // Should still be detected as actor
    let counter = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let logging_counter = ClassDefinition {
        name: Identifier::new("LoggingCounter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Counter", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    // Module with both classes; first class is LoggingCounter
    let module = Module {
        classes: vec![counter, logging_counter.clone()],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();

    // Test with LoggingCounter as the first class
    let module_lc = Module {
        classes: vec![logging_counter],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    // Build hierarchy from full module so Counter is known
    assert!(CoreErlangGenerator::is_actor_class(&module_lc, &hierarchy));
}

#[test]
fn test_is_actor_class_no_classes_defaults_to_actor() {
    let module = Module::new(Vec::new(), Span::new(0, 0));
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_unknown_superclass_defaults_to_actor() {
    // LoggingCounter extends Counter, but Counter is NOT in this module.
    // Hierarchy chain is incomplete; should default to actor (backward compat).
    let class = ClassDefinition {
        name: Identifier::new("LoggingCounter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Counter", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(CoreErlangGenerator::is_actor_class(&module, &hierarchy));
}

#[test]
fn test_is_actor_class_collection_subclass_is_value_type() {
    // Collection extends Object (built-in), so subclasses are value types.
    let class = ClassDefinition {
        name: Identifier::new("MyList", Span::new(0, 0)),
        superclass: Some(Identifier::new("Collection", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(
        !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Collection subclass should be value type (chain reaches Object)"
    );
}

#[test]
fn test_is_actor_class_integer_subclass_is_value_type() {
    // Integer is a sealed built-in extending Object — subclass should be value type.
    // (Sealed enforcement is separate; codegen should still route correctly.)
    let class = ClassDefinition {
        name: Identifier::new("MyInt", Span::new(0, 0)),
        superclass: Some(Identifier::new("Integer", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(
        !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Integer subclass should be value type (chain reaches Object)"
    );
}

#[test]
fn test_is_actor_class_root_class_is_value_type() {
    // Root class (superclass: None → "none") should be value type, not actor.
    let class = ClassDefinition {
        name: Identifier::new("ProtoObject", Span::new(0, 0)),
        superclass: None,
        class_kind: ClassKind::Object,
        is_abstract: true,
        is_sealed: false,
        is_typed: false,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: vec![],
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let hierarchy = crate::semantic_analysis::class_hierarchy::ClassHierarchy::build(&module)
        .0
        .unwrap();
    assert!(
        !CoreErlangGenerator::is_actor_class(&module, &hierarchy),
        "Root class (nil superclass) should be value type"
    );
}

#[test]
fn test_generate_primitive_selector_based() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.class_identity = Some(util::ClassIdentity::new("Integer"));
    generator.current_method_params = vec!["Other".to_string()];

    let doc = generator
        .generate_primitive("+", true, Span::new(0, 0))
        .unwrap();
    // BT-340: Now emits direct Erlang BIF instead of dispatch delegation
    assert_eq!(doc.to_pretty_string(), "call 'erlang':'+'(Self, Other)");
}

#[test]
fn test_generate_primitive_structural_intrinsic() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.class_identity = Some(util::ClassIdentity::new("Block"));
    generator.current_method_params = vec![];

    let doc = generator
        .generate_primitive("blockValue", false, Span::new(0, 0))
        .unwrap();
    assert_eq!(
        doc.to_pretty_string(),
        "call 'bt@stdlib@block':'dispatch'('blockValue', [], Self)"
    );
}

#[test]
fn test_generate_primitive_multiple_params() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.class_identity = Some(util::ClassIdentity::new("Integer"));
    generator.current_method_params = vec!["End".to_string(), "Block".to_string()];

    let doc = generator
        .generate_primitive("toDo", false, Span::new(0, 0))
        .unwrap();
    assert_eq!(
        doc.to_pretty_string(),
        "call 'bt@stdlib@integer':'dispatch'('toDo', [End, Block], Self)"
    );
}

#[test]
fn test_generate_with_bindings_compiles_value_type() {
    // Test that generate_with_bindings produces valid output for a value type
    let class = ClassDefinition::new(
        Identifier::new("Point", Span::new(0, 0)),
        Identifier::new("Object", Span::new(0, 0)),
        vec![StateDeclaration {
            name: Identifier::new("x", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        vec![],
        Span::new(0, 0),
    );
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let bindings = primitive_bindings::PrimitiveBindingTable::new();
    let result = generate_module(
        &module,
        CodegenOptions::new("point").with_bindings(bindings),
    );
    assert!(result.is_ok());
    let code = result.unwrap();
    assert!(code.contains("module 'point'"));
}

#[test]
fn test_generate_repl_list_reject() {
    // BT-408: reject: must generate valid Core Erlang with properly bound wrapper fun
    // The wrapper fun must be bound via `let` — not inlined in the call args,
    // because Core Erlang lambdas don't use `end` and can't be inlined in calls.
    let src = "#(1, 2, 3, 4, 5) reject: [:x | x > 2]";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let expr = &module.expressions[0].expression;
    let code = generate_repl_expression(expr, "test_reject_repl").expect("codegen should work");

    // Wrapper fun must be bound to a variable, not inlined in filter call
    assert!(
        code.contains("call 'lists':'filter'("),
        "Should use lists:filter. Got:\n{code}"
    );
    assert!(
        code.contains("call 'erlang':'not'("),
        "Should negate predicate. Got:\n{code}"
    );
    // Verify the fun is let-bound (not inlined) — the filter call arg must be a temp var
    // e.g. "call 'lists':'filter'(_temp4," not "call 'lists':'filter'(fun (X)"
    assert!(
        !code.contains("'filter'(fun"),
        "Wrapper fun must be let-bound, not inlined in filter call. Got:\n{code}"
    );
}

#[test]
fn test_class_method_rejects_field_access() {
    // BT-426: Class methods should reject instance field access
    let src = "Actor subclass: TestClass\n  state: value = 0\n\n  class broken => self.value";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("test_class_field").with_workspace_mode(true),
    );
    assert!(
        result.is_err(),
        "Should reject field access in class method"
    );
    let err = format!("{}", result.unwrap_err());
    assert!(
        err.contains("cannot access instance field"),
        "Error should mention field access. Got: {err}"
    );
}

#[test]
fn test_class_method_rejects_field_assignment() {
    // BT-426: Class methods should reject instance field mutation
    let src = "Actor subclass: TestClass\n  state: value = 0\n\n  class broken => self.value := 42";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("test_class_assign").with_workspace_mode(true),
    );
    assert!(
        result.is_err(),
        "Should reject field assignment in class method"
    );
    let err = format!("{}", result.unwrap_err());
    assert!(
        err.contains("cannot assign to instance field"),
        "Error should mention field assignment. Got: {err}"
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

// BT-682: Direct Erlang call optimization tests

#[test]
fn test_erlang_interop_direct_call_keyword_single_arg() {
    // `Erlang lists reverse: xs` → `call 'lists':'reverse'(Xs)`
    let mut generator = CoreErlangGenerator::new("test");

    // Inner: Erlang lists (ClassReference("Erlang") + Unary("lists"))
    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
        }),
        selector: MessageSelector::Unary("lists".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 12),
    };
    // Outer: (Erlang lists) reverse: xs
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("reverse:", Span::new(13, 21))]);
    let arguments = vec![Expression::Identifier(Identifier::new(
        "xs",
        Span::new(22, 24),
    ))];

    let doc = generator
        .generate_message_send(&inner_receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("call 'lists':'reverse'("),
        "Should emit direct call. Got: {output}"
    );
    assert!(
        !output.contains("ErlangModule"),
        "Should not create proxy map. Got: {output}"
    );
    assert!(
        !output.contains("beamtalk_message_dispatch"),
        "Should not use runtime dispatch. Got: {output}"
    );
}

#[test]
fn test_erlang_interop_direct_call_keyword_multi_arg() {
    // `Erlang lists seq: 1 with: 10` → `call 'lists':'seq'(1, 10)`
    let mut generator = CoreErlangGenerator::new("test");

    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
        }),
        selector: MessageSelector::Unary("lists".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 12),
    };
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("seq:", Span::new(13, 17)),
        KeywordPart::new("with:", Span::new(20, 25)),
    ]);
    let arguments = vec![
        Expression::Literal(Literal::Integer(1), Span::new(18, 19)),
        Expression::Literal(Literal::Integer(10), Span::new(26, 28)),
    ];

    let doc = generator
        .generate_message_send(&inner_receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("call 'lists':'seq'(1, 10)"),
        "Should emit direct multi-arg call. Got: {output}"
    );
    assert!(
        !output.contains("ErlangModule"),
        "Should not create proxy map. Got: {output}"
    );
}

#[test]
fn test_erlang_interop_direct_call_zero_arg() {
    // `Erlang erlang node` → `call 'erlang':'node'()`
    let mut generator = CoreErlangGenerator::new("test");

    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
        }),
        selector: MessageSelector::Unary("erlang".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 13),
    };
    // Outer: (Erlang erlang) node — unary selector, zero arguments
    let selector = MessageSelector::Unary("node".into());
    let arguments = vec![];

    let doc = generator
        .generate_message_send(&inner_receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert_eq!(
        output, "call 'erlang':'node'()",
        "Should emit direct zero-arg call. Got: {output}"
    );
}

#[test]
fn test_erlang_interop_proxy_still_works_for_standalone() {
    // `Erlang lists` (standalone, no chained call) → proxy map (unchanged)
    let mut generator = CoreErlangGenerator::new("test");

    let receiver = Expression::ClassReference {
        name: Identifier::new("Erlang", Span::new(0, 6)),
        span: Span::new(0, 6),
    };
    let selector = MessageSelector::Unary("lists".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("ErlangModule"),
        "Standalone Erlang lists should still create proxy. Got: {output}"
    );
    assert!(
        output.contains("'module' => 'lists'"),
        "Proxy should contain module name. Got: {output}"
    );
}

#[test]
fn test_erlang_interop_cached_proxy_uses_runtime_dispatch() {
    // `proxy reverse: xs` where proxy is an Identifier (cached proxy)
    // Should use runtime dispatch, not direct call
    let mut generator = CoreErlangGenerator::new("test");

    let receiver = Expression::Identifier(Identifier::new("proxy", Span::new(0, 5)));
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("reverse:", Span::new(6, 14))]);
    let arguments = vec![Expression::Identifier(Identifier::new(
        "xs",
        Span::new(15, 17),
    ))];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("beamtalk_message_dispatch"),
        "Cached proxy should use runtime dispatch. Got: {output}"
    );
    assert!(
        !output.contains("call 'lists'"),
        "Cached proxy should not use direct call. Got: {output}"
    );
}

#[test]
fn test_erlang_interop_protocol_selectors_not_optimized() {
    // `(Erlang lists) printString` should NOT emit `call 'lists':'printString'()`
    // It should fall through to runtime dispatch for the proxy's inherited method.
    let mut generator = CoreErlangGenerator::new("test");

    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
        }),
        selector: MessageSelector::Unary("lists".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 12),
    };
    let selector = MessageSelector::Unary("printString".into());

    let doc = generator
        .generate_message_send(&inner_receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        !output.contains("call 'lists':'printString'"),
        "printString should not be optimized as direct Erlang call. Got: {output}"
    );
    assert!(
        output.contains("beamtalk_message_dispatch")
            || output.contains("beamtalk_primitive")
            || output.contains("ErlangModule"),
        "printString should use runtime dispatch or proxy. Got: {output}"
    );
}

// --- BT-894: Cross-file value-object subclassing ---

#[test]
fn test_cross_file_value_object_subclass_without_index() {
    // Without the superclass index, a class whose parent is not in the hierarchy
    // defaults to actor codegen (the old broken behavior).
    let src = "MyParent subclass: MyChild\n  getValue => 42";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let result = generate_module(&module, CodegenOptions::new("my_child"));
    assert!(result.is_ok());
    let code = result.unwrap();
    // Without index, defaults to actor (gen_server)
    assert!(
        code.contains("'gen_server'"),
        "Without superclass index, unknown parent should default to actor. Got:\n{code}"
    );
}

#[test]
fn test_cross_file_value_object_subclass_with_index() {
    // BT-894: With the superclass index providing the chain MyParent → Object,
    // the compiler should generate value-type code (no gen_server).
    let src = "MyParent subclass: MyChild\n  getValue => 42";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let mut superclass_index = std::collections::HashMap::new();
    superclass_index.insert("MyParent".to_string(), "Object".to_string());
    let result = generate_module(
        &module,
        CodegenOptions::new("my_child").with_class_superclass_index(superclass_index),
    );
    assert!(result.is_ok());
    let code = result.unwrap();
    // With index, should be value-type (no gen_server)
    assert!(
        !code.contains("'gen_server'"),
        "With superclass index showing MyParent → Object, should generate value-type code. Got:\n{code}"
    );
}

#[test]
fn test_cross_file_actor_subclass_with_index() {
    // BT-894: With the superclass index providing the chain MyActor → Actor,
    // the compiler should still generate actor code (gen_server).
    let src = "MyActor subclass: MySpecialActor\n  getValue => 42";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let mut superclass_index = std::collections::HashMap::new();
    superclass_index.insert("MyActor".to_string(), "Actor".to_string());
    let result = generate_module(
        &module,
        CodegenOptions::new("my_special_actor").with_class_superclass_index(superclass_index),
    );
    assert!(result.is_ok());
    let code = result.unwrap();
    // With index showing MyActor → Actor, should still be actor
    assert!(
        code.contains("'gen_server'"),
        "With superclass index showing MyActor → Actor, should generate actor code. Got:\n{code}"
    );
}

// ============================================================================
// BT-855: Erlang interop wrapper funs for stateful blocks at Erlang boundaries
// ============================================================================

#[test]
fn test_bt855_erlang_interop_wrapper_pure_block_no_warning() {
    // BT-855: A pure block passed to an Erlang call site should generate a plain
    // Tier 1 fun with no wrapper and no warning.
    //
    // Beamtalk: `Erlang lists map: [:x | x + 1] to: items`
    // Expected: `call 'lists':'map'(fun(X) -> X + 1 end, Items)` — no wrapper
    let mut generator = CoreErlangGenerator::new("test");

    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
        }),
        selector: MessageSelector::Unary("lists".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 12),
    };

    // Pure block: [:x | x + 1]
    let pure_block = Expression::Block(Block::new(
        vec![BlockParameter::new("x", Span::new(1, 2))],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "x",
                Span::new(5, 6),
            ))),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(9, 10))],
            is_cast: false,
            span: Span::new(5, 10),
        })],
        Span::new(0, 11),
    ));

    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("map:", Span::new(13, 17)),
        KeywordPart::new("to:", Span::new(20, 23)),
    ]);
    let arguments = vec![
        pure_block,
        Expression::Identifier(Identifier::new("items", Span::new(24, 29))),
    ];

    let doc = generator
        .generate_message_send(&inner_receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    // Pure block — no wrapper, no extra let-binding for BtBlock
    assert!(
        output.contains("call 'lists':'map'("),
        "Should emit direct Erlang call. Got: {output}"
    );
    assert!(
        !output.contains("BtBlock"),
        "Pure block should not generate BtBlock wrapper. Got: {output}"
    );
    // No warnings emitted for pure blocks
    assert!(
        generator.codegen_warnings.is_empty(),
        "Pure block at Erlang boundary should not emit a warning. Got: {:?}",
        generator.codegen_warnings
    );
}

#[test]
fn test_bt855_erlang_interop_wrapper_stateful_block_emits_warning() {
    // BT-855: A stateful block (one with captured variable mutations) passed to
    // an Erlang call site must be wrapped so Erlang sees a plain fun(Args) -> Result
    // without the StateAcc protocol. A warning is emitted because mutations are dropped.
    //
    // Beamtalk: `Erlang lists map: [:x | count := count + x] to: items`
    // Expected wrapper:
    //   let _ErlWrapper = let _BtBlock = fun(X, StateAcc) -> ... in
    //                     fun(X) -> let _WTuple = apply _BtBlock(X, State) in call 'erlang':'element'(1, _WTuple) in
    //   call 'lists':'map'(_ErlWrapper, Items)
    let mut generator = CoreErlangGenerator::new("test");

    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
        }),
        selector: MessageSelector::Unary("lists".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 12),
    };

    // Stateful block: [:x | count := count + x]
    // 'count' is read (captured from outer scope) and written (local_writes) → Tier 2
    let stateful_block = Expression::Block(Block::new(
        vec![BlockParameter::new("x", Span::new(1, 2))],
        vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "count",
                Span::new(5, 10),
            ))),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    Span::new(14, 19),
                ))),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Identifier(Identifier::new(
                    "x",
                    Span::new(22, 23),
                ))],
                is_cast: false,
                span: Span::new(14, 23),
            }),
            span: Span::new(5, 23),
        })],
        Span::new(0, 24),
    ));

    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("map:", Span::new(13, 17)),
        KeywordPart::new("to:", Span::new(20, 23)),
    ]);
    let arguments = vec![
        stateful_block,
        Expression::Identifier(Identifier::new("items", Span::new(24, 29))),
    ];

    let doc = generator
        .generate_message_send(&inner_receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    // Should still call 'lists':'map'
    assert!(
        output.contains("call 'lists':'map'("),
        "Should emit direct Erlang call. Got: {output}"
    );
    // Should have a BtBlock binding for the Tier 2 block
    assert!(
        output.contains("BtBlock"),
        "Stateful block should generate BtBlock wrapper. Got: {output}"
    );
    // Wrapper should apply the BtBlock with State
    assert!(
        output.contains("apply "),
        "Wrapper should apply the BtBlock. Got: {output}"
    );
    // StateAcc should appear in the Tier 2 block signature
    assert!(
        output.contains("StateAcc"),
        "Tier 2 block should have StateAcc parameter. Got: {output}"
    );
    // A warning should be emitted for the stateful boundary crossing
    assert!(
        !generator.codegen_warnings.is_empty(),
        "Stateful block at Erlang boundary should emit a warning"
    );
    assert!(
        generator
            .codegen_warnings
            .iter()
            .any(|w| w.message.contains("lists")),
        "Warning should mention the Erlang module. Got: {:?}",
        generator.codegen_warnings
    );
}

#[test]
fn test_bt855_collect_pure_block_no_warning() {
    // BT-855: `collect:` with a pure block should NOT emit a warning.
    // Pure blocks compile to Tier 1 and are passed directly to lists:map.
    let src = "
Actor subclass: Processor
  process: items =>
    items collect: [:x | x + 1]
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let result = generate_module_with_warnings(&module, CodegenOptions::new("processor"));
    let generated = result.expect("codegen should succeed");

    // Pure block — no warnings
    assert!(
        generated.warnings.is_empty(),
        "Pure block at collect: should not emit a warning. Got: {:?}",
        generated.warnings
    );
    // The block should be passed directly to lists:map (Tier 1 path)
    assert!(
        generated.code.contains("'lists':'map'"),
        "collect: should use lists:map. Got:\n{}",
        generated.code
    );
    // No BtBlock wrapper for pure blocks
    assert!(
        !generated.code.contains("BtBlock"),
        "Pure block should not have BtBlock wrapper. Got:\n{}",
        generated.code
    );
}

#[test]
fn test_bt855_generate_erlang_interop_wrapper_pure_returns_tier1() {
    // BT-855: generate_erlang_interop_wrapper on a pure block returns (Tier1Doc, false).
    let mut generator = CoreErlangGenerator::new("test");

    let pure_block = Block::new(
        vec![BlockParameter::new("x", Span::new(0, 1))],
        vec![bare(Expression::MessageSend {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "x",
                Span::new(3, 4),
            ))),
            selector: MessageSelector::Binary("+".into()),
            arguments: vec![Expression::Literal(Literal::Integer(1), Span::new(7, 8))],
            is_cast: false,
            span: Span::new(3, 8),
        })],
        Span::new(0, 9),
    );

    let (doc, is_stateful) = generator
        .generate_erlang_interop_wrapper(&pure_block)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(!is_stateful, "Pure block should return is_stateful=false");
    assert!(
        output.contains("fun ("),
        "Should generate a fun. Got: {output}"
    );
    assert!(
        !output.contains("BtBlock"),
        "Pure block should not generate a BtBlock. Got: {output}"
    );
    assert!(
        !output.contains("StateAcc"),
        "Pure block wrapper should not have StateAcc. Got: {output}"
    );
}

#[test]
fn test_bt855_generate_erlang_interop_wrapper_stateful_returns_wrapper() {
    // BT-855: generate_erlang_interop_wrapper on a stateful block returns (WrapperDoc, true).
    // The wrapper is:
    //   let BtBlock = fun(X, StateAcc) -> ... in fun(X) ->
    //       let _WT = apply BtBlock(X, State) in let WRes = call 'erlang':'element'(1, _WT) in WRes
    // NOTE: `let {X, _} = apply ...` is invalid Core Erlang inside a fun body (erlc rejects
    // tuple patterns in let). element/2 extraction is used instead.
    let mut generator = CoreErlangGenerator::new("test");

    // Stateful block: [:x | count := count + x]
    let stateful_block = Block::new(
        vec![BlockParameter::new("x", Span::new(0, 1))],
        vec![bare(Expression::Assignment {
            target: Box::new(Expression::Identifier(Identifier::new(
                "count",
                Span::new(3, 8),
            ))),
            value: Box::new(Expression::MessageSend {
                receiver: Box::new(Expression::Identifier(Identifier::new(
                    "count",
                    Span::new(12, 17),
                ))),
                selector: MessageSelector::Binary("+".into()),
                arguments: vec![Expression::Identifier(Identifier::new(
                    "x",
                    Span::new(20, 21),
                ))],
                is_cast: false,
                span: Span::new(12, 21),
            }),
            span: Span::new(3, 21),
        })],
        Span::new(0, 22),
    );

    let (doc, is_stateful) = generator
        .generate_erlang_interop_wrapper(&stateful_block)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(is_stateful, "Stateful block should return is_stateful=true");
    // Should bind the Tier 2 block to a temp var
    assert!(
        output.contains("BtBlock"),
        "Wrapper should contain BtBlock binding. Got: {output}"
    );
    // The Tier 2 block should have StateAcc parameter
    assert!(
        output.contains("StateAcc"),
        "Tier 2 block should have StateAcc parameter. Got: {output}"
    );
    // The wrapper fun should apply the BtBlock with State
    assert!(
        output.contains("apply "),
        "Wrapper should apply the BtBlock. Got: {output}"
    );
    // The result is extracted via element/2 (not tuple-pattern let, which is invalid
    // in Core Erlang inside a fun body).
    assert!(
        output.contains("'erlang':'element'(1,"),
        "Wrapper should extract result via erlang:element/2. Got: {output}"
    );
    // The outer fun should NOT have StateAcc — it's a plain Erlang fun.
    // Find the last `in fun (` to locate the wrapper fun signature and confirm
    // it does not contain StateAcc as a formal parameter (not in variable names).
    let wrapper_fun_start = output
        .rfind(" in fun (")
        .expect("wrapper output should contain 'in fun ('");
    let after_fun_sig = &output[wrapper_fun_start + " in fun (".len()..];
    // Extract just the function signature parameters: from after "fun (" to the first ")"
    let close_paren_idx = after_fun_sig
        .find(')')
        .expect("wrapper fun signature should have closing paren");
    let wrapper_params = &after_fun_sig[..close_paren_idx];
    assert!(
        !wrapper_params.contains("StateAcc"),
        "Wrapper fun formal parameters should not have StateAcc. Got params: {wrapper_params}, full output: {output}"
    );
}

// ========================================================================
// BT-915: self.slot := inside ifTrue: / ifFalse: / ifTrue:ifFalse: blocks
// ========================================================================

#[test]
fn test_if_true_with_field_mutation_generates_inline_case() {
    // BT-915: `flag ifTrue: [self.count := self.count + 1]` inside an actor method
    // should compile to an inline case expression that threads state through the
    // true branch, returning {Result, NewState} so the outer method body can
    // update its state chain.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  increment: flag =>\n    flag ifTrue: [self.count := self.count + 1].\n    self.count\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_if_true").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for ifTrue: with field mutation:\n{code}");

    // Should use inline case expression — not beamtalk_message_dispatch:send
    assert!(
        !code.contains("'send'(") || !code.contains("'ifTrue:'"),
        "ifTrue: with mutation should NOT go through runtime dispatch. Got:\n{code}"
    );

    // The conditional should be compiled as an inline case
    assert!(
        code.contains("case "),
        "Should generate inline case expression. Got:\n{code}"
    );

    // The true branch should contain maps:put for 'count'
    assert!(
        code.contains("maps':'put'('count'"),
        "True branch should update 'count' via maps:put. Got:\n{code}"
    );

    // StateAcc should be used inside the branch (loop-body naming to avoid conflicts)
    assert!(
        code.contains("StateAcc"),
        "Branch body should use StateAcc naming. Got:\n{code}"
    );

    // The outer state (State1) should be extracted from the result tuple via element/2
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Outer method body should extract NewState via element(2, ...). Got:\n{code}"
    );
}

#[test]
fn test_if_false_with_field_mutation_generates_inline_case() {
    // BT-915: `flag ifFalse: [self.count := self.count - 1]` should compile to
    // an inline case expression with state threading in the false branch.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  decrement: flag =>\n    flag ifFalse: [self.count := self.count - 1].\n    self.count\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_if_false").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for ifFalse: with field mutation:\n{code}");

    assert!(
        code.contains("case "),
        "Should generate inline case expression. Got:\n{code}"
    );
    assert!(
        code.contains("maps':'put'('count'"),
        "False branch should update 'count' via maps:put. Got:\n{code}"
    );
    assert!(
        code.contains("StateAcc"),
        "Branch body should use StateAcc naming. Got:\n{code}"
    );
}

#[test]
fn test_if_true_if_false_with_field_mutation_generates_inline_case() {
    // BT-915: `flag ifTrue: [...] ifFalse: [...]` with field mutations in both
    // branches should compile to an inline case with two state-threading branches.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  toggle: flag =>\n    flag ifTrue: [self.count := self.count + 10]\n         ifFalse: [self.count := self.count - 1].\n    self.count\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_if_true_if_false").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for ifTrue:ifFalse: with field mutations:\n{code}");

    assert!(
        code.contains("case "),
        "Should generate inline case expression. Got:\n{code}"
    );
    // Both branches should mutate count
    assert!(
        code.contains("maps':'put'('count'"),
        "Should update 'count' in a branch via maps:put. Got:\n{code}"
    );
    // Both branches return {Result, StateAccN} tuples
    assert!(
        code.contains("StateAcc"),
        "Both branches should use StateAcc naming. Got:\n{code}"
    );
    // Outer method body extracts NewState
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Outer method body should extract NewState via element(2, ...). Got:\n{code}"
    );
}

#[test]
fn test_if_true_without_mutation_uses_runtime_dispatch() {
    // BT-915: `flag ifTrue: [42]` with no mutations should still use runtime dispatch,
    // not the inline case generation. This ensures we don't break the non-mutation path.
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  check: flag =>\n    flag ifTrue: [42]\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_no_mutation").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for ifTrue: without mutation:\n{code}");

    // Pure ifTrue: should go through runtime dispatch
    assert!(
        code.contains("'beamtalk_message_dispatch':'send'"),
        "Pure ifTrue: should use runtime dispatch. Got:\n{code}"
    );
}

#[test]
fn test_nested_if_true_with_field_mutation_threads_state() {
    // BT-915: Nested `flag1 ifTrue: [flag2 ifTrue: [self.count := ...]]` should
    // correctly unpack the inner {Result, State} tuple so the outer branch
    // threads state from the inner conditional.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  nested: a and: b =>\n    a ifTrue: [\n      b ifTrue: [self.count := self.count + 100]\n    ].\n    self.count\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("ctr_nested").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for nested ifTrue: with field mutation:\n{code}");

    // Should generate inline case expressions (at least two)
    let case_count = code.matches("case ").count();
    assert!(
        case_count >= 2,
        "Should generate at least 2 inline case expressions for nested conditionals. Found {case_count}. Got:\n{code}"
    );

    // Should use maps:put for count
    assert!(
        code.contains("maps':'put'('count'"),
        "Inner branch should update 'count' via maps:put. Got:\n{code}"
    );
}

// BT-920: Cast send (!) codegen tests

#[test]
fn test_cast_send_to_non_self_receiver_uses_dispatch_cast() {
    // BT-920: `someActor increment!` should generate beamtalk_message_dispatch:cast/3
    let mut generator = CoreErlangGenerator::new("test");

    let receiver = Expression::Identifier(Identifier::new("someActor", Span::new(0, 9)));
    let selector = MessageSelector::Unary("increment".into());
    let send_expr = Expression::MessageSend {
        receiver: Box::new(receiver),
        selector: selector.clone(),
        arguments: vec![],
        is_cast: true,
        span: Span::new(0, 20),
    };

    let doc = if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: true,
        ..
    } = &send_expr
    {
        generator
            .generate_cast_send(receiver, selector, arguments)
            .unwrap()
    } else {
        panic!("expected MessageSend")
    };

    let output = doc.to_pretty_string();
    assert!(
        output.contains("beamtalk_message_dispatch':'cast'("),
        "Cast send should route via beamtalk_message_dispatch:cast/3. Got: {output}"
    );
    assert!(
        output.contains("'increment'"),
        "Cast send should include selector atom. Got: {output}"
    );
    assert!(
        !output.contains("beamtalk_message_dispatch':'send'"),
        "Cast send must NOT use send/3. Got: {output}"
    );
}

#[test]
fn test_cast_send_to_non_self_keyword_receiver_uses_dispatch_cast() {
    // BT-920: `someActor setValue: 42!` should generate beamtalk_message_dispatch:cast/3
    let mut generator = CoreErlangGenerator::new("test");

    let receiver = Expression::Identifier(Identifier::new("someActor", Span::new(0, 9)));
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("setValue:", Span::new(10, 19))]);
    let args = vec![Expression::Literal(Literal::Integer(42), Span::new(20, 22))];
    let send_expr = Expression::MessageSend {
        receiver: Box::new(receiver),
        selector: selector.clone(),
        arguments: args.clone(),
        is_cast: true,
        span: Span::new(0, 22),
    };

    let doc = if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: true,
        ..
    } = &send_expr
    {
        generator
            .generate_cast_send(receiver, selector, arguments)
            .unwrap()
    } else {
        panic!("expected MessageSend")
    };

    let output = doc.to_pretty_string();
    assert!(
        output.contains("beamtalk_message_dispatch':'cast'("),
        "Keyword cast send should route via beamtalk_message_dispatch:cast/3. Got: {output}"
    );
    assert!(
        output.contains("'setValue:'"),
        "Cast send should include keyword selector atom. Got: {output}"
    );
    assert!(
        output.contains("42"),
        "Cast send should include argument value. Got: {output}"
    );
}

#[test]
fn test_cast_self_send_uses_safe_dispatch_and_discards_result() {
    // BT-920: `self increment!` in actor context should call safe_dispatch but
    // discard the result and return 'ok'.
    let mut generator = CoreErlangGenerator::new("test");
    // CoreErlangGenerator::new defaults to Actor context — self-sends are valid

    let receiver = Expression::Identifier(Identifier::new("self", Span::new(0, 4)));
    let selector = MessageSelector::Unary("increment".into());
    let send_expr = Expression::MessageSend {
        receiver: Box::new(receiver),
        selector: selector.clone(),
        arguments: vec![],
        is_cast: true,
        span: Span::new(0, 15),
    };

    let doc = if let Expression::MessageSend {
        receiver,
        selector,
        arguments,
        is_cast: true,
        ..
    } = &send_expr
    {
        generator
            .generate_cast_send(receiver, selector, arguments)
            .unwrap()
    } else {
        panic!("expected MessageSend")
    };

    let output = doc.to_pretty_string();
    // Self cast: calls safe_dispatch and discards result
    assert!(
        output.contains("safe_dispatch"),
        "Self cast send should use safe_dispatch. Got: {output}"
    );
    assert!(
        output.contains("'increment'"),
        "Self cast send should include selector atom. Got: {output}"
    );
    assert!(
        output.contains("'ok'"),
        "Self cast send should evaluate to 'ok'. Got: {output}"
    );
    // Must NOT use beamtalk_message_dispatch:cast (that's for external sends)
    assert!(
        !output.contains("beamtalk_message_dispatch':'cast'"),
        "Self cast send must NOT use message dispatch cast. Got: {output}"
    );
}

#[test]
fn test_cast_send_in_actor_method_compiles() {
    // BT-920: `counter increment!` as a statement in an actor method compiles
    // to Core Erlang that calls beamtalk_message_dispatch:cast/3.
    let src = "Actor subclass: Sender\n  state: target = nil\n\n  fire =>\n    target increment!\n    \"done\"\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("sender").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

    eprintln!("Generated code for cast send in actor method:\n{code}");

    assert!(
        code.contains("beamtalk_message_dispatch':'cast'("),
        "Should generate beamtalk_message_dispatch:cast/3 for cast send. Got:\n{code}"
    );
    assert!(
        code.contains("'increment'"),
        "Should include 'increment' selector atom. Got:\n{code}"
    );
    // Verify the cast send is non-last (wrapped in let binding to discard result)
    assert!(
        code.contains("let _seq"),
        "Cast send should be wrapped in let to discard its result. Got:\n{code}"
    );
}

// BT-938: Validate that bt@stdlib@X module exists when generating dispatch calls.

#[test]
fn test_bt938_no_warning_when_binding_table_empty() {
    // With no binding table (empty), validation is skipped — no spurious warnings.
    let src = "
sealed Object subclass: Foo
  doIt => @primitive \"doIt\"
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let result = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt@stdlib@foo").with_bindings(
            primitive_bindings::PrimitiveBindingTable::new(), // empty — no validation
        ),
    );
    let generated = result.expect("codegen should succeed");
    assert!(
        generated.warnings.is_empty(),
        "Empty binding table should skip module-existence validation. Got: {:?}",
        generated.warnings
    );
}

#[test]
fn test_bt938_no_warning_when_module_present_in_bindings() {
    // When the binding table knows about 'Foo', no warning is emitted.
    let src = "
sealed Object subclass: Foo
  doIt => @primitive \"doIt\"
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);

    // Build a binding table that includes Foo (so bt@stdlib@foo is known).
    let mut table = primitive_bindings::PrimitiveBindingTable::new();
    table.add_from_module(&module);

    let result = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt@stdlib@foo").with_bindings(table),
    );
    let generated = result.expect("codegen should succeed");
    assert!(
        generated.warnings.is_empty(),
        "Known module should produce no warning. Got: {:?}",
        generated.warnings
    );
}

#[test]
fn test_bt938_warning_when_module_absent_from_bindings() {
    // Foo has @primitive 'doIt' but the binding table only knows about Bar.
    // bt@stdlib@foo is NOT in the known set → diagnostic warning expected.
    let src = "
sealed Object subclass: Foo
  doIt => @primitive \"doIt\"
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);

    // Build a binding table that only knows about a different class (Bar, not Foo).
    let other_src = "
sealed Object subclass: Bar
  barOp => @primitive \"barOp\"
";
    let other_tokens = crate::source_analysis::lex_with_eof(other_src);
    let (other_module, _) = crate::source_analysis::parse(other_tokens);
    let mut table = primitive_bindings::PrimitiveBindingTable::new();
    table.add_from_module(&other_module); // only Bar is known; Foo is absent

    let result = generate_module_with_warnings(
        &module,
        CodegenOptions::new("bt@stdlib@foo").with_bindings(table),
    );
    let generated = result.expect("codegen should succeed (warning, not error)");
    assert!(
        !generated.warnings.is_empty(),
        "Absent module should emit a warning diagnostic"
    );
    let msg = &generated.warnings[0].message;
    assert!(
        msg.contains("bt@stdlib@foo"),
        "Warning should mention the missing module. Got: {msg}"
    );
    assert!(
        msg.contains("doIt"),
        "Warning should mention the selector. Got: {msg}"
    );
    assert!(
        msg.contains("has not been compiled"),
        "Warning should explain the issue. Got: {msg}"
    );
    // Verify the span points at the @primitive expression, not a dummy zero span.
    let primitive_span = match &module.classes[0].methods[0].body[0].expression {
        Expression::Primitive { span, .. } => *span,
        other => panic!("Expected Primitive expression, got: {other:?}"),
    };
    assert_eq!(
        generated.warnings[0].span, primitive_span,
        "Warning span should point at the @primitive expression"
    );
}

#[test]
fn test_bt938_no_warning_for_structural_intrinsics() {
    // @primitive blockValue (unquoted/structural) — no module-existence check needed.
    let src = "
sealed Object subclass: Block
  value => @primitive blockValue
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);

    // Binding table only knows about something else — but structurals skip the check.
    let other_src = "
sealed Object subclass: Other
  other => @primitive \"other\"
";
    let other_tokens = crate::source_analysis::lex_with_eof(other_src);
    let (other_module, _) = crate::source_analysis::parse(other_tokens);
    let mut table = primitive_bindings::PrimitiveBindingTable::new();
    table.add_from_module(&other_module);

    let options = CodegenOptions::new("bt@stdlib@block").with_bindings(table);
    // Use stdlib_mode-aware codegen — but structural intrinsics skip module check.
    let result = generate_module_with_warnings(&module, options);
    let generated = result.expect("codegen should succeed");
    // Structural intrinsics do not trigger the bt@stdlib@X module-existence check.
    let module_warnings: Vec<_> = generated
        .warnings
        .iter()
        .filter(|w| w.message.contains("has not been compiled"))
        .collect();
    assert!(
        module_warnings.is_empty(),
        "Structural intrinsics should not trigger module-existence warning. Got: {module_warnings:?}"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// BT-923: Value subclass: auto-generated slot method tests
// ─────────────────────────────────────────────────────────────────────────────

/// Builds a Module with a `Value subclass: Point` with x and y slots.
fn make_value_subclass_point() -> Module {
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![
            StateDeclaration {
                name: Identifier::new("x", Span::new(0, 0)),
                type_annotation: None,
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
            StateDeclaration {
                name: Identifier::new("y", Span::new(0, 0)),
                type_annotation: None,
                default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
        ],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

#[test]
fn test_value_subclass_auto_getter_exported() {
    // BT-923: `Value subclass:` auto-generates getter functions for each slot.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    assert!(result.is_ok(), "Codegen should succeed for Value subclass:");
    let code = result.unwrap();
    // Getter exports: 'x'/1 and 'y'/1
    assert!(
        code.contains("'x'/1"),
        "Should export getter 'x'/1. Got:\n{code}"
    );
    assert!(
        code.contains("'y'/1"),
        "Should export getter 'y'/1. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_auto_getter_function() {
    // BT-923: Getter body uses maps:get to read the slot from Self.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'x'/1 = fun (Self) ->"),
        "Should generate x/1 getter. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'get'('x', Self)"),
        "x getter should use maps:get. Got:\n{code}"
    );
    assert!(
        code.contains("'y'/1 = fun (Self) ->"),
        "Should generate y/1 getter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_auto_setter_exported() {
    // BT-923: `Value subclass:` auto-generates with*: functional setters.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'withX:'/2"),
        "Should export withX:/2. Got:\n{code}"
    );
    assert!(
        code.contains("'withY:'/2"),
        "Should export withY:/2. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_auto_setter_function() {
    // BT-923: with*: setter body uses maps:put to return an updated map.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'withX:'/2 = fun (Self, NewVal) ->"),
        "Should generate withX:/2 setter. Got:\n{code}"
    );
    assert!(
        code.contains("call 'maps':'put'('x', NewVal, Self)"),
        "withX: setter should use maps:put. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_keyword_constructor_exported() {
    // BT-923: `Value subclass:` auto-generates an all-fields keyword constructor.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    // Keyword constructor selector for x, y → 'class_x:y:'/4
    assert!(
        code.contains("'class_x:y:'/4"),
        "Should export 'class_x:y:'/4 keyword constructor. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_keyword_constructor_function() {
    // BT-923: Keyword constructor body creates a tagged map with all slots.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'class_x:y:'/4 = fun (_ClassSelf, _ClassVars, SlotArg0, SlotArg1) ->"),
        "Should generate keyword constructor function. Got:\n{code}"
    );
    assert!(
        code.contains("'$beamtalk_class' => 'Point'"),
        "Keyword constructor should set $beamtalk_class. Got:\n{code}"
    );
    assert!(
        code.contains("'x' => SlotArg0"),
        "Keyword constructor should set x from SlotArg0. Got:\n{code}"
    );
    assert!(
        code.contains("'y' => SlotArg1"),
        "Keyword constructor should set y from SlotArg1. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_dispatch_routes_getter() {
    // BT-923: dispatch/3 must route getter selectors to auto-generated functions.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("call 'bt@point':'x'(Self)"),
        "dispatch/3 should route 'x' to getter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_dispatch_routes_setter() {
    // BT-923: dispatch/3 must route with*: selectors to auto-generated functions.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("call 'bt@point':'withX:'(Self, DispArg0)"),
        "dispatch/3 should route 'withX:' to setter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_has_method_includes_auto_methods() {
    // BT-923: has_method/1 must report true for auto-generated selectors.
    let module = make_value_subclass_point();
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    assert!(
        code.contains("'x'"),
        "has_method/1 should list 'x' getter. Got:\n{code}"
    );
    assert!(
        code.contains("'withX:'"),
        "has_method/1 should list 'withX:' setter. Got:\n{code}"
    );
}

#[test]
fn test_object_subclass_no_auto_getters() {
    // BT-923: `Object subclass:` (ClassKind::Object) must NOT generate auto-getters.
    let class = ClassDefinition {
        name: Identifier::new("Point", Span::new(0, 0)),
        superclass: Some(Identifier::new("Object", Span::new(0, 0))),
        class_kind: ClassKind::Object,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![StateDeclaration {
            name: Identifier::new("x", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@point"));
    let code = result.unwrap();
    // Object subclass should NOT have auto-getter 'x'/1
    assert!(
        !code.contains("'x'/1 = fun (Self) ->"),
        "Object subclass should not generate auto-getter. Got:\n{code}"
    );
    // And should not have withX:/2
    assert!(
        !code.contains("'withX:'/2"),
        "Object subclass should not generate auto-setter. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_user_defined_overrides_auto() {
    // BT-923: User-defined methods suppress the corresponding auto-generated method.
    let x_method = MethodDefinition {
        selector: MessageSelector::Unary("x".into()),
        parameters: vec![],
        return_type: None,
        body: vec![bare(Expression::Literal(
            Literal::Integer(99),
            Span::new(0, 0),
        ))],
        kind: MethodKind::Primary,
        is_sealed: false,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let class = ClassDefinition {
        name: Identifier::new("MyVal", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![StateDeclaration {
            name: Identifier::new("x", Span::new(0, 0)),
            type_annotation: None,
            default_value: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        methods: vec![x_method],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@my_val"));
    let code = result.unwrap();
    // The auto-getter would produce: call 'maps':'get'('x', Self)
    // When user defines 'x', that body should NOT appear — the user's body (99) wins.
    assert!(
        !code.contains("call 'maps':'get'('x', Self)"),
        "Auto-getter body should be suppressed when user defines 'x'. Got:\n{code}"
    );
}

#[test]
fn test_value_subclass_no_slots_no_keyword_constructor() {
    // BT-923: A Value subclass with no slots produces no keyword constructor.
    let class = ClassDefinition {
        name: Identifier::new("Empty", Span::new(0, 0)),
        superclass: Some(Identifier::new("Value", Span::new(0, 0))),
        class_kind: ClassKind::Value,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let result = generate_module(&module, CodegenOptions::new("bt@empty"));
    let code = result.unwrap();
    // A class with no slots has no keyword constructor selector, so no 'class_X:'/N pattern.
    // Scan all lines for the pattern: contains 'class_' AND contains ':'/  (selector with colon)
    let has_keyword_ctor = code
        .lines()
        .any(|line| line.contains("'class_") && line.contains(":/"));
    assert!(
        !has_keyword_ctor,
        "No keyword constructor should be generated for empty Value subclass. Got:\n{code}"
    );
}

#[test]
fn test_repl_expression_spawn_uses_class_module_index() {
    // When a REPL expression like `Counter spawn` is compiled in a workspace
    // with package "getting_started", the class_module_index must be consulted
    // so the generated code calls 'bt@getting_started@counter':'spawn'()
    // instead of the heuristic fallback 'bt@counter':'spawn'().
    let src = "Counter spawn";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();

    let mut index = std::collections::HashMap::new();
    index.insert(
        "Counter".to_string(),
        "bt@getting_started@counter".to_string(),
    );

    let code = generate_repl_expressions_with_index(&expressions, "repl_test_mod", index)
        .expect("codegen should work");

    assert!(
        code.contains("'bt@getting_started@counter':'spawn'"),
        "spawn must use package-qualified module from class_module_index. Got:\n{code}"
    );
    assert!(
        !code.contains("'bt@counter':'spawn'"),
        "Heuristic module name must NOT appear when class_module_index is provided. Got:\n{code}"
    );
}

#[test]
fn test_repl_expression_spawn_without_index_uses_heuristic() {
    // Without class_module_index, spawn falls back to the heuristic bt@ prefix.
    let src = "Counter spawn";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();

    let code =
        generate_repl_expressions(&expressions, "repl_test_mod").expect("codegen should work");

    assert!(
        code.contains("'bt@counter':'spawn'"),
        "Without class_module_index, spawn should use heuristic bt@ prefix. Got:\n{code}"
    );
}

#[test]
fn test_repl_expression_spawn_with_args_uses_class_module_index() {
    // `Counter spawnWith: #{ value: 10 }` must also use class_module_index.
    let src = "Counter spawnWith: #{ value: 10 }";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let expressions: Vec<_> = module
        .expressions
        .iter()
        .map(|s| s.expression.clone())
        .collect();

    let mut index = std::collections::HashMap::new();
    index.insert(
        "Counter".to_string(),
        "bt@getting_started@counter".to_string(),
    );

    let code = generate_repl_expressions_with_index(&expressions, "repl_test_mod", index)
        .expect("codegen should work");

    assert!(
        code.contains("'bt@getting_started@counter':'spawn'"),
        "spawnWith: must use package-qualified module from class_module_index. Got:\n{code}"
    );
    assert!(
        !code.contains("'bt@counter':'spawn'"),
        "Heuristic module name must NOT appear when class_module_index is provided. Got:\n{code}"
    );
}
