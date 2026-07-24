// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for message dispatch Core Erlang code generation.
//!
//! Covers unary/keyword message sends, futures and await, actor spawn,
//! cascade messages, Erlang interop, cross-file dispatch, block value
//! messages, cast sends, and module-existence warning diagnostics.

use super::*;

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
        package: None,
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
        package: None,
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
        type_annotation: None,
        span: Span::new(0, 10),
    };

    let module = Module::new(vec![bare(value_assignment)], Span::new(0, 10));
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // Check that spawn/0 and spawn/1 are exported
    assert!(code.contains("'spawn'/0"));
    assert!(code.contains("'spawn'/1"));

    // Check that spawn/0 function exists and calls safe_spawn with empty map
    assert!(code.contains("'spawn'/0 = fun () ->"));

    // Check that spawn/1 function exists and calls gen_server:start_link with InitArgs
    assert!(code.contains("'spawn'/1 = fun (InitArgs) ->"));
    // BT-1541: spawn/1 also uses safe_spawn
    assert!(code.contains("call 'beamtalk_actor':'safe_spawn'('counter', InitArgs)"));

    // BT-1541: spawn uses safe_spawn for trap_exit + initialize sync
    assert!(
        code.contains("call 'beamtalk_actor':'safe_spawn'('counter', ~{}~)"),
        "spawn/0 must use safe_spawn. Got: {code}"
    );
    assert!(code.contains("<{'ok', Pid}> when 'true' ->"));

    // Check that it returns a #beamtalk_object{} record (class='Counter', class_mod='counter', pid=Pid)
    assert!(
        code.contains("{'beamtalk_object', 'Counter', 'counter', Pid}"),
        "spawn functions should return #beamtalk_object{{}} record. Got: {code}"
    );

    // Check that it handles errors
    assert!(code.contains("<{'error', Reason}> when 'true' ->"));
    // BT-1541: Error now includes hint with actual Reason
    assert!(code.contains("call 'beamtalk_error':'with_hint'(SpawnErr1, Reason)"));
    assert!(code.contains("call 'beamtalk_error':'raise'(SpawnErr2)"));

    // class_name/0 replaces $beamtalk_class in init state: correct after hot-reload
    assert!(
        code.contains("'class_name'/0 = fun () -> 'Counter'"),
        "class_name/0 must return the class atom. Got:\n{code}"
    );
    // Check that init/1 creates the default state with fields and merges with InitArgs
    assert!(code.contains("'init'/1 = fun (InitArgs) ->"));
    assert!(code.contains("let DefaultState = ~{"));
    assert!(
        !code.contains("'$beamtalk_class' => 'Counter'"),
        "$beamtalk_class must not appear in compiled actor init state. Got:\n{code}"
    );
    assert!(code.contains("'__class_mod__' => 'counter'"));
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
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new("listeners", Span::new(0, 9)),
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 1))),
            type_annotation: None,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: Span::new(0, 10),
        }],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 50),
    };

    let module = Module {
        classes: vec![class],
        type_aliases: Vec::new(),
        expressions: vec![],
        method_definitions: vec![],
        protocols: Vec::new(),
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

    // BT-1541: safe_spawn must use the full name
    assert!(
        code.contains(&format!(
            "call 'beamtalk_actor':'safe_spawn'('{full_name}', ~{{}}~)"
        )),
        "safe_spawn in spawn/0 should use full module name, not '{simplified_name}'. Got:\n{code}"
    );

    // safe_spawn in spawn/1 must also use full name
    assert!(
        code.contains(&format!(
            "call 'beamtalk_actor':'safe_spawn'('{full_name}', InitArgs)"
        )),
        "safe_spawn in spawn/1 should use full module name. Got:\n{code}"
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
    {
        let mut index = std::collections::HashMap::new();
        index.insert(
            "EventBus".to_string(),
            "bt@gang_of_four@observer@event_bus".to_string(),
        );
        generator.set_class_module_index(index);
    }

    let receiver = Expression::ClassReference {
        name: Identifier::new("EventBus", Span::new(0, 8)),
        span: Span::new(0, 8),
        package: None,
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
    // Binary ops no longer wrap operands with maybe_await (ADR-0043: all sends are sync)
    assert!(
        !output.contains("maybe_await"),
        "value: binary ops should not wrap operands with maybe_await. Got: {output}"
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
fn test_value_keyword_erlang_ffi_receiver_routes_to_erlang_interop() {
    // BT-1260: `(Erlang maps) value: key` must route through Erlang interop, not block apply.
    // Before the fix, this emitted `apply Fun(Key)` which crashes at runtime.
    let mut generator = CoreErlangGenerator::new("test");

    // Build: (Erlang maps) value: key
    let erlang_proxy = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
            package: None,
        }),
        selector: MessageSelector::Unary("maps".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 11),
    };
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(12, 18))]);
    let arguments = vec![Expression::Identifier(Identifier::new(
        "key",
        Span::new(19, 22),
    ))];

    let doc = generator
        .generate_message_send(&erlang_proxy, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("beamtalk_erlang_proxy"),
        "Should route through Erlang interop. Got: {output}"
    );
    assert!(
        !output.contains("is_function"),
        "Should not emit is_function guard for FFI receiver. Got: {output}"
    );
    assert!(
        !output.contains("apply"),
        "Should not emit block apply for FFI receiver. Got: {output}"
    );
}

#[test]
fn test_value_keyword_unknown_receiver_emits_is_function_guard() {
    // BT-1260: `someVar value: arg` where receiver is unknown emits a runtime
    // is_function guard: if it's a function, apply it; otherwise dispatch via send.
    let mut generator = CoreErlangGenerator::new("test");

    let receiver = Expression::Identifier(Identifier::new("someVar", Span::new(0, 7)));
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(8, 14))]);
    let arguments = vec![Expression::Literal(Literal::Integer(42), Span::new(15, 17))];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("is_function"),
        "Should emit erlang:is_function guard. Got: {output}"
    );
    assert!(
        output.contains("apply"),
        "Should emit apply for the function path. Got: {output}"
    );
    assert!(
        output.contains("beamtalk_primitive':'send'"),
        "Should emit beamtalk_primitive:send fallback. Got: {output}"
    );
    assert!(
        output.contains("'value:'"),
        "Should pass 'value:' selector to send. Got: {output}"
    );
}

#[test]
fn test_value_keyword_block_literal_receiver_still_uses_fast_apply() {
    // BT-1260: Block literal receivers must still use the fast inline apply path (no regression).
    // [:x | x + 1] value: 5 → let _Fun = ... in apply _Fun (5)
    let mut generator = CoreErlangGenerator::new("test");

    let block = Block::new(
        vec![BlockParameter::new("x", Span::new(1, 2))],
        vec![bare(Expression::Literal(
            Literal::Integer(0),
            Span::new(5, 6),
        ))],
        Span::new(0, 8),
    );
    let receiver = Expression::Block(block);
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(9, 15))]);
    let arguments = vec![Expression::Literal(Literal::Integer(5), Span::new(16, 17))];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("apply"),
        "Block literal should use apply. Got: {output}"
    );
    assert!(
        !output.contains("is_function"),
        "Block literal should not emit is_function guard. Got: {output}"
    );
    assert!(
        !output.contains("beamtalk_primitive':'send'"),
        "Block literal should not use send fallback. Got: {output}"
    );
}

#[test]
fn test_value_value_keyword_erlang_ffi_receiver_routes_to_erlang_interop() {
    // BT-1260: `(Erlang maps) value: key value: default` must route through Erlang
    // interop, not block apply, for value:value: selector.
    let mut generator = CoreErlangGenerator::new("test");

    let erlang_proxy = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
            package: None,
        }),
        selector: MessageSelector::Unary("maps".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 11),
    };
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("value:", Span::new(12, 18)),
        KeywordPart::new("value:", Span::new(22, 28)),
    ]);
    let arguments = vec![
        Expression::Identifier(Identifier::new("key", Span::new(19, 22))),
        Expression::Identifier(Identifier::new("def", Span::new(29, 32))),
    ];

    let doc = generator
        .generate_message_send(&erlang_proxy, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("beamtalk_erlang_proxy"),
        "value:value: FFI receiver should route through Erlang interop. Got: {output}"
    );
    assert!(
        !output.contains("is_function"),
        "Should not emit is_function guard for FFI receiver. Got: {output}"
    );
    assert!(
        !output.contains("apply"),
        "Should not emit block apply for FFI receiver. Got: {output}"
    );
}

#[test]
fn test_value_value_value_keyword_unknown_receiver_emits_correct_selector() {
    // BT-1260: `someVar value: a value: b value: c` emits the full 'value:value:value:'
    // selector atom in the beamtalk_primitive:send fallback.
    let mut generator = CoreErlangGenerator::new("test");

    let receiver = Expression::Identifier(Identifier::new("someVar", Span::new(0, 7)));
    let selector = MessageSelector::Keyword(vec![
        KeywordPart::new("value:", Span::new(8, 14)),
        KeywordPart::new("value:", Span::new(18, 24)),
        KeywordPart::new("value:", Span::new(28, 34)),
    ]);
    let arguments = vec![
        Expression::Literal(Literal::Integer(1), Span::new(15, 16)),
        Expression::Literal(Literal::Integer(2), Span::new(25, 26)),
        Expression::Literal(Literal::Integer(3), Span::new(35, 36)),
    ];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("is_function"),
        "Should emit is_function guard. Got: {output}"
    );
    assert!(
        output.contains("'value:value:value:'"),
        "Fallback send must use the full 'value:value:value:' selector atom. Got: {output}"
    );
    assert!(
        output.contains("apply"),
        "Should emit apply for the function path. Got: {output}"
    );
}

#[test]
fn test_value_keyword_class_protocol_receiver_uses_is_function_guard() {
    // BT-1260: `(Erlang class) value: x` — class-protocol selectors must NOT be
    // treated as FFI module proxies; they fall through to the is_function guard.
    let mut generator = CoreErlangGenerator::new("test");

    let erlang_class = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
            package: None,
        }),
        selector: MessageSelector::Unary("class".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(0, 12),
    };
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(13, 19))]);
    let arguments = vec![Expression::Literal(Literal::Integer(1), Span::new(20, 21))];

    let doc = generator
        .generate_message_send(&erlang_class, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    // (Erlang class) is a metaclass, not a module proxy — must use the is_function guard,
    // NOT fall through to Erlang interop.
    assert!(
        output.contains("is_function"),
        "Class-protocol receiver should use is_function guard. Got: {output}"
    );
    assert!(
        !output.contains("beamtalk_erlang_proxy"),
        "Class-protocol receiver should not emit Erlang interop call. Got: {output}"
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

    // Should bind the underlying receiver once. BT-2365 (ADR 0081 Phase 1): a
    // free REPL identifier now resolves via a locals maps:find with a runtime
    // resolve_name fallthrough rather than a bare maps:get.
    assert!(
        code.contains("let _Receiver1 = case call 'maps':'find'('x', State) of"),
        "Should bind receiver x via locals find. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_name'(State, 'x')"),
        "Should fall through to resolve_name for free identifier x. Got:\n{code}"
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
        type_annotation: None,
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
        type_annotation: None,
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
        type_annotation: None,
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
fn test_standalone_class_reference_uses_dynamic_module_name() {
    // BT-215: Test that standalone ClassReference uses module_name/1 dynamically
    // Review comment: Should match generate_beamtalk_class_named pattern (lines 915-922)
    use crate::ast::{Expression, Identifier, Module};
    use crate::source_analysis::Span;

    // Create expression: Point (standalone class reference)
    let expr = Expression::ClassReference {
        name: Identifier::new("Point", Span::new(0, 5)),
        span: Span::new(0, 5),
        package: None,
    };

    let module = Module {
        type_aliases: Vec::new(),
        expressions: vec![bare(expr)],
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 5),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_repl_expression(&module.expressions[0].expression, "repl_eval")
        .expect("codegen should succeed");

    // BT-2365 (ADR 0081 Phase 1): an unqualified REPL class reference checks the
    // session locals map first (so a local shadows the class), then delegates to
    // the shared runtime resolver. The class object construction and dynamic
    // module_name lookup now live in beamtalk_workspace:resolve_class_reference/2.

    // Should check the session locals map first (shadowing support).
    assert!(
        code.contains("call 'maps':'find'('Point', "),
        "Should check locals map for the class name first. Got:\n{code}"
    );

    // Should delegate the miss path to the shared runtime resolver.
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_class_reference'("),
        "Should delegate to resolve_class_reference on a locals miss. Got:\n{code}"
    );

    // Should pass the class name as an atom to the resolver.
    assert!(
        code.contains("'resolve_class_reference'(") && code.contains("'Point')"),
        "Should pass the class name atom to the resolver. Got:\n{code}"
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
        package: None,
    };

    let module = Module {
        type_aliases: Vec::new(),
        expressions: vec![bare(expr)],
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 16),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_repl_expression(&module.expressions[0].expression, "repl_eval")
        .expect("codegen should succeed");

    // BT-2365 (ADR 0081 Phase 1): undefined-class validation now happens in the
    // runtime resolver (beamtalk_workspace:resolve_class_reference/2), which
    // raises the same class_not_found error. The REPL codegen emits a locals
    // check then delegates to that resolver.

    // Should check the session locals map first (shadowing support).
    assert!(
        code.contains("call 'maps':'find'('NonExistentClass', "),
        "Should check locals map for the class name first. Got:\n{code}"
    );

    // Should delegate to the shared resolver, which raises class_not_found for
    // a genuinely unknown class.
    assert!(
        code.contains("call 'beamtalk_workspace':'resolve_class_reference'(")
            && code.contains("'NonExistentClass')"),
        "Should delegate undefined-class handling to resolve_class_reference. Got:\n{code}"
    );
}

#[test]
fn test_erlang_interop_direct_call_keyword_single_arg() {
    // `Erlang lists reverse: xs` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'reverse', [Xs])`
    // BT-1127: Routes through proxy for binary→charlist coercion support
    let mut generator = CoreErlangGenerator::new("test");

    // Inner: Erlang lists (ClassReference("Erlang") + Unary("lists"))
    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
            package: None,
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
        output.contains("call 'beamtalk_erlang_proxy':'direct_call'("),
        "Should emit proxy-routed call. Got: {output}"
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
    // `Erlang lists seq: 1 with: 10` → `call 'beamtalk_erlang_proxy':'direct_call'('lists', 'seq', [1, 10])`
    // BT-1127: Routes through proxy for binary→charlist coercion support
    let mut generator = CoreErlangGenerator::new("test");

    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
            package: None,
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
        output.contains("call 'beamtalk_erlang_proxy':'direct_call'("),
        "Should emit proxy-routed multi-arg call. Got: {output}"
    );
    assert!(
        !output.contains("ErlangModule"),
        "Should not create proxy map. Got: {output}"
    );
}

#[test]
fn test_bt2685_parenthesized_erlang_ffi_error_routes_to_proxy() {
    // BT-2685: `(Erlang beamtalk_console) error: msg` is an Erlang FFI call to a
    // function named `error`, NOT the `Object >> error:` error-signaling intrinsic.
    // The receiver is wrapped in `Parenthesized` (the canonical `(Erlang mod)` form),
    // which the FFI-receiver check must peel through. Regression for the bug where
    // `Console error:` raised a `user_error` instead of writing to stderr.
    let mut generator = CoreErlangGenerator::new("test");

    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(1, 7)),
            span: Span::new(1, 7),
            package: None,
        }),
        selector: MessageSelector::Unary("beamtalk_console".into()),
        arguments: vec![],
        is_cast: false,
        span: Span::new(1, 24),
    };
    // The parser wraps `(Erlang beamtalk_console)` in a Parenthesized node.
    let receiver = Expression::Parenthesized {
        expression: Box::new(inner_receiver),
        span: Span::new(0, 25),
    };
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("error:", Span::new(26, 32))]);
    let arguments = vec![Expression::Literal(
        Literal::String("boom".into()),
        Span::new(33, 39),
    )];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    // The regression: error-signaling (`Object >> error:`) must NOT fire. It
    // routes to the Erlang FFI proxy instead (the parenthesized receiver compiles
    // to an `ErlangModule` proxy map dispatched at runtime to `beamtalk_console:error`).
    assert!(
        !output.contains("user_error") && !output.contains("with_message"),
        "Must NOT generate the error-signaling intrinsic. Got: {output}"
    );
    assert!(
        output.contains("ErlangModule") || output.contains("beamtalk_erlang_proxy"),
        "Should route to the Erlang FFI proxy. Got: {output}"
    );
}

#[test]
fn test_erlang_interop_direct_call_zero_arg() {
    // `Erlang erlang node` → `call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [])`
    // BT-1127: Routes through proxy for consistent validation/coercion
    let mut generator = CoreErlangGenerator::new("test");

    let inner_receiver = Expression::MessageSend {
        receiver: Box::new(Expression::ClassReference {
            name: Identifier::new("Erlang", Span::new(0, 6)),
            span: Span::new(0, 6),
            package: None,
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

    assert!(
        output.contains("call 'beamtalk_erlang_proxy':'direct_call'('erlang', 'node', [])"),
        "Should emit proxy-routed zero-arg call. Got: {output}"
    );
}

#[test]
fn test_erlang_interop_proxy_still_works_for_standalone() {
    // `Erlang lists` (standalone, no chained call) → proxy map (unchanged)
    let mut generator = CoreErlangGenerator::new("test");

    let receiver = Expression::ClassReference {
        name: Identifier::new("Erlang", Span::new(0, 6)),
        span: Span::new(0, 6),
        package: None,
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
            package: None,
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
            package: None,
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
        output.contains("call 'beamtalk_erlang_proxy':'direct_call'('lists', 'map',"),
        "Should emit proxy-routed Erlang call. Got: {output}"
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
            package: None,
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
            type_annotation: None,
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
        output.contains("call 'beamtalk_erlang_proxy':'direct_call'('lists', 'map',"),
        "Should emit proxy-routed Erlang call. Got: {output}"
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
            type_annotation: None,
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
    // Foo has @primitive "doIt" but the binding table only knows about Bar.
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

#[test]
fn test_bt2233_unmapped_quoted_primitive_errors_in_stdlib_mode() {
    // BT-2233: In stdlib mode, a quoted @primitive in a value-type class with no
    // inline BIF lowering is a hard error (naming class + selector) instead of a
    // silent runtime-dispatch fallback that DNUs at runtime (the BT-2232 bug).
    let src = "
sealed Object subclass: Foo
  doIt => @primitive \"doIt\"
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);

    let options = CodegenOptions::new("bt@stdlib@foo").with_stdlib_mode(true);
    let result = generate_module_with_warnings(&module, options);
    let err = result.expect_err("unmapped quoted @primitive must fail the stdlib build");
    let msg = err.to_string();
    assert!(
        msg.contains("doIt"),
        "error should name the selector: {msg}"
    );
    assert!(msg.contains("Foo"), "error should name the class: {msg}");
}

#[test]
fn test_bt2233_unmapped_quoted_primitive_warns_outside_stdlib_mode() {
    // BT-2233: Outside stdlib mode (e.g. user FFI @primitive via
    // --allow-primitives) the BT-938 warn-and-fallback path is preserved — the
    // strict failure is scoped to the stdlib build it protects. The binding
    // table knows a different class, so `bt@stdlib@foo` is absent and BT-938
    // emits a warning (not a hard error).
    let src = "
sealed Object subclass: Foo
  doIt => @primitive \"doIt\"
";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);

    let other_src = "
sealed Object subclass: Bar
  barOp => @primitive \"barOp\"
";
    let other_tokens = crate::source_analysis::lex_with_eof(other_src);
    let (other_module, _) = crate::source_analysis::parse(other_tokens);
    let mut table = primitive_bindings::PrimitiveBindingTable::new();
    table.add_from_module(&other_module); // only Bar is known; Foo is absent

    let options = CodegenOptions::new("bt@stdlib@foo").with_bindings(table);
    let result = generate_module_with_warnings(&module, options);
    let generated = result
        .expect("non-stdlib quoted @primitive must not hard-error (BT-938 warn-and-fallback)");
    assert!(
        generated
            .warnings
            .iter()
            .any(|w| w.message.contains("bt@stdlib@foo")),
        "expected a BT-938 warning about the absent module, got: {:?}",
        generated.warnings
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

// --- BT-1321: intrinsic async_send → sync_send migration ---

use super::codegen as codegen_source;

/// Returns `true` if the generated code contains a `beamtalk_actor:sync_send` call
/// with the given selector atom (e.g. `"'fieldNames'"`) as the second argument.
/// Used to assert selector-specific dispatch rather than module-wide presence.
fn has_sync_send_for_selector(code: &str, selector: &str) -> bool {
    code.match_indices("'beamtalk_actor':'sync_send'(")
        .any(|(idx, _)| {
            let end = (idx + 200).min(code.len());
            code[idx..end].contains(selector)
        })
}

#[test]
fn test_field_names_actor_uses_sync_send() {
    // BT-1321: fieldNames on an actor receiver must use sync_send, not async_send + future.
    // Assertion is selector-specific: checks sync_send includes the 'fieldNames' atom.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: x = 0\n\n",
        "  run: other =>\n",
        "    other fieldNames\n",
    );
    let code = codegen_source(src);
    assert!(
        has_sync_send_for_selector(&code, "'fieldNames'"),
        "fieldNames on actor must call sync_send with 'fieldNames' selector. Got:\n{code}"
    );
    assert!(
        !code.contains("'async_send'"),
        "fieldNames must not use async_send. Got:\n{code}"
    );
    assert!(
        !code.contains("'beamtalk_future':'new'"),
        "fieldNames must not allocate a future. Got:\n{code}"
    );
}

#[test]
fn test_field_at_actor_uses_sync_send() {
    // BT-1321: fieldAt: on an actor receiver must use sync_send.
    // Assertion is selector-specific: checks sync_send includes the 'fieldAt:' atom.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: x = 0\n\n",
        "  run: other with: name =>\n",
        "    other fieldAt: name\n",
    );
    let code = codegen_source(src);
    assert!(
        has_sync_send_for_selector(&code, "'fieldAt:'"),
        "fieldAt: on actor must call sync_send with 'fieldAt:' selector. Got:\n{code}"
    );
    assert!(
        !code.contains("'async_send'"),
        "fieldAt: must not use async_send. Got:\n{code}"
    );
    assert!(
        !code.contains("'beamtalk_future':'new'"),
        "fieldAt: must not allocate a future. Got:\n{code}"
    );
}

#[test]
fn test_field_at_put_actor_uses_sync_send() {
    // BT-1321: fieldAt:put: on an actor receiver must use sync_send.
    // Assertion is selector-specific: checks sync_send includes the 'fieldAt:put:' atom.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: x = 0\n\n",
        "  run: other with: name with: val =>\n",
        "    other fieldAt: name put: val\n",
    );
    let code = codegen_source(src);
    assert!(
        has_sync_send_for_selector(&code, "'fieldAt:put:'"),
        "fieldAt:put: on actor must call sync_send with 'fieldAt:put:' selector. Got:\n{code}"
    );
    assert!(
        !code.contains("'async_send'"),
        "fieldAt:put: must not use async_send. Got:\n{code}"
    );
    assert!(
        !code.contains("'beamtalk_future':'new'"),
        "fieldAt:put: must not allocate a future. Got:\n{code}"
    );
}

#[test]
fn test_field_names_self_no_sync_send() {
    // BT-1321: `self fieldNames` inside an actor must NOT route through sync_send.
    // Self is a #beamtalk_object{..., pid: self()} tuple; sync_send(self()) would be
    // gen_server:call(self(), ...) → deadlock. Must use beamtalk_primitive:send(State).
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: x = 0\n\n",
        "  getFieldNames =>\n",
        "    self fieldNames\n",
    );
    let code = codegen_source(src);
    assert!(
        !has_sync_send_for_selector(&code, "'fieldNames'"),
        "self fieldNames must not route through sync_send (deadlock risk). Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_primitive':'send'"),
        "self fieldNames must route through beamtalk_primitive:send. Got:\n{code}"
    );
}

#[test]
fn test_field_at_self_no_sync_send() {
    // BT-1321: `self fieldAt: name` inside an actor must NOT route through sync_send.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: x = 0\n\n",
        "  getField: name =>\n",
        "    self fieldAt: name\n",
    );
    let code = codegen_source(src);
    assert!(
        !has_sync_send_for_selector(&code, "'fieldAt:'"),
        "self fieldAt: must not route through sync_send (deadlock risk). Got:\n{code}"
    );
    assert!(
        code.contains("'beamtalk_primitive':'send'"),
        "self fieldAt: must route through beamtalk_primitive:send. Got:\n{code}"
    );
}

#[test]
fn test_field_at_put_self_no_sync_send() {
    // BT-1321: `self fieldAt: name put: val` inside an actor must NOT route through sync_send.
    // BT-1324: Must thread state update via maps:put so subsequent reads see the new value.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: x = 0\n\n",
        "  setField: name to: val =>\n",
        "    self fieldAt: name put: val\n",
    );
    let code = codegen_source(src);
    assert!(
        !has_sync_send_for_selector(&code, "'fieldAt:put:'"),
        "self fieldAt:put: must not route through sync_send (deadlock risk). Got:\n{code}"
    );
    assert!(
        code.contains("'maps':'put'"),
        "self fieldAt:put: must thread state via maps:put (BT-1324). Got:\n{code}"
    );
}

#[test]
fn test_field_at_put_self_threads_state() {
    // BT-1324: After `self fieldAt: #x put: val`, subsequent `self fieldAt: #x`
    // must read from the updated state, not the stale snapshot.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: x = 0\n\n",
        "  mutateAndRead =>\n",
        "    self fieldAt: #x put: 42.\n",
        "    self fieldAt: #x\n",
    );
    let code = codegen_source(src);
    // The maps:put must produce a new State variable (State1)
    // and the subsequent fieldAt: read must use State1, not State0.
    assert!(
        code.contains("'maps':'put'"),
        "self fieldAt:put: must use maps:put for state threading. Got:\n{code}"
    );
    // Verify the fieldAt: read uses the threaded State1 (not stale State)
    assert!(
        code.contains("'beamtalk_primitive':'send'(State1, 'fieldAt:'"),
        "self fieldAt: read must reference State1 after fieldAt:put: threading. Got:\n{code}"
    );
}

// --- BT-1270: State-mutation hoisting in block value: apply paths ---

#[test]
fn test_block_value_keyword_field_assignment_arg_hoisted() {
    // [:x | x] value: (self.y := 5)
    //
    // The field-assignment StateN binding must be hoisted before the
    // `let _Fun = ... in apply` wrapper, not nested inside it.
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();

    let field_assignment = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "self",
                Span::new(0, 4),
            ))),
            field: Identifier::new("y", Span::new(5, 6)),
            span: Span::new(0, 6),
        }),
        value: Box::new(Expression::Literal(Literal::Integer(5), Span::new(10, 11))),
        type_annotation: None,
        span: Span::new(0, 11),
    };

    let block_param = BlockParameter {
        name: "x".into(),
        span: Span::new(0, 1),
    };
    let block = Expression::Block(Block::new(
        vec![block_param],
        vec![bare(Expression::Identifier(Identifier::new(
            "x",
            Span::new(3, 4),
        )))],
        Span::new(0, 5),
    ));

    let send = Expression::MessageSend {
        receiver: Box::new(block),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(0, 6))]),
        arguments: vec![field_assignment],
        is_cast: false,
        span: Span::new(0, 20),
    };

    let doc = generator.generate_expression(&send).unwrap();
    let output = doc.to_pretty_string();

    // State binding must appear before the apply, not nested inside it.
    // _Fun1 = fun_var, _x2 = block param (from receiver codegen), _Val3 = hoisted val.
    // Receiver is evaluated first, then args are hoisted.
    assert!(
        output.contains("let _Val3 = 5 in let State1 = call 'maps':'put'('y', _Val3, State) in"),
        "State binding should be hoisted before apply. Got:\n{output}"
    );
    // The apply argument must be the val var, not the full assignment expression.
    assert!(
        output.contains("apply _Fun1 (_Val3)"),
        "Apply argument should be val var _Val3. Got:\n{output}"
    );
    // Receiver must be evaluated before arguments: Fun binding precedes State binding.
    assert!(
        output.contains("let _Fun1 = ") && output.find("let _Fun1 = ") < output.find("State1"),
        "Receiver (_Fun1) must be bound before State1 mutation. Got:\n{output}"
    );

    generator.pop_scope();
}

#[test]
fn test_value_keyword_guard_field_assignment_arg_hoisted() {
    // someVar value: (self.y := 5)   (unknown receiver — triggers is_function guard)
    //
    // The StateN binding must be outside the `let _ValArgN = ...` binding so it
    // remains in scope after the case expression.
    let mut generator = CoreErlangGenerator::new("test");
    generator.push_scope();

    let field_assignment = Expression::Assignment {
        target: Box::new(Expression::FieldAccess {
            receiver: Box::new(Expression::Identifier(Identifier::new(
                "self",
                Span::new(0, 4),
            ))),
            field: Identifier::new("y", Span::new(5, 6)),
            span: Span::new(0, 6),
        }),
        value: Box::new(Expression::Literal(Literal::Integer(5), Span::new(10, 11))),
        type_annotation: None,
        span: Span::new(0, 11),
    };

    // Non-block, non-FFI receiver → triggers generate_value_keyword_guard
    let recv = Expression::Identifier(Identifier::new("someVar", Span::new(0, 7)));

    let send = Expression::MessageSend {
        receiver: Box::new(recv),
        selector: MessageSelector::Keyword(vec![KeywordPart::new("value:", Span::new(0, 6))]),
        arguments: vec![field_assignment],
        is_cast: false,
        span: Span::new(0, 20),
    };

    let doc = generator.generate_expression(&send).unwrap();
    let output = doc.to_pretty_string();

    // State binding must be hoisted before the _ValArgN binding.
    // _ValRecv1 is first, _ValArg2 allocated next (before field_assignment_open),
    // then _Val3 + State1 from hoisting, then _ValArg2 = _Val3.
    assert!(
        output.contains("let _Val3 = 5 in let State1 = call 'maps':'put'('y', _Val3, State) in"),
        "State binding should be hoisted before _ValArgN binding. Got:\n{output}"
    );
    // The _ValArgN should be bound to the val var, not the full assignment expression.
    assert!(
        output.contains("let _ValArg2 = _Val3 in"),
        "_ValArg2 should be bound to _Val3. Got:\n{output}"
    );
    // The is_function guard must reference _ValRecv1.
    assert!(
        output.contains("call 'erlang':'is_function'(_ValRecv1)"),
        "Guard should check _ValRecv1. Got:\n{output}"
    );

    generator.pop_scope();
}

// --- BT-1420: Self-call state threading ---

#[test]
fn test_self_call_non_last_threads_state() {
    // BT-1420: `self setup` followed by `self.value` — the self-send must
    // thread NewState so the subsequent field read sees the mutation.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: value = 0\n\n",
        "  setup =>\n",
        "    self.value := 42\n\n",
        "  doWork =>\n",
        "    self setup\n",
        "    self.value\n",
    );
    let code = codegen_source(src);
    // The self-send must produce a state variable (SDState/State1) that is used
    // by the subsequent maps:get for self.value
    assert!(
        code.contains("safe_dispatch") || code.contains("dispatch"),
        "Self-call should use dispatch. Got:\n{code}"
    );
    // The state must be threaded: extract element(2, ...) into a new State var
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Self-call must extract NewState from dispatch result. Got:\n{code}"
    );
    // The field read (self.value) must reference the threaded state, not the original
    assert!(
        code.contains("'maps':'get'('value', State1)"),
        "Field read after self-call must use threaded State1. Got:\n{code}"
    );
}

#[test]
fn test_self_call_as_last_expression_threads_state() {
    // BT-1420: `self setup` as the last expression must thread NewState
    // into the reply tuple so gen_server state is updated.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: value = 0\n\n",
        "  setup =>\n",
        "    self.value := 42\n\n",
        "  doWork =>\n",
        "    self setup\n",
    );
    let code = codegen_source(src);
    // The reply tuple must use the threaded state
    assert!(
        code.contains("'erlang':'element'(2,"),
        "Last self-call must extract NewState. Got:\n{code}"
    );
    // Reply should use element(1, ...) for the result
    assert!(
        code.contains("'erlang':'element'(1,"),
        "Last self-call must extract result via element(1, ...). Got:\n{code}"
    );
}

#[test]
fn test_self_call_error_branch_handles_both_error_shapes() {
    // BT-2816: A self-send's error branch must handle TWO distinct shapes
    // returned by safe_dispatch/dispatch, not just one:
    //
    // 1. A caught exception: safe_dispatch/3's try/catch packs it as the
    //    3-tuple {'error', {Type, Reason, Stacktrace}, State} — must be
    //    destructured and routed through beamtalk_exception_handler:reraise/4
    //    (with a selector/class breadcrumb — see BT-2822's
    //    test_self_call_error_branch_threads_selector_class_breadcrumb below)
    //    (passing the whole triple straight to beamtalk_error:raise/1 crashes
    //    with function_clause, since raise/1 only accepts a raw
    //    #beamtalk_error{} record — the original BT-2816 bug).
    // 2. A plain returned error: dispatch/4's DNU fallback (and other
    //    non-exception error paths) *return* {'error', Error, State} where
    //    Error is a bare #beamtalk_error{} record — never reaching
    //    safe_dispatch's try/catch at all. This shape must still fall through
    //    to a `call 'beamtalk_error':'raise'(Error)` clause, mirroring the
    //    two-clause pattern already used at the handle_cast/handle_info
    //    boundary (gen_server/callbacks.rs) — dropping this fallback clause
    //    would crash a self-send that resolves to DNU with case_clause.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: value = 0\n\n",
        "  setup =>\n",
        "    self.value := 42\n\n",
        "  doWork =>\n",
        "    self setup\n",
        "    self.value\n",
    );
    let code = codegen_source(src);

    // Clause 1: destructured triple routed through reraise/4 (with a
    // selector/class breadcrumb, since BT-2822).
    assert!(
        code.contains("call 'beamtalk_exception_handler':'reraise'("),
        "Self-dispatch error branch must destructure the caught-exception \
         triple via beamtalk_exception_handler:reraise/4. Got:\n{code}"
    );
    // Clause 2: fallback for a plain #beamtalk_error{} returned (not caught).
    assert!(
        code.contains("call 'beamtalk_error':'raise'("),
        "Self-dispatch error branch must keep the plain-error fallback clause \
         (e.g. DNU returns from dispatch/4 that never reach safe_dispatch's \
         try/catch). Got:\n{code}"
    );
    // The buggy pre-fix form (raising the whole triple directly) must be gone.
    assert!(
        !code.contains("call 'beamtalk_error':'raise'(SelfError"),
        "Self-dispatch must not pass the raw {{Type, Reason, Stacktrace}} \
         triple straight to beamtalk_error:raise/1. Got:\n{code}"
    );
}

#[test]
fn test_self_call_error_branch_threads_selector_class_breadcrumb() {
    // BT-2822 (BT-2816 follow-up): the caught-exception clause of a
    // self-dispatch error branch must call reraise/4 with a
    // #{selector => ..., class => ...} breadcrumb — mirroring
    // beamtalk_actor:sync_send_remote/3's cross-actor Context construction —
    // so a raw Erlang error escaping a forwarded self-send gets the same
    // `ClassName>>selector: ...` location prefix as the cross-actor case.
    //
    // BT-2833: `class` is now resolved via a runtime `lookup_class/1` call
    // on `self()` — not a compile-time literal atom baked in from
    // `class_name()` — so a self-send inside a method a subclass inherits
    // without overriding still reports the actor's actual runtime class,
    // not the compile-time defining class. Since the fix makes the
    // breadcrumb codegen entirely class-name-agnostic (it never emits a
    // literal atom for 'class' at all, regardless of which class is being
    // compiled), a single compile of one class is sufficient to prove the
    // mechanism.
    //
    // A BUnit e2e test (subclass instance invoking a purely-inherited,
    // non-overridden self-dispatch method that raises) was attempted but is
    // currently blocked by a separate, pre-existing bug: the runtime
    // hierarchy-walk fallback (`generate_hierarchy_walk` in
    // `gen_server/dispatch.rs`) discards the real error returned by
    // `beamtalk_dispatch:super/5` whenever an inherited method raises,
    // fabricating a bogus `does_not_understand` instead — masking this
    // fix's effect for any selector not matched locally. Tracked in BT-2842.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: value = 0\n\n",
        "  setup =>\n",
        "    self.value := 42\n\n",
        "  doWork =>\n",
        "    self setup\n",
        "    self.value\n",
    );
    let code = codegen_source(src);

    // reraise/4 (four args: Type, Reason, Stacktrace, Context) is used, not
    // the context-free reraise/3. The 4th arg is a breadcrumb map whose
    // 'selector' is a literal atom (known at compile time) and whose 'class'
    // is a variable bound just above from a runtime lookup_class/1 call —
    // the exact fresh-var numbering is an implementation detail. Note the
    // capture group is `\w+` (no quotes) — this would fail to match a
    // literal quoted atom like 'Srv', so a successful match here already
    // proves 'class' is a variable, not a compile-time literal.
    let reraise_with_breadcrumb = regex::Regex::new(
        r"call 'beamtalk_exception_handler':'reraise'\(\w+, \w+, \w+, ~\{'selector' => 'setup', 'class' => (\w+)\}~\)",
    )
    .unwrap();
    let captures = reraise_with_breadcrumb.captures(&code).unwrap_or_else(|| {
        panic!(
            "Self-dispatch error branch must call reraise/4 with a \
             #{{selector => 'setup', class => <runtime-lookup var>}} \
             breadcrumb map. Got:\n{code}"
        )
    });
    let class_var = &captures[1];

    // The 'class' value must come from a runtime lookup, not a literal
    // 'Srv' atom — BT-2833 fixes the inheritance mismatch by resolving the
    // actor's actual runtime class via beamtalk_actor:lookup_class/1.
    let lookup_binding = regex::Regex::new(&format!(
        r"let {class_var} = call 'beamtalk_actor':'lookup_class'\(call 'erlang':'self'\(\)\) in"
    ))
    .unwrap();
    assert!(
        lookup_binding.is_match(&code),
        "Breadcrumb 'class' variable {class_var} must be bound from a \
         runtime beamtalk_actor:lookup_class(self()) call. Got:\n{code}"
    );
}

#[test]
fn test_self_call_error_branch_sealed_direct_call_breadcrumb() {
    // BT-2833: generate_direct_sealed_call (a sealed class self-sending one
    // of its own sealed methods, in *closed* expression position — e.g. as
    // the receiver of another message) had no breadcrumb coverage at all.
    // A self-send nested inside a binary op forces codegen through the
    // closed generate_self_dispatch -> generate_sealed_self_dispatch ->
    // generate_direct_sealed_call path (the most optimized sealed-method
    // call, bypassing safe_dispatch/dispatch entirely) rather than the
    // top-level-statement generate_self_dispatch_open path already covered
    // by test_self_call_error_branch_threads_selector_class_breadcrumb.
    let src = concat!(
        "sealed Actor subclass: Srv\n",
        "  state: value = 0\n\n",
        "  setup => 42\n\n",
        "  doWork =>\n",
        "    self setup + 1\n",
    );
    let code = codegen_source(src);

    // Confirm this actually reached the direct __sealed_setup standalone
    // function call — otherwise the test wouldn't be exercising
    // generate_direct_sealed_call at all.
    assert!(
        code.contains("'__sealed_setup'"),
        "Test setup must exercise generate_direct_sealed_call (direct \
         __sealed_setup call), not a different self-dispatch path. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_actor':'lookup_class'(call 'erlang':'self'())"),
        "generate_direct_sealed_call's error branch must resolve the \
         breadcrumb 'class' via a runtime lookup_class(self()) call. Got:\n{code}"
    );
    let reraise_with_breadcrumb = regex::Regex::new(
        r"call 'beamtalk_exception_handler':'reraise'\(\w+, \w+, \w+, ~\{'selector' => 'setup', 'class' => \w+\}~\)",
    )
    .unwrap();
    assert!(
        reraise_with_breadcrumb.is_match(&code),
        "generate_direct_sealed_call must thread the selector/class \
         breadcrumb into reraise/4. Got:\n{code}"
    );
}

#[test]
fn test_self_call_error_branch_tier2_self_send_breadcrumb() {
    // BT-2833: generate_tier2_self_send_open (a self-send passing a
    // stateful block argument — one that captures and mutates an outer
    // local, e.g. a user-defined HOM invocation like
    // `self eachItem: [:x | count := count + x]`) had no breadcrumb
    // coverage at all. Mirrors the real-world pattern verified in
    // stdlib/test/fixtures/hom_composability_actor.bt's
    // testMutatingBlockInCustomLoop.
    let src = concat!(
        "Actor subclass: Srv\n",
        "  state: value = 0\n\n",
        "  eachItem: aBlock =>\n",
        "    last := nil\n",
        "    #(1, 2, 3) do: [:each | last := aBlock value: each]\n",
        "    last\n\n",
        "  doWork =>\n",
        "    count := 0\n",
        "    self eachItem: [:x | count := count + x]\n",
        "    count\n",
    );
    let code = codegen_source(src);

    // Sanity: the Tier 2 stateful-block plumbing (StateAcc threading) is
    // present, proving this test exercises generate_tier2_self_send_open
    // and not a plain self-dispatch path.
    assert!(
        code.contains("StateAcc"),
        "Test setup must exercise the Tier 2 stateful self-send path \
         (generate_tier2_self_send_open). Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_actor':'lookup_class'(call 'erlang':'self'())"),
        "generate_tier2_self_send_open's error branch must resolve the \
         breadcrumb 'class' via a runtime lookup_class(self()) call. Got:\n{code}"
    );
    let reraise_with_breadcrumb = regex::Regex::new(
        r"call 'beamtalk_exception_handler':'reraise'\(\w+, \w+, \w+, ~\{'selector' => 'eachItem:', 'class' => \w+\}~\)",
    )
    .unwrap();
    assert!(
        reraise_with_breadcrumb.is_match(&code),
        "generate_tier2_self_send_open must thread the selector/class \
         breadcrumb into reraise/4. Got:\n{code}"
    );
}

// --- ADR 0070 Phase 2: Package-qualified class reference codegen tests ---

#[test]
fn test_qualified_class_reference_standalone() {
    // ADR 0070 Phase 2: `json@Parser` as a standalone expression should use
    // the class registry with the short name 'Parser' and produce the
    // display name 'json@Parser class' in the class object tuple.
    use crate::ast::{Expression, Identifier, Module};
    use crate::source_analysis::Span;

    let expr = Expression::ClassReference {
        name: Identifier::new("Parser", Span::new(5, 11)),
        package: Some(Identifier::new("json", Span::new(0, 4))),
        span: Span::new(0, 11),
    };

    let module = Module {
        type_aliases: Vec::new(),
        expressions: vec![bare(expr)],
        classes: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 11),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_repl_expression(&module.expressions[0].expression, "repl_eval")
        .expect("codegen should succeed");

    // Registry lookup uses the short class name
    assert!(
        code.contains("call 'beamtalk_class_registry':'whereis_class'('Parser')"),
        "Should look up 'Parser' (short name) in class registry. Got:\n{code}"
    );

    // Display name in the class object tuple includes package qualifier
    assert!(
        code.contains("'json@Parser class'"),
        "Class object tuple should use 'json@Parser class' as display name. Got:\n{code}"
    );
}

#[test]
fn test_qualified_class_method_call() {
    // ADR 0070 Phase 2: `json@Parser parse: input` should use the class
    // registry to find the class PID and dispatch the class method.
    let mut generator = CoreErlangGenerator::new("bt@my_app@main");

    let receiver = Expression::ClassReference {
        name: Identifier::new("Parser", Span::new(5, 11)),
        package: Some(Identifier::new("json", Span::new(0, 4))),
        span: Span::new(0, 11),
    };
    let selector = MessageSelector::Keyword(vec![KeywordPart::new("parse:", Span::new(12, 18))]);
    let arguments = vec![Expression::Identifier(Identifier::new(
        "input",
        Span::new(19, 24),
    ))];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    // Should look up by short class name in class registry
    assert!(
        output.contains("'beamtalk_class_registry':'whereis_class'('Parser')"),
        "Should use short class name for registry lookup. Got:\n{output}"
    );

    // Should dispatch via class_send
    assert!(
        output.contains("'beamtalk_object_class':'class_send'"),
        "Should dispatch class method via class_send. Got:\n{output}"
    );
}

#[test]
fn test_qualified_actor_spawn() {
    // ADR 0070 Phase 2: `json@Worker spawn` should use the fully-qualified
    // BEAM module name 'bt@json@worker' for the spawn call.
    let mut generator = CoreErlangGenerator::new("bt@my_app@main");

    let receiver = Expression::ClassReference {
        name: Identifier::new("Worker", Span::new(5, 11)),
        package: Some(Identifier::new("json", Span::new(0, 4))),
        span: Span::new(0, 11),
    };
    let selector = MessageSelector::Unary("spawn".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();

    // Should use the fully-qualified BEAM module name
    assert!(
        output.contains("call 'bt@json@worker':'spawn'()"),
        "Qualified spawn should use 'bt@json@worker' module name. Got:\n{output}"
    );
}

#[test]
fn test_qualified_actor_spawn_with_args() {
    // ADR 0070 Phase 2: `json@Worker spawnWith: config` should use the
    // fully-qualified BEAM module name 'bt@json@worker'.
    let mut generator = CoreErlangGenerator::new("bt@my_app@main");

    let receiver = Expression::ClassReference {
        name: Identifier::new("Worker", Span::new(5, 11)),
        package: Some(Identifier::new("json", Span::new(0, 4))),
        span: Span::new(0, 11),
    };
    let selector =
        MessageSelector::Keyword(vec![KeywordPart::new("spawnWith:", Span::new(12, 22))]);
    let arguments = vec![Expression::Literal(Literal::Integer(42), Span::new(23, 25))];

    let doc = generator
        .generate_message_send(&receiver, &selector, &arguments)
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("call 'bt@json@worker':'spawn'(42)"),
        "Qualified spawnWith: should use 'bt@json@worker' module name. Got:\n{output}"
    );
}

#[test]
fn test_qualified_class_reference_multi_word() {
    // ADR 0070 Phase 2: `utils@MyClass spawn` should produce 'bt@utils@my_class'.
    let mut generator = CoreErlangGenerator::new("bt@my_app@main");

    let receiver = Expression::ClassReference {
        name: Identifier::new("MyClass", Span::new(6, 13)),
        package: Some(Identifier::new("utils", Span::new(0, 5))),
        span: Span::new(0, 13),
    };
    let selector = MessageSelector::Unary("spawn".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("call 'bt@utils@my_class':'spawn'()"),
        "Multi-word class name should use snake_case in qualified module name. Got:\n{output}"
    );
}

#[test]
fn test_unqualified_class_reference_unchanged() {
    // ADR 0070 Phase 2: Unqualified class references should continue to
    // work exactly as before — no regressions.
    let mut generator = CoreErlangGenerator::new("bt@my_app@main");

    let receiver = Expression::ClassReference {
        name: Identifier::new("Counter", Span::new(0, 7)),
        package: None,
        span: Span::new(0, 7),
    };
    let selector = MessageSelector::Unary("spawn".into());

    let doc = generator
        .generate_message_send(&receiver, &selector, &[])
        .unwrap();
    let output = doc.to_pretty_string();

    // Unqualified spawn should use the existing heuristic module name
    assert!(
        output.contains("call 'bt@my_app@counter':'spawn'()"),
        "Unqualified spawn should use package heuristic. Got:\n{output}"
    );
}

// ---------------------------------------------------------------------------
// BT-2276: classMethods: block arity validation (compile-time).
// A `classMethods:` block whose parameter count does not match the selector
// (`self` plus one per selector slot) must be rejected with a clear
// BlockArityError naming the selector, not silently lowered to a wrong-arity
// fun that crashes with an opaque `error:undef` only when first called.
// ---------------------------------------------------------------------------

/// Parse `src` as a single top-level cascade expression and run it through
/// `expression_doc` (the path the REPL / workspace evaluator takes for a bare
/// `Object classBuilder …; register` cascade). Returns the codegen Result so
/// failures can be asserted.
fn try_codegen(src: &str) -> std::result::Result<String, CodeGenError> {
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _diags) = crate::source_analysis::parse(tokens);
    let expr = module
        .expressions
        .into_iter()
        .next()
        .expect("expected one top-level expression")
        .expression;
    let mut generator = CoreErlangGenerator::new("test");
    generator
        .expression_doc(&expr)
        .map(|doc| doc.to_pretty_string())
}

#[test]
fn test_bt2276_class_methods_zero_arg_block_rejected() {
    // `#greet => [42]` forgot the leading `:self`: a unary class-method block
    // needs exactly one parameter (`self`).
    let src = "Object classBuilder name: #BT2276Z; superclass: Object; \
               classMethods: #{ #greet => [42] }; register";
    match try_codegen(src) {
        Err(CodeGenError::BlockArityError {
            selector,
            expected,
            actual,
            ..
        }) => {
            assert!(
                selector.contains("greet"),
                "error should name the selector, got: {selector}"
            );
            assert_eq!(expected, "1", "unary class method expects `self` only");
            assert_eq!(actual, 0, "the block declared zero parameters");
        }
        other => panic!("expected BlockArityError for zero-arg classMethods block, got: {other:?}"),
    }
}

#[test]
fn test_bt2276_class_methods_keyword_wrong_arity_block_rejected() {
    // Keyword selector `scale:` needs `self` + one arg = 2 params; the block
    // declares only `self`.
    let src = "Object classBuilder name: #BT2276K; superclass: Object; \
               classMethods: #{ #scale: => [:self | 1] }; register";
    match try_codegen(src) {
        Err(CodeGenError::BlockArityError {
            selector,
            expected,
            actual,
            hint,
        }) => {
            assert!(
                selector.contains("scale:"),
                "error should name the selector, got: {selector}"
            );
            assert_eq!(expected, "2", "scale: expects self + one arg");
            assert_eq!(actual, 1, "the block declared one parameter");
            assert!(
                hint.contains("classMethods:"),
                "hint should mention classMethods:, got: {hint}"
            );
        }
        other => {
            panic!(
                "expected BlockArityError for wrong-arity keyword classMethods block, got: {other:?}"
            )
        }
    }
}

#[test]
fn test_bt2276_class_methods_correct_block_unaffected() {
    // A correctly-shaped block (`self` plus one per selector slot) still lowers
    // to a class-method fun and compiles cleanly.
    let src = "Object classBuilder name: #BT2276OK; superclass: Object; \
               classMethods: #{ #answer => [:self | 42], #scale: => [:self :n | n * 3] }; register";
    let code = try_codegen(src).expect("correct classMethods blocks must compile");
    assert!(
        code.contains("fun (ClassSelf, ClassVars"),
        "correct classMethods block should lower to a class-method fun. Got:\n{code}"
    );
}

#[test]
fn test_bt2276_class_methods_computed_fun_passes_compile() {
    // A computed (non-block) value cannot have its arity checked at compile
    // time, so it must NOT be rejected by the compiler — its arity is validated
    // at registration time (beamtalk_class_builder).
    let src = "Object classBuilder name: #BT2276C; superclass: Object; \
               classMethods: #{ #greet => someFun }; register";
    try_codegen(src).expect("computed classMethods fun must compile (validated at register time)");
}

#[test]
fn test_class_method_self_send_long_selector_uses_hashed_atom() {
    // BT-1408 follow-up (#3051): inside a class-side method, a self-send whose
    // selector would form a 'class_<selector>' atom exceeding Erlang's 255-byte
    // hard limit must hash via safe_class_method_fn_name, not emit the raw
    // overlong atom that erlc's core_scan rejects at BEAM-compile time.
    // The `class` keyword declares a class-side method; `self <sel>` inside it
    // triggers generate_class_method_self_send → line 1267 → the fixed path.
    // "class_" (6 bytes) + 250 'a's = 256 bytes — one over the limit.
    let long_sel = "a".repeat(250);
    let src = format!(
        "Actor subclass: LongSel\n  state: x = 0\n\n  class {long_sel} => 42\n\n  class go => self {long_sel}\n"
    );
    let code = codegen_source(&src);

    // The raw 'class_<selector>' atom (256 bytes) must NOT appear anywhere —
    // erlc's atom scanner rejects atoms > 255 bytes at BEAM-compile time.
    let raw_class_atom = format!("class_{long_sel}");
    assert!(
        !code.contains(&raw_class_atom),
        "oversized 'class_<selector>' atom must not appear in generated code. Got:\n{code}"
    );
    // The self-send call in `go` must use the hashed 'class_kw_<hex>' form,
    // not the raw oversized atom. codegen_source uses module name "test".
    assert!(
        code.contains("call 'test':'class_kw_"),
        "class method self-send must use hashed call target 'class_kw_<hex>'. Got:\n{code}"
    );
}

// ─── gen_server/spawn.rs coverage ────────────────────────────────────────────

#[test]
fn test_abstract_actor_spawn_raises_instantiation_error() {
    // BT-105: Abstract Actor subclasses must emit spawn/0 and spawn/1 as
    // instantiation_error stubs — not real safe_spawn calls.
    use crate::ast::*;

    let class = ClassDefinition {
        name: Identifier::new("AbstractQueue", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: true,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        type_aliases: Vec::new(),
        expressions: vec![],
        method_definitions: vec![],
        protocols: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let code = generate_module(&module, CodegenOptions::new("bt@abstract_queue"))
        .expect("codegen should succeed");

    assert!(
        code.contains("'spawn'/0 = fun () ->"),
        "Abstract actor spawn/0 must have arity-0 fun signature. Got:\n{code}"
    );
    assert!(
        !code.contains("beamtalk_actor':'safe_spawn'"),
        "Abstract actor spawn must NOT call safe_spawn. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_error':'new'('instantiation_error', 'AbstractQueue')"),
        "Abstract actor spawn must raise instantiation_error for the class. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_error':'with_selector'(Error0, 'spawn')"),
        "spawn/0 error stub must name selector 'spawn'. Got:\n{code}"
    );
    assert!(
        code.contains("'spawn'/1 = fun (_InitArgs) ->"),
        "Abstract actor spawn/1 must discard InitArgs (_InitArgs). Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_error':'with_selector'(Error0, 'spawnWith:')"),
        "spawn/1 error stub must name selector 'spawnWith:'. Got:\n{code}"
    );
    let abstract_hint = CoreErlangGenerator::binary_string_literal(
        "Abstract classes cannot be instantiated. Subclass it first.",
    );
    assert_eq!(
        code.matches(abstract_hint.as_str()).count(),
        2,
        "Both spawn/0 and spawn/1 stubs must include the abstract-class hint. Got:\n{code}"
    );
    // Abstract stubs must never emit an instance-registration call.
    assert!(
        !code.contains("'beamtalk_object_instances':'register'"),
        "Abstract actor spawn stubs must not register instances. Got:\n{code}"
    );
}

#[test]
fn test_actor_spawn_registers_instance_for_hot_reload() {
    // BT-572: spawn/0 and spawn/1 ok-branches must register the new pid with
    // beamtalk_object_instances:register/2, wrapped in try-catch so a missing
    // registry (e.g. in stdlib unit tests) does not crash the spawn.
    use crate::ast::*;

    let class = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    assert!(
        code.contains("of _RegOk -> _RegOk"),
        "instance registration success arm must return the ok value. Got:\n{code}"
    );
    assert!(
        code.contains("catch <_RegT, _RegE, _RegS> -> 'ok'"),
        "instance registration must swallow errors when registry is absent. Got:\n{code}"
    );
    // Check each spawn function independently so a missing registration in one
    // is reported precisely rather than as a count mismatch.
    let spawn0_start = code
        .find("'spawn'/0 = fun () ->")
        .expect("spawn/0 must be generated");
    let spawn1_start = code
        .find("'spawn'/1 = fun (InitArgs) ->")
        .expect("spawn/1 must be generated");
    let spawn1_end = code[spawn1_start..]
        .find("'new'/0 = fun () ->")
        .map_or(code.len(), |offset| spawn1_start + offset);
    // spawn/0 and spawn/1 are emitted in that order, before new/0, in the generated module.
    assert!(
        spawn0_start < spawn1_start,
        "spawn/0 must appear before spawn/1 in generated code"
    );
    let spawn0_body = &code[spawn0_start..spawn1_start];
    let spawn1_body = &code[spawn1_start..spawn1_end];
    assert!(
        spawn0_body.contains("let _InstReg = try call 'beamtalk_object_instances':'register'("),
        "spawn/0 ok-branch must register the instance. Got spawn/0 body:\n{spawn0_body}"
    );
    assert!(
        spawn1_body.contains("let _InstReg = try call 'beamtalk_object_instances':'register'("),
        "spawn/1 ok-branch must register the instance. Got spawn/1 body:\n{spawn1_body}"
    );
}

#[test]
fn test_spawn_with_args_validates_map_argument() {
    // BT-473: spawn/1 must guard InitArgs with erlang:is_map before calling
    // safe_spawn. A non-map raises type_error with the 'spawnWith:' selector
    // and a descriptive hint.
    use crate::ast::*;

    let class = ClassDefinition {
        name: Identifier::new("Counter", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    assert!(
        code.contains("case call 'erlang':'is_map'(InitArgs)"),
        "spawn/1 must guard InitArgs with erlang:is_map. Got:\n{code}"
    );
    assert!(
        code.contains("<'false'> when 'true' ->"),
        "spawn/1 must have a false branch for non-map InitArgs. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_error':'new'('type_error', 'Counter')"),
        "spawn/1 false-branch must raise type_error for the class. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_error':'with_selector'(TypeErr0, 'spawnWith:')"),
        "type_error must name selector 'spawnWith:'. Got:\n{code}"
    );
    let dict_hint =
        CoreErlangGenerator::binary_string_literal("spawnWith: expects a Dictionary argument");
    assert!(
        code.contains(dict_hint.as_str()),
        "type_error must include the Dictionary hint. Got:\n{code}"
    );
}
