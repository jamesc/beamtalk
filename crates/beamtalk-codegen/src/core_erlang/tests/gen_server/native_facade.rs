// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Codegen for `native:`-backed classes: the generated Erlang facade
//! module (spawn, dispatch, meta, `register_class`) and native class
//! construction validation (ADR 0056).

use super::*;

// ─── Native Facade (ADR 0056) ───────────────────────────────────────────────

/// Build a Module for `Actor subclass: TestNative native: test_backing_mod`
/// with two delegate methods.
fn make_native_actor_module() -> Module {
    let self_expr = || Expression::Identifier(Identifier::new("self", Span::new(0, 0)));
    let delegate_send = || {
        bare(Expression::MessageSend {
            receiver: Box::new(self_expr()),
            selector: MessageSelector::Unary("delegate".into()),
            arguments: vec![],
            is_cast: false,
            span: Span::new(0, 0),
        })
    };

    let class = ClassDefinition {
        name: Identifier::new("TestNative", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![
            MethodDefinition {
                selector: MessageSelector::Unary("doWork".into()),
                parameters: vec![],
                body: vec![delegate_send()],
                kind: MethodKind::Primary,
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
            MethodDefinition {
                selector: MessageSelector::Keyword(vec![KeywordPart::new(
                    "process:",
                    Span::new(0, 0),
                )]),
                parameters: vec![ParameterDefinition::new(Identifier::new(
                    "data",
                    Span::new(0, 0),
                ))],
                body: vec![delegate_send()],
                kind: MethodKind::Primary,
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
        ],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: Some(Identifier::new("test_backing_mod", Span::new(0, 0))),
        handle_scope: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

#[test]
fn test_native_facade_spawn_calls_backing_module() {
    // ADR 0056: spawn/1 should call BackingModule:start_link, not gen_server:start_link
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'test_backing_mod':'start_link'(Config)"),
        "spawn/1 should call backing module's start_link. Got:\n{code}"
    );
    // Should NOT contain gen_server:start_link (that's for regular actors)
    assert!(
        !code.contains("'gen_server':'start_link'"),
        "Native facade should not use gen_server:start_link. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_spawn_wraps_beamtalk_object() {
    // ADR 0056: spawn result is wrapped as #beamtalk_object{} record
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("{'beamtalk_object', 'TestNative', 'bt@test_native', Pid}"),
        "spawn should wrap result as beamtalk_object. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_spawn_0_delegates_to_spawn_1() {
    // ADR 0056: spawn/0 calls spawn/1 with empty map
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'spawn'/0 = fun () ->"),
        "Should have spawn/0. Got:\n{code}"
    );
    assert!(
        code.contains("'bt@test_native':'spawn'(~{}~)"),
        "spawn/0 should call spawn/1 with empty map. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_has_method_includes_all_selectors() {
    // ADR 0056: has_method/1 returns true for all declared selectors
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    // Extract the has_method/1 function body to avoid matching selectors in method_info/meta
    let has_method_fn =
        extract_core_fn(&code, "'has_method'/1 = fun").expect("has_method/1 not found");
    assert!(
        has_method_fn.contains("'doWork'"),
        "has_method/1 body should include 'doWork'. Got:\n{has_method_fn}"
    );
    assert!(
        has_method_fn.contains("'process:'"),
        "has_method/1 body should include 'process:'. Got:\n{has_method_fn}"
    );
}

#[test]
fn test_native_facade_meta_includes_native_flag() {
    // ADR 0056: __beamtalk_meta/0 includes native => true and backing_module
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    // Extract the __beamtalk_meta/0 function body to avoid matching keys in BuilderState.meta
    let meta_fn =
        extract_core_fn(&code, "'__beamtalk_meta'/0 = fun").expect("__beamtalk_meta/0 not found");
    assert!(
        meta_fn.contains("'native' => 'true'"),
        "__beamtalk_meta/0 body should include native => true. Got:\n{meta_fn}"
    );
    assert!(
        meta_fn.contains("'backing_module' => 'test_backing_mod'"),
        "__beamtalk_meta/0 body should include backing_module. Got:\n{meta_fn}"
    );
}

#[test]
fn test_meta_superclass_is_single_quoted_atom() {
    // the leaf-constructor migration must emit the meta-map superclass as a
    // single-quoted atom (`'superclass' => 'Actor'`). A stray leading quote ahead of
    // leaf::atom produced `''Actor'`, which desyncs Core Erlang atom quoting.
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    let meta_fn =
        extract_core_fn(&code, "'__beamtalk_meta'/0 = fun").expect("__beamtalk_meta/0 not found");
    assert!(
        meta_fn.contains("'superclass' => 'Actor'"),
        "__beamtalk_meta/0 body should include 'superclass' => 'Actor'. Got:\n{meta_fn}"
    );
    assert!(
        !meta_fn.contains("=> ''"),
        "__beamtalk_meta/0 must not emit doubled-quote atoms (e.g. ''Actor'). Got:\n{meta_fn}"
    );
}

#[test]
fn test_native_facade_no_gen_server_behaviour() {
    // ADR 0056: Native facade does not declare gen_server behaviour
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        !code.contains("'behaviour' = ['gen_server']"),
        "Native facade should not declare gen_server behaviour. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_no_gen_server_callbacks() {
    // ADR 0056: Native facade should not have init/1, handle_cast/2, etc.
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        !code.contains("'init'/1"),
        "Native facade should not have init/1. Got:\n{code}"
    );
    assert!(
        !code.contains("'handle_cast'/2"),
        "Native facade should not have handle_cast/2. Got:\n{code}"
    );
    assert!(
        !code.contains("'handle_call'/3"),
        "Native facade should not have handle_call/3. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_register_class_includes_meta() {
    // ADR 0056: register_class/0 should include native meta in BuilderState
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    // Extract the register_class/0 function body
    let register_fn =
        extract_core_fn(&code, "'register_class'/0 = fun").expect("register_class/0 not found");
    assert!(
        register_fn.contains("'beamtalk_class_builder':'register'"),
        "register_class/0 should call beamtalk_class_builder:register. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'isConstructible' => 'false'"),
        "BuilderState should mark native actors as not constructible. Got:\n{register_fn}"
    );
    // BuilderState.meta should contain native-specific keys
    assert!(
        register_fn.contains("'native' => 'true'"),
        "BuilderState.meta should include native => true. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'backing_module' => 'test_backing_mod'"),
        "BuilderState.meta should include backing_module. Got:\n{register_fn}"
    );
}

/// `native:` facade `register_class/0` bakes a `methodXref` list into
/// its `BuilderState`, exactly like the standard `register_class/0` path. Before
/// this fix native classes (e.g. `Subprocess`, `TranscriptStream`) loaded with no
/// baked `method_xref`, so they were absent from `beamtalk_xref` and every
/// navigation query source-scanned them via the miss-policy fallback.
#[test]
fn test_native_facade_register_class_bakes_method_xref() {
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    let register_fn =
        extract_core_fn(&code, "'register_class'/0 = fun").expect("register_class/0 not found");
    // The methodXref field is present and a list (not a `~{ }~` map).
    assert!(
        register_fn.contains("'methodXref' => ["),
        "native register_class/0 should bake a methodXref list. Got:\n{register_fn}"
    );
    // The instance methods `doWork` and `process:` are recorded, instance-side,
    // and tagged indexed (they carry analysable Beamtalk source).
    assert!(
        register_fn.contains("'selector' => 'doWork'"),
        "doWork xref entry missing. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'selector' => 'process:'"),
        "process: xref entry missing. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'class_side' => 'false'"),
        "instance-side entries should carry 'class_side' => 'false'. Got:\n{register_fn}"
    );
    assert!(
        register_fn.contains("'source_status' => 'indexed'"),
        "native instance-method rows should be tagged indexed. Got:\n{register_fn}"
    );
}

#[test]
fn test_native_facade_spawn_error_raises_instantiation_error() {
    // ADR 0056: spawn failure should raise instantiation_error with reason in details
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'instantiation_error'"),
        "Should raise instantiation_error on spawn failure. Got:\n{code}"
    );
    assert!(
        code.contains("'reason' => Reason"),
        "Should include reason in error details. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_spawn_handles_ignore() {
    // spawn/1 should handle `ignore` from start_link (init/1 returned ignore)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("<'ignore'> when 'true' ->"),
        "spawn/1 should have an 'ignore' match arm. Got:\n{code}"
    );
    assert!(
        code.contains("'reason' => 'ignore'"),
        "ignore case should set reason => 'ignore' in details. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_spawn_wraps_crash_in_try_catch() {
    // spawn/1 should wrap start_link in try-catch for crash handling
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("let StartResult = try call"),
        "spawn/1 should wrap start_link in try-catch. Got:\n{code}"
    );
    assert!(
        code.contains("of _StartOk -> _StartOk"),
        "try-catch should have of clause for success passthrough. Got:\n{code}"
    );
    assert!(
        code.contains("{'__bt_spawn_crash', SpawnCrashReason}"),
        "catch arm should wrap crash reason in __bt_spawn_crash tuple. Got:\n{code}"
    );
    assert!(
        code.contains("<{'__bt_spawn_crash', SpawnCrashReason}> when 'true' ->"),
        "case should match __bt_spawn_crash tuple. Got:\n{code}"
    );
    assert!(
        code.contains("'reason' => SpawnCrashReason"),
        "crash case should include SpawnCrashReason in details. Got:\n{code}"
    );
}

/// Build a native actor with class methods and class variables for richer tests.
fn make_native_actor_with_class_methods() -> Module {
    let class = ClassDefinition {
        name: Identifier::new("TestNativeRich", Span::new(0, 0)),
        superclass: Some(Identifier::new("Actor", Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![MethodDefinition {
            selector: MessageSelector::Unary("status".into()),
            parameters: vec![],
            body: vec![bare(Expression::Identifier(Identifier::new(
                "self",
                Span::new(0, 0),
            )))],
            kind: MethodKind::Primary,
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: false,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        class_methods: vec![MethodDefinition {
            selector: MessageSelector::Keyword(vec![KeywordPart::new("connect:", Span::new(0, 0))]),
            parameters: vec![ParameterDefinition::new(Identifier::new(
                "config",
                Span::new(0, 0),
            ))],
            body: vec![bare(Expression::Identifier(Identifier::new(
                "config",
                Span::new(0, 0),
            )))],
            kind: MethodKind::Primary,
            return_type: None,
            is_sealed: false,
            is_internal: false,
            is_class_method: true,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            span: Span::new(0, 0),
        }],
        class_variables: vec![StateDeclaration {
            name: Identifier::new("current", Span::new(0, 0)),
            type_annotation: None,
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: Span::new(0, 0),
        }],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: Some("A test native actor with class methods.".to_string()),
        backing_module: Some(Identifier::new("test_rich_backing", Span::new(0, 0))),
        handle_scope: None,
        span: Span::new(0, 0),
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

#[test]
fn test_native_facade_class_methods_exported() {
    // ADR 0056: Class methods on native actors compile normally
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    assert!(
        code.contains("'class_connect:'/3"),
        "Should export class method 'class_connect:'/3. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_class_method_alias_param_emits_user_type_and_named_type() {
    // `gen_server/native_facade.rs`'s `generate_class_specs` call
    // site must resolve alias-typed annotations to `user_type` references,
    // with the module also declaring the matching named `-type` in the
    // same attribute list (an `erlc` compile error otherwise). Native
    // facade modules use the same `is_value_type: false` spec path as
    // regular actors (instance methods don't get standalone
    // specs), so this uses the class-side `connect:` method.
    let mut module = make_native_actor_with_class_methods();
    module.type_aliases.push(TypeAliasDefinition {
        name: Identifier::new("RestartStrategy", Span::new(0, 0)),
        annotation: TypeAnnotation::union(
            vec![
                TypeAnnotation::singleton("temporary", Span::new(0, 0)),
                TypeAnnotation::singleton("transient", Span::new(0, 0)),
                TypeAnnotation::singleton("permanent", Span::new(0, 0)),
            ],
            Span::new(0, 0),
        ),
        is_internal: false,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    });
    module.classes[0].class_methods[0].parameters[0].type_annotation =
        Some(TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)));

    let code = generate_module(&module, CodegenOptions::new("bt@test_rich"))
        .expect("codegen should succeed");
    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "class method param typed with the alias should emit a user_type reference. Got:\n{code}"
    );
    assert!(
        code.contains("'restart_strategy'"),
        "module must declare the matching named -type for the alias. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_cross_module_alias_reference_emits_user_type() {
    // same wiring check as
    // `test_native_facade_class_method_alias_param_emits_user_type_and_named_type`
    // above, but the alias is declared in a *different* compiled module —
    // threaded in via `CodegenOptions::with_pre_loaded_aliases` — instead of
    // this module's own `type_aliases`.
    let strategy_alias = TypeAliasDefinition {
        name: Identifier::new("RestartStrategy", Span::new(0, 0)),
        annotation: TypeAnnotation::union(
            vec![
                TypeAnnotation::singleton("temporary", Span::new(0, 0)),
                TypeAnnotation::singleton("transient", Span::new(0, 0)),
                TypeAnnotation::singleton("permanent", Span::new(0, 0)),
            ],
            Span::new(0, 0),
        ),
        is_internal: false,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    };
    let pre_loaded_aliases = vec![
        beamtalk_core::semantic_analysis::alias_registry::AliasInfo::from_definition(
            &strategy_alias,
        ),
    ];

    // No `type_aliases` of its own — the module only references the name.
    let mut module = make_native_actor_with_class_methods();
    module.classes[0].class_methods[0].parameters[0].type_annotation =
        Some(TypeAnnotation::simple("RestartStrategy", Span::new(0, 0)));

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@test_rich_cross_module")
            .with_pre_loaded_aliases(pre_loaded_aliases),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains("{'user_type', 0, 'restart_strategy', []}"),
        "class method param typed with a cross-module alias should emit a user_type reference. \
         Got:\n{code}"
    );
    assert!(
        code.contains("'restart_strategy'"),
        "module must declare the matching named -type for the cross-module alias. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_class_variables_in_builder_state() {
    // ADR 0056: classState: should appear in BuilderState
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    assert!(
        code.contains("'classState' => ~{'current' =>"),
        "BuilderState should include class variables. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_doc_comments_in_builder_state() {
    // Doc comments should propagate to BuilderState
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    assert!(
        code.contains("'classDoc' =>"),
        "BuilderState should include classDoc. Got:\n{code}"
    );
    // classDoc should not be 'none' since we set a doc comment
    assert!(
        !code.contains("'classDoc' => 'none'"),
        "classDoc should not be 'none' when doc comment is set. Got:\n{code}"
    );
}

// ===========================================================================
// Dispatch functions for self delegate methods
// ===========================================================================

#[test]
fn test_native_facade_dispatch_exported() {
    // Dispatch functions for self delegate methods must be exported
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'dispatch_doWork'/1"),
        "dispatch_doWork/1 should be exported. Got:\n{code}"
    );
    assert!(
        code.contains("'dispatch_process:'/2"),
        "dispatch_process:/2 should be exported. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_dispatch_extracts_pid() {
    // Dispatch functions extract pid from Self via element(4, Self)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    // Extract dispatch function body (starts with "= fun")
    let dispatch_dowork = code
        .split("'dispatch_doWork'/1 = fun")
        .nth(1)
        .expect("dispatch_doWork function body should exist");
    assert!(
        dispatch_dowork.contains("call 'erlang':'element'(4, Self)"),
        "dispatch should extract pid via element(4, Self). Got:\n{dispatch_dowork}"
    );
}

#[test]
fn test_native_facade_dispatch_calls_sync_send() {
    // Dispatch functions call beamtalk_actor:sync_send/3
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("call 'beamtalk_actor':'sync_send'(Pid, 'doWork', [])"),
        "dispatch_doWork should call sync_send with empty args. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_actor':'sync_send'(Pid, 'process:', [Data])"),
        "dispatch_process: should call sync_send with [Data] args. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_dispatch_unary_arity() {
    // Unary self delegate dispatch has arity 1 (just Self)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'dispatch_doWork'/1 = fun (Self) ->"),
        "Unary dispatch should take only Self. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_dispatch_keyword_arity() {
    // Keyword self delegate dispatch has arity = params + 1 (for Self)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert!(
        code.contains("'dispatch_process:'/2 = fun (Data, Self) ->"),
        "Keyword dispatch should take params then Self. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_no_dispatch_for_beamtalk_body() {
    // Methods with full Beamtalk bodies should NOT get dispatch functions
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    // status => self (not self delegate) should not have a dispatch function
    assert!(
        !code.contains("'dispatch_status'"),
        "Non-delegate method should NOT get a dispatch function. Got:\n{code}"
    );
}

// ─── bare `new` on an opaque `native:` class ────────────────────────

#[test]
fn test_bt_2998_native_class_without_fields_raises_on_new() {
    // A `native:` class keeps its state in the shape its backing module
    // defines, so `basicNew`'s `~{'$beamtalk_class' => 'X'}~` is a hollow
    // instance every later method call trips over. Refuse it up front.
    let src = concat!(
        "Value subclass: DateTime native: beamtalk_datetime\n",
        "  class sealed now -> DateTime => self delegate\n",
        "  class sealed monotonicNow -> Integer => self delegate\n",
        "  class sealed fromString: str :: String -> DateTime => self delegate\n",
        "  year -> Integer => self delegate\n",
    );
    let code = super::codegen(src);

    assert!(
        code.contains("call 'beamtalk_error':'new'('instantiation_error', 'DateTime')"),
        "bare new on an opaque native class must raise instantiation_error. Got:\n{code}"
    );
    assert!(
        !code.contains("~{'$beamtalk_class' => 'DateTime'}~"),
        "must not still build the hollow tagged map. Got:\n{code}"
    );
    // (The hint text itself is a Core Erlang binary literal, so it is asserted
    // on in `value_type_codegen`'s unit tests rather than the emitted code.)
    // `new:` merges over `new`, so it is just as hollow and refuses too.
    assert!(
        code.contains("'new'/1 = fun (_InitArgs) ->"),
        "new/1 must also refuse rather than merge over a hollow default. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_error':'with_selector'(Error0, 'new:')"),
        "the new/1 refusal must name selector 'new:'. Got:\n{code}"
    );
}

#[test]
fn test_bt_2998_native_class_with_declared_fields_still_builds_default_instance() {
    // A `native:` class that *does* declare fields has a real default
    // instance (`Package`, `SupervisionNode`), so `basicNew` stays correct.
    let src = concat!(
        "Value subclass: Package native: beamtalk_package\n",
        "  field: name = nil\n",
        "  class sealed named: n :: String -> Package => self delegate\n",
    );
    let code = super::codegen(src);
    assert!(
        code.contains("'$beamtalk_class' => 'Package'"),
        "field-carrying native class must still build its default map. Got:\n{code}"
    );
    assert!(
        !code.contains("'instantiation_error', 'Package'"),
        "field-carrying native class must not refuse new. Got:\n{code}"
    );
}

#[test]
fn test_bt_2998_native_class_with_own_class_new_keeps_it() {
    // `Random`/`Queue` declare a working zero-arg `new`; `new/0` must keep
    // delegating to it rather than being replaced by the refusal.
    let src = concat!(
        "Value subclass: Random native: beamtalk_random\n",
        "  class sealed new -> Random => self delegate\n",
    );
    let code = super::codegen(src);
    let new_body = extract_core_fn(&code, "'new'/0 = fun").expect("should have new/0");
    assert!(
        new_body.contains("'class_new'('undefined', 'undefined')"),
        "new/0 must delegate to the declared class method. Got:\n{new_body}"
    );
    assert!(
        !new_body.contains("instantiation_error"),
        "a class with its own `new` must not get the refusal. Got:\n{new_body}"
    );
}

#[test]
fn test_bt_2998_non_native_value_class_unaffected() {
    // Plain value types build their instance from field defaults as before.
    let src = concat!(
        "Value subclass: Point\n",
        "  field: x = 0\n",
        "  field: y = 0\n",
    );
    let code = super::codegen(src);
    assert!(
        !code.contains("'instantiation_error', 'Point'"),
        "non-native value class must still be constructible. Got:\n{code}"
    );
    assert!(
        code.contains("'$beamtalk_class' => 'Point'"),
        "non-native value class must build its default map. Got:\n{code}"
    );
}

#[test]
fn test_bt_2998_opaque_native_class_registers_as_non_constructible() {
    // The compile-time `isConstructible` flag must agree with the
    // raising `new/0`, instead of leaving the runtime to discover it.
    let src = concat!(
        "Value subclass: Uuid native: beamtalk_uuid\n",
        "  class sealed v4 -> Uuid => self delegate\n",
    );
    let code = super::codegen(src);
    assert!(
        code.contains("'isConstructible' => 'false'"),
        "opaque native class must register isConstructible => false. Got:\n{code}"
    );
}
