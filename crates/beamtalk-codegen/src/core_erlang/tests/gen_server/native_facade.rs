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
        methods: vec![
            MethodDefinition {
                pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
                literal_comments: beamtalk_core::ast::LiteralComments::default(),
                body: vec![delegate_send()],
                ..MethodDefinition::test("doWork")
            },
            MethodDefinition {
                pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
                literal_comments: beamtalk_core::ast::LiteralComments::default(),
                ..MethodDefinition::new(
                    MessageSelector::Keyword(vec![KeywordPart::new("process:", Span::new(0, 0))]),
                    vec![ParameterDefinition::new(Identifier::new(
                        "data",
                        Span::new(0, 0),
                    ))],
                    vec![delegate_send()],
                    Span::new(0, 0),
                )
            },
        ],
        backing_module: Some(Identifier::new("test_backing_mod", Span::new(0, 0))),
        ..ClassDefinition::test("TestNative", Some("Actor"), ClassKind::Actor)
    };
    Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
    }
}

#[test]
fn test_native_facade_spawn_calls_backing_module() {
    // ADR 0056: spawn/1 should call BackingModule:start_link, not gen_server:start_link
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_has(
        "spawn/1 should call backing module's start_link",
        &code,
        &["'test_backing_mod':'start_link'(Config)"],
    );
    // Should NOT contain gen_server:start_link (that's for regular actors)
    assert_lacks(
        "Native facade should not use gen_server:start_link",
        &code,
        &["'gen_server':'start_link'"],
    );
}

#[test]
fn test_native_facade_spawn_wraps_beamtalk_object() {
    // ADR 0056: spawn result is wrapped as #beamtalk_object{} record
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_has(
        "spawn should wrap result as beamtalk_object",
        &code,
        &["{'beamtalk_object', 'TestNative', 'bt@test_native', Pid}"],
    );
}

#[test]
fn test_native_facade_spawn_0_delegates_to_spawn_1() {
    // ADR 0056: spawn/0 calls spawn/1 with empty map
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_has("Should have spawn/0", &code, &["'spawn'/0 = fun () ->"]);
    assert_has(
        "spawn/0 should call spawn/1 with empty map",
        &code,
        &["'bt@test_native':'spawn'(~{}~)"],
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
    assert_has(
        "has_method/1 body should include 'doWork'",
        has_method_fn,
        &["'doWork'"],
    );
    assert_has(
        "has_method/1 body should include 'process:'",
        has_method_fn,
        &["'process:'"],
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
    assert_has(
        "__beamtalk_meta/0 body should include native => true",
        meta_fn,
        &["'native' => 'true'"],
    );
    assert_has(
        "__beamtalk_meta/0 body should include backing_module",
        meta_fn,
        &["'backing_module' => 'test_backing_mod'"],
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
    assert_has(
        "__beamtalk_meta/0 body should include 'superclass' => 'Actor'",
        meta_fn,
        &["'superclass' => 'Actor'"],
    );
    assert_lacks(
        "__beamtalk_meta/0 must not emit doubled-quote atoms (e.g. ''Actor')",
        meta_fn,
        &["=> ''"],
    );
}

#[test]
fn test_native_facade_no_gen_server_behaviour() {
    // ADR 0056: Native facade does not declare gen_server behaviour
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_lacks(
        "Native facade should not declare gen_server behaviour",
        &code,
        &["'behaviour' = ['gen_server']"],
    );
}

#[test]
fn test_native_facade_no_gen_server_callbacks() {
    // ADR 0056: Native facade should not have init/1, handle_cast/2, etc.
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_lacks("Native facade should not have init/1", &code, &["'init'/1"]);
    assert_lacks(
        "Native facade should not have handle_cast/2",
        &code,
        &["'handle_cast'/2"],
    );
    assert_lacks(
        "Native facade should not have handle_call/3",
        &code,
        &["'handle_call'/3"],
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
    assert_has(
        "register_class/0 should call beamtalk_class_builder:register",
        register_fn,
        &["'beamtalk_class_builder':'register'"],
    );
    assert_has(
        "BuilderState should mark native actors as not constructible",
        register_fn,
        &["'isConstructible' => 'false'"],
    );
    // BuilderState.meta should contain native-specific keys
    assert_has(
        "BuilderState.meta should include native => true",
        register_fn,
        &["'native' => 'true'"],
    );
    assert_has(
        "BuilderState.meta should include backing_module",
        register_fn,
        &["'backing_module' => 'test_backing_mod'"],
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
    assert_has(
        "native register_class/0 should bake a methodXref list",
        register_fn,
        &["'methodXref' => ["],
    );
    // The instance methods `doWork` and `process:` are recorded, instance-side,
    // and tagged indexed (they carry analysable Beamtalk source).
    assert_has(
        "doWork xref entry missing",
        register_fn,
        &["'selector' => 'doWork'"],
    );
    assert_has(
        "process: xref entry missing",
        register_fn,
        &["'selector' => 'process:'"],
    );
    assert_has(
        "instance-side entries should carry 'class_side' => 'false'",
        register_fn,
        &["'class_side' => 'false'"],
    );
    assert_has(
        "native instance-method rows should be tagged indexed",
        register_fn,
        &["'source_status' => 'indexed'"],
    );
}

#[test]
fn test_native_facade_spawn_error_raises_instantiation_error() {
    // ADR 0056: spawn failure should raise instantiation_error with reason in details
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_has(
        "Should raise instantiation_error on spawn failure",
        &code,
        &["'instantiation_error'"],
    );
    assert_has(
        "Should include reason in error details",
        &code,
        &["'reason' => Reason"],
    );
}

#[test]
fn test_native_facade_spawn_handles_ignore() {
    // spawn/1 should handle `ignore` from start_link (init/1 returned ignore)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_has(
        "spawn/1 should have an 'ignore' match arm",
        &code,
        &["<'ignore'> when 'true' ->"],
    );
    assert_has(
        "ignore case should set reason => 'ignore' in details",
        &code,
        &["'reason' => 'ignore'"],
    );
}

#[test]
fn test_native_facade_spawn_wraps_crash_in_try_catch() {
    // spawn/1 should wrap start_link in try-catch for crash handling
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_has(
        "spawn/1 should wrap start_link in try-catch",
        &code,
        &["let StartResult = try call"],
    );
    assert_has(
        "try-catch should have of clause for success passthrough",
        &code,
        &["of _StartOk -> _StartOk"],
    );
    assert_has(
        "catch arm should wrap crash reason in __bt_spawn_crash tuple",
        &code,
        &["{'__bt_spawn_crash', SpawnCrashReason}"],
    );
    assert_has(
        "case should match __bt_spawn_crash tuple",
        &code,
        &["<{'__bt_spawn_crash', SpawnCrashReason}> when 'true' ->"],
    );
    assert_has(
        "crash case should include SpawnCrashReason in details",
        &code,
        &["'reason' => SpawnCrashReason"],
    );
}

/// Build a native actor with class methods and class variables for richer tests.
fn make_native_actor_with_class_methods() -> Module {
    let class = ClassDefinition {
        methods: vec![MethodDefinition {
            pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
            literal_comments: beamtalk_core::ast::LiteralComments::default(),
            body: vec![bare(Expression::Identifier(Identifier::new(
                "self",
                Span::new(0, 0),
            )))],
            ..MethodDefinition::test("status")
        }],
        class_methods: vec![MethodDefinition {
            pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
            literal_comments: beamtalk_core::ast::LiteralComments::default(),
            is_class_method: true,
            ..MethodDefinition::new(
                MessageSelector::Keyword(vec![KeywordPart::new("connect:", Span::new(0, 0))]),
                vec![ParameterDefinition::new(Identifier::new(
                    "config",
                    Span::new(0, 0),
                ))],
                vec![bare(Expression::Identifier(Identifier::new(
                    "config",
                    Span::new(0, 0),
                )))],
                Span::new(0, 0),
            )
        }],
        class_variables: vec![StateDeclaration {
            default_value: Some(Expression::Literal(Literal::Integer(0), Span::new(0, 0))),
            slot_kind: beamtalk_core::ast::SlotKind::default(),
            ..StateDeclaration::test("current")
        }],
        doc_comment: Some("A test native actor with class methods.".to_string()),
        backing_module: Some(Identifier::new("test_rich_backing", Span::new(0, 0))),
        ..ClassDefinition::test("TestNativeRich", Some("Actor"), ClassKind::Actor)
    };
    Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
    }
}

#[test]
fn test_native_facade_class_methods_exported() {
    // ADR 0056: Class methods on native actors compile normally
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    assert_has(
        "Should export class method 'class_connect:'/2",
        &code,
        &["'class_connect:'/2"],
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
    assert_has(
        "class method param typed with the alias should emit a user_type reference",
        &code,
        &["{'user_type', 0, 'restart_strategy', []}"],
    );
    assert_has(
        "module must declare the matching named -type for the alias",
        &code,
        &["'restart_strategy'"],
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
    assert_has(
        "module must declare the matching named -type for the cross-module alias",
        &code,
        &["'restart_strategy'"],
    );
}

#[test]
fn test_native_facade_builder_state_has_no_class_state_entry() {
    // ADR 0153 Phase 3e (BT-5255): class variables are gone, so the
    // BuilderState carries no `'classState'` entry at all.
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    assert_lacks(
        "BuilderState must not carry a 'classState' entry",
        &code,
        &["'classState'"],
    );
}

#[test]
fn test_native_facade_doc_comments_in_builder_state() {
    // Doc comments should propagate to BuilderState
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    assert_has(
        "BuilderState should include classDoc",
        &code,
        &["'classDoc' =>"],
    );
    // classDoc should not be 'none' since we set a doc comment
    assert_lacks(
        "classDoc should not be 'none' when doc comment is set",
        &code,
        &["'classDoc' => 'none'"],
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
    assert_has(
        "dispatch_doWork/1 should be exported",
        &code,
        &["'dispatch_doWork'/1"],
    );
    assert_has(
        "dispatch_process:/2 should be exported",
        &code,
        &["'dispatch_process:'/2"],
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
    assert_has(
        "dispatch should extract pid via element(4, Self)",
        dispatch_dowork,
        &["call 'erlang':'element'(4, Self)"],
    );
}

#[test]
fn test_native_facade_dispatch_calls_sync_send_known_with_static_class() {
    // BT-4949: dispatch functions call beamtalk_actor:sync_send_known/4 with the
    // facade's static class, so the send never looks the class up from the pid
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_has(
        "dispatch_doWork should call sync_send_known with the static class and empty args",
        &code,
        &["call 'beamtalk_actor':'sync_send_known'(Pid, 'TestNative', 'doWork', [])"],
    );
    assert!(
        code.contains(
            "call 'beamtalk_actor':'sync_send_known'(Pid, 'TestNative', 'process:', [Data@])"
        ),
        "dispatch_process: should call sync_send_known with the static class and [Data] args. Got:\n{code}"
    );
}

#[test]
fn test_native_facade_dispatch_unary_arity() {
    // Unary self delegate dispatch has arity 1 (just Self)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_has(
        "Unary dispatch should take only Self",
        &code,
        &["'dispatch_doWork'/1 = fun (Self) ->"],
    );
}

#[test]
fn test_native_facade_dispatch_keyword_arity() {
    // Keyword self delegate dispatch has arity = params + 1 (for Self)
    let module = make_native_actor_module();
    let result = generate_module(&module, CodegenOptions::new("bt@test_native"));
    let code = result.unwrap();
    assert_has(
        "Keyword dispatch should take params then Self",
        &code,
        &["'dispatch_process:'/2 = fun (Data@, Self) ->"],
    );
}

#[test]
fn test_native_facade_no_dispatch_for_beamtalk_body() {
    // Methods with full Beamtalk bodies should NOT get dispatch functions
    let module = make_native_actor_with_class_methods();
    let result = generate_module(&module, CodegenOptions::new("bt@test_rich"));
    let code = result.unwrap();
    // status => self (not self delegate) should not have a dispatch function
    assert_lacks(
        "Non-delegate method should NOT get a dispatch function",
        &code,
        &["'dispatch_status'"],
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

    assert_has(
        "bare new on an opaque native class must raise instantiation_error",
        &code,
        &["call 'beamtalk_error':'new'('instantiation_error', 'DateTime')"],
    );
    assert_lacks(
        "must not still build the hollow tagged map",
        &code,
        &["~{'$beamtalk_class' => 'DateTime'}~"],
    );
    // (The hint text itself is a Core Erlang binary literal, so it is asserted
    // on in `value_type_codegen`'s unit tests rather than the emitted code.)
    // `new:` merges over `new`, so it is just as hollow and refuses too.
    assert_has(
        "new/1 must also refuse rather than merge over a hollow default",
        &code,
        &["'new'/1 = fun (_InitArgs) ->"],
    );
    assert_has(
        "the new/1 refusal must name selector 'new:'",
        &code,
        &["call 'beamtalk_error':'with_selector'(Error0, 'new:')"],
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
    assert_has(
        "field-carrying native class must still build its default map",
        &code,
        &["'$beamtalk_class' => 'Package'"],
    );
    assert_lacks(
        "field-carrying native class must not refuse new",
        &code,
        &["'instantiation_error', 'Package'"],
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
    assert_has(
        "new/0 must delegate to the declared class method",
        new_body,
        &["'class_new'('undefined')"],
    );
    assert_lacks(
        "a class with its own `new` must not get the refusal",
        new_body,
        &["instantiation_error"],
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
    assert_lacks(
        "non-native value class must still be constructible",
        &code,
        &["'instantiation_error', 'Point'"],
    );
    assert_has(
        "non-native value class must build its default map",
        &code,
        &["'$beamtalk_class' => 'Point'"],
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
    assert_has(
        "opaque native class must register isConstructible => false",
        &code,
        &["'isConstructible' => 'false'"],
    );
}

#[test]
fn test_native_facade_class_side_same_file_extension_is_folded() {
    // BT-4721: a class-side `X class >> foo` next to a `native:` class folds
    // into the facade module; nothing registers through `beamtalk_extensions`.
    let tokens = beamtalk_core::source_analysis::lex_with_eof(
        "Actor subclass: Wrapper native: my_mod\n  doIt => self delegate\nWrapper class >> extra => 1\n",
    );
    let module = beamtalk_core::source_analysis::parse(tokens).0;
    let code =
        generate_module(&module, CodegenOptions::new("bt@test_native")).expect("codegen succeeds");
    assert_lacks(
        "same-file extension must not register",
        &code,
        &["call 'beamtalk_extensions':'register'"],
    );
    assert_has("folded class method missing", &code, &["extra"]);
}
