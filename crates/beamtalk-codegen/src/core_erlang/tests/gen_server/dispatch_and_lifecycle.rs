// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `gen_server` lifecycle callbacks (`terminate`, `code_change`) and
//! dispatch-structure codegen: `has_method`, `safe_dispatch`, and
//! keyword method-argument destructuring.

use super::*;

// ── code_change / terminate codegen coverage ─────────────────────────────────
//
// Target: gen_server/callbacks.rs generate_code_change (lines 1583-1599) and
// generate_terminate (lines 1618-1675) — zero coverage in the 2026-07-20 CI run.
//
// Strategy: exercise both functions via generate_module on a minimal Actor class
// and via generate() on the plain-module path, asserting on the key fragments
// that each function is responsible for emitting.

#[test]
fn test_code_change_delegates_to_beamtalk_hot_reload() {
    // generate_code_change must emit 'code_change'/3 that delegates entirely to
    // beamtalk_hot_reload:code_change/3 for OTP hot-code-reload state migration.
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("'code_change'/3"),
        "Module must export code_change/3. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_hot_reload':'code_change'(OldVsn, State, Extra)"),
        "code_change/3 must delegate to beamtalk_hot_reload:code_change/3. Got:\n{code}"
    );
}

#[test]
fn test_terminate_lifecycle_stop_telemetry() {
    // generate_terminate must report the stop through
    // beamtalk_actor:lifecycle_stopped(Class, Reason) (BT-4997): it builds no
    // event metadata itself.
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("'terminate'/2"),
        "Module must export terminate/2. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_actor':'lifecycle_stopped'('TestActor', Reason)"),
        "terminate/2 must report the stop. Got:\n{code}"
    );
    assert!(
        !code.contains("maybe_execute_telemetry"),
        "generated code must not build lifecycle event metadata. Got:\n{code}"
    );
}

#[test]
fn test_terminate_uses_class_name_for_telemetry_metadata() {
    // terminate/2's lifecycle call must pass the clean Beamtalk
    // class name (e.g. 'EventStore'), not the compiled module name (e.g.
    // 'bt@event_store'). This matches how dispatch traces report class names.
    let src = "Actor subclass: EventStore\n  state: count = 0\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("bt@event_store"))
        .expect("codegen should succeed");
    assert!(
        code.contains("call 'beamtalk_actor':'lifecycle_stopped'('EventStore', Reason)"),
        "terminate/2 must use class name 'EventStore' for the lifecycle stop. Got:\n{code}"
    );
}

#[test]
fn test_terminate_wraps_dispatch_in_try_catch() {
    // generate_terminate must wrap the 'terminate:' method dispatch in try-catch
    // so that user exceptions cannot prevent OTP gen_server shutdown.
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("let _TermDisp = try call"),
        "terminate: dispatch must be the body of the try expression. Got:\n{code}"
    );
    assert!(
        code.contains("catch <_TermT, _TermE, _TermS> -> 'ok'"),
        "terminate/2 catch clause must swallow all exceptions and return ok. Got:\n{code}"
    );
    assert!(
        code.contains("'terminate:'"),
        "terminate/2 must dispatch the 'terminate:' method. Got:\n{code}"
    );
}

#[test]
fn test_terminate_calls_make_self_before_dispatch() {
    // terminate/2 must build a self-object via beamtalk_actor:make_self before
    // calling dispatch so the Beamtalk object is available to terminate: handlers.
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("call 'beamtalk_actor':'make_self'(State)"),
        "terminate/2 must call beamtalk_actor:make_self/1 to build the self-object. Got:\n{code}"
    );
    assert!(
        code.contains("in 'ok'"),
        "terminate/2 must end with 'ok' as its return value. Got:\n{code}"
    );
}

#[test]
fn test_terminate_plain_module_uses_module_name_as_class_label() {
    // When the module has no explicit class definition, generate_terminate falls
    // back to the module name as the class label in telemetry metadata.
    // generate() uses module name 'bt_module'.
    let module = Module::new(Vec::new(), Span::new(0, 0));
    let result = generate(&module);
    assert!(
        result.is_ok(),
        "codegen should succeed for plain module: {result:?}"
    );
    let code = result.unwrap();
    assert!(
        code.contains("'terminate'/2"),
        "Plain module must still export terminate/2. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_actor':'lifecycle_stopped'('bt_module', Reason)"),
        "Plain module terminate/2 must use module name 'bt_module' as class label. Got:\n{code}"
    );
}

// ── Direct unit tests for dispatch.rs codegen helpers ───────────────────────
//
// These call `generate_class_name_function`, `generate_has_method`, and
// `generate_safe_dispatch` directly (without going through the full
// `generate_module` pipeline) to pin their exact Core Erlang output and to
// reach the macro-expanded lines that full-pipeline tests miss.

#[test]
fn test_generate_class_name_function_derives_from_module_name() {
    // CoreErlangGenerator::class_name() converts the module name from
    // snake_case → CamelCase when no explicit class identity is set.
    let generator = CoreErlangGenerator::new("my_counter");
    let module = Module::new(vec![], Span::new(0, 0));
    // _module arg is unused by production code; only self.module_name matters
    let doc = generator.generate_class_name_function(&module);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'class_name'/0 = fun () -> 'MyCounter'"),
        "class_name/0 should return CamelCase atom from module name. Got: {output}"
    );
}

#[test]
fn test_generate_class_name_function_single_word_module() {
    // Single-word module name: "counter" → "Counter".
    let generator = CoreErlangGenerator::new("counter");
    let module = Module::new(vec![], Span::new(0, 0));
    // _module arg is unused by production code; only self.module_name matters
    let doc = generator.generate_class_name_function(&module);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'class_name'/0 = fun () -> 'Counter'"),
        "class_name/0 should return 'Counter' for module 'counter'. Got: {output}"
    );
}

#[test]
fn test_generate_has_method_empty_module_produces_empty_member_list() {
    // An empty module has no methods; has_method/1 should always return false
    // (member of an empty list is always false).
    let generator = CoreErlangGenerator::new("counter");
    let module = Module::new(vec![], Span::new(0, 0));
    let doc = generator.generate_has_method(&module);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'has_method'/1 = fun (Selector) ->"),
        "Should generate has_method/1 header. Got: {output}"
    );
    assert!(
        output.contains("call 'lists':'member'(Selector, [])"),
        "Empty module should yield empty member list. Got: {output}"
    );
}

#[test]
fn test_generate_has_method_lists_primary_class_methods() {
    // A module with an Actor class should list all primary methods in has_method/1.
    use beamtalk_core::ast::{ClassDefinition, MethodDefinition};

    let class = ClassDefinition {
        methods: vec![
            MethodDefinition {
                pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
                literal_comments: beamtalk_core::ast::LiteralComments::default(),
                body: vec![bare(Expression::Literal(
                    Literal::Integer(0),
                    Span::new(0, 0),
                ))],
                ..MethodDefinition::test("increment")
            },
            MethodDefinition {
                pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
                literal_comments: beamtalk_core::ast::LiteralComments::default(),
                ..MethodDefinition::new(
                    MessageSelector::Keyword(vec![KeywordPart::new("setValue:", Span::new(0, 0))]),
                    vec![ParameterDefinition {
                        name: Identifier::new("value", Span::new(0, 0)),
                        type_annotation: None,
                    }],
                    vec![bare(Expression::Literal(
                        Literal::Integer(0),
                        Span::new(0, 0),
                    ))],
                    Span::new(0, 0),
                )
            },
        ],
        ..ClassDefinition::test("Counter", Some("Actor"), ClassKind::Actor)
    };
    let module = Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
    };

    let generator = CoreErlangGenerator::new("counter");
    let doc = generator.generate_has_method(&module);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'increment'"),
        "has_method/1 should list 'increment'. Got: {output}"
    );
    assert!(
        output.contains("'setValue:'"),
        "has_method/1 should list 'setValue:'. Got: {output}"
    );
    assert!(
        output.contains("call 'lists':'member'(Selector, ["),
        "has_method/1 should call lists:member on the method list. Got: {output}"
    );
}

#[test]
fn test_generate_has_method_from_expression_based_module() {
    // Script/workspace modules use top-level `name := [block]` assignments as
    // methods; has_method/1 must include those names.
    let src = "increment := [self.value + 1]. getValue := [self.value]";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let generator = CoreErlangGenerator::new("counter");
    let doc = generator.generate_has_method(&module);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'increment'"),
        "has_method/1 should include 'increment' from script method. Got: {output}"
    );
    assert!(
        output.contains("'getValue'"),
        "has_method/1 should include 'getValue' from script method. Got: {output}"
    );
}

// ── actor has_method/1 matches value-type has_method/1 ────────────
//
// `gen_server::dispatch::generate_has_method` (actor) and
// `value_type_codegen::generate_primitive_has_method` (value type) implement
// the same reflection surface — "does this class understand `Selector`,
// locally or via an ancestor?" — and had drifted: the actor version never
// delegated to its superclass, and never short-circuited for a catch-all-DNU
// class, so `respondsTo:` answered differently for an actor than for a value
// type given the identical situation. Both now render through the same
// `DispatchSpec`-driven emitter (`dispatch_spec::generate_has_method_from_spec`).
// These tests — an inherited method and a DNU-catch-all class — pinned the
// pre-fix divergence in an earlier revision of this same file; they now
// assert the corrected, unified behavior.

/// A minimal actor `ClassDefinition` for `has_method` pinning tests — same
/// shape as the literal built in `test_generate_has_method_lists_primary_class_methods`
/// above, factored out since three tests below each need one with a
/// different method list.
fn actor_class_def(
    name: &str,
    superclass: &str,
    methods: Vec<MethodDefinition>,
) -> ClassDefinition {
    ClassDefinition {
        methods,
        ..ClassDefinition::test(name, Some(superclass), ClassKind::Actor)
    }
}

fn module_with_class(class: ClassDefinition) -> Module {
    Module {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        classes: vec![class],
        ..empty_module()
    }
}

fn unary_method(name: &str) -> MethodDefinition {
    MethodDefinition {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        body: vec![bare(Expression::Literal(
            Literal::Integer(0),
            Span::new(0, 0),
        ))],
        ..MethodDefinition::test(name)
    }
}

/// A `doesNotUnderstand:args:` method whose body is a structural (unquoted)
/// intrinsic — the shape `class_has_catch_all_dnu` recognizes as a
/// catch-all DNU handler (e.g. `Erlang`/`ErlangModule` in stdlib), as
/// opposed to a regular Beamtalk-body DNU override (e.g. `TimeoutProxy`),
/// which does *not* count.
fn catch_all_dnu_method() -> MethodDefinition {
    MethodDefinition {
        pipeline_sends: beamtalk_core::ast::PipelineSends::default(),
        literal_comments: beamtalk_core::ast::LiteralComments::default(),
        ..MethodDefinition::new(
            MessageSelector::Keyword(vec![
                KeywordPart::new("doesNotUnderstand:", Span::new(0, 0)),
                KeywordPart::new("args:", Span::new(0, 0)),
            ]),
            vec![
                ParameterDefinition {
                    name: Identifier::new("selector", Span::new(0, 0)),
                    type_annotation: None,
                },
                ParameterDefinition {
                    name: Identifier::new("args", Span::new(0, 0)),
                    type_annotation: None,
                },
            ],
            vec![bare(Expression::Primitive {
                name: "erlangModuleLookup".into(),
                is_quoted: false,
                is_intrinsic: true,
                is_inferred: false,
                no_user_code: false,
                span: Span::new(0, 0),
            })],
            Span::new(0, 0),
        )
    }
}

#[test]
fn test_generate_has_method_actor_consults_no_extension_registry() {
    // ADR 0152: neither actor nor value-type has_method/1 consults an
    // extension registry; a selector the class does not define locally is
    // answered by its superclass.
    let class = actor_class_def("Counter", "Actor", vec![unary_method("increment")]);
    let module = module_with_class(class);
    let generator = CoreErlangGenerator::new("counter");
    let doc = generator.generate_has_method(&module);
    let output = doc.to_pretty_string();
    assert!(
        !output.contains("beamtalk_extensions"),
        "actor has_method/1 must not consult an extension registry. Got:\n{output}"
    );
}

#[test]
fn test_generate_has_method_actor_delegates_to_superclass() {
    // Value-type has_method/1 delegates to its superclass module for a
    // selector it doesn't recognize locally, so an inherited method reports
    // `respondsTo:` true. Actor has_method/1 now does the same
    // reflection — an inherited selector answers `respondsTo:` true — but
    // *dynamically*, via `beamtalk_dispatch:responds_to/2`'s live
    // class-registry walk (the same mechanism actor message dispatch and
    // `respondsTo:` already use), not a compile-time module reference — see
    // `SuperclassDelegation`'s doc comment. A subclass no longer answers
    // `respondsTo:` false for a selector only an ancestor defines, and stays
    // correct across a hot-reloaded ancestor.
    let class = actor_class_def("Counter", "Actor", vec![unary_method("increment")]);
    let module = module_with_class(class);
    let generator = CoreErlangGenerator::new("counter");
    let doc = generator.generate_has_method(&module);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'beamtalk_dispatch':'responds_to'(Selector, 'Actor')"),
        "actor has_method/1 must delegate to its superclass *by class name*, \
         through beamtalk_dispatch:responds_to/2's live registry walk, not a \
         compiled module reference. Got:\n{output}"
    );
}

#[test]
fn test_generate_has_method_actor_honors_catch_all_dnu() {
    // A class whose doesNotUnderstand:args: is a structural (unquoted)
    // intrinsic (e.g. Erlang, ErlangModule) accepts every selector —
    // value-type has_method/1 short-circuits to `true` unconditionally for
    // such a class. Actor has_method/1 now does the same, via the
    // shared `DispatchSpec` emitter. Actor has_method/1 also emits
    // a has_method_local/1 sibling — the strictly-local probe used by
    // beamtalk_dispatch:class_chain_step/6 — which short-circuits to true
    // too, since a catch-all-DNU class handles every selector at its own
    // dispatch/4.
    let class = actor_class_def("Proxy", "Actor", vec![catch_all_dnu_method()]);
    let module = module_with_class(class);
    let generator = CoreErlangGenerator::new("proxy");
    let doc = generator.generate_has_method(&module);
    let output = doc.to_pretty_string();
    assert_eq!(
        output,
        "'has_method'/1 = fun (_Selector) ->\n    'true'\n\n\
         'has_method_local'/1 = fun (_Selector) ->\n    'true'\n\n",
        "actor has_method/1 must short-circuit to true for a catch-all-DNU \
         class, matching value-type has_method/1. Got:\n{output}"
    );
}

#[test]
fn test_generate_safe_dispatch_structure() {
    // safe_dispatch/3 must wrap dispatch/4 in a try/catch that returns the
    // stacktrace on failure and calls beamtalk_actor:make_self/1 first.
    // The generated call must reference the module's own dispatch fn.
    let mut generator = CoreErlangGenerator::new("my_counter");
    let doc = generator.generate_safe_dispatch();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("'safe_dispatch'/3 = fun (Selector, Args, State) ->"),
        "Should generate safe_dispatch/3 header. Got: {output}"
    );
    // BT-4265: the module's own state builds Self inline from the class and
    // module codegen knows, with no runtime call...
    assert!(
        output.contains("<~{'__class_mod__' := 'my_counter'}~> when call 'erlang':'not'(call 'erlang':'is_map_key'('$beamtalk_class', State))"),
        "Should recognise this module's own state. Got: {output}"
    );
    assert!(
        output.contains("{'beamtalk_object', 'MyCounter', 'my_counter', call 'erlang':'self'()}"),
        "Should build Self inline for this module's own state. Got: {output}"
    );
    // ...and any other state still derives Self via make_self/1.
    assert!(
        output.contains("<_> when 'true' -> call 'beamtalk_actor':'make_self'(State)"),
        "Should fall back to make_self/1. Got: {output}"
    );
    // The try must call the module's own dispatch function
    assert!(
        output.contains("'my_counter':'dispatch'(Selector, Args, Self, State)"),
        "Should dispatch to my_counter:dispatch/4. Got: {output}"
    );
    // The try/catch structure
    assert!(
        output.contains("try call"),
        "Should use try/catch for error isolation. Got: {output}"
    );
    assert!(
        output.contains("of Result -> Result"),
        "Happy path should pass Result through. Got: {output}"
    );
    // stacktrace captured and returned in error tuple
    assert!(
        output.contains("catch <Type, Error, Stacktrace>"),
        "Should catch with stacktrace variable. Got: {output}"
    );
    assert!(
        output.contains("{'error', {Type, Error, Stacktrace}, State}"),
        "Should return error tuple containing the stacktrace. Got: {output}"
    );
}

#[test]
fn test_script_module_keyword_method_dispatch_destructures_args() {
    // A script/workspace module with a multi-parameter block (keyword method)
    // must generate a dispatch clause that:
    //   1. Matches the selector atom in case Selector of
    //   2. Matches Args as a list of named variables (Args destructuring)
    //   3. Falls back to 'bad_arity' on arg count mismatch
    //
    // This exercises generate_legacy_method_clause with non-empty param_vars
    // (line 329) and build_dispatch_clause's Args-case branch.
    let src = "add := [:a :b | a + b]";
    let code = codegen(src);

    let dispatch_body =
        extract_core_fn(&code, "'dispatch'/4 = fun").expect("should have dispatch/4");
    assert!(
        dispatch_body.contains("<'add'>"),
        "dispatch/4 should have 'add' case arm. Got:\n{dispatch_body}"
    );
    // Args must be destructured into a list pattern when params are present
    assert!(
        dispatch_body.contains("case Args of"),
        "Keyword-style method must destructure Args. Got:\n{dispatch_body}"
    );
    assert!(
        dispatch_body.contains("<["),
        "Args case should pattern-match into a list. Got:\n{dispatch_body}"
    );
    assert!(
        dispatch_body.contains("'bad_arity'"),
        "Should fall back to 'bad_arity' on arity mismatch. Got:\n{dispatch_body}"
    );
}

#[test]
fn test_method_table_with_script_methods_includes_arity() {
    // Script/workspace modules emit method_table entries for every
    // `name := [block]` binding, with the block arity as the value.
    let src = "unary := [42]. binary := [:a :b | a + b]";
    let code = codegen(src);

    let table_body =
        extract_core_fn(&code, "'method_table'/0 = fun").expect("should have method_table/0");
    assert!(
        table_body.contains("'unary' => 0"),
        "method_table should list 'unary' with arity 0. Got:\n{table_body}"
    );
    assert!(
        table_body.contains("'binary' => 2"),
        "method_table should list 'binary' with arity 2. Got:\n{table_body}"
    );
}

// ── '__beamtalk_method_lines'/0 (BT-4817) ───────────────────────────────────

#[test]
fn test_method_lines_table_pins_renderer_contract() {
    // The Erlang renderer (`beamtalk_stack_frame:lookup_method_line/2`) reads a
    // tuple of `{StartLine, Selector}` sorted by line, from an exported
    // function returning a literal. This pins the generator side of that
    // contract; `beamtalk_stack_frame_tests` pins the reader.
    let src = "Actor subclass: Ticker\n  state: n = 0\n\n  tick: by :: Integer =>\n    self.n := self.n + by\n\n  reset => self.n := 0\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@ticker")
            .with_source(src)
            .with_source_path_opt(Some("ticker.bt")),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("'__beamtalk_method_lines'/0"),
        "must be exported. Got:\n{code}"
    );
    let body = extract_core_fn(&code, "'__beamtalk_method_lines'/0 = fun")
        .expect("should have __beamtalk_method_lines/0");
    // One `{File, Rows}` entry: the file as the annotations' path (a binary
    // literal, "ticker.bt"), then the rows sorted by line.
    let flat = body.split_whitespace().collect::<Vec<_>>().join(" ");
    assert!(
        flat.starts_with("() -> [{#{") && flat.ends_with("}#, {{4, 'tick:'}, {7, 'reset'}}}]"),
        "Got:\n{body}"
    );
    assert_eq!(
        flat.matches("#<").count(),
        "ticker.bt".len(),
        "Got:\n{body}"
    );
}

// ── format_status / '__beamtalk_secrets' (BT-4567) ──────────────────────────

#[test]
fn test_format_status_delegates_to_redaction_runtime() {
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("'format_status'/1, '__beamtalk_secrets'/0"),
        "Actor modules must export format_status/1 and '__beamtalk_secrets'/0. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_actor_redaction':'format_status'('test', Status)"),
        "format_status/1 must delegate to beamtalk_actor_redaction. Got:\n{code}"
    );
    // No declarations: empty lists, and no chain past the Actor root.
    assert!(
        code.contains("'__beamtalk_secrets'/0 = fun () ->\n    {[], []}"),
        "An actor declaring nothing has no secrets. Got:\n{code}"
    );
}

#[test]
fn test_secrets_metadata_bakes_declared_literals() {
    let code = codegen(
        "Actor subclass: Vault\n  state: token = nil\n  \
         class secretState => #(#token)\n  \
         class secretSelectors => #(#unlock:, #peek)\n  \
         unlock: x => x\n  peek => self.token\n",
    );
    assert!(
        code.contains("{['token'], ['unlock:', 'peek']}"),
        "'__beamtalk_secrets'/0 must answer the declared literals. Got:\n{code}"
    );
}

#[test]
fn test_secrets_metadata_chains_to_superclass_module() {
    let code =
        codegen("Vault subclass: PinVault\n  state: pin = nil\n  class secretState => #(#pin)\n");
    assert!(
        code.contains("let <ParentSecrets> = call 'bt@vault':'__beamtalk_secrets'() in"),
        "A subclass must chain to its superclass module's secrets. Got:\n{code}"
    );
    assert!(
        code.contains(
            "call 'beamtalk_actor_redaction':'merge_secrets'(ParentSecrets, ['pin'], [])"
        ),
        "A subclass must append its own secrets to its superclass's. Got:\n{code}"
    );
}
