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
    // generate_terminate must emit lifecycle-stop telemetry via
    // beamtalk_actor:maybe_execute_telemetry with the 'stop' event path.
    let code = codegen("Actor subclass: TestActor\n  state: x = 0\n");
    assert!(
        code.contains("'terminate'/2"),
        "Module must export terminate/2. Got:\n{code}"
    );
    assert!(
        code.contains("call 'beamtalk_actor':'maybe_execute_telemetry'("),
        "terminate/2 must emit lifecycle telemetry. Got:\n{code}"
    );
    assert!(
        code.contains("'lifecycle', 'stop']"),
        "terminate/2 telemetry must include the lifecycle 'stop' event name. Got:\n{code}"
    );
}

#[test]
fn test_terminate_uses_class_name_for_telemetry_metadata() {
    // terminate/2 telemetry 'class' metadata must use the clean Beamtalk
    // class name (e.g. 'EventStore'), not the compiled module name (e.g.
    // 'bt@event_store'). This matches how dispatch traces report class names.
    let src = "Actor subclass: EventStore\n  state: count = 0\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("bt@event_store"))
        .expect("codegen should succeed");
    assert!(
        code.contains("'class' => 'EventStore'"),
        "terminate/2 must use class name 'EventStore' in telemetry metadata. Got:\n{code}"
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
        code.contains("'class' => 'bt_module'"),
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
    let doc = generator.generate_class_name_function(&module).unwrap();
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
    let doc = generator.generate_class_name_function(&module).unwrap();
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
    let doc = generator.generate_has_method(&module).unwrap();
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
    use beamtalk_core::ast::{ClassDefinition, MethodDefinition, MethodKind};

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
        methods: vec![
            MethodDefinition {
                selector: MessageSelector::Unary("increment".into()),
                parameters: vec![],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(0),
                    Span::new(0, 0),
                ))],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
                expect: None,
                comments: CommentAttachment::default(),
                doc_comment: None,
                span: Span::new(0, 0),
            },
            MethodDefinition {
                selector: MessageSelector::Keyword(vec![KeywordPart::new(
                    "setValue:",
                    Span::new(0, 0),
                )]),
                parameters: vec![ParameterDefinition {
                    name: Identifier::new("value", Span::new(0, 0)),
                    type_annotation: None,
                }],
                body: vec![bare(Expression::Literal(
                    Literal::Integer(0),
                    Span::new(0, 0),
                ))],
                return_type: None,
                is_sealed: false,
                is_internal: false,
                is_class_method: false,
                kind: MethodKind::Primary,
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
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    };
    let module = Module {
        classes: vec![class],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let generator = CoreErlangGenerator::new("counter");
    let doc = generator.generate_has_method(&module).unwrap();
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
    let doc = generator.generate_has_method(&module).unwrap();
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
// locally, via a foreign extension, or via an ancestor?" — and had drifted:
// the actor version never checked the extension registry, never delegated
// to its superclass, and never short-circuited for a catch-all-DNU class,
// so `respondsTo:` answered differently for an actor than for a value type
// given the identical situation. Both now render through the same
// `DispatchSpec`-driven emitter (`dispatch_spec::generate_has_method_from_spec`).
// These three tests — an extension method, an inherited method, and a
// DNU-catch-all class — pinned the pre-fix divergence in an earlier revision
// of this same file; they now assert the corrected, unified behavior.

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
        name: Identifier::new(name, Span::new(0, 0)),
        superclass: Some(Identifier::new(superclass, Span::new(0, 0))),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods,
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        comments: CommentAttachment::default(),
        doc_comment: None,
        backing_module: None,
        handle_scope: None,
        span: Span::new(0, 0),
    }
}

fn module_with_class(class: ClassDefinition) -> Module {
    Module {
        classes: vec![class],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: vec![],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

fn unary_method(name: &str) -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Unary(name.into()),
        parameters: vec![],
        body: vec![bare(Expression::Literal(
            Literal::Integer(0),
            Span::new(0, 0),
        ))],
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    }
}

/// A `doesNotUnderstand:args:` method whose body is a structural (unquoted)
/// intrinsic — the shape `class_has_catch_all_dnu` recognizes as a
/// catch-all DNU handler (e.g. `Erlang`/`ErlangModule` in stdlib), as
/// opposed to a regular Beamtalk-body DNU override (e.g. `TimeoutProxy`),
/// which does *not* count.
fn catch_all_dnu_method() -> MethodDefinition {
    MethodDefinition {
        selector: MessageSelector::Keyword(vec![
            KeywordPart::new("doesNotUnderstand:", Span::new(0, 0)),
            KeywordPart::new("args:", Span::new(0, 0)),
        ]),
        parameters: vec![
            ParameterDefinition {
                name: Identifier::new("selector", Span::new(0, 0)),
                type_annotation: None,
            },
            ParameterDefinition {
                name: Identifier::new("args", Span::new(0, 0)),
                type_annotation: None,
            },
        ],
        body: vec![bare(Expression::Primitive {
            name: "erlangModuleLookup".into(),
            is_quoted: false,
            is_intrinsic: true,
            is_inferred: false,
            span: Span::new(0, 0),
        })],
        return_type: None,
        is_sealed: false,
        is_internal: false,
        is_class_method: false,
        kind: MethodKind::Primary,
        expect: None,
        comments: CommentAttachment::default(),
        doc_comment: None,
        span: Span::new(0, 0),
    }
}

#[test]
fn test_generate_has_method_actor_checks_extension_registry() {
    // Value-type has_method/1 always checks `beamtalk_extensions:has/2` for
    // a selector it doesn't recognize locally (`generate_primitive_has_method`),
    // so `anActor respondsTo: #anExtensionMethod` and `aValue respondsTo:
    // #anExtensionMethod` must answer the same way for the identical
    // situation. Actor has_method/1 now consults the extension
    // registry too, via the shared `DispatchSpec` emitter.
    let class = actor_class_def("Counter", "Actor", vec![unary_method("increment")]);
    let module = module_with_class(class);
    let generator = CoreErlangGenerator::new("counter");
    let doc = generator.generate_has_method(&module).unwrap();
    let output = doc.to_pretty_string();
    assert!(
        output.contains("call 'beamtalk_extensions':'has'('Counter', Selector)"),
        "actor has_method/1 must consult the extension registry, matching \
         value-type has_method/1. Got:\n{output}"
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
    let doc = generator.generate_has_method(&module).unwrap();
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
    let doc = generator.generate_has_method(&module).unwrap();
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
    let doc = generator.generate_safe_dispatch().unwrap();
    let output = doc.to_pretty_string();

    assert!(
        output.contains("'safe_dispatch'/3 = fun (Selector, Args, State) ->"),
        "Should generate safe_dispatch/3 header. Got: {output}"
    );
    // Self must be constructed via make_self before dispatch
    assert!(
        output.contains("call 'beamtalk_actor':'make_self'(State)"),
        "Should construct Self via make_self/1. Got: {output}"
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
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

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
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("test").with_workspace_mode(true),
    )
    .expect("codegen should succeed");

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
