// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Actor `initialize`/`handle_continue` chaining (ADR 0078) and
//! typed-field-without-default validation, including inherited and
//! cross-file ancestor `ClassInfo` lookups.

use super::*;

// ── initialize chain codegen coverage ────────────────────────────

/// When an Actor defines an `initialize` method, `init/1`
/// must NOT call it inline. Instead it emits a `__skip_initialize__` guard
/// and returns `{'ok', CleanState1, {'continue', 'initialize'}}` so OTP
/// invokes `handle_continue/2` after the message loop starts, avoiding
/// deadlock on self-sends from within initialize.
#[test]
fn test_actor_with_initialize_defers_to_handle_continue() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: value = 0\n\n",
        "  initialize =>\n",
        "    self.value := 10\n\n",
        "  getValue => self.value\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // init/1 must contain the __skip_initialize__ guard so that when
    // a subclass calls this as a parent state-builder, initialize is not
    // dispatched a second time.
    assert!(
        code.contains("'__skip_initialize__'"),
        "init/1 must guard against double-dispatch with __skip_initialize__. Got:\n{code}"
    );

    // The non-helper branch must return {ok, State, {continue, initialize}} to
    // hand off to handle_continue.
    assert!(
        code.contains("{'continue', 'initialize'}"),
        "init/1 must return {{continue, initialize}} to defer initialize dispatch. Got:\n{code}"
    );

    // The CleanState variants strip the flag from state before returning.
    assert!(
        code.contains("'__skip_initialize__', FinalState"),
        "init/1 must strip __skip_initialize__ flag from FinalState. Got:\n{code}"
    );
}

/// ADR 0078: When an Actor defines `initialize`, `handle_continue/2`
/// must build a pdict-stash + `safe_dispatch` loop so each class in the
/// initialize chain gets a chance to run. Verifies the pdict stash/restore,
/// the `safe_dispatch` call, and the final `noreply` return.
#[test]
fn test_handle_continue_dispatches_initialize_chain() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: value = 0\n\n",
        "  initialize =>\n",
        "    self.value := 10\n\n",
        "  getValue => self.value\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code =
        generate_module(&module, CodegenOptions::new("counter")).expect("codegen should succeed");

    // The callback function must be present.
    assert!(
        code.contains("'handle_continue'/2 = fun (Continue, State) ->"),
        "handle_continue/2 must be generated. Got:\n{code}"
    );

    // The <'initialize'> pattern dispatches the chain.
    assert!(
        code.contains("<'initialize'> when 'true' ->"),
        "handle_continue/2 must match on 'initialize' continuation. Got:\n{code}"
    );

    // pdict stash/restore brackets every safe_dispatch call to
    // preserve re-entrant self-send semantics inside initialize.
    assert!(
        code.contains("'$bt_actor_state'"),
        "handle_continue/2 must stash/restore $bt_actor_state for re-entrant sends. Got:\n{code}"
    );

    // The chain dispatches initialize via safe_dispatch on the class module.
    assert!(
        code.contains("'safe_dispatch'('initialize',"),
        "handle_continue/2 must dispatch 'initialize' via safe_dispatch. Got:\n{code}"
    );

    // On success (the reply arm), the outer result is a noreply continuation.
    assert!(
        code.contains("'noreply'"),
        "handle_continue/2 must return noreply on successful initialize. Got:\n{code}"
    );
}

/// When a class inherits from a user-defined Actor (not directly
/// from `Actor`), `init/1` must call the parent's `init/1` to accumulate
/// inherited state, then merge the child's own fields on top, and propagate
/// any `{error, Reason}` the parent returns.
///
/// This is also the cross-file inherited-state regression coverage.
/// The parent (`Counter`) is compiled in a *separate* module — its AST is absent
/// here — yet the child correctly pulls the parent's state via `bt@counter:init/1`.
/// This is why the old AST-only `collect_inherited_fields` was removed: the
/// super-init chain already handles cross-file / stdlib / package parents.
#[test]
fn test_init_parent_actor_subclass_calls_parent_init() {
    // LoggingCounter extends Counter (itself an Actor subclass).
    // Compiling only LoggingCounter — Counter's AST is absent, but the
    // superclass name "Counter" != "Actor" triggers the parent-init path.
    let src = concat!(
        "Counter subclass: LoggingCounter\n",
        "  state: logCount = 0\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("logging_counter"))
        .expect("codegen should succeed");

    // init/1 must delegate to the parent module's init/1.
    assert!(
        code.contains("'bt@counter':'init'("),
        "init/1 must call parent bt@counter:init/1. Got:\n{code}"
    );

    // The parent's returned state is bound and then merged with child fields.
    assert!(
        code.contains("ParentState"),
        "init/1 must bind parent state as ParentState. Got:\n{code}"
    );
    assert!(
        code.contains("ChildFields"),
        "init/1 must create ChildFields map for child-only state. Got:\n{code}"
    );
    assert!(
        code.contains("MergedState"),
        "init/1 must merge parent and child state into MergedState. Got:\n{code}"
    );
    assert!(
        code.contains("FinalState"),
        "init/1 must produce FinalState (MergedState + InitArgs overrides). Got:\n{code}"
    );

    // Parent init errors must be propagated, not swallowed.
    assert!(
        code.contains("{'error', Reason}"),
        "init/1 must propagate parent {{error, Reason}} without modification. Got:\n{code}"
    );

    // The child's own state field must appear in ChildFields.
    assert!(
        code.contains("'logCount'"),
        "ChildFields must include the child's own logCount state field. Got:\n{code}"
    );
}

// ── Type-annotation codegen coverage ─────────────────────────────────────────
//
// Target: gen_server/callbacks.rs — is_nilable_type Union branch,
// type_annotation_display Singleton/Generic/FalseOr/SelfType/SelfClass/ClassOf
// variants, user_defined_initialize_chain fallback when class_hierarchy is None,
// and inherited_typed_no_default_fields fallback.
// (Function names rather than line numbers so these references don't drift as
// callbacks.rs evolves.)
//
// Strategy:
// - Tests 1-6: generate_module with actor having one typed-no-default field per
//   TypeAnnotation variant; coverage comes from the hierarchy path in
//   inherited_typed_no_default_fields that calls is_nilable_type and
//   type_annotation_display.
// - Tests 7-15: direct CoreErlangGenerator unit tests (class_hierarchy = None)
//   to exercise the no-hierarchy fallback paths in both functions.

/// Shared helper: a single-class Actor Module with one typed-no-default state field.
fn make_actor_typed_no_default(field_name: &str, ty: TypeAnnotation) -> Module {
    let s = Span::new(0, 0);
    let class = ClassDefinition {
        name: Identifier::new("TestActor", s),
        superclass: Some(Identifier::new("Actor", s)),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![StateDeclaration {
            name: Identifier::new(field_name, s),
            type_annotation: Some(ty),
            default_value: None,
            expect: None,
            comments: CommentAttachment::default(),
            doc_comment: None,
            declared_keyword: DeclaredKeyword::default(),
            span: s,
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
        span: s,
    };
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: Vec::new(),
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s,
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

#[test]
fn test_actor_typed_union_nil_field_is_nilable() {
    // is_nilable_type Union branch: Union([Integer, Nil]) is nilable.
    // The field is excluded from typed-no-default so no initialize continuation emitted.
    let s = Span::new(0, 0);
    let union_nil = TypeAnnotation::union(
        vec![
            TypeAnnotation::simple("Integer", s),
            TypeAnnotation::simple("Nil", s),
        ],
        s,
    );
    let module = make_actor_typed_no_default("optValue", union_nil);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        !code.contains("{'continue', 'initialize'}"),
        "Nil-union field is nilable so no initialize continuation is needed. Got:\n{code}"
    );
    // Positive guard (symmetric with test 2): a nilable field must NOT emit the
    // typed-no-default validation. Without this, the negative assertion above
    // would pass trivially if init generation broke for any unrelated reason.
    assert!(
        !code.contains("'uninitialized_state_error'"),
        "Nilable Union field should not trigger typed-no-default validation. Got:\n{code}"
    );
    assert!(
        code.contains("'init'"),
        "init/1 callback should still be generated for the actor. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_union_non_nil_field_triggers_validation() {
    // is_nilable_type Union branch: Union([Integer, String]) is not nilable → included.
    // type_annotation_display Union branch also exercised.
    let s = Span::new(0, 0);
    let union_no_nil = TypeAnnotation::union(
        vec![
            TypeAnnotation::simple("Integer", s),
            TypeAnnotation::simple("String", s),
        ],
        s,
    );
    let module = make_actor_typed_no_default("combo", union_no_nil);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "Non-nil Union field should trigger typed-no-default validation. Got:\n{code}"
    );
    // the typed-no-default field-check path must also strip __local__
    // threading temps from the committed post-initialize state — the `let
    // InitCleanState = …` binding is emitted before the nested field-check case,
    // and the success arm replies with it.
    assert!(
        code.contains(
            "let InitCleanState = call 'beamtalk_actor':'strip_local_temps'(InitNewState) in"
        ),
        "typed-no-default post-init path must strip __local__ temps. Got:\n{code}"
    );
    assert!(
        code.contains("{'noreply', InitCleanState}"),
        "typed-no-default post-init success arm must reply with the cleaned state. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_singleton_field_triggers_validation() {
    // type_annotation_display Singleton branch.
    let s = Span::new(0, 0);
    let singleton = TypeAnnotation::Singleton {
        name: "ok".into(),
        span: s,
    };
    let module = make_actor_typed_no_default("status", singleton);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "Singleton-typed field should trigger typed-no-default validation. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_generic_field_triggers_validation() {
    // type_annotation_display Generic branch.
    let s = Span::new(0, 0);
    let generic = TypeAnnotation::generic(
        Identifier::new("Collection", s),
        vec![TypeAnnotation::simple("Integer", s)],
        s,
    );
    let module = make_actor_typed_no_default("items", generic);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "Generic-typed field should trigger typed-no-default validation. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_false_or_field_triggers_validation() {
    // type_annotation_display FalseOr branch.
    let s = Span::new(0, 0);
    let false_or = TypeAnnotation::false_or(TypeAnnotation::simple("Integer", s), s);
    let module = make_actor_typed_no_default("result", false_or);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "FalseOr-typed field should trigger typed-no-default validation. Got:\n{code}"
    );
}

#[test]
fn test_actor_typed_class_of_field_triggers_validation() {
    // type_annotation_display ClassOf branch.
    let s = Span::new(0, 0);
    let class_of = TypeAnnotation::ClassOf {
        class_name: Identifier::new("Actor", s),
        span: s,
    };
    let module = make_actor_typed_no_default("actorClass", class_of);
    let result = generate_module(&module, CodegenOptions::new("bt@test_actor"));
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();
    assert!(
        code.contains("'uninitialized_state_error'"),
        "ClassOf-typed field should trigger typed-no-default validation. Got:\n{code}"
    );
}

#[test]
fn test_user_defined_initialize_chain_fallback_with_initialize() {
    // user_defined_initialize_chain fallback: class_hierarchy is
    // None, actor defines initialize → fallback returns chain containing the leaf.
    let src = concat!(
        "Actor subclass: TestActor\n",
        "  state: value = 0\n\n",
        "  initialize =>\n",
        "    self.value := 42\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let chain = generator.user_defined_initialize_chain(&module, "TestActor");
    assert_eq!(
        chain.len(),
        1,
        "Fallback should return one entry for the initialize method"
    );
    assert_eq!(
        chain[0].class_name, "TestActor",
        "Chain entry should name the leaf class"
    );
}

#[test]
fn test_user_defined_initialize_chain_fallback_without_initialize() {
    // user_defined_initialize_chain fallback: class_hierarchy is
    // None, no initialize method → fallback returns empty chain.
    let src = concat!(
        "Actor subclass: TestActor\n",
        "  state: value = 0\n",
        "  getValue => self.value\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let chain = generator.user_defined_initialize_chain(&module, "TestActor");
    assert!(
        chain.is_empty(),
        "No initialize method should produce an empty fallback chain"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_union_nil_excluded() {
    // inherited_typed_no_default_fields fallback: Union([Integer, Nil])
    // is nilable → field excluded from the typed-no-default list.
    let s = Span::new(0, 0);
    let union_nil = TypeAnnotation::union(
        vec![
            TypeAnnotation::simple("Integer", s),
            TypeAnnotation::simple("Nil", s),
        ],
        s,
    );
    let module = make_actor_typed_no_default("optValue", union_nil);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert!(
        fields.is_empty(),
        "Nil-union field is nilable so it should be excluded from typed-no-default"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_singleton_type_display() {
    // inherited_typed_no_default_fields fallback: Singleton type annotation →
    // type_annotation_display returns "#ok" → field included with the correct
    // display name.
    let s = Span::new(0, 0);
    let singleton = TypeAnnotation::Singleton {
        name: "ok".into(),
        span: s,
    };
    let module = make_actor_typed_no_default("status", singleton);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "Singleton field should appear in typed-no-default"
    );
    assert_eq!(fields[0].field_name, "status");
    assert_eq!(
        fields[0].type_name, "#ok",
        "Singleton display should be '#ok'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_self_type_display() {
    // type_annotation_display SelfType branch.
    let s = Span::new(0, 0);
    let self_type = TypeAnnotation::SelfType { span: s };
    let module = make_actor_typed_no_default("selfRef", self_type);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "SelfType field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Self",
        "SelfType display should be 'Self'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_self_class_display() {
    // type_annotation_display SelfClass branch.
    let s = Span::new(0, 0);
    let self_class = TypeAnnotation::SelfClass { span: s };
    let module = make_actor_typed_no_default("classRef", self_class);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "SelfClass field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Self class",
        "SelfClass display should be 'Self class'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_generic_type_display() {
    // type_annotation_display Generic branch: directly assert the fallback
    // display string (the generate_module test only checks it indirectly via
    // emitted Core Erlang).
    let s = Span::new(0, 0);
    let generic = TypeAnnotation::generic(
        Identifier::new("Collection", s),
        vec![TypeAnnotation::simple("Integer", s)],
        s,
    );
    let module = make_actor_typed_no_default("items", generic);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "Generic field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Collection(Integer)",
        "Generic display should be 'Collection(Integer)'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_class_of_type_display() {
    // type_annotation_display ClassOf branch: directly assert the fallback
    // display string (the generate_module test only checks it indirectly via
    // emitted Core Erlang).
    let s = Span::new(0, 0);
    let class_of = TypeAnnotation::ClassOf {
        class_name: Identifier::new("Actor", s),
        span: s,
    };
    let module = make_actor_typed_no_default("actorClass", class_of);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "ClassOf field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Actor class",
        "ClassOf display should be 'Actor class'"
    );
}

#[test]
fn test_inherited_typed_no_default_fallback_false_or_type_display() {
    // type_annotation_display FalseOr branch: directly assert the fallback
    // display string (the generate_module test only checks it indirectly via
    // emitted Core Erlang).
    let s = Span::new(0, 0);
    let false_or = TypeAnnotation::false_or(TypeAnnotation::simple("Integer", s), s);
    let module = make_actor_typed_no_default("result", false_or);
    let generator = crate::core_erlang::CoreErlangGenerator::new("bt@test_actor");
    let fields = generator.inherited_typed_no_default_fields(&module, "TestActor");
    assert_eq!(
        fields.len(),
        1,
        "FalseOr field should appear in typed-no-default"
    );
    assert_eq!(
        fields[0].type_name, "Integer | False",
        "FalseOr display should be 'Integer | False'"
    );
}

#[test]
fn test_bt_2720_native_object_instance_delegate_lowers_to_native_call() {
    // ADR 0101: an instance-side `self delegate` on a `native:`
    // Object lowers through beamtalk_erlang_proxy:native_call/4, prepending
    // Self and carrying {Class, Sel} context.
    let src = concat!(
        "Object subclass: Stream native: beamtalk_stream\n",
        "  select: predicate :: Block -> Object => self delegate\n",
        "  asList -> Object => self delegate\n",
    );
    let code = super::codegen(src);
    assert!(
        code.contains(
            "call 'beamtalk_erlang_proxy':'native_call'('beamtalk_stream', 'select', [Self, "
        ),
        "select: should lower to native_call('beamtalk_stream', 'select', [Self, Pred], ...). Got:\n{code}"
    );
    assert!(
        code.contains("{'Stream', 'select:'}"),
        "native_call should carry {{Class, Sel}} = {{'Stream', 'select:'}}. Got:\n{code}"
    );
    // Unary delegate: asList -> native_call(..., 'asList', [Self], {'Stream', 'asList'})
    assert!(
        code.contains("call 'beamtalk_erlang_proxy':'native_call'('beamtalk_stream', 'asList', [Self], {'Stream', 'asList'})"),
        "asList should lower to native_call('beamtalk_stream', 'asList', [Self], {{'Stream', 'asList'}}). Got:\n{code}"
    );
    // Must NOT emit a bare module:fn call for the delegate body.
    assert!(
        !code.contains("call 'beamtalk_stream':'select'"),
        "native: delegate must route through the proxy, not a bare beamtalk_stream:select. Got:\n{code}"
    );
}

#[test]
fn test_bt_2720_native_object_class_delegate_omits_self() {
    // ADR 0101: a class-side `self delegate` omits self from the arg
    // list (class methods are not instances).
    let src = concat!(
        "Object subclass: Stream native: beamtalk_stream\n",
        "  class from: start :: Integer -> Object => self delegate\n",
    );
    let code = super::codegen(src);
    assert!(
        code.contains("call 'beamtalk_erlang_proxy':'native_call'('beamtalk_stream', 'from', [")
            && code.contains("], {'Stream', 'from:'})"),
        "class from: should lower to native_call('beamtalk_stream', 'from', [Start], {{'Stream', 'from:'}}). Got:\n{code}"
    );
    // The class-side arg list must omit ClassSelf / ClassVars (class methods
    // are not instances).
    assert!(
        !code.contains("'native_call'('beamtalk_stream', 'from', [ClassSelf"),
        "class-side native_call must omit ClassSelf from the arg list. Got:\n{code}"
    );
}

/// BT-nightly: When an Actor subclass has BOTH an intermediate parent (`has_parent_init=true`)
/// AND its own `initialize` method (`has_initialize=true`), `init/1` must call the parent's
/// init, merge state, and then use the `__skip_initialize__` guard to defer initialize
/// dispatch to `handle_continue` — not call it inline.
#[test]
fn test_actor_with_parent_init_and_initialize_defers_to_handle_continue() {
    let src = concat!(
        "Counter subclass: LoggingInitCounter\n",
        "  state: logCount = 0\n\n",
        "  initialize =>\n",
        "    self.logCount := 0\n\n",
        "  increment =>\n",
        "    self.logCount := self.logCount + 1\n",
        "    super increment\n\n",
        "  getLogCount => self.logCount\n",
    );
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(&module, CodegenOptions::new("logging_init_counter"))
        .expect("codegen should succeed");

    // init/1 must delegate to the parent module's init/1.
    assert!(
        code.contains("'bt@counter':'init'("),
        "init/1 must call parent bt@counter:init/1. Got:\n{code}"
    );

    // Parent state must be merged with child fields.
    assert!(
        code.contains("ParentState"),
        "init/1 must bind parent state as ParentState. Got:\n{code}"
    );
    assert!(
        code.contains("FinalState"),
        "init/1 must produce FinalState. Got:\n{code}"
    );

    // Because initialize is defined, init/1 must use the __skip_initialize__ guard
    // and defer to handle_continue — NOT call initialize inline.
    assert!(
        code.contains("'__skip_initialize__'"),
        "init/1 must guard initialize dispatch with __skip_initialize__. Got:\n{code}"
    );
    assert!(
        code.contains("{'continue', 'initialize'}"),
        "init/1 must return {{continue, initialize}} to defer initialize. Got:\n{code}"
    );

    // handle_continue must exist and dispatch initialize.
    assert!(
        code.contains("'handle_continue'/2"),
        "Module must export handle_continue/2. Got:\n{code}"
    );
    assert!(
        code.contains("'safe_dispatch'('initialize'"),
        "handle_continue must dispatch initialize via safe_dispatch. Got:\n{code}"
    );
}

// ── Cross-file ancestor ClassInfo path ───────────────────────────────────────
//
// Target: gen_server/callbacks.rs — `is_nilable_type_name` and the ClassInfo
// branch of `inherited_typed_no_default_fields` (the `else if let Some(info)
// = hierarchy.get_class(&name)` arm).  Reached only when an ancestor class is
// absent from the current module's AST but present in the pre-loaded
// ClassHierarchy (BEAM metadata / cross-file compilation).

/// Exercises `is_nilable_type_name()` via the `ClassInfo` path in
/// `inherited_typed_no_default_fields()`.
///
/// A cross-file ancestor is injected via `CodegenOptions::with_class_hierarchy`.
/// Its typed-no-default fields exercise every branch of `is_nilable_type_name`:
///
/// - `nilField :: Nil`          → `type_name == "Nil"` returns true → excluded
/// - `nilUnionField :: Integer | Nil` → union `split(" | ").any(…)` → excluded
/// - `reqField :: Integer`       → neither branch → included → validation fires
///
/// The validation output for `reqField` confirms the `ClassInfo` loop ran.
/// The absence of `nilField` / `nilUnionField` in the output confirms the
/// nilability guards work correctly.
#[test]
fn test_cross_file_ancestor_nil_typed_fields_excluded_from_validation() {
    use beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo;
    use std::collections::HashMap;

    let ancestor = ClassInfo {
        surface_incomplete: false,
        name: ecow::EcoString::from("BaseActor"),
        superclass: Some(ecow::EcoString::from("Actor")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![
            ecow::EcoString::from("nilField"),
            ecow::EcoString::from("nilUnionField"),
            ecow::EcoString::from("reqField"),
        ],
        state_types: {
            let mut m = HashMap::new();
            m.insert(
                ecow::EcoString::from("nilField"),
                beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::parse("Nil"),
            );
            m.insert(
                ecow::EcoString::from("nilUnionField"),
                beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::parse(
                    "Integer | Nil",
                ),
            );
            m.insert(
                ecow::EcoString::from("reqField"),
                beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType::parse("Integer"),
            );
            m
        },
        state_has_default: {
            let mut m = HashMap::new();
            m.insert(ecow::EcoString::from("nilField"), false);
            m.insert(ecow::EcoString::from("nilUnionField"), false);
            m.insert(ecow::EcoString::from("reqField"), false);
            m
        },
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    // LogChild extends cross-file BaseActor; only LogChild's AST is present.
    let src = "BaseActor subclass: LogChild\n  logCount = 0\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let result = generate_module(
        &module,
        CodegenOptions::new("bt@log_child").with_class_hierarchy(vec![ancestor]),
    );
    assert!(result.is_ok(), "Codegen should succeed: {result:?}");
    let code = result.unwrap();

    // reqField :: Integer is not nilable → typed-no-default validation must fire.
    assert!(
        code.contains("'uninitialized_state_error'"),
        "Non-nilable cross-file ancestor field must trigger typed-no-default validation. Got:\n{code}"
    );
    assert!(
        code.contains("'reqField'"),
        "Non-nilable field 'reqField' must appear in the validation error hint. Got:\n{code}"
    );

    // nilField :: Nil — excluded by `type_name == "Nil"` branch of is_nilable_type_name.
    assert!(
        !code.contains("'nilField'"),
        "Nil-typed field must be excluded by is_nilable_type_name. Got:\n{code}"
    );

    // nilUnionField :: Integer | Nil — excluded by the union-split branch.
    assert!(
        !code.contains("'nilUnionField'"),
        "Integer|Nil union field must be excluded by is_nilable_type_name. Got:\n{code}"
    );

    // Because BaseActor ≠ Actor/Object (has_parent_init=true) AND reqField is a
    // typed-no-default field (has_initialize=true via chain_has_typed_no_default),
    // init/1 must call the parent and defer to handle_continue.
    assert!(
        code.contains("'bt@base_actor':'init'("),
        "init/1 must call parent bt@base_actor:init/1 for has_parent_init path. Got:\n{code}"
    );
    assert!(
        code.contains("{'continue', 'initialize'}"),
        "init/1 must return {{continue, initialize}} to defer post-initialize check. Got:\n{code}"
    );
}
