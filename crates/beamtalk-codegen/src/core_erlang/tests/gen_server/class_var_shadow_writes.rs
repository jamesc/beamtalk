// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Class-variable mutation and ADR 0110 shadow-write codegen,
//! including the nested-loop mutation shapes that must raise a
//! compile error rather than silently drop the shadow write.

use super::*;

#[test]
fn test_class_var_mutation_emits_shadow_write() {
    // ADR 0110: a top-frame class-var mutation in a class
    // method must write the just-updated ClassVars map into the
    // '$bt_class_vars_shadow' process-dictionary key, immediately after the
    // maps:put threading, so a foreign NLR relayed out of the method can
    // recover the mutation (read + erased by invoke_class_method, 7).
    let src = "Object subclass: ShadowCounter\n  classState: runs = 0\n\n  class bump =>\n    self.runs := self.runs + 1\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@shadowcounter").with_workspace_mode(true),
    )
    .expect("codegen should succeed");
    assert!(
        code.contains(
            "call 'erlang':'put'({'$bt_class_vars_shadow', call 'erlang':'element'(2, ClassSelf)}, ClassVars1)"
        ),
        "class-var mutation should emit the ADR 0110 (BT-3039) class-keyed shadow write. Got:\n{code}"
    );
}

#[test]
fn test_class_var_mutation_in_while_loop_body_compiles_and_threads_class_vars() {
    // A class-var mutation made directly inside a
    // whileTrue: loop body must not thread through
    // `generate_field_assignment_open`'s generic State/StateAcc mechanism —
    // that would silently write into the loop's own scratch StateAcc map
    // instead of ClassVars, losing the mutation on both normal return and a
    // foreign NLR escape (confirmed empirically via a throwaway BUnit
    // driver/probe fixture, mirroring fixtures/collection_driver.bt/
    // collection_probe.bt's shape with the mutation moved inside the loop) —
    // the same "can't thread this state" shape `FieldAssignmentInUnsupportedBlock`
    // rejects elsewhere. `ClassVars` (ADR 0111 Addendum 9) instead threads
    // through the loop's own recursive tail call as an extra fun parameter,
    // tagged with the loop's real frame and a real ADR 0110 shadow write
    // each iteration. See
    // `stdlib/test/loop_class_var_mutation_test.bt`'s
    // `testFieldAssignmentInWhileLoopAccumulates` for the
    // runtime-behavior pin (this test only pins the codegen shape).
    let src = "Object subclass: LoopShadowCounter\n  classState: runs = 0\n\n  class countUpTo: n =>\n    i := 0\n    [i < n] whileTrue: [\n      self.runs := self.runs + 1\n      i := i + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@loopshadowcounter").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!("self.runs := ... inside a whileTrue: body must compile. Got: {e:?}")
    });
    assert!(
        code.contains("fun (StateAcc, ClassVars)"),
        "The whileTrue: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
    assert!(
        code.contains(
            "call 'erlang':'put'({'$bt_class_vars_shadow', call 'erlang':'element'(2, ClassSelf)}, ClassVars1)"
        ),
        "the loop-body mutation must still emit the ADR 0110 shadow write. Got:\n{code}"
    );
}

#[test]
fn test_bare_class_var_mutation_in_times_repeat_body_hits_existing_stored_closure_guard() {
    // `needs_mutation_threading` deliberately excludes bare
    // field writes/self-sends from triggering StateAcc threading in a class
    // method — a `timesRepeat:` body with ONLY a class-var write and no other
    // mutation never reaches `generate_threaded_loop_body`/
    // `generate_field_assignment_open` at all; it falls through to the
    // generic block path and is already caught by
    // `FieldAssignmentInUnsupportedBlock` (`validate_stored_closure`). Pinned
    // here as the contrast case to the co-occurring-local-mutation shape
    // below, which DOES reach the class-var-threading construct above.
    let src = "Object subclass: TimesRepeatShadowCounter\n  classState: runs = 0\n\n  class bumpN: n =>\n    n timesRepeat: [self.runs := self.runs + 1]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@timesrepeatshadowcounter").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::FieldAssignmentInUnsupportedBlock { .. })
        ),
        "Expected the pre-existing FieldAssignmentInUnsupportedBlock guard for a \
         bare class-var mutation (no co-occurring local/self-send mutation) inside \
         a timesRepeat: body. Got: {result:?}"
    );
}

#[test]
fn test_class_var_mutation_alongside_local_in_times_repeat_body_compiles() {
    // once a `timesRepeat:` body ALSO has a local-variable
    // mutation (or self-send), `needs_mutation_threading` fires for that
    // reason and the body IS routed through `generate_threaded_loop_body` —
    // this is the shape that actually matters: a loop that legitimately
    // needs local threading (an accumulator, a counter) with a class-var
    // write alongside it. Now compiles and threads ClassVars correctly
    // (ADR 0111 Addendum 9) instead of being rejected.
    let src = "Object subclass: TimesRepeatShadowCounter2\n  classState: runs = 0\n\n  class bumpN: n =>\n    seen := 0\n    n timesRepeat: [\n      self.runs := self.runs + 1\n      seen := seen + 1\n    ]\n    seen";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@timesrepeatshadowcounter2").with_workspace_mode(true),
    );
    let code = result.unwrap_or_else(|e| {
        panic!(
            "self.runs := ... alongside a local mutation inside a timesRepeat: body must \
             compile. Got: {e:?}"
        )
    });
    assert!(
        code.contains(", StateAcc, ClassVars) ->"),
        "The timesRepeat: letrec fun must thread ClassVars as an extra param. Got:\n{code}"
    );
}

#[test]
fn test_class_var_mutation_before_loop_still_emits_shadow_write() {
    // contrast case: a class-var mutation BEFORE a whileTrue: loop
    // (top frame, block_depth == 0, not inside the loop's threaded body) is
    // the already-proven ADR 0110 shape and must keep compiling + emitting
    // the shadow write — the class-var-threading rejection is scoped to
    // mutations literally inside the loop body, not merely a method that
    // also has one.
    let src = "Object subclass: LoopShadowCounterOk\n  classState: runs = 0\n\n  class bumpThenLoop: n =>\n    self.runs := self.runs + 1\n    i := 0\n    [i < n] whileTrue: [i := i + 1]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@loopshadowcounterok").with_workspace_mode(true),
    )
    .expect("mutation before the loop, at top frame, must still compile");
    assert!(
        code.contains("$bt_class_vars_shadow"),
        "top-frame class-var mutation before the loop must still emit the \
         ADR 0110 shadow write. Got:\n{code}"
    );
}

#[test]
fn test_nested_letrec_direct_field_mutation_in_inner_loop_is_compile_error() {
    // (follow-up): a `Letrec` loop nested inside another
    // `Letrec` loop, where the INNER loop directly mutates a class var, but
    // the OUTER loop's own top-level statements (`j := 0`, the inner
    // `whileTrue:` send, `i := i + 1`) have no bare class-var mutation of
    // their own — so `loop_body_threads_class_vars` correctly returns
    // `false` for the outer loop (per its own narrow, top-level-only
    // design), meaning the outer loop's fun/tail call never threads
    // `ClassVars` at all. Before this fix, this compiled successfully but
    // silently discarded every inner-loop mutation (confirmed empirically:
    // both the method's own return and a later fresh read of the class var
    // returned `0` instead of `9` for `nestedBump: 3`). Now rejected at
    // compile time instead — see
    // `CodeGenError::ClassVarMutationLostAcrossNestedLoop`'s doc comment.
    let src = "Object subclass: NestedLoopShadowCounter\n  classState: runs = 0\n\n  class nestedBump: n =>\n    i := 0\n    [i < n] whileTrue: [\n      j := 0\n      [j < n] whileTrue: [\n        self.runs := self.runs + 1\n        j := j + 1\n      ]\n      i := i + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedloopshadowcounter").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "class variable 'runs'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a class-var write inside a \
             loop nested inside another loop. Got: {other:?}"
        ),
    }
}

#[test]
fn test_nested_letrec_self_send_mutation_in_inner_loop_is_compile_error() {
    // The same gap via a same-class self-send (the same shape as
    // `test_nested_letrec_direct_field_mutation_in_inner_loop_is_compile_error`
    // above) inside the inner loop instead of a direct field write — the inner
    // loop's own `loop_body_threads_class_vars` matches `is_class_method_self_send`,
    // not `is_class_var_assignment`, but the outer loop's discard is
    // identical either way.
    let src = "Object subclass: NestedLoopSelfSendCounter\n  classState: runs = 0\n\n  class bump => self.runs := self.runs + 1\n\n  class nestedBumpViaSelfSend: n =>\n    i := 0\n    [i < n] whileTrue: [\n      j := 0\n      [j < n] whileTrue: [\n        self bump\n        j := j + 1\n      ]\n      i := i + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedloopselfsendcounter").with_workspace_mode(true),
    );
    match result {
        Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { mutation, .. }) => {
            assert_eq!(mutation, "'self bump'");
        }
        other => panic!(
            "Expected ClassVarMutationLostAcrossNestedLoop for a self-send inside a loop \
             nested inside another loop. Got: {other:?}"
        ),
    }
}

#[test]
fn test_nested_timesrepeat_class_var_mutation_in_inner_loop_is_compile_error() {
    // the same gap via `timesRepeat:`/`to:do:` nesting, not just
    // `whileTrue:` — `nested_letrec_loop_body` must recognize all four
    // Letrec-shaped loop selectors, not just `whileTrue:`/`whileFalse:`. The
    // outer body needs its own co-occurring local-variable mutation
    // (`seen := seen + 1`) so the OUTER `timesRepeat:` itself reaches
    // `generate_threaded_loop_body` (mirroring `bumpTimes:` in
    // `loop_class_var_mutation.bt`) instead of being caught earlier, for an
    // unrelated reason, by the generic `FieldAssignmentInUnsupportedBlock`
    // block-validator guard that a bare-class-var-only body (no co-occurring
    // local mutation) hits regardless of nesting.
    let src = "Object subclass: NestedCountedLoopCounter\n  classState: runs = 0\n\n  class nestedBumpTimes: n =>\n    seen := 0\n    n timesRepeat: [\n      n timesRepeat: [\n        self.runs := self.runs + 1\n      ]\n      seen := seen + 1\n    ]\n    self.runs";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let result = generate_module(
        &module,
        CodegenOptions::new("bt@nestedcountedloopcounter").with_workspace_mode(true),
    );
    assert!(
        matches!(
            result,
            Err(CodeGenError::ClassVarMutationLostAcrossNestedLoop { .. })
        ),
        "Expected ClassVarMutationLostAcrossNestedLoop for a class-var write inside a \
         timesRepeat: nested inside another timesRepeat:. Got: {result:?}"
    );
}

#[test]
fn test_instance_field_mutation_does_not_emit_shadow_write() {
    // ADR 0110: the shadow write is scoped to class-var mutations in class
    // methods — ordinary actor state threading must not gain a pdict write.
    let src = "Actor subclass: PlainCounter\n  state: count = 0\n\n  bump => self.count := self.count + 1";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let code = generate_module(
        &module,
        CodegenOptions::new("bt@plaincounter").with_workspace_mode(true),
    )
    .expect("codegen should succeed");
    assert!(
        !code.contains("$bt_class_vars_shadow"),
        "instance field mutation must not emit the class-var shadow write. Got:\n{code}"
    );
}

#[test]
fn test_bt1213_block_value_with_captured_mutation_actor() {
    // [count := count + 1] value in actor context
    // Parse from source to get a realistic AST
    // Build AST manually: Object subclass: BT1213Actor
    //   testIt => count := 0. [count := count + 1] value. count
    let s = Span::new(0, 0);
    let count_id = || Expression::Identifier(Identifier::new("count", s));

    // count := count + 1
    let add_expr = Expression::MessageSend {
        receiver: Box::new(count_id()),
        selector: MessageSelector::Binary("+".into()),
        arguments: vec![Expression::Literal(Literal::Integer(1), s)],
        is_cast: false,
        span: s,
    };
    let assign = Expression::Assignment {
        target: Box::new(count_id()),
        value: Box::new(add_expr),
        type_annotation: None,
        span: s,
    };

    // [count := count + 1] value
    let block = Block::new(vec![], vec![bare(assign)], s);
    let block_value = Expression::MessageSend {
        receiver: Box::new(Expression::Block(block)),
        selector: MessageSelector::Unary("value".into()),
        arguments: vec![],
        is_cast: false,
        span: s,
    };

    // count := 0
    let init_count = Expression::Assignment {
        target: Box::new(count_id()),
        value: Box::new(Expression::Literal(Literal::Integer(0), s)),
        type_annotation: None,
        span: s,
    };

    let method = MethodDefinition::new(
        MessageSelector::Unary("testIt".into()),
        vec![],
        vec![bare(init_count), bare(block_value), bare(count_id())],
        s,
    );

    let class = ClassDefinition {
        name: Identifier::new("BT1213Actor", s),
        superclass: Some(Identifier::new("Actor", s)),
        superclass_package: None,
        class_kind: ClassKind::Actor,
        is_abstract: false,
        is_sealed: false,
        is_typed: false,
        is_internal: false,
        supervisor_kind: None,
        state: vec![],
        methods: vec![method],
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
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: Span::new(0, 0),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };

    let code = generate_module(&module, CodegenOptions::new("bt@bt1213_actor"))
        .expect("codegen should work");

    // Actor codegen should thread count through StateAcc
    assert!(
        code.contains("__local__count"),
        "Should thread count through StateAcc. Got:\n{code}"
    );
}
