// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `ifTrue:`/`ifFalse:` conditional codegen with field mutation:
//! inline-case vs runtime-dispatch selection and nested
//! conditionals.

use super::*;

#[test]
fn test_if_true_with_field_mutation_generates_inline_case() {
    // `flag ifTrue: [self.count := self.count + 1]` inside an actor method
    // should compile to an inline case expression that threads state through the
    // true branch, returning {Result, NewState} so the outer method body can
    // update its state chain.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  increment: flag =>\n    flag ifTrue: [self.count := self.count + 1].\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
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
    // `flag ifFalse: [self.count := self.count - 1]` should compile to
    // an inline case expression with state threading in the false branch.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  decrement: flag =>\n    flag ifFalse: [self.count := self.count - 1].\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
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
    // `flag ifTrue: [...] ifFalse: [...]` with field mutations in both
    // branches should compile to an inline case with two state-threading branches.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  toggle: flag =>\n    flag ifTrue: [self.count := self.count + 10]\n         ifFalse: [self.count := self.count - 1].\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
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
    // `flag ifTrue: [42]` with no mutations should still use runtime dispatch,
    // not the inline case generation. This ensures we don't break the non-mutation path.
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  check: flag =>\n    flag ifTrue: [42]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
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
    // Nested `flag1 ifTrue: [flag2 ifTrue: [self.count := ...]]` should
    // correctly unpack the inner {Result, State} tuple so the outer branch
    // threads state from the inner conditional.
    let src = "Actor subclass: Ctr\n  state: count = 0\n\n  nested: a and: b =>\n    a ifTrue: [\n      b ifTrue: [self.count := self.count + 100]\n    ].\n    self.count\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _diags) = beamtalk_core::source_analysis::parse(tokens);
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
