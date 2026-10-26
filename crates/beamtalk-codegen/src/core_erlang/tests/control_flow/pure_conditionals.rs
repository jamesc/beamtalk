// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4070: pure-conditional inline — `ifTrue:`/`ifFalse:`/
//! `ifTrue:ifFalse:`/`and:`/`or:` with literal, non-mutating blocks compile
//! to an inline `case` over the once-evaluated receiver, with a dynamic-send
//! fallback arm for non-Boolean receivers — in every context (value-type,
//! actor, class-method, REPL).

use super::*;

fn compile(src: &str, module: &str) -> String {
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module_ast, _diags) = beamtalk_core::source_analysis::parse(tokens);
    generate_module(&module_ast, CodegenOptions::new(module)).expect("codegen should succeed")
}

/// The module sliced to the pure-inline `case`: everything from its `in
/// case _CondN of` scrutinee on. Module boilerplate (`init/1`, `spawn`,
/// metadata) holds its own `case` expressions, so every marker search below
/// runs in this scope, where the first match is the conditional's own.
pub(super) fn case_scope(code: &str) -> &str {
    let marker = " in case _Cond";
    let start = code
        .find(marker)
        .expect("expected a pure-inline `in case _CondN of`");
    &code[start..]
}

/// The `_CondN` variable the pure-inline case scrutinizes.
pub(super) fn cond_var(scope: &str) -> &str {
    let marker = " in case ";
    let start = scope
        .find(marker)
        .expect("expected a pure-inline case scrutinee")
        + marker.len();
    let rest = &scope[start..];
    let end = rest
        .find(|c: char| !(c.is_alphanumeric() || c == '_'))
        .unwrap_or(rest.len());
    &scope[start..start + end]
}

/// The arm body between two clause markers, whitespace-stripped (robust to
/// pretty-printer line wrapping).
pub(super) fn arm_between(scope: &str, start_marker: &str, end_marker: &str) -> String {
    let start = scope
        .find(start_marker)
        .unwrap_or_else(|| panic!("expected arm marker {start_marker:?}"))
        + start_marker.len();
    let rest = &scope[start..];
    let end = rest
        .find(end_marker)
        .unwrap_or_else(|| panic!("expected arm end {end_marker:?} after {start_marker:?}"));
    rest[..end].chars().filter(|c| !c.is_whitespace()).collect()
}

/// The emitted `case` before its `CondNoMatch` fallback arm: the two Boolean
/// arms, which must hold no dynamic send.
pub(super) fn boolean_arms(scope: &str) -> &str {
    let end = scope
        .find("CondNoMatch")
        .expect("expected a CondNoMatch fallback arm");
    &scope[..end]
}

#[test]
fn if_true_pure_answers_receiver_on_false_branch() {
    // `flag ifTrue: [1]` with pure blocks: `true -> 1`, `false -> C`
    // (matching `False>>ifTrue: => self`), `_ -> send`.
    let src = "Value subclass: Probe\n  check: flag =>\n    flag ifTrue: [1]\n";
    let code = compile(src, "bt@probe_if_true_pure");
    eprintln!("ifTrue: pure:\n{code}");
    let scope = case_scope(&code);

    let cond = cond_var(scope).to_string();
    assert_eq!(
        arm_between(scope, "<'true'> when 'true' ->", "<'false'>"),
        "1",
        "true arm must inline the block body. Got:\n{code}"
    );
    assert_eq!(
        arm_between(scope, "<'false'> when 'true' ->", "<_CondNoMatch"),
        cond,
        "false arm must answer the receiver, not nil. Got:\n{code}"
    );
    assert!(
        !boolean_arms(scope).contains("beamtalk_message_dispatch':'send'"),
        "Boolean arms must hold no dynamic send. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'"),
        "fallback arm must keep the dynamic send. Got:\n{code}"
    );
    assert!(
        code.contains("'ifTrue:'"),
        "fallback must send the same selector. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_if_true_pure", &code);
}

#[test]
fn if_false_pure_answers_receiver_on_true_branch() {
    let src = "Value subclass: Probe\n  check: flag =>\n    flag ifFalse: [2]\n";
    let code = compile(src, "bt@probe_if_false_pure");
    eprintln!("ifFalse: pure:\n{code}");
    let scope = case_scope(&code);

    let cond = cond_var(scope).to_string();
    assert_eq!(
        arm_between(scope, "<'true'> when 'true' ->", "<'false'>"),
        cond,
        "true arm must answer the receiver, not nil. Got:\n{code}"
    );
    assert_eq!(
        arm_between(scope, "<'false'> when 'true' ->", "<_CondNoMatch"),
        "2",
        "false arm must inline the block body. Got:\n{code}"
    );
    assert!(
        !boolean_arms(scope).contains("beamtalk_message_dispatch':'send'"),
        "Boolean arms must hold no dynamic send. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_if_false_pure", &code);
}

#[test]
fn if_true_if_false_pure_inlines_both_branches() {
    // The issue's repro shape: `(x =:= 1) ifTrue: [1] ifFalse: [2]`.
    let src = "typed Value subclass: ProbeTyped\n  c: x :: Integer -> Integer =>\n    (x =:= 1) ifTrue: [1] ifFalse: [2]\n";
    let code = compile(src, "bt@probe_if_true_if_false_pure");
    eprintln!("ifTrue:ifFalse: pure:\n{code}");
    let scope = case_scope(&code);

    assert_eq!(
        arm_between(scope, "<'true'> when 'true' ->", "<'false'>"),
        "1",
        "true arm must inline the block body. Got:\n{code}"
    );
    assert_eq!(
        arm_between(scope, "<'false'> when 'true' ->", "<_CondNoMatch"),
        "2",
        "false arm must inline the block body. Got:\n{code}"
    );
    assert!(
        !boolean_arms(scope).contains("beamtalk_message_dispatch':'send'"),
        "Boolean arms must hold no dynamic send. Got:\n{code}"
    );
    // The fallback sends with both closures (scoped to the send call
    // itself: the module holds other `fun`s past the case).
    let send_start = scope.find("beamtalk_message_dispatch':'send'(").unwrap();
    let send_end = scope[send_start..].find("])").unwrap() + send_start;
    let fallback = &scope[send_start..send_end];
    assert_eq!(
        fallback.matches("fun (").count(),
        2,
        "fallback must allocate both closures. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_if_true_if_false_pure", &code);
}

#[test]
fn and_pure_short_circuits_to_literal_false() {
    let src = "Value subclass: Probe\n  check: flag =>\n    flag and: [flag]\n";
    let code = compile(src, "bt@probe_and_pure");
    eprintln!("and: pure:\n{code}");
    let scope = case_scope(&code);

    assert_eq!(
        arm_between(scope, "<'false'> when 'true' ->", "<_CondNoMatch"),
        "'false'",
        "false arm must be the literal false (Boolean>>and:). Got:\n{code}"
    );
    assert!(
        !boolean_arms(scope).contains("beamtalk_message_dispatch':'send'"),
        "Boolean arms must hold no dynamic send. Got:\n{code}"
    );
    assert!(
        code.contains("'and:'"),
        "fallback must send the same selector. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_and_pure", &code);
}

#[test]
fn or_pure_short_circuits_to_literal_true() {
    let src = "Value subclass: Probe\n  check: flag =>\n    flag or: [flag]\n";
    let code = compile(src, "bt@probe_or_pure");
    eprintln!("or: pure:\n{code}");
    let scope = case_scope(&code);

    assert_eq!(
        arm_between(scope, "<'true'> when 'true' ->", "<'false'>"),
        "'true'",
        "true arm must be the literal true (Boolean>>or:). Got:\n{code}"
    );
    assert!(
        !boolean_arms(scope).contains("beamtalk_message_dispatch':'send'"),
        "Boolean arms must hold no dynamic send. Got:\n{code}"
    );
    assert!(
        code.contains("'or:'"),
        "fallback must send the same selector. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_or_pure", &code);
}

#[test]
fn pure_inline_in_actor_method() {
    // The gate sits outside the Actor/Repl/loop-body early return, so an
    // actor method with pure blocks inlines (plain value, no tuple).
    let src = "Actor subclass: Ctr\n  state: x = 0\n\n  check: flag =>\n    flag ifTrue: [42]\n";
    let code = compile(src, "ctr_pure_actor");
    eprintln!("actor ifTrue: pure:\n{code}");
    let scope = case_scope(&code);

    let cond = cond_var(scope).to_string();
    assert_eq!(
        arm_between(scope, "<'false'> when 'true' ->", "<_CondNoMatch"),
        cond,
        "false arm must answer the receiver, not nil. Got:\n{code}"
    );
    assert!(
        !boolean_arms(scope).contains("beamtalk_message_dispatch':'send'"),
        "Boolean arms must hold no dynamic send. Got:\n{code}"
    );
    assert_compiles_through_erlc("ctr_pure_actor", &code);
}

#[test]
fn pure_inline_in_class_method() {
    let src = concat!(
        "Object subclass: Klassed\n",
        "  class pick: flag =>\n",
        "    flag ifTrue: [1] ifFalse: [2]\n",
    );
    let code = compile(src, "bt@klassed_pure");
    eprintln!("class-method ifTrue:ifFalse: pure:\n{code}");
    let scope = case_scope(&code);

    assert_eq!(
        arm_between(scope, "<'true'> when 'true' ->", "<'false'>"),
        "1",
        "true arm must inline the block body. Got:\n{code}"
    );
    assert!(
        !boolean_arms(scope).contains("beamtalk_message_dispatch':'send'"),
        "Boolean arms must hold no dynamic send. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@klassed_pure", &code);
}

#[test]
fn pure_inline_at_repl() {
    // The REPL compiles the input through the same handler chain, so a
    // pure conditional inlines there too (no bindings-tuple unpacking:
    // the gate excludes binding writes).
    use crate::core_erlang::{CodeGenContext, CoreErlangGenerator};
    let src = "flag ifTrue: [1] ifFalse: [2]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module_ast, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let mut generator = CoreErlangGenerator::new("repl_pure");
    generator.context = CodeGenContext::Repl;
    generator.set_is_repl_mode(true);
    generator.push_scope();
    generator.bind_var("__bindings__", "Bindings");
    generator.bind_var("flag", "Flag");
    let mut docs = Vec::new();
    for stmt in &module_ast.expressions {
        let (doc, _mutated) = generator
            .expression_doc_with_repl_mutation_tracking(&stmt.expression)
            .expect("repl codegen should succeed");
        docs.push(doc);
    }
    let code = docs
        .iter()
        .map(beamtalk_cerl_doc::Document::to_pretty_string)
        .collect::<Vec<_>>()
        .join("\n");
    eprintln!("repl ifTrue:ifFalse: pure:\n{code}");

    assert!(
        code.contains("case "),
        "REPL pure conditional must inline a case. Got:\n{code}"
    );
    assert!(
        code.contains("CondNoMatch"),
        "REPL case must keep the send fallback. Got:\n{code}"
    );
}

#[test]
fn arity_1_block_keeps_runtime_dispatch() {
    // `flag ifTrue: [:x | x]` keeps today's runtime arity-error behavior:
    // no inline case, a plain dynamic send.
    let src = "Value subclass: Probe\n  check: flag =>\n    flag ifTrue: [:x | x]\n";
    let code = compile(src, "bt@probe_arity_1");
    eprintln!("arity-1 ifTrue::\n{code}");

    assert!(
        !code.contains("CondNoMatch"),
        "arity-1 block must not pure-inline. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'"),
        "arity-1 block must keep the dynamic send. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_arity_1", &code);
}

#[test]
fn parenthesized_block_keeps_runtime_dispatch() {
    // The gate matches direct literal blocks only (same as the mutation
    // gate): `flag ifTrue: ([1])` stays dynamic.
    let src = "Value subclass: Probe\n  check: flag =>\n    flag ifTrue: ([1])\n";
    let code = compile(src, "bt@probe_paren_block");
    eprintln!("parenthesized-block ifTrue::\n{code}");

    assert!(
        !code.contains("CondNoMatch"),
        "parenthesized block must not pure-inline. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_paren_block", &code);
}

#[test]
fn block_variable_keeps_runtime_dispatch() {
    // `cond ifTrue: b` (a block held in a variable) stays dynamic.
    let src = "Value subclass: Probe\n  check: flag =>\n    b := [1].\n    flag ifTrue: b\n";
    let code = compile(src, "bt@probe_block_var");
    eprintln!("block-variable ifTrue::\n{code}");

    assert!(
        !code.contains("CondNoMatch"),
        "block variable must not pure-inline. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_block_var", &code);
}

#[test]
fn self_receiver_keeps_self_dispatch() {
    // `self ifTrue: [...]` keeps today's dispatch: in an actor method the
    // self-send producer claims it; in a class method the direct
    // class-side call does (a runtime send there could deadlock).
    let actor_src = "Actor subclass: Ctr\n  state: x = 0\n\n  check =>\n    self ifTrue: [1]\n";
    let actor_code = compile(actor_src, "ctr_self_recv");
    eprintln!("actor self ifTrue::\n{actor_code}");
    assert!(
        !actor_code.contains("CondNoMatch"),
        "actor self-receiver must not pure-inline. Got:\n{actor_code}"
    );

    let class_src = concat!(
        "Object subclass: Klassed\n",
        "  class check =>\n",
        "    self ifTrue: [1]\n",
    );
    let class_code = compile(class_src, "bt@klassed_self_recv");
    eprintln!("class-method self ifTrue::\n{class_code}");
    assert!(
        !class_code.contains("CondNoMatch"),
        "class-method self-receiver must not pure-inline. Got:\n{class_code}"
    );
    assert_compiles_through_erlc("bt@klassed_self_recv", &class_code);
}

#[test]
fn class_reference_receiver_keeps_class_dispatch() {
    let src = concat!(
        "Object subclass: Klassed\n",
        "  class check =>\n",
        "    Klassed ifTrue: [1]\n",
    );
    let code = compile(src, "bt@klassed_ref_recv");
    eprintln!("class-reference ifTrue::\n{code}");
    assert!(
        !code.contains("CondNoMatch"),
        "class-reference receiver must not pure-inline. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@klassed_ref_recv", &code);
}

#[test]
fn receiver_evaluated_once() {
    // A side-effecting pure receiver (`counter next`, a non-self send in a
    // value-type method) is evaluated once and bound, not re-evaluated
    // per arm.
    let src = "Value subclass: Probe\n  check: counter =>\n    counter next ifTrue: [1]\n";
    let code = compile(src, "bt@probe_recv_once");
    eprintln!("receiver-once ifTrue::\n{code}");

    assert!(
        code.contains("CondNoMatch"),
        "expected a pure-inline case. Got:\n{code}"
    );
    let scope = case_scope(&code);
    let fallback = &scope[scope.find("CondNoMatch").unwrap()..];
    assert!(
        fallback.contains("beamtalk_message_dispatch':'send'"),
        "the case must be the pure-inline one (send fallback), not the mutation path. Got:\n{code}"
    );
    // Scoped to the send shape (`'next', [`): the method-xref metadata
    // also names the selector.
    assert_eq!(
        code.matches("'next', [").count(),
        1,
        "receiver send must be evaluated exactly once. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_recv_once", &code);
}

#[test]
fn nonlocal_return_in_pure_branch_throws() {
    // `^` in a pure branch keeps non-local-return semantics: the method
    // installs its NLR catch (the `^` sits in a block literal, as far as
    // detection is concerned) and the inlined arm throws to it.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  check: flag =>\n",
        "    flag ifTrue: [^42].\n",
        "    0\n",
    );
    let code = compile(src, "ctr_pure_nlr");
    eprintln!("nlr ifTrue: pure:\n{code}");

    assert!(
        code.contains("CondNoMatch"),
        "expected a pure-inline case. Got:\n{code}"
    );
    let scope = case_scope(&code);
    let fallback = &scope[scope.find("CondNoMatch").unwrap()..];
    assert!(
        fallback.contains("beamtalk_message_dispatch':'send'"),
        "the case must be the pure-inline one (send fallback), not the mutation path. Got:\n{code}"
    );
    assert!(
        code.contains("'throw'({'$bt_nlr',"),
        "the branch's early return must throw the NLR tuple. Got:\n{code}"
    );
    assert_compiles_through_erlc("ctr_pure_nlr", &code);
}

#[test]
fn block_local_temp_does_not_leak_past_case() {
    // A temp assigned only inside a pure arm (`[w := 5. w]`, no outer `w`
    // — a write to an outer local takes the mutation path instead) is
    // bound in the arm's own scope: each arm compiles in a pushed scope,
    // as every closed-`fun` lowering does.
    let src = concat!(
        "Value subclass: Probe\n",
        "  check: flag =>\n",
        "    flag ifTrue: [w := 5. w].\n",
        "    99\n",
    );
    let code = compile(src, "bt@probe_temp_scope");
    eprintln!("temp-scope ifTrue: pure:\n{code}");

    assert!(
        code.contains("CondNoMatch"),
        "expected a pure-inline case. Got:\n{code}"
    );
    let scope = case_scope(&code);
    let fallback = &scope[scope.find("CondNoMatch").unwrap()..];
    assert!(
        fallback.contains("beamtalk_message_dispatch':'send'"),
        "the case must be the pure-inline one (send fallback), not the mutation path. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_temp_scope", &code);
}

#[test]
fn guarded_operator_in_actor_arm_stays_dynamic() {
    // In an actor instance method (where calls are bracketed), a branch
    // holding a guarded operator keeps dynamic dispatch: the arm lowers
    // in a closure frame whose `Loop(DirectParams)` kind the ADR 0137
    // bracket machinery would read as a stateless loop, tripping
    // `UnbracketedGuardedOperator` (see
    // `try_generate_pure_boolean_conditional`'s doc comment).
    // Fixture: `stdlib/test/fixtures/actor_nlr_computed.bt`.
    let src = "Actor subclass: ActorNlrComputed\n  state: x = 0\n\n  doubleIfPositive: n =>\n    n > 0 ifTrue: [^n * 2]\n    0\n";
    let code = compile(src, "bt@nlrcomputed");
    eprintln!("guarded-op actor arm:\n{code}");

    assert!(
        !code.contains("CondNoMatch"),
        "guarded-operator arm must not pure-inline. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'"),
        "guarded-operator arm must keep the dynamic send. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@nlrcomputed", &code);
}

#[test]
fn guarded_operator_in_actor_receiver_still_inlines() {
    // The receiver compiles in the enclosing (truthful) frame via
    // `compile_conditional_receiver`, so its operators bracket normally
    // and the gate still fires.
    let src =
        "Actor subclass: Ctr\n  state: x = 0\n\n  check: n =>\n    (n * 2) > 10 ifTrue: [1]\n";
    let code = compile(src, "ctr_guarded_recv");
    eprintln!("guarded-op actor receiver:\n{code}");

    let scope = case_scope(&code);
    let fallback = &scope[scope.find("CondNoMatch").unwrap()..];
    assert!(
        fallback.contains("beamtalk_message_dispatch':'send'"),
        "the case must be the pure-inline one (send fallback). Got:\n{code}"
    );
    assert_compiles_through_erlc("ctr_guarded_recv", &code);
}

#[test]
fn stateful_while_condition_tail_keeps_dynamic_dispatch() {
    // BT-4070: a stateful while-condition tail keeps today's dynamic
    // dispatch: its value splices into the `{Bool, StateAcc[, Self]}`
    // hand-back tuple, where a bare `case` breaks erlc's `beam_clean`
    // (`undefined_label`) in complex nestings. Shape minimized from
    // `while_condition_carrier_corpus` run70/run139.
    let src = concat!(
        "Value subclass: Probe\n",
        "  field: f = 0\n",
        "  run =>\n",
        "    loc0 := [self.f := self.f + 1. (self done) and: [\"Q\"]] whileFalse: [1].\n",
        "    0\n",
    );
    let code = compile(src, "bt@probe_splice_while_carry");
    eprintln!("decline while-carry:\n{code}");

    assert!(
        !code.contains("CondNoMatch"),
        "condition tail must not pure-inline. Got:\n{code}"
    );
    assert!(
        code.contains("beamtalk_message_dispatch':'send'"),
        "condition tail must keep the dynamic send. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_splice_while_carry", &code);
}

#[test]
fn stateful_while_condition_full_shape_compiles() {
    // The corpus run139 shape verbatim (body writes outer locals, NLR
    // tail after the loop): the other half of the minimized pair.
    let src = concat!(
        "Value subclass: Probe\n",
        "  field: f = 0\n",
        "  run139 =>\n",
        "    loc0 := [self.f := self.f + 1. (self done) and: [\"Q\"]]\n",
        "      whileFalse: [loc0 := false. loc1 := nil. 1428995853].\n",
        "    [^loc0] value\n",
    );
    let code = compile(src, "bt@probe_splice_while_full");
    eprintln!("decline while-full:\n{code}");

    assert!(
        !code.contains("CondNoMatch"),
        "condition tail must not pure-inline. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_splice_while_full", &code);
}

#[test]
fn stateful_while_condition_plain_tuple_keeps_dynamic_dispatch() {
    // Same decline on the `{Value, StateAcc}` (no `Self` slot) path: a
    // local-threading condition with a pure tail.
    let src = concat!(
        "Value subclass: Probe\n",
        "  field: f = 0\n",
        "  run: c =>\n",
        "    self.f := 1.\n",
        "    t := 0.\n",
        "    [t := t + 1. c ifTrue: [t > 0] ifFalse: [false]] whileTrue: [1].\n",
        "    t\n",
    );
    let code = compile(src, "bt@probe_splice_while_plain");
    eprintln!("decline while-plain:\n{code}");

    assert!(
        !code.contains("CondNoMatch"),
        "condition tail must not pure-inline. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_splice_while_plain", &code);
}

#[test]
fn mutation_arm_tail_still_inlines() {
    // A mutation arm whose tail is a pure conditional (C13 closer) keeps
    // inlining: only stateful loop-condition tails decline.
    let src = concat!(
        "Actor subclass: Ctr\n",
        "  state: x = 0\n\n",
        "  run: flag other: other =>\n",
        "    self.x := self.x + 1.\n",
        "    flag ifTrue: [self.x := self.x + 1. other ifTrue: [1] ifFalse: [2]].\n",
        "    self.x\n",
    );
    let code = compile(src, "ctr_splice_arm_tail");
    eprintln!("inline arm-tail:\n{code}");

    let scope = case_scope(&code);
    let fallback = &scope[scope.find("CondNoMatch").unwrap()..];
    assert!(
        fallback.contains("beamtalk_message_dispatch':'send'"),
        "the case must be the pure-inline one (send fallback). Got:\n{code}"
    );
    assert_compiles_through_erlc("ctr_splice_arm_tail", &code);
}

#[test]
fn vt_arm_tail_still_inlines() {
    // A value-type mutation arm in value position whose tail is a pure
    // conditional keeps inlining: only stateful loop-condition tails
    // decline.
    let src = concat!(
        "Value subclass: Probe\n",
        "  field: m = 0\n",
        "  run: c1 c2: c2 =>\n",
        "    x := c1 ifTrue: [self.m := self.m + 1. c2 ifTrue: [5] ifFalse: [6]] ifFalse: [0].\n",
        "    x\n",
    );
    let code = compile(src, "bt@probe_splice_vt_arm");
    eprintln!("inline vt-arm:\n{code}");

    let scope = case_scope(&code);
    let fallback = &scope[scope.find("CondNoMatch").unwrap()..];
    assert!(
        fallback.contains("beamtalk_message_dispatch':'send'"),
        "the case must be the pure-inline one (send fallback). Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_splice_vt_arm", &code);
}

#[test]
fn tuple_splice_fold_tail_with_field_write_compiles() {
    // A field-writing fold whose tail is a pure conditional: the fold
    // machinery binds accumulator values itself (`_AccOut`), so the
    // tail never lands bare in the family `Return` tuple.
    let src = concat!(
        "Value subclass: Probe\n",
        "  field: total = 0\n",
        "  run: coll =>\n",
        "    coll inject: 0 into: [:a :e |\n",
        "      self.total := self.total + 1.\n",
        "      e =:= 1 ifTrue: [a + 1] ifFalse: [a]]\n",
    );
    let code = compile(src, "bt@probe_splice_fold");
    eprintln!("splice fold:\n{code}");

    assert_compiles_through_erlc("bt@probe_splice_fold", &code);
}

#[test]
fn operand_positions_with_pure_case_compile() {
    // A pure `case` nested as a send's last argument and as another
    // send's receiver, in a field-writing method (the deep-combination
    // shape): pins that operand splices stay erlc-clean.
    let src = concat!(
        "Value subclass: Probe\n",
        "  field: f = 0\n",
        "  combine: a with: b => a + b\n",
        "  run: c1 c2: c2 coll: coll =>\n",
        "    self.f := self.f + 1.\n",
        "    x := self combine: 1 with: (c1 ifTrue: [10] ifFalse: [20]).\n",
        "    y := (c2 ifTrue: [coll] ifFalse: [#()]) size.\n",
        "    x + y\n",
    );
    let code = compile(src, "bt@probe_splice_operands");
    eprintln!("splice operands:\n{code}");

    assert_compiles_through_erlc("bt@probe_splice_operands", &code);
}

#[test]
fn guarded_operator_in_value_type_arm_still_inlines() {
    // Value-type methods bracket nothing, so a guarded operator in an arm
    // inlines there.
    let src = "Value subclass: Probe\n  check: flag n: n =>\n    flag ifTrue: [n * 2]\n";
    let code = compile(src, "bt@probe_vt_op_arm");
    eprintln!("guarded-op value-type arm:\n{code}");

    let scope = case_scope(&code);
    let fallback = &scope[scope.find("CondNoMatch").unwrap()..];
    assert!(
        fallback.contains("beamtalk_message_dispatch':'send'"),
        "the case must be the pure-inline one (send fallback). Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_vt_op_arm", &code);
}

#[test]
fn nested_pure_conditionals_grow_linearly() {
    // BT-4070 review: pure-inline is declined inside fallback closures,
    // so 6 nested levels emit 6 fallback arms — not 2^6 - 1. (Module
    // boilerplate holds no `CondNoMatch`, so count over the whole module.)
    let src = concat!(
        "Value subclass: Probe\n",
        "  check: a b: b c: c d: d e: e f: f =>\n",
        "    a ifTrue: [1] ifFalse: [b ifTrue: [2] ifFalse: [c ifTrue: [3] ifFalse: [",
        "d ifTrue: [4] ifFalse: [e ifTrue: [5] ifFalse: [f ifTrue: [6] ifFalse: [7]]]]]]\n",
    );
    let code = compile(src, "bt@probe_nested_pure");
    let count = code.matches("CondNoMatch").count();
    assert_eq!(
        count, 6,
        "6 nested levels must emit 6 fallback arms (linear), got {count}. Got:\n{code}"
    );
    assert_compiles_through_erlc("bt@probe_nested_pure", &code);
}

#[test]
fn fallback_closure_warnings_are_not_duplicated() {
    // BT-4070 review: the fallback closure lowers the same AST the inline
    // arm already reported, so a warning raised while lowering the block
    // (non-literal callable at the Erlang boundary) must appear exactly
    // once, not once per lowering.
    let src = "Value subclass: Probe\n  check: flag items: items do: aBlock =>\n    flag ifTrue: [items do: aBlock]\n";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module_ast, _diags) = beamtalk_core::source_analysis::parse(tokens);
    let generated = crate::core_erlang::generate_module_with_warnings(
        &module_ast,
        crate::core_erlang::CodegenOptions::new("bt@probe_warn_once"),
    )
    .expect("codegen should succeed");
    assert!(
        generated.code.contains("CondNoMatch"),
        "expected the pure-inline case to fire. Got:\n{}",
        generated.code
    );
    let count = generated
        .warnings
        .iter()
        .filter(|w| w.message.contains("non-literal callable"))
        .count();
    assert_eq!(
        count,
        1,
        "expected exactly one non-literal-callable warning, got {count}: {:?}",
        generated
            .warnings
            .iter()
            .map(|w| w.message.as_str())
            .collect::<Vec<_>>()
    );
}

#[test]
fn dedup_append_keeps_lowering_specific_findings() {
    // BT-4070 review: `dedup_append_codegen_warnings` drops exact repeats
    // of already-recorded diagnostics but keeps lowering-specific ones —
    // e.g. an `InternalVerifier` finding raised only while lowering the
    // fallback closure must survive (a wholesale `truncate` would discard
    // it, losing the release-mode miscompile signal).
    use beamtalk_core::source_analysis::{Diagnostic, DiagnosticCategory, Span};
    let mut generator = crate::core_erlang::CoreErlangGenerator::new("bt@probe_dedup");
    let span = Span::new(0, 0);
    let reported = Diagnostic::warning("non-literal callable", span);
    generator.add_codegen_warning(reported.clone());
    let verifier_finding = Diagnostic::warning("internal: synthetic", span)
        .with_category(DiagnosticCategory::InternalVerifier);
    generator.dedup_append_codegen_warnings(vec![reported, verifier_finding.clone()]);
    let kept = generator.take_codegen_warnings();
    assert_eq!(
        kept.len(),
        2,
        "expected the repeat dropped and the new finding kept, got {kept:?}"
    );
    assert!(
        kept.contains(&verifier_finding),
        "lowering-specific finding must survive dedup, got {kept:?}"
    );
}
