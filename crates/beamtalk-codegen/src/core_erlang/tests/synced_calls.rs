// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0137 Phase 2a (BT-4020): the cell bracket (`SyncedCall`) and its
//! check (`UnbracketedUserCodeSend`).

use crate::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

fn compile(src: &str) -> String {
    let (module, _) = parse(lex_with_eof(src));
    generate_module(&module, CodegenOptions::new("test")).expect("codegen should succeed")
}

/// How many bracketed calls `out` holds (each is `let _Synced... = put(...)`).
fn brackets(out: &str) -> usize {
    count(out, "let _Synced")
}

/// The panic message of compiling `src` with the flag on (a verifier finding
/// is a debug-build panic), or `None` when it compiles.
fn enforce_report(src: &str) -> Option<String> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| compile(src)))
        .err()
        .map(|payload| {
            payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
                .unwrap_or_default()
        })
}

fn count(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

#[test]
fn a_send_to_another_object_is_stored_called_and_reloaded() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  poke: other =>\n    other process.\n    self.n := self.n\n";
    let on = compile(src);
    assert_eq!(brackets(&on), 1, "{on}");
    // The reload is the next `State` version, bound right after the call.
    assert!(
        on.contains("call 'erlang':'put'('$bt_actor_state', State) in let _Synced")
            && on.contains("let State1 = call 'erlang':'get'('$bt_actor_state') in"),
        "{on}"
    );
}

#[test]
fn a_compiled_self_send_keeps_explicit_threading_and_no_bracket() {
    // The callee is a compiled method of this actor, so it brackets whatever it
    // runs itself (amended ADR 0137 §1): the caller's output is unchanged.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  a => self b\n\n  b => self.n := 1\n";
    let out = compile(src);
    assert_eq!(brackets(&out), 0, "{out}");
}

#[test]
fn a_self_send_that_may_reach_the_default_arm_stores_before_the_dispatch() {
    // `printString` is an inherited `Object` method: compiled without actor
    // `State`, reached through `dispatch/4`'s default arm, so it is bracketed
    // (store-only: the explicit `{reply, R, State}` still brings `State` back).
    let src =
        "Actor subclass: Probe\n  state: n = 0\n\n  a =>\n    self printString.\n    self.n\n";
    let on = compile(src);
    let control = compile("Actor subclass: Probe\n  state: n = 0\n\n  a =>\n    self.n\n");
    let store = "let _ = call 'erlang':'put'('$bt_actor_state', State) in";
    assert_eq!(count(&on, store), count(&control, store) + 1, "{on}");
    assert!(!on.contains("let State1 = call 'erlang':'get'"), "{on}");
}

#[test]
fn an_intrinsic_that_never_runs_user_code_is_not_bracketed() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  a: x =>\n    x isNil.\n    x class\n";
    let out = compile(src);
    assert_eq!(brackets(&out), 0, "{out}");
}

#[test]
fn an_inlined_conditional_is_not_a_call() {
    let src =
        "Actor subclass: Probe\n  state: n = 0\n\n  a: x =>\n    x ifTrue: [1] ifFalse: [2]\n";
    let out = compile(src);
    assert_eq!(brackets(&out), 0, "{out}");
}

#[test]
fn arithmetic_on_a_declared_integer_field_stays_a_bare_bif() {
    let src =
        "Actor subclass: Probe\n  state: n :: Integer = 0\n\n  bump => self.n := self.n + 1\n";
    let out = compile(src);
    assert_eq!(brackets(&out), 0, "{out}");
}

#[test]
fn arithmetic_on_an_untyped_field_is_bracketed_where_it_can_dispatch() {
    // The operator guard's dispatching branch can reach a user `+`.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  bump => self.n := self.n + 1\n";
    let on = compile(src);
    assert_eq!(brackets(&on), 1, "{on}");
}

#[test]
fn a_class_method_and_a_value_type_are_never_bracketed() {
    let value = "Value subclass: Pt\n  field: x = 0\n\n  plus: other => self.x + other\n";
    assert_eq!(brackets(&compile(value)), 0);
    let class_side =
        "Actor subclass: Probe\n  state: n = 0\n\n  class make: other => other process\n";
    assert_eq!(brackets(&compile(class_side)), 0);
}

#[test]
fn a_block_body_called_through_a_loop_is_one_bracketed_call() {
    // A call in a loop body is no state effect of the loop (ADR 0137 §3): the
    // body is a closure that runs against the cell, and the call that hands it
    // to the runtime is the bracket. The fold keeps its plain shape.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  each: items with: other =>\n    items do: [:x | other process: x].\n    self.n\n";
    let on = compile(src);
    assert!(!on.contains("StateAcc"), "{on}");
    assert_eq!(brackets(&on), 1, "{on}");
    assert_eq!(enforce_report(src), None);
}

#[test]
fn a_call_in_an_inlined_block_values_argument_is_bracketed() {
    // The argument of a `value:` sent to a literal block that writes a captured
    // local is compiled in the construct's own statement sequence (BT-4020).
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  run: a =>\n    loc := 0.\n    [:p | loc := p] value: a < a.\n    loc\n";
    assert_eq!(enforce_report(src), None);
    let on = compile(src);
    assert!(on.contains("let _Synced"), "{on}");
}

#[test]
fn a_loop_with_a_non_literal_body_is_a_call_not_a_construct_tuple() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  run: a with: body =>\n    [a] whileTrue: body printString.\n    self.n\n";
    assert_eq!(enforce_report(src), None);
}

#[test]
fn a_cast_in_a_fold_body_does_not_make_the_fold_threaded() {
    // A cast posts to a mailbox: it is no call into user code, so the fold
    // keeps its plain closure (and a `Set` receiver keeps working). Handing a
    // closure to the runtime fold is itself a call, bracketed as one.
    let src = "Actor subclass: Room\n  state: members = Set new\n\n  say: text =>\n    self.members do: [:m | m receiveMessage: text!]\n";
    let on = compile(src);
    assert!(!on.contains("StateAcc"), "{on}");
    assert!(on.contains("let _Synced"), "{on}");
    super::assert_compiles_through_erlc("test", &on);
}

#[test]
fn a_fold_whose_body_only_reads_a_field_is_a_plain_closure_over_the_cell() {
    // Reading a field is no state effect: the fold stays a plain closure whose
    // body reads the cell, and the call that runs it is bracketed.
    let src = "Actor subclass: Probe\n  state: count :: Integer = 0\n  state: sum = 0\n\n  run: items =>\n    items do: [:m | self.count + 1].\n    self.sum\n";
    let on = compile(src);
    super::assert_compiles_through_erlc("test", &on);
    assert!(!on.contains("StateAcc"), "{on}");
    assert!(
        on.contains("call 'beamtalk_actor_state':'get'(Self, 'count',"),
        "{on}"
    );
}

#[test]
fn a_state_default_with_a_collection_op_has_no_state_to_thread() {
    // Built while the state map itself is made: no `State` to store or thread.
    let src = "Actor subclass: Probe\n  state: items = (#(1, 2, 3) collect: [:x | x printString])\n\n  run => self.items\n";
    let out = compile(src);
    assert_eq!(brackets(&out), 0, "{out}");
    super::assert_compiles_through_erlc("test", &out);
}

#[test]
fn a_bracket_around_a_call_whose_operand_is_bracketed_inside_a_closed_document_stores_the_prelude_version()
 {
    // `ifTrue:ifFalse:` with a non-literal argument is a real send; its operand
    // `[^0] printString` is bracketed inside the dispatch's closed document, so
    // the outer store must name the `State` the method has, not the inner version.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  run: a =>\n    x := a ifTrue: [1] ifFalse: [^0] printString.\n    self.n\n";
    let on = compile(src);
    super::assert_compiles_through_erlc("test", &on);
    assert_eq!(enforce_report(src), None);
}

/// ADR 0137 §2: every `dispatch/4` exit that is not a compiled Beamtalk method (an
/// ADR 0066 extension fun, the inherited-method walk) runs through
/// `beamtalk_actor_state:call_foreign/3`; the only other exit is the actor's own
/// compiled `dispatch/4` (the `doesNotUnderstand:args:` handler). So the default arm
/// holds no direct call into `beamtalk_dispatch`: each is the `make_fun` argument of a
/// `call_foreign`.
#[test]
fn every_dispatch_exit_is_compiled_or_goes_through_call_foreign() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  run => self.n\n";
    let code = compile(src);
    let default_arm = code
        .split("<OtherSelector> when 'true' ->")
        .nth(1)
        .expect("the default arm of dispatch/4")
        .split("'method_table'/0")
        .next()
        .expect("dispatch/4 ends before method_table/0");
    let direct = default_arm.matches("call 'beamtalk_dispatch':").count();
    assert_eq!(
        direct, 0,
        "a dispatch/4 exit bypasses call_foreign:\n{default_arm}"
    );
    let foreign = default_arm
        .matches("call 'beamtalk_actor_state':'call_foreign'(State, call 'erlang':'make_fun'('beamtalk_dispatch', ")
        .count();
    assert_eq!(foreign, 2, "extension and inherited exits:\n{default_arm}");
}

/// A `^` out of a direct-params loop throws with the method's current field-read
/// `State` (`current_field_read_state_var()`): the NLR fix of BT-4020, which the
/// flag-off build had no test for.
#[test]
fn an_early_return_out_of_a_direct_params_loop_compiles_through_erlc() {
    let src = "Actor subclass: Probe\n  state: n = 5\n\n  run =>\n    i := 0\n    [i < 3] whileTrue: [i := i + 1. i == 2 ifTrue: [^ self.n]]\n    0\n";
    let out = compile(src);
    super::assert_compiles_through_erlc("test", &out);
    assert_eq!(enforce_report(src), None);
}
