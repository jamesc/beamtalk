// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0137 Phase 2a (BT-4020): the cell bracket (`SyncedCall`) and its
//! check (`UnbracketedUserCodeSend`).

use super::codegen;

/// How many bracketed calls `out` holds (each stores through `store/1`).
fn brackets(out: &str) -> usize {
    // A BT-4279 fast-path bracket binds its value as `let <_Synced@N, ...>`.
    count(out, "let _Synced@") + count(out, "let <_Synced@")
}

/// The panic message of compiling `src` with the flag on (a verifier finding
/// is a debug-build panic), or `None` when it compiles.
fn enforce_report(src: &str) -> Option<String> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| codegen(src)))
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
    let on = codegen(src);
    assert_eq!(brackets(&on), 1, "{on}");
    // The reload is the next `State` version, bound right after the call.
    assert!(
        on.contains("call 'beamtalk_actor_state':'store'(State) in let _Synced")
            && on.contains("let State1 = call 'beamtalk_actor_state':'take'() in"),
        "{on}"
    );
}

#[test]
fn a_compiled_self_send_keeps_explicit_threading_and_no_bracket() {
    // The callee is a compiled method of this actor, so it brackets whatever it
    // runs itself (amended ADR 0137 §1): the caller's output is unchanged.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  a => self b\n\n  b => self.n := 1\n";
    let out = codegen(src);
    assert_eq!(brackets(&out), 0, "{out}");
}

#[test]
fn a_self_send_that_may_reach_the_default_arm_stores_before_the_dispatch() {
    // `printString` is an inherited `Object` method: compiled without actor
    // `State`, reached through `dispatch/4`'s default arm, so it is bracketed
    // (store-only: the explicit `{reply, R, State}` still brings `State` back).
    let src =
        "Actor subclass: Probe\n  state: n = 0\n\n  a =>\n    self printString.\n    self.n\n";
    let on = codegen(src);
    let control = codegen("Actor subclass: Probe\n  state: n = 0\n\n  a =>\n    self.n\n");
    let store = "let _ = call 'beamtalk_actor_state':'store'(State) in";
    assert_eq!(count(&on, store), count(&control, store) + 1, "{on}");
    assert!(!on.contains("call 'beamtalk_actor_state':'take'()"), "{on}");
}

#[test]
fn an_intrinsic_that_never_runs_user_code_is_not_bracketed() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  a: x =>\n    x isNil.\n    x class\n";
    let out = codegen(src);
    assert_eq!(brackets(&out), 0, "{out}");
}

#[test]
fn an_inlined_conditional_is_not_a_call() {
    let src =
        "Actor subclass: Probe\n  state: n = 0\n\n  a: x =>\n    x ifTrue: [1] ifFalse: [2]\n";
    let out = codegen(src);
    assert_eq!(brackets(&out), 0, "{out}");
}

#[test]
fn arithmetic_on_a_declared_integer_field_stays_a_bare_bif() {
    let src =
        "Actor subclass: Probe\n  state: n :: Integer = 0\n\n  bump => self.n := self.n + 1\n";
    let out = codegen(src);
    assert_eq!(brackets(&out), 0, "{out}");
}

#[test]
fn arithmetic_on_an_untyped_field_is_bracketed_where_it_can_dispatch() {
    // The operator guard's dispatching branch can reach a user `+`.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  bump => self.n := self.n + 1\n";
    let on = codegen(src);
    assert_eq!(brackets(&on), 1, "{on}");
}

#[test]
fn a_class_method_and_a_value_type_are_never_bracketed() {
    let value = "Value subclass: Pt\n  field: x = 0\n\n  plus: other => self.x + other\n";
    assert_eq!(brackets(&codegen(value)), 0);
    let class_side =
        "Actor subclass: Probe\n  state: n = 0\n\n  class make: other => other process\n";
    assert_eq!(brackets(&codegen(class_side)), 0);
}

#[test]
fn a_block_body_called_through_a_loop_is_one_bracketed_call() {
    // A call in a loop body is no state effect of the loop (ADR 0137 §3): the
    // body is a closure that runs against the cell, and the call that hands it
    // to the runtime is the bracket. The fold keeps its plain shape.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  each: items with: other =>\n    items do: [:x | other process: x].\n    self.n\n";
    let on = codegen(src);
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
    let on = codegen(src);
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
    let on = codegen(src);
    assert!(!on.contains("StateAcc"), "{on}");
    assert!(on.contains("let _Synced"), "{on}");
    super::assert_compiles_through_erlc("test", &on);
}

#[test]
fn a_fold_whose_body_only_reads_a_field_is_a_plain_closure_over_the_cell() {
    // Reading a field is no state effect: the fold stays a plain closure whose
    // body reads the cell, and the call that runs it is bracketed.
    let src = "Actor subclass: Probe\n  state: count :: Integer = 0\n  state: sum = 0\n\n  run: items =>\n    items do: [:m | self.count + 1].\n    self.sum\n";
    let on = codegen(src);
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
    let out = codegen(src);
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
    let on = codegen(src);
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
    let code = codegen(src);
    let default_arm = code
        .split("<OtherSelector> when 'true' ->")
        .nth(1)
        .expect("the default arm of dispatch/4")
        .split("'method_table'/0")
        .next()
        .expect("dispatch/4 ends before method_table/0");
    // BT-4280: the only direct exits are the memoised verdict and, under its
    // `{'compiled', ...}` arm alone, `invoke_resolved/6` of a compiled
    // `State`-threading actor method (the run-time twin of `CompiledSelfSend`).
    let direct = default_arm.matches("call 'beamtalk_dispatch':").count();
    assert_eq!(
        direct, 2,
        "a dispatch/4 exit bypasses call_foreign:\n{default_arm}"
    );
    let verdict = default_arm
        .find("call 'beamtalk_dispatch':'inherited_target'('Probe', OtherSelector)")
        .expect("the inherited_target verdict");
    let compiled_arm = default_arm
        .find("<{'compiled', InhOwner, InhModule}> when 'true' ->")
        .expect("the compiled arm");
    let invoke = default_arm
        .find("call 'beamtalk_dispatch':'invoke_resolved'(InhOwner, InhModule, OtherSelector, Args, Self, State)")
        .expect("the direct invoke");
    let slow_arm = default_arm
        .find("<_InhSlow> when 'true' ->")
        .expect("the slow arm");
    assert!(
        verdict < compiled_arm && compiled_arm < invoke && invoke < slow_arm,
        "invoke_resolved must sit under the compiled arm only:\n{default_arm}"
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
    let out = codegen(src);
    super::assert_compiles_through_erlc("test", &out);
    assert_eq!(enforce_report(src), None);
}

/// A method that built a bracket leaves the cell at the last stored version, so its
/// reply stores the final `State` back; a method without a bracket does not.
#[test]
fn a_bracketed_method_stores_its_final_state_on_exit() {
    let with = codegen(
        "Actor subclass: Probe\n  state: n = 0\n\n  poke: other =>\n    other process.\n    self.n := 1\n",
    );
    assert!(with.contains("_ExitReply"), "{with}");
    let without = codegen("Actor subclass: Probe\n  state: n = 0\n\n  bump => self.n := 1\n");
    assert!(!without.contains("_ExitReply"), "{without}");
    super::assert_compiles_through_erlc("test", &with);
}

#[test]
fn a_send_as_a_match_scrutinee_after_a_field_write_is_bracketed() {
    // BT-4136 (fixed by BT-4018's match `SyncedCall` bracket): catalina's
    // `Handler>>handleContinuation:` reported `result kind` as an
    // `UnbracketedUserCodeSend`. Debug assertions are on, so a verifier
    // finding panics here. The sealed-typed receiver of the original report
    // is pinned in `typed_sealed.rs`.
    let code = super::codegen(concat!(
        "Server subclass: Handler\n",
        "  state: userState :: Object = nil\n\n",
        "  handleContinuation: result =>\n",
        "    self.userState := result state\n",
        "    result kind match: [\n",
        "      #continue -> self continueReading: result;\n",
        "      _ -> self failWith: result reason\n",
        "    ]\n",
        "    nil\n\n",
        "  continueReading: result -> Nil => nil\n\n",
        "  failWith: reason -> Nil => nil\n",
    ));
    super::assert_compiles_through_erlc("test", &code);
}

/// BT-4280: `__beamtalk_meta/0` carries `'threads_actor_state' => 'true'`
/// exactly for the classes whose compiled methods thread actor `State`
/// (`ClassHierarchy::threads_actor_state`, the rule behind
/// `CompiledSelfSend`), which `beamtalk_dispatch:inherited_target/2` reads.
#[test]
fn meta_flags_threads_actor_state_only_for_actor_subclasses() {
    let flag = "'threads_actor_state' => 'true'";
    let actor = codegen("Actor subclass: Probe\n  state: n = 0\n\n  run => self.n\n");
    assert!(actor.contains(flag), "{actor}");
    let value = codegen("Value subclass: Pt\n  field: x :: Integer = 0\n");
    assert!(!value.contains(flag), "{value}");
    let object = codegen("Object subclass: Thing\n  run => 1\n");
    assert!(!object.contains(flag), "{object}");
}

/// The `let <R, State> = case <test> of <'true'> ... -> <Fast, State> ...`
/// head a BT-4279 fast-path bracket renders, or `None`.
fn fast_path_head(out: &str) -> Option<&str> {
    let start = out.find("let <_Synced")?;
    Some(&out[start..out[start..].find("<_> when 'true' -> let _ = ")? + start])
}

/// BT-4279: a guarded operator on a number hands the stored `State` through
/// on its fast arm (no store, no reload); only the dispatching arm brackets.
#[test]
fn a_guarded_operator_brackets_only_its_dispatching_arm() {
    let out = codegen("Actor subclass: Probe\n  state: n = 0\n\n  bump: v => self.n := v + 1\n");
    let head = fast_path_head(&out).expect("a fast-path bracket");
    assert!(head.contains("call 'erlang':'is_number'("), "{head}");
    assert!(head.contains("<call 'erlang':'+'("), "{head}");
    assert!(
        !head.contains("'store'") && !head.contains("'take'"),
        "{head}"
    );
    assert!(out.contains("let _SyncedSlowState"), "{out}");
    super::assert_compiles_through_erlc("test", &out);
}

/// BT-4279: an arithmetic operator whose right operand is not statically
/// numeric reaches the number-on-the-left coercion `try`/`catch` on its true
/// arm, which can dispatch a reflected selector. Integer `+ - *` never raise,
/// so they take a fast path under an integer test; `/` (zero divisor) keeps
/// the whole bracket.
#[test]
fn a_coercing_operator_has_a_fast_path_only_for_integer_add_sub_mul() {
    let add =
        codegen("Actor subclass: Probe\n  state: n = 0\n\n  add: a to: b => self.n := a + b\n");
    let head = fast_path_head(&add).expect("a fast-path bracket");
    assert!(
        head.contains("call 'erlang':'and'(call 'erlang':'is_integer'("),
        "{head}"
    );
    assert!(!head.contains("try"), "{head}");
    super::assert_compiles_through_erlc("test", &add);
    let div =
        codegen("Actor subclass: Probe\n  state: n = 0\n\n  div: a by: b => self.n := a / b\n");
    assert!(fast_path_head(&div).is_none(), "{div}");
}

/// BT-4279: a unary send of a field name to a typed tagged Value reads the
/// slot with no bracket when the loaded module's `__is_slot/1` says it is
/// still the auto getter; otherwise the guarded send is bracketed.
#[test]
fn a_typed_slot_read_brackets_only_its_slow_arm() {
    let out = codegen(concat!(
        "Actor subclass: Probe\n  state: n = 0\n\n  read: p :: Pt => self.n := p x\n\n",
        "sealed Value subclass: Pt\n  field: x :: Integer = 0\n",
    ));
    let head = fast_path_head(&out).expect("a fast-path bracket");
    assert!(head.contains(":'__is_slot'('x')"), "{head}");
    assert!(head.contains("<call 'maps':'get'('x', "), "{head}");
    assert!(
        !head.contains("'store'") && !head.contains("'take'"),
        "{head}"
    );
    super::assert_compiles_through_erlc("test", &out);
}

/// BT-4279: a unary send that names no field of the Value class (a
/// user-written method) keeps the whole bracket.
#[test]
fn a_typed_method_send_keeps_the_whole_bracket() {
    let out = codegen(concat!(
        "Actor subclass: Probe\n  state: n = 0\n\n  read: p :: Pt => self.n := p twice\n\n",
        "sealed Value subclass: Pt\n  field: x :: Integer = 0\n\n  twice -> Integer => self.x\n",
    ));
    assert!(fast_path_head(&out).is_none(), "{out}");
}

/// BT-4279: `__is_slot/1` answers `true` for exactly the auto getters: a
/// field whose getter the class overrides with a method is not a slot read.
#[test]
fn is_slot_lists_exactly_the_auto_getters() {
    let out = codegen(concat!(
        "Value subclass: Pt\n  field: x :: Integer = 0\n  field: y :: Integer = 0\n\n",
        "  y -> Integer => self.x + 1\n",
    ));
    let is_slot = out
        .split("'__is_slot'/1 = fun")
        .nth(1)
        .expect("__is_slot/1")
        .split("\n\n")
        .next()
        .expect("its body");
    assert!(is_slot.contains("<'x'> when 'true' -> 'true'"), "{is_slot}");
    assert!(!is_slot.contains("'y'"), "{is_slot}");
    assert!(out.contains("'__is_slot'/1,"), "exported: {out}");
}
