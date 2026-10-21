// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0137 Phase 2a (BT-4020): the cell bracket (`SyncedCall`) and its
//! check (`UnbracketedUserCodeSend`), behind the synced-calls flag.
//!
//! Every test sets the flag through [`CodegenOptions::with_synced_calls`], so
//! the verdicts hold whatever `BEAMTALK_SYNCED_CALLS` is in the environment
//! (the flag-on suites run with it set).

use crate::core_erlang::{CodegenOptions, generate_module};
use beamtalk_core::source_analysis::{lex_with_eof, parse};

fn compile(src: &str, synced: bool) -> String {
    let (module, _) = parse(lex_with_eof(src));
    generate_module(
        &module,
        CodegenOptions::new("test").with_synced_calls(synced),
    )
    .expect("codegen should succeed")
}

/// The panic message of compiling `src` with the flag on (a verifier finding
/// is a debug-build panic), or `None` when it compiles.
fn enforce_report(src: &str) -> Option<String> {
    std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| compile(src, true)))
        .err()
        .map(|payload| {
            payload
                .downcast_ref::<String>()
                .cloned()
                .or_else(|| payload.downcast_ref::<&str>().map(|s| (*s).to_string()))
                .unwrap_or_default()
        })
}

const CELL_PUT: &str = "call 'erlang':'put'('$bt_actor_state'";
const CELL_GET: &str = "call 'erlang':'get'('$bt_actor_state')";

fn count(haystack: &str, needle: &str) -> usize {
    haystack.matches(needle).count()
}

#[test]
fn a_send_to_another_object_is_stored_called_and_reloaded() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  poke: other =>\n    other process.\n    self.n := self.n\n";
    let off = compile(src, false);
    let on = compile(src, true);
    assert_eq!(count(&on, CELL_PUT), count(&off, CELL_PUT) + 1, "{on}");
    assert_eq!(count(&on, CELL_GET), count(&off, CELL_GET) + 1, "{on}");
    // The reload is the next `State` version, bound right after the call.
    assert!(
        on.contains("call 'erlang':'put'('$bt_actor_state', State) in let _Synced")
            && on.contains("let State1 = call 'erlang':'get'('$bt_actor_state') in"),
        "{on}"
    );
}

#[test]
fn flag_off_adds_no_cell_traffic() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  poke: other =>\n    other process.\n    other size\n";
    let off = compile(src, false);
    assert!(!off.contains("_Synced"), "{off}");
}

#[test]
fn a_compiled_self_send_keeps_explicit_threading_and_no_bracket() {
    // The callee is a compiled method of this actor, so it brackets whatever it
    // runs itself (amended ADR 0137 §1): the caller's output is unchanged.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  a => self b\n\n  b => self.n := 1\n";
    assert_eq!(compile(src, true), compile(src, false));
}

#[test]
fn a_self_send_that_may_reach_the_default_arm_stores_before_the_dispatch() {
    // `printString` is an inherited `Object` method: compiled without actor
    // `State`, reached through `dispatch/4`'s default arm, so it is bracketed
    // (store-only: the explicit `{reply, R, State}` still brings `State` back).
    let src =
        "Actor subclass: Probe\n  state: n = 0\n\n  a =>\n    self printString.\n    self.n\n";
    let off = compile(src, false);
    let on = compile(src, true);
    assert_eq!(count(&on, CELL_PUT), count(&off, CELL_PUT) + 1, "{on}");
    assert_eq!(count(&on, CELL_GET), count(&off, CELL_GET), "{on}");
}

#[test]
fn an_intrinsic_that_never_runs_user_code_is_not_bracketed() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  a: x =>\n    x isNil.\n    x class\n";
    assert_eq!(compile(src, true), compile(src, false));
}

#[test]
fn an_inlined_conditional_is_not_a_call() {
    let src =
        "Actor subclass: Probe\n  state: n = 0\n\n  a: x =>\n    x ifTrue: [1] ifFalse: [2]\n";
    assert_eq!(compile(src, true), compile(src, false));
}

#[test]
fn arithmetic_on_a_declared_integer_field_stays_a_bare_bif() {
    let src =
        "Actor subclass: Probe\n  state: n :: Integer = 0\n\n  bump => self.n := self.n + 1\n";
    assert_eq!(compile(src, true), compile(src, false));
}

#[test]
fn arithmetic_on_an_untyped_field_is_bracketed_where_it_can_dispatch() {
    // The operator guard's dispatching branch can reach a user `+`.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  bump => self.n := self.n + 1\n";
    let off = compile(src, false);
    let on = compile(src, true);
    assert_eq!(count(&on, CELL_PUT), count(&off, CELL_PUT) + 1, "{on}");
    assert_eq!(count(&on, CELL_GET), count(&off, CELL_GET) + 1, "{on}");
}

#[test]
fn a_class_method_and_a_value_type_are_never_bracketed() {
    let value = "Value subclass: Pt\n  field: x = 0\n\n  plus: other => self.x + other\n";
    assert_eq!(compile(value, true), compile(value, false));
    let class_side =
        "Actor subclass: Probe\n  state: n = 0\n\n  class make: other => other process\n";
    assert_eq!(compile(class_side, true), compile(class_side, false));
}

#[test]
fn a_block_body_called_through_a_loop_is_bracketed_in_the_loop_frame() {
    // The fold threads `State` (a bracketed call is a state effect), so the call
    // in its body is stored and reloaded in the fold's own frame.
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  each: items with: other =>\n    items do: [:x | other process: x].\n    self.n\n";
    let on = compile(src, true);
    assert!(on.contains("StateAcc"), "{on}");
    assert_eq!(enforce_report(src), None);
}

// PIN-BUG BT-4020: the argument of a `value:` sent to a literal block that
// writes a captured local (`generate_block_value_inline_with_mutations`) is
// compiled outside the producers that bracket, so a call in it is reported as
// an unbracketed user-code send. The owning node is that inlined block-value
// lowering; fixing it flips this pin.
#[test]
fn pin_a_call_in_an_inlined_block_values_argument_is_an_unbracketed_send() {
    let src = "Actor subclass: Probe\n  state: n = 0\n\n  run: a =>\n    loc := 0.\n    [:p | loc := p] value: a < a.\n    loc\n";
    let report = enforce_report(src).expect("the gap is reported");
    assert!(
        report.contains("UnbracketedUserCodeSend") && report.contains("selector: \"<\""),
        "{report}"
    );
}

#[test]
fn a_cast_in_a_fold_body_does_not_make_the_fold_threaded() {
    // A cast posts to a mailbox: it is no call into user code, so the fold
    // keeps its plain closure (and a `Set` receiver keeps working).
    let src = "Actor subclass: Room\n  state: members = Set new\n\n  say: text =>\n    self.members do: [:m | m receiveMessage: text!]\n";
    assert_eq!(compile(src, true), compile(src, false));
}

#[test]
fn a_threaded_fold_whose_body_advances_nothing_still_hands_back_the_state_map() {
    // The block analysis cannot see that `self.count` is a declared Integer, so
    // it threads the fold; codegen proves the `+` a bare BIF and brackets
    // nothing. The map-accumulator lambda must still answer `StateAcc`, never
    // the body's value.
    let src = "Actor subclass: Probe\n  state: count :: Integer = 0\n  state: sum = 0\n\n  run: items =>\n    items do: [:m | self.count + 1].\n    self.sum\n";
    let on = compile(src, true);
    super::assert_compiles_through_erlc("test", &on);
    assert!(
        on.contains("fun (M, StateAcc) -> let _ = ") && on.contains(" in StateAcc"),
        "{on}"
    );
}
