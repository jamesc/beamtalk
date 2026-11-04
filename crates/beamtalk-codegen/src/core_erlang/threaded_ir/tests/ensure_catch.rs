// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4312: the `ensure:` catch. `verify()`'s `EnsureCleanupLostOnNlr` and
//! the clause `render()` emits for the scope's own `$bt_nlr` throw.

use super::*;
use beamtalk_cerl_doc::Document;

fn vars() -> EnsureCatchVars {
    let v = |n: &str| n.to_string();
    EnsureCatchVars {
        type_var: v("Type1"),
        error_var: v("Error1"),
        stack_var: v("Stack1"),
        nlr_tok_var: v("Tok1"),
        nlr_val_var: v("Val1"),
        nlr_carried_var: v("Carried1"),
        other_pair_var: v("Other1"),
    }
}

/// The REPL's relay of `[^5] ensure: [w := 9]`: the cleanup threads from the
/// carried bindings map and the throw resumes with its result.
fn relay(rethrow: RethrowSlot) -> EnsureNlrRelay {
    EnsureNlrRelay {
        seed: Document::Str("let StateAcc = Carried1 in "),
        cleanup: Document::Str("let StateAcc1 = call 'maps':'put'('w', 9, StateAcc) in"),
        cell_store: false,
        rethrow,
    }
}

fn node(
    carry: Option<NlrCarry>,
    cleanup_final: Option<usize>,
    relay: Option<EnsureNlrRelay>,
) -> ThreadedStmt {
    ThreadedStmt::EnsureCatch {
        vars: Box::new(vars()),
        token: Some(TokenId::new("NlrToken1")),
        carry,
        cleanup_final,
        relay,
        other: Document::Str("primop 'raw_raise'(Type1, Error1, Stack1)"),
        span: span(),
    }
}

fn defect(ir: &[ThreadedStmt]) -> Option<EnsureRelayDefect> {
    match verify(ir).as_slice() {
        [] => None,
        [VerifyError::EnsureCleanupLostOnNlr { defect: found, at }] => {
            assert_eq!(*at, span());
            Some(found.clone())
        }
        other => panic!("expected one EnsureCleanupLostOnNlr, got {other:?}"),
    }
}

#[test]
fn a_relay_that_rethrows_the_cleanups_state_verifies_and_renders() {
    let ir = [node(
        Some(NlrCarry::StateAcc),
        Some(1),
        Some(relay(RethrowSlot::Version(1))),
    )];
    assert_eq!(defect(&ir), None);
    let text = lower_and_render(&ir).to_pretty_string();
    assert!(
        text.contains("<{'throw', {'$bt_nlr', Tok1, Val1, Carried1}}> when call 'erlang':'=:='(Tok1, NlrToken1) -> let StateAcc = Carried1 in")
            && text.contains("call 'erlang':'throw'({'$bt_nlr', Tok1, Val1, StateAcc1})")
            && text.contains("<Other1> when 'true' -> primop 'raw_raise'(Type1, Error1, Stack1) end"),
        "{text}"
    );
}

/// The repro: the catch re-raises the throw after a cleanup that wrote the
/// carried state, so its boundary answers the state at the `^`.
#[test]
fn a_writing_cleanup_without_a_relay_fails() {
    let ir = [node(Some(NlrCarry::StateAcc), Some(1), None)];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::NoRelay));
    let text = lower_and_render(&ir).to_pretty_string();
    assert!(!text.contains("'$bt_nlr'"), "{text}");
    let ir = [node(Some(NlrCarry::SelfVt), Some(2), None)];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::NoRelay));
}

#[test]
fn a_relay_that_rethrows_a_stale_version_fails() {
    let ir = [node(
        Some(NlrCarry::StateAcc),
        Some(2),
        Some(relay(RethrowSlot::Version(1))),
    )];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::RethrowsStaleState));
}

/// Where the state is the actor-state cell, the cleanup writes it in place:
/// every relay re-throws the cell as it left it.
#[test]
fn a_live_carry_needs_a_live_relay() {
    let ir = [node(Some(NlrCarry::Live), None, None)];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::NoRelay));
    let ir = [node(
        Some(NlrCarry::Live),
        None,
        Some(relay(RethrowSlot::Version(0))),
    )];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::RethrowsStaleState));
    let ir = [node(
        Some(NlrCarry::Live),
        None,
        Some(relay(RethrowSlot::Live(Document::Str("Cell1")))),
    )];
    assert_eq!(defect(&ir), None);
}

/// Nothing to relay: a cleanup that writes nothing, a scope whose throw
/// carries no state (a class method), or no `^` of the scope at all.
#[test]
fn nothing_to_relay_verifies_clean() {
    assert_eq!(defect(&[node(Some(NlrCarry::StateAcc), None, None)]), None);
    assert_eq!(defect(&[node(None, Some(1), None)]), None);
    let no_token = ThreadedStmt::EnsureCatch {
        vars: Box::new(vars()),
        token: None,
        carry: Some(NlrCarry::StateAcc),
        cleanup_final: Some(1),
        relay: None,
        other: Document::Str("primop 'raw_raise'(Type1, Error1, Stack1)"),
        span: span(),
    };
    assert_eq!(defect(&[no_token]), None);
}
