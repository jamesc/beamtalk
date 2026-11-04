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

/// An inlined cleanup ending at `final_version` of the carried family, which
/// may write the actor-state cell.
fn inlined(final_version: Option<usize>) -> EnsureCleanup {
    EnsureCleanup::Inlined {
        final_version,
        writes_cell: true,
    }
}

/// A cleanup applied as a closure that may write the actor-state cell.
const CLOSURE: EnsureCleanup = EnsureCleanup::Closure { writes_cell: true };

fn node(
    carry: Option<NlrCarry>,
    cleanup: EnsureCleanup,
    relay: Option<EnsureNlrRelay>,
) -> ThreadedStmt {
    ThreadedStmt::EnsureCatch {
        vars: Box::new(vars()),
        token: Some(TokenId::new("NlrToken1")),
        carry,
        cleanup,
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
        Some(NlrCarry::Bindings),
        inlined(Some(1)),
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
    let ir = [node(Some(NlrCarry::Bindings), inlined(Some(1)), None)];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::NoRelay));
    let text = lower_and_render(&ir).to_pretty_string();
    assert!(!text.contains("'$bt_nlr'"), "{text}");
    let ir = [node(Some(NlrCarry::ActorState), inlined(Some(1)), None)];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::NoRelay));
    let ir = [node(Some(NlrCarry::SelfVt), inlined(Some(2)), None)];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::NoRelay));
}

#[test]
fn a_relay_that_rethrows_a_stale_version_fails() {
    let ir = [node(
        Some(NlrCarry::Bindings),
        inlined(Some(2)),
        Some(relay(RethrowSlot::Version(1))),
    )];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::RethrowsStaleState));
}

/// Where the state is the actor-state cell, the cleanup writes it in place:
/// every relay re-throws the carried state with the cell over it, never a
/// version, and never a state rebuilt from the `ensure:` site's bindings
/// (finding A of the BT-4312 review: a merged Tier 2 fun's captured locals
/// re-flushed as bound at the `ensure:`, not as the `^` carried them).
#[test]
fn a_live_carry_needs_the_cell_over_the_carried_state() {
    for cleanup in [inlined(None), CLOSURE] {
        let ir = [node(Some(NlrCarry::Live), cleanup, None)];
        assert_eq!(defect(&ir), Some(EnsureRelayDefect::NoRelay));
        let ir = [node(
            Some(NlrCarry::Live),
            cleanup,
            Some(relay(RethrowSlot::Version(0))),
        )];
        assert_eq!(defect(&ir), Some(EnsureRelayDefect::RethrowsStaleState));
        let ir = [node(
            Some(NlrCarry::Live),
            cleanup,
            Some(relay(RethrowSlot::SiteState(Document::Str("Rebuilt1")))),
        )];
        assert_eq!(
            defect(&ir),
            Some(EnsureRelayDefect::RebuildsCarriedStateAtSite)
        );
        let ir = [node(
            Some(NlrCarry::Live),
            cleanup,
            Some(relay(RethrowSlot::CellOverCarried)),
        )];
        assert_eq!(defect(&ir), None);
        let text = lower_and_render(&ir).to_pretty_string();
        assert!(
            text.contains(
                "call 'erlang':'throw'({'$bt_nlr', Tok1, Val1, call 'beamtalk_actor_state':'refresh'(Self, Carried1)})"
            ),
            "{text}"
        );
    }
}

/// Finding B of the BT-4312 review: an `ensure:` whose receiver or cleanup is
/// not a block literal applies the cleanup as a closure. In an actor method
/// the closure writes the cell, which a `^` stored the carried `State` to
/// before it threw, so the relay must re-throw the carried state with the
/// cell over it. A closure cannot write the REPL's bindings or a value type's
/// `Self`: nothing to relay there.
#[test]
fn a_closure_cleanup_in_an_actor_method_needs_the_cell_over_the_carried_state() {
    let ir = [node(Some(NlrCarry::ActorState), CLOSURE, None)];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::NoRelay));
    let ir = [node(
        Some(NlrCarry::ActorState),
        CLOSURE,
        Some(relay(RethrowSlot::Version(0))),
    )];
    assert_eq!(defect(&ir), Some(EnsureRelayDefect::RethrowsStaleState));
    let ir = [node(
        Some(NlrCarry::ActorState),
        CLOSURE,
        Some(relay(RethrowSlot::CellOverCarried)),
    )];
    assert_eq!(defect(&ir), None);
    for carry in [NlrCarry::Bindings, NlrCarry::SelfVt] {
        assert_eq!(defect(&[node(Some(carry), CLOSURE, None)]), None);
    }
}

/// A cleanup that cannot write the cell (a block literal touching neither the
/// actor's state nor user code) has nothing to relay where the state lives
/// in the cell.
#[test]
fn a_cleanup_that_cannot_write_the_cell_needs_no_relay() {
    let quiet = [
        EnsureCleanup::Closure { writes_cell: false },
        EnsureCleanup::Inlined {
            final_version: None,
            writes_cell: false,
        },
    ];
    for cleanup in quiet {
        for carry in [NlrCarry::Live, NlrCarry::ActorState] {
            assert_eq!(defect(&[node(Some(carry), cleanup, None)]), None);
        }
    }
}

/// Nothing to relay: a cleanup that writes nothing, a scope whose throw
/// carries no state (a class method), or no `^` of the scope at all.
#[test]
fn nothing_to_relay_verifies_clean() {
    assert_eq!(
        defect(&[node(Some(NlrCarry::Bindings), inlined(None), None)]),
        None
    );
    assert_eq!(defect(&[node(None, inlined(Some(1)), None)]), None);
    assert_eq!(defect(&[node(None, CLOSURE, None)]), None);
    let no_token = ThreadedStmt::EnsureCatch {
        vars: Box::new(vars()),
        token: None,
        carry: Some(NlrCarry::ActorState),
        cleanup: CLOSURE,
        relay: None,
        other: Document::Str("primop 'raw_raise'(Type1, Error1, Stack1)"),
        span: span(),
    };
    assert_eq!(defect(&[no_token]), None);
}
