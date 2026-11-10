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
        cell_snapshot_var: v("Snap1"),
        foreign_nlr_var: v("Foreign1"),
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
        private_scratch: false,
        cleanup,
        relay,
        cell_live: false,
        foreign: None,
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

/// BT-4433: a construct whose scratch map is private (a direct-params loop
/// body, a class or value-type method) threads the REPL's bindings or an
/// actor's `State` through a map that dies with the scope, so the verifier,
/// reading the node's typed `private_scratch`, owes it no relay. A `Self` or
/// a cell-held state is written whatever the scratch map is.
#[test]
fn a_private_scratch_map_is_exempt_only_for_a_state_acc_carry() {
    let private = |carry, relay| match node(Some(carry), inlined(Some(1)), relay) {
        ThreadedStmt::EnsureCatch {
            vars,
            token,
            carry,
            cleanup,
            relay,
            other,
            span,
            ..
        } => ThreadedStmt::EnsureCatch {
            vars,
            token,
            carry,
            private_scratch: true,
            cleanup,
            relay,
            cell_live: false,
            foreign: None,
            other,
            span,
        },
        _ => unreachable!(),
    };
    for carry in [NlrCarry::Bindings, NlrCarry::ActorState] {
        assert_eq!(defect(&[private(carry, None)]), None, "{carry:?}");
    }
    for carry in [NlrCarry::SelfVt, NlrCarry::Live] {
        assert_eq!(
            defect(&[private(carry, None)]),
            Some(EnsureRelayDefect::NoRelay),
            "{carry:?}"
        );
    }
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
        private_scratch: false,
        cleanup: CLOSURE,
        relay: None,
        cell_live: false,
        foreign: None,
        other: Document::Str("primop 'raw_raise'(Type1, Error1, Stack1)"),
        span: span(),
    };
    assert_eq!(defect(&[no_token]), None);
}

/// BT-4429: `Block>>ensure:` reached by `perform:` runs in the Block method,
/// where every `^` is foreign: no token of the scope, a state kind known only
/// at run time. Its closure cleanup may write the cell, so the relay must
/// resume every `$bt_nlr` throw with the cell over the carried state when
/// that state is the cell's; re-raising it (the fallback before the fix)
/// loses the cleanup's write.
#[test]
fn a_foreign_throw_needs_the_cell_over_the_carried_state_when_it_is_the_cells() {
    let catch = |carry: NlrCarry, cleanup: EnsureCleanup, relay: Option<EnsureNlrRelay>| {
        ThreadedStmt::EnsureCatch {
            vars: Box::new(vars()),
            token: None,
            carry: Some(carry),
            private_scratch: false,
            cleanup,
            relay,
            cell_live: false,
            foreign: None,
            other: Document::Str("primop 'raw_raise'(Type1, Error1, Stack1)"),
            span: span(),
        }
    };
    let foreign = |relay| catch(NlrCarry::Foreign, CLOSURE, relay);
    assert_eq!(defect(&[foreign(None)]), Some(EnsureRelayDefect::NoRelay));
    for stale in [RethrowSlot::CellOverCarried, RethrowSlot::Version(0)] {
        assert_eq!(
            defect(&[foreign(Some(relay(stale)))]),
            Some(EnsureRelayDefect::RethrowsStaleState)
        );
    }
    let ir = [foreign(Some(relay(RethrowSlot::CellIfCarried)))];
    assert_eq!(defect(&ir), None);
    let text = lower_and_render(&ir).to_pretty_string();
    assert!(
        text.contains("<{'throw', {'$bt_nlr', Tok1, Val1, Carried1}}> when 'true' -> let Snap1 = call 'beamtalk_actor_state':'snapshot'() in let StateAcc = Carried1 in")
            && text.contains(
                "call 'erlang':'throw'({'$bt_nlr', Tok1, Val1, call 'beamtalk_actor_state':'refresh_carried'(Snap1, Carried1)})"
            ),
        "{text}"
    );
    // A cleanup that cannot write the cell has nothing to relay.
    let quiet = catch(
        NlrCarry::Foreign,
        EnsureCleanup::Closure { writes_cell: false },
        None,
    );
    assert_eq!(defect(&[quiet]), None);
    // The scope's own carries still need their token.
    assert_eq!(defect(&[catch(NlrCarry::ActorState, CLOSURE, None)]), None);
}

/// ADR 0146 R1: an `ensure:` whose cleanup is `cleanup`, where the
/// actor-state cell is `cell_live`, with `foreign` the clause a foreign
/// `$bt_nlr` throw takes.
fn foreign_node(
    cell_live: bool,
    cleanup: EnsureCleanup,
    foreign: Option<EnsureForeignNlr>,
) -> ThreadedStmt {
    ThreadedStmt::EnsureCatch {
        vars: Box::new(vars()),
        token: None,
        carry: Some(NlrCarry::ActorState),
        private_scratch: false,
        cleanup,
        relay: None,
        cell_live,
        foreign,
        other: Document::Str("primop 'raw_raise'(Type1, Error1, Stack1)"),
        span: span(),
    }
}

fn foreign_clause(store: Option<usize>) -> EnsureForeignNlr {
    EnsureForeignNlr {
        body: ForeignCleanupBody::Own {
            seed: Document::Str(
                "let StateAcc = call 'beamtalk_actor_state':'refresh'(Self, StateAcc) in ",
            ),
            cleanup: Document::Str("let StateAcc1 = call 'maps':'put'('n', 40, StateAcc) in"),
        },
        store,
    }
}

fn foreign_defect(ir: &[ThreadedStmt]) -> Option<ForeignStoreDefect> {
    match verify(ir).as_slice() {
        [] => None,
        [VerifyError::EnsureForeignCleanupNotStored { defect, at }] => {
            assert_eq!(*at, span());
            Some(*defect)
        }
        other => panic!("expected one EnsureForeignCleanupNotStored, got {other:?}"),
    }
}

/// ADR 0146 R1: a foreign `^` (one of another method of the same actor)
/// unwinding through an inlined cleanup that advanced `StateAcc` must leave
/// the cleanup's final version in the cell. The clause verifies and renders
/// the cleanup, the store and the unchanged re-raise, before the error path.
#[test]
fn a_foreign_throw_stores_the_cleanups_final_state_acc_where_the_cell_is_live() {
    let ir = [foreign_node(
        true,
        inlined(Some(1)),
        Some(foreign_clause(Some(1))),
    )];
    assert_eq!(foreign_defect(&ir), None);
    let text = lower_and_render(&ir).to_pretty_string();
    assert!(
        text.contains(
            "case {Type1, Error1} of <{'throw', Foreign1}> when call 'erlang':'=:='(call 'erlang':'element'(1, Foreign1), '$bt_nlr') -> let StateAcc = call 'beamtalk_actor_state':'refresh'(Self, StateAcc) in let StateAcc1 = call 'maps':'put'('n', 40, StateAcc) in let _ = call 'beamtalk_actor_state':'store_unwound'(Foreign1, StateAcc1) in primop 'raw_raise'(Type1, Error1, Stack1) <Other1> when 'true' -> primop 'raw_raise'(Type1, Error1, Stack1) end"
        ),
        "{text}"
    );
}

/// The repro: no foreign clause, so the throw takes the error path, whose
/// cleanup runs with write-through off; or a clause that stores nothing, or
/// a stale version.
#[test]
fn a_foreign_throw_that_does_not_store_the_cleanups_final_state_acc_fails() {
    assert_eq!(
        foreign_defect(&[foreign_node(true, inlined(Some(1)), None)]),
        Some(ForeignStoreDefect::NoClause)
    );
    assert_eq!(
        foreign_defect(&[foreign_node(
            true,
            inlined(Some(1)),
            Some(foreign_clause(None))
        )]),
        Some(ForeignStoreDefect::NoStore)
    );
    assert_eq!(
        foreign_defect(&[foreign_node(
            true,
            inlined(Some(2)),
            Some(foreign_clause(Some(1)))
        )]),
        Some(ForeignStoreDefect::StoresStaleVersion {
            stored: 1,
            expected: 2
        })
    );
}

/// Nothing to store: a cleanup that threads no write, a closure cleanup
/// (it writes the cell in place), or a scope where the cell is not live (a
/// class method running in its caller, a value type, the REPL, a closure).
#[test]
fn no_foreign_store_is_owed_where_there_is_nothing_to_store() {
    assert_eq!(
        foreign_defect(&[foreign_node(true, inlined(None), None)]),
        None
    );
    assert_eq!(foreign_defect(&[foreign_node(true, CLOSURE, None)]), None);
    assert_eq!(
        foreign_defect(&[foreign_node(false, inlined(Some(1)), None)]),
        None
    );
}

/// BT-4598: an actor method's own relay, seeded from the cell and storing to
/// it, resuming with `StateAcc{version}`.
fn cell_relay(version: usize) -> EnsureNlrRelay {
    EnsureNlrRelay {
        seed: Document::Str(
            "let StateAcc = call 'beamtalk_actor_state':'refresh'(Self, StateAcc) in ",
        ),
        cleanup: Document::Str("let StateAcc1 = call 'maps':'put'('n', 40, StateAcc) in"),
        cell_store: true,
        rethrow: RethrowSlot::Version(version),
    }
}

/// BT-4598: an `ensure:` of an actor method's threaded body whose foreign
/// clause shares `relay`'s cleanup.
fn shared_node(token: bool, relay: Option<EnsureNlrRelay>) -> ThreadedStmt {
    ThreadedStmt::EnsureCatch {
        vars: Box::new(vars()),
        token: token.then(|| TokenId::new("NlrToken1")),
        carry: Some(NlrCarry::ActorState),
        private_scratch: false,
        cleanup: inlined(Some(1)),
        relay,
        cell_live: true,
        foreign: Some(EnsureForeignNlr {
            body: ForeignCleanupBody::SharedWithRelay,
            store: Some(1),
        }),
        other: Document::Str("primop 'raw_raise'(Type1, Error1, Stack1)"),
        span: span(),
    }
}

/// BT-4598: where the cell is live, the relay and the foreign clause share
/// one compiled cleanup: a single `$bt_nlr` clause runs it once, then the
/// scope's own throw stores (`store/1`) and resumes with the cleanup's
/// `StateAcc`, and any other stores it (`store_unwound/2`) and is re-raised
/// as it came.
#[test]
fn a_foreign_clause_sharing_the_relays_cleanup_renders_one_clause() {
    let ir = [shared_node(true, Some(cell_relay(1)))];
    assert_eq!(verify(&ir), vec![]);
    let text = lower_and_render(&ir).to_pretty_string();
    assert!(
        text.contains(
            "case {Type1, Error1} of <{'throw', Foreign1}> when call 'erlang':'=:='(call 'erlang':'element'(1, Foreign1), '$bt_nlr') -> let StateAcc = call 'beamtalk_actor_state':'refresh'(Self, StateAcc) in let StateAcc1 = call 'maps':'put'('n', 40, StateAcc) in case Foreign1 of {'$bt_nlr', Tok1, Val1, Carried1} when call 'erlang':'=:='(Tok1, NlrToken1) -> let _ = call 'beamtalk_actor_state':'store'(StateAcc1) in call 'erlang':'throw'({'$bt_nlr', Tok1, Val1, StateAcc1}) <_> when 'true' -> let _ = call 'beamtalk_actor_state':'store_unwound'(Foreign1, StateAcc1) in primop 'raw_raise'(Type1, Error1, Stack1) end <Other1> when 'true' -> primop 'raw_raise'(Type1, Error1, Stack1) end"
        ),
        "{text}"
    );
    assert_eq!(
        text.matches("'maps':'put'('n', 40,").count(),
        1,
        "the cleanup is compiled once:\n{text}"
    );
}

/// BT-4598: a foreign clause that shares the relay's cleanup has nothing to
/// run without a relay of the scope's own throw that runs it from the cell.
#[test]
fn a_foreign_clause_sharing_no_cell_relay_fails() {
    let not_from_cell = EnsureNlrRelay {
        cell_store: false,
        ..cell_relay(1)
    };
    for ir in [
        shared_node(true, None),
        shared_node(false, Some(cell_relay(1))),
        shared_node(true, Some(not_from_cell)),
    ] {
        assert!(
            verify(std::slice::from_ref(&ir)).contains(
                &VerifyError::EnsureForeignCleanupNotStored {
                    defect: ForeignStoreDefect::SharedWithoutCellRelay,
                    at: span(),
                }
            ),
            "{ir:?}"
        );
    }
}
