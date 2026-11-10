// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0041 §State-Carrying NLR: the `NlrThrow` node's rendering and the
//! every-scope `NlrThrowWithoutSlot` check.

use super::*;

fn state(version: usize, frame: FrameId) -> VersionedVar {
    VersionedVar::new(VersionPrefix::State, version, frame)
}

fn nlr_throw(slot: Vec<VersionedVar>) -> ThreadedStmt {
    ThreadedStmt::NlrThrow {
        token: TokenId::new("NlrTok"),
        value: ValueRef::Var("Val".to_string()),
        slot,
        result_var: "_NlrThrow1".to_string(),
        cell_live: false,
        cell_store: false,
        span: span(),
    }
}

#[test]
fn an_nlr_throw_with_an_empty_slot_is_rejected_in_every_scope() {
    // `render_nlr_throw` would render the slot as `{}`: an empty tuple no catch
    // unpacks.
    let empty = vec![nlr_throw(Vec::new())];
    for scope in [
        ScopeKind::ClassMethod,
        ScopeKind::ValueType,
        ScopeKind::Instance,
    ] {
        assert_eq!(
            verify_in_scope(&empty, scope),
            vec![VerifyError::NlrThrowWithoutSlot { at: span() }],
            "{scope:?}"
        );
    }
    // A throw that names its slot verifies clean.
    let named = vec![nlr_throw(vec![state(0, FrameId::ROOT)])];
    assert_eq!(verify_in_scope(&named, ScopeKind::Instance), Vec::new());
}

#[test]
fn nlr_throw_renders_its_slot_bare_or_as_a_tuple() {
    let frame = FrameId::ROOT;
    assert_eq!(
        lower_and_render(&[nlr_throw(vec![self_var(2, frame)])]).to_pretty_string(),
        "let _NlrThrow1 = call 'erlang':'throw'({'$bt_nlr', NlrTok, Val, Self2}) in "
    );
    assert_eq!(
        lower_and_render(&[nlr_throw(vec![state(1, frame), self_var(2, frame)])])
            .to_pretty_string(),
        "let _NlrThrow1 = call 'erlang':'throw'({'$bt_nlr', NlrTok, Val, {State1, Self2}}) in "
    );
}

/// BT-4535: where the actor-state cell is live a throw stores its slot to the
/// cell before it throws (`NlrThrowWithoutCellStore` otherwise).
#[test]
fn a_cell_live_throw_without_its_cell_store_is_rejected() {
    let with = |cell_live, cell_store| {
        let ThreadedStmt::NlrThrow {
            token,
            value,
            slot,
            result_var,
            span,
            ..
        } = nlr_throw(vec![state(0, FrameId::ROOT)])
        else {
            unreachable!()
        };
        vec![ThreadedStmt::NlrThrow {
            token,
            value,
            slot,
            result_var,
            cell_live,
            cell_store,
            span,
        }]
    };
    assert_eq!(
        verify_in_scope(&with(true, false), ScopeKind::Instance),
        vec![VerifyError::NlrThrowWithoutCellStore { at: span() }]
    );
    assert_eq!(
        verify_in_scope(&with(true, true), ScopeKind::Instance),
        Vec::new()
    );
    assert_eq!(
        verify_in_scope(&with(false, false), ScopeKind::Instance),
        Vec::new()
    );
    assert_eq!(
        lower_and_render(&with(true, true)).to_pretty_string(),
        "let _ = call 'beamtalk_actor_state':'store'(State) in let _NlrThrow1 = call 'erlang':'throw'({'$bt_nlr', NlrTok, Val, State}) in "
    );
}
