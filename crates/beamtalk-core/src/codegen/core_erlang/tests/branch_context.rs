// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-3131: Discipline-pinning tests for `with_branch_context`'s per-prefix
//! save/reset/restore policy. Unifying `StateThreading`/`class_var_version`/
//! `self_version` behind the shared `VersionCounter` must unify their
//! naming/identity *shape* only — each prefix's branch-entry/exit
//! *discipline* stays exactly as documented on `BranchContextGuard`
//! (ADR 0111 §Phase A2 / BT-1449 / BT-1550 / BT-3131).

use super::*;

/// state: reset to 0 on entry, restored to the outer version on exit.
#[test]
fn with_branch_context_state_resets_on_entry_and_restores_on_exit() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_state_version(3);
    let entry_version = generator.with_branch_context(|g| {
        let entry = g.state_version();
        g.next_state_var();
        entry
    });
    assert_eq!(entry_version, 0, "state resets to 0 on branch entry");
    assert_eq!(
        generator.state_version(),
        3,
        "state restores to the outer version on exit"
    );
}

/// `class_vars`: NOT reset on entry — the branch inherits the outer scope's
/// current version — but restored to it on exit (BT-1449/BT-1550).
#[test]
fn with_branch_context_class_vars_inherits_on_entry_and_restores_on_exit() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_class_var_version(2);
    let entry_version = generator.with_branch_context(|g| {
        let entry = g.class_var_version();
        g.next_class_var();
        entry
    });
    assert_eq!(
        entry_version, 2,
        "class_vars inherits the outer version on branch entry (no reset)"
    );
    assert_eq!(
        generator.class_var_version(),
        2,
        "class_vars restores to the outer version on exit"
    );
}

/// `class_var_mutated`: sticky — set inside a branch, deliberately NOT
/// restored when the branch exits (BT-1550).
#[test]
fn with_branch_context_class_var_mutated_is_sticky_across_exit() {
    let mut generator = CoreErlangGenerator::new("test");
    assert!(!generator.class_var_mutated());
    generator.with_branch_context(|g| {
        g.next_class_var(); // sets class_var_mutated = true
    });
    assert!(
        generator.class_var_mutated(),
        "class_var_mutated must stay sticky after with_branch_context exits"
    );
}

/// self: BT-3131 decision, revised during review — NOT reset on entry (the
/// branch inherits the outer scope's current version, same as
/// `class_vars`), restored to it on exit. Fixes the prior "live landmine"
/// of neither save nor restore without introducing a stale-read regression:
/// unlike `state`, `Self{N}` has no loop-body rename, so a reset-on-entry
/// policy would silently read the pre-mutation value on a `self.field`
/// read inside a branch/loop that follows an earlier `self.field :=` in
/// the same method.
#[test]
fn with_branch_context_self_inherits_on_entry_and_restores_on_exit() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_self_version(4);
    let entry_version = generator.with_branch_context(|g| {
        let entry = g.self_version();
        g.next_self_var();
        entry
    });
    assert_eq!(
        entry_version, 4,
        "self inherits the outer version on branch entry (no reset, BT-3131)"
    );
    assert_eq!(
        generator.self_version(),
        4,
        "self restores to the outer version on exit"
    );
}

/// `in_loop_body`: true for the duration of `f`, restored on exit.
#[test]
fn with_branch_context_sets_in_loop_body_and_restores_it() {
    let mut generator = CoreErlangGenerator::new("test");
    assert!(!generator.in_loop_body);
    let inner = generator.with_branch_context(|g| g.in_loop_body);
    assert!(inner, "in_loop_body is true for the duration of f");
    assert!(
        !generator.in_loop_body,
        "in_loop_body restores to false on exit"
    );
}

/// Nested `with_branch_context` calls each independently save/restore —
/// pins that the RAII guard composes correctly when branches nest (e.g. a
/// conditional inside a loop body, or sibling/nested conditionals).
#[test]
fn with_branch_context_nests_independently() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_state_version(1);
    generator.next_state_var(); // outer scope: State2
    assert_eq!(generator.state_version(), 2);

    generator.with_branch_context(|outer| {
        assert_eq!(outer.state_version(), 0, "outer branch resets to 0");
        outer.next_state_var(); // outer branch: State1

        outer.with_branch_context(|inner| {
            assert_eq!(inner.state_version(), 0, "nested branch resets again");
            inner.next_state_var();
            assert_eq!(inner.state_version(), 1);
        });

        assert_eq!(
            outer.state_version(),
            1,
            "outer branch's version is restored after the nested branch exits"
        );
    });

    assert_eq!(
        generator.state_version(),
        2,
        "outermost state is restored after all branches exit"
    );
}

/// `with_branch_context`'s restore runs even when `f` returns an `Err` — the
/// same "unconditional restore" guarantee the pre-BT-3131 manual
/// save/restore documented, now provided by `BranchContextGuard`'s `Drop`.
#[test]
fn with_branch_context_restores_even_when_f_returns_err() {
    let mut generator = CoreErlangGenerator::new("test");
    generator.set_state_version(5);
    generator.set_class_var_version(2);
    generator.set_self_version(7);

    let result: std::result::Result<(), &'static str> = generator.with_branch_context(|g| {
        g.next_state_var();
        g.next_class_var();
        g.next_self_var();
        Err("boom")
    });

    assert_eq!(result, Err("boom"));
    assert_eq!(generator.state_version(), 5);
    assert_eq!(generator.class_var_version(), 2);
    assert_eq!(generator.self_version(), 7);
}
