// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `VersionedVar`/`FrameId` naming and `VersionCounter` basics.

use super::*;

// ── VersionedVar / FrameId basics ───────────────────────────────────

#[test]
fn render_name_version_zero_is_bare_prefix() {
    assert_eq!(
        VersionedVar::new(VersionPrefix::State, 0, FrameId::ROOT).render_name(),
        "State"
    );
}

#[test]
fn render_name_nonzero_version_appends_number() {
    assert_eq!(
        VersionedVar::new(VersionPrefix::ClassVars, 2, FrameId::ROOT).render_name(),
        "ClassVars2"
    );
    assert_eq!(local("Sum", 1, FrameId::ROOT).render_name(), "Sum1");
}

// ── VersionCounter ────────────────────────────────────────────────────
// Pins the same semantics the pre-existing `StateThreading` struct
// (`state_codegen.rs`) pinned, now against the single shared
// implementation reused for all three prefixes.

#[test]
fn version_counter_starts_at_zero() {
    let counter = VersionCounter::new();
    assert_eq!(counter.version(), 0);
    assert_eq!(counter.current_var(VersionPrefix::State), "State");
}

#[test]
fn version_counter_next_var_increments_and_persists() {
    let mut counter = VersionCounter::new();
    assert_eq!(counter.next_var(VersionPrefix::State), "State1");
    assert_eq!(counter.version(), 1);
    assert_eq!(counter.current_var(VersionPrefix::State), "State1");
    assert_eq!(counter.next_var(VersionPrefix::State), "State2");
    assert_eq!(counter.version(), 2);
}

#[test]
fn version_counter_reset_returns_to_zero() {
    let mut counter = VersionCounter::new();
    counter.next_var(VersionPrefix::SelfVt);
    counter.next_var(VersionPrefix::SelfVt);
    assert_eq!(counter.version(), 2);
    counter.reset();
    assert_eq!(counter.version(), 0);
    assert_eq!(counter.current_var(VersionPrefix::SelfVt), "Self");
}

#[test]
fn version_counter_set_version_overwrites_directly() {
    let mut counter = VersionCounter::new();
    counter.set_version(5);
    assert_eq!(counter.version(), 5);
    assert_eq!(counter.current_var(VersionPrefix::State), "State5");
}

#[test]
fn version_counter_is_reused_identically_across_prefixes() {
    // Same counter value, three different prefixes — pins that naming is
    // purely a function of (prefix, version), never counter identity.
    let mut counter = VersionCounter::new();
    counter.set_version(3);
    assert_eq!(counter.current_var(VersionPrefix::State), "State3");
    assert_eq!(counter.current_var(VersionPrefix::ClassVars), "ClassVars3");
    assert_eq!(counter.current_var(VersionPrefix::SelfVt), "Self3");
}
