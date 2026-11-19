// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Reach recording for the `*_open` lowerings (BT-5041).
//!
//! Each `*_open` function documents, in a `# Reached by` note, the input that
//! makes the codegen call it. [`hit`] lets `tests/open_reach.rs` compile that
//! input and assert the function ran, so the note cannot go stale unnoticed
//! (a function no input reaches is dead and should be deleted). In a non-test
//! build [`hit`] is an empty inline function.

/// Records that the lowering `name` ran on this thread (test builds only).
#[cfg(test)]
pub(crate) fn hit(name: &'static str) {
    HITS.with(|h| {
        h.borrow_mut().insert(name);
    });
}

/// Records that the lowering `name` ran (a no-op outside test builds).
#[cfg(not(test))]
#[inline(always)]
pub(crate) fn hit(_name: &'static str) {}

#[cfg(test)]
thread_local! {
    static HITS: std::cell::RefCell<std::collections::BTreeSet<&'static str>> =
        const { std::cell::RefCell::new(std::collections::BTreeSet::new()) };
}

/// Takes (and clears) the lowerings recorded on this thread.
#[cfg(test)]
pub(crate) fn take() -> std::collections::BTreeSet<&'static str> {
    HITS.with(|h| std::mem::take(&mut *h.borrow_mut()))
}
