// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared announce-selector vocabulary.
//!
//! `announce:` and its two variants are recognised by two independent
//! consumers that must agree on the exact same three strings:
//!
//! - **`semantic_analysis::type_checker::validation::check_arg_sendability`**
//!   (ADR 0103) — treats an announce send's payload argument as crossing a
//!   process boundary, the same as an actor-instance message argument.
//! - **`language_service::announce_sites_query`** — mines `announce:` emission sites
//!   out of a method's AST for `SystemNavigation announcementsSentBy:`
//!   (BT-2475).
//!
//! `semantic_analysis` (Compilation) and `queries` (Language Service) must
//! never depend on each other (ADR 0117 §1); since both need this exact
//! vocabulary fact, it lives here instead — a leaf module beneath both,
//! following the `synthetic_selectors.rs` / `state_threading_selectors.rs`
//! pattern (BT-3341).

/// The announce selectors recognised as emission sites. The event argument is
/// always the first keyword argument; `announceAndWait:timeout:` carries the
/// timeout as a second argument.
const ANNOUNCE_SELECTORS: [&str; 3] = ["announce:", "announceAndWait:", "announceAndWait:timeout:"];

/// Whether `selector_name` is one of the recognised announce selectors
/// (`announce:`, `announceAndWait:`, `announceAndWait:timeout:`).
#[must_use]
pub(crate) fn is_announce_selector(selector_name: &str) -> bool {
    ANNOUNCE_SELECTORS.contains(&selector_name)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn recognises_all_three_announce_selectors() {
        assert!(is_announce_selector("announce:"));
        assert!(is_announce_selector("announceAndWait:"));
        assert!(is_announce_selector("announceAndWait:timeout:"));
    }

    #[test]
    fn rejects_non_announce_selectors() {
        assert!(!is_announce_selector("announce"));
        assert!(!is_announce_selector("doSomething:"));
        assert!(!is_announce_selector(""));
    }
}
