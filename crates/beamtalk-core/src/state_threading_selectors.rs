// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Shared selector classification helpers for state-threading control flow.
//!
//! Used by both `semantic_analysis` (for `DispatchKind::ControlFlow` classification)
//! and `codegen` (for block-mutation analysis and state-threading code generation).
//!
//! BT-3362 (ADR 0117 Decision step 5): the module and its four predicates
//! widened from `pub(crate)` to `pub` — `codegen`'s consumer moved out into
//! the standalone `beamtalk-codegen` crate, so a `pub(crate)` item it reached
//! had to become genuinely `pub` once that consumer left the crate.
//!
//! Predicates in this module classify selectors for block-mutation analysis.
//! Where a selector is also part of [`WellKnownSelector`](crate::ast::WellKnownSelector)
//! — conditionals, `on:do:` — the predicate matches on the enum via
//! [`WellKnownSelector::from_name`] so a typo in either place is a compile
//! error at the call site. Loop and iteration selectors (`whileTrue:`, `do:`,
//! `collect:`, etc.) are not classified as well-known and so remain matched
//! by string; the split is deliberate — `WellKnownSelector` is reserved for
//! selectors the type-checker/codegen *intrinsify*, not every selector the
//! compiler happens to recognise.
//!
//! BT-3402: `and:`/`or:` join the string-matched group for the same reason
//! as the loop/iteration selectors — they are ordinary self-hosted
//! `Boolean` methods (`Boolean.bt`'s `and:`/`or:`, each defined in terms of
//! `ifTrue:ifFalse:`), not selectors the type-checker itself intrinsifies.
//! Codegen still inlines their literal call shape the same way it inlines
//! `ifTrue:`/`ifFalse:` (see `try_generate_boolean_protocol`'s `and:`/`or:`
//! arms in `beamtalk-codegen`), so they need the same state-threading
//! classification here — both as a state-threading keyword selector (so
//! `check_actor_field_mutation_in_closure`/`DispatchKind::ControlFlow`
//! treat a mutation inside their block argument as safe) and as a
//! conditional selector (so `control_flow_has_mutations`/
//! `classify_body_expr` route the enclosing statement through the same
//! `{Result, NewState}` unpacking as `ifTrue:`/`ifFalse:`).

use crate::ast::WellKnownSelector;

/// Returns `true` if `sel` is a state-threading keyword selector.
///
/// These selectors receive block arguments that are analysed for field mutations
/// as part of state-threading control flow.
#[must_use]
pub fn is_state_threading_keyword_selector(sel: &str) -> bool {
    if matches!(
        WellKnownSelector::from_name(sel),
        Some(
            WellKnownSelector::OnDo
                | WellKnownSelector::IfTrue
                | WellKnownSelector::IfFalse
                | WellKnownSelector::IfTrueIfFalse
                | WellKnownSelector::IfNotNil
                // BT-3420 (ADR 0118 phase 4): `ifNil:`/`ifNil:ifNotNil:`/
                // `ifNotNil:ifNil:` get the same `_with_mutations` inline-case
                // treatment as `ifNotNil:` — their block argument(s) are
                // analysed for field mutations the same way.
                | WellKnownSelector::IfNil
                | WellKnownSelector::IfNilIfNotNil
                | WellKnownSelector::IfNotNilIfNil,
        )
    ) {
        return true;
    }
    matches!(
        sel,
        "whileTrue:"
            | "whileFalse:"
            | "timesRepeat:"
            | "to:do:"
            | "to:by:do:"
            | "do:"
            | "collect:"
            | "select:"
            | "reject:"
            | "anySatisfy:"
            | "allSatisfy:"
            | "detect:"
            | "detect:ifNone:"
            | "count:"
            | "flatMap:"
            | "takeWhile:"
            | "dropWhile:"
            | "groupBy:"
            | "partition:"
            | "sort:"
            | "inject:into:"
            | "eachWithIndex:"
            | "do:separatedBy:"
            | "doWithKey:"
            | "keysAndValuesDo:"
            | "ensure:"
            // BT-3402: see module doc comment.
            | "and:"
            | "or:"
    )
}

/// Returns `true` if `sel` is a state-threading unary selector.
#[must_use]
pub fn is_state_threading_unary_selector(sel: &str) -> bool {
    matches!(sel, "whileTrue" | "whileFalse" | "timesRepeat")
}

/// Returns `true` if `sel` is an exception-handling selector (`on:do:` or `ensure:`).
///
/// For these selectors the *receiver* (try body) is a block that must also be
/// analysed for field mutations, in addition to the argument blocks.
#[must_use]
pub fn is_exception_selector(sel: &str) -> bool {
    // `on:do:` is a well-known selector; `ensure:` is not (it's not
    // intrinsified by the type-checker, only by codegen's state-threading).
    matches!(
        WellKnownSelector::from_name(sel),
        Some(WellKnownSelector::OnDo)
    ) || sel == "ensure:"
}

/// Returns `true` if `sel` is a Boolean/optional conditional selector.
///
/// For these selectors every block argument must be analysed independently,
/// because mutations may appear in the first branch but not the second.
///
/// BT-3402: `and:`/`or:` are not `WellKnownSelector`s (see module doc
/// comment) so they're matched by string, the same way `is_exception_selector`
/// folds in `ensure:` alongside the well-known `on:do:`.
///
/// BT-3420 (ADR 0118 phase 4): `ifNil:`/`ifNil:ifNotNil:`/`ifNotNil:ifNil:`
/// join `ifNotNil:` here — each has its own `_with_mutations` inline-case
/// generator (`generate_nil_conditional_with_mutations` in
/// `beamtalk-codegen`), so a mutation in any of their block(s) threads the
/// same way `ifTrue:`/`ifFalse:` does.
#[must_use]
pub fn is_conditional_selector(sel: &str) -> bool {
    matches!(
        WellKnownSelector::from_name(sel),
        Some(
            WellKnownSelector::IfTrue
                | WellKnownSelector::IfFalse
                | WellKnownSelector::IfTrueIfFalse
                | WellKnownSelector::IfNotNil
                | WellKnownSelector::IfNil
                | WellKnownSelector::IfNilIfNotNil
                | WellKnownSelector::IfNotNilIfNil,
        )
    ) || matches!(sel, "and:" | "or:")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keyword_selectors() {
        assert!(is_state_threading_keyword_selector("whileTrue:"));
        assert!(is_state_threading_keyword_selector("do:"));
        assert!(is_state_threading_keyword_selector("on:do:"));
        assert!(is_state_threading_keyword_selector("ifTrue:"));
        assert!(is_state_threading_keyword_selector("doWithKey:"));
        assert!(is_state_threading_keyword_selector("keysAndValuesDo:"));
        // BT-1486: New block-accepting selectors
        assert!(is_state_threading_keyword_selector("detect:"));
        assert!(is_state_threading_keyword_selector("detect:ifNone:"));
        assert!(is_state_threading_keyword_selector("count:"));
        assert!(is_state_threading_keyword_selector("flatMap:"));
        // BT-1487: Medium-risk list selectors
        assert!(is_state_threading_keyword_selector("takeWhile:"));
        assert!(is_state_threading_keyword_selector("dropWhile:"));
        assert!(is_state_threading_keyword_selector("groupBy:"));
        assert!(is_state_threading_keyword_selector("partition:"));
        assert!(is_state_threading_keyword_selector("sort:"));
        // BT-2703: enumeration helpers self-hosted on inject:into:
        assert!(is_state_threading_keyword_selector("eachWithIndex:"));
        assert!(is_state_threading_keyword_selector("do:separatedBy:"));
        assert!(!is_state_threading_keyword_selector("perform:"));
        // BT-3402: `and:`/`or:` block arguments compile inline the same way.
        assert!(is_state_threading_keyword_selector("and:"));
        assert!(is_state_threading_keyword_selector("or:"));
        // BT-3420: ifNil:/ifNil:ifNotNil:/ifNotNil:ifNil: block(s) compile
        // inline the same way as ifTrue:/ifNotNil:.
        assert!(is_state_threading_keyword_selector("ifNil:"));
        assert!(is_state_threading_keyword_selector("ifNil:ifNotNil:"));
        assert!(is_state_threading_keyword_selector("ifNotNil:ifNil:"));
    }

    #[test]
    fn unary_selectors() {
        assert!(is_state_threading_unary_selector("whileTrue"));
        assert!(is_state_threading_unary_selector("whileFalse"));
        assert!(is_state_threading_unary_selector("timesRepeat"));
        assert!(!is_state_threading_unary_selector("do:"));
    }

    #[test]
    fn exception_selectors() {
        assert!(is_exception_selector("on:do:"));
        assert!(is_exception_selector("ensure:"));
        assert!(!is_exception_selector("ifTrue:"));
        assert!(!is_exception_selector("do:"));
    }

    #[test]
    fn conditional_selectors() {
        assert!(is_conditional_selector("ifTrue:"));
        assert!(is_conditional_selector("ifFalse:"));
        assert!(is_conditional_selector("ifTrue:ifFalse:"));
        assert!(is_conditional_selector("ifNotNil:"));
        assert!(!is_conditional_selector("on:do:"));
        assert!(!is_conditional_selector("do:"));
        // BT-3402
        assert!(is_conditional_selector("and:"));
        assert!(is_conditional_selector("or:"));
        // BT-3420 (ADR 0118 phase 4)
        assert!(is_conditional_selector("ifNil:"));
        assert!(is_conditional_selector("ifNil:ifNotNil:"));
        assert!(is_conditional_selector("ifNotNil:ifNil:"));
    }
}
