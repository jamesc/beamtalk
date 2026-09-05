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
//! `Boolean` methods (`boolean.bt`'s `and:`/`or:`, each defined in terms of
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

/// BT-3423 (ADR 0118 §Decision 7): the single "which selectors thread which
/// block-argument positions" table, replacing the parallel copies that
/// previously lived in `beamtalk-core::ast::well_known::is_state_threaded_block_arg`
/// (consulted by `beamtalk-codegen`'s `block_arg_for_selector`) and (via a
/// thin per-crate wrapper) `beamtalk-lint`'s `DeadAssignment` check — the
/// second of which was missing `and:`/`or:` from BT-3402 onward (the gap
/// this issue closes).
///
/// Returns the 0-based argument indices of `sel` whose block-literal
/// argument's outer-local mutations are threaded back to the caller via the
/// `StateAcc` map (not silently lost). An empty slice at a given index does
/// **not** mean that argument is compiled as an isolated closure — see the
/// caveat below for the one selector where that distinction bites
/// (`detect:ifNone:`). An index can be excluded from this table because:
/// `sel` isn't a state-threading selector at all; its threading is
/// context-dependent (`eachWithIndex:`/`do:separatedBy:` only thread inside
/// an Actor's own fold, not always — see
/// `CoreErlangGenerator::enumeration_threads_actor_state` in
/// `beamtalk-codegen`); the threaded block is the *receiver*, not an
/// argument (`whileTrue:`/`whileFalse:`'s condition block, `on:do:`/
/// `ensure:`'s try/protected block) — receiver-position threading is
/// decided separately by each caller (codegen already special-cases the
/// receiver for these selectors; the lint never inspects a receiver block
/// at all); or (`detect:ifNone:`'s `ifNone:` handler, index 1) the
/// argument's own local-variable writes simply aren't part of the fold this
/// table tracks, even though the block itself is still compiled inline
/// (not a closure) once codegen detects a mutation in *either* argument —
/// see [`crate::semantic_analysis::facts::StateEffects`]'s doc comment,
/// which answers the "is this a closure boundary" question this table does
/// NOT answer, and deliberately does not derive its answer from this one.
///
/// Gated on [`is_state_threading_keyword_selector`] so this table can never
/// claim an argument index for a selector that isn't classified as
/// state-threading in the first place — the two are derived from the same
/// selector data, even though the index mapping below is necessarily
/// selector-specific (arities and block-argument positions vary).
#[must_use]
pub fn state_threaded_block_arg_indices(sel: &str) -> &'static [usize] {
    if !is_state_threading_keyword_selector(sel) {
        return &[];
    }
    match sel {
        // Context-dependent — see this function's doc comment.
        "eachWithIndex:" | "do:separatedBy:" => &[],
        "to:do:" | "inject:into:" | "on:do:" => &[1],
        "to:by:do:" => &[2],
        // Two-block conditionals: `generate_*_with_mutations` threads both
        // branches (BT-1392/BT-2359 for `ifTrue:ifFalse:`; BT-3420's
        // `generate_nil_conditional_with_mutations` for the `ifNil:`
        // two-block forms).
        "ifTrue:ifFalse:" | "ifNil:ifNotNil:" | "ifNotNil:ifNil:" => &[0, 1],
        // BT-3402/BT-3423: `and:`/`or:` join `ifTrue:`/`ifFalse:` here —
        // same single-block-argument shape.
        _ => &[0],
    }
}

/// Returns `true` if a block-literal argument at `arg_index` for `selector`
/// is state-threaded per [`state_threaded_block_arg_indices`].
#[must_use]
pub fn is_state_threaded_block_arg(selector: &str, arg_index: usize) -> bool {
    state_threaded_block_arg_indices(selector).contains(&arg_index)
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

    // ---------------------------------------------------------------
    // state_threaded_block_arg_indices / is_state_threaded_block_arg
    // (BT-3423 / ADR 0118 §7 — the one selector table)
    // ---------------------------------------------------------------

    #[test]
    fn threaded_block_arg_single_block_selectors_are_index_zero() {
        for sel in [
            "whileTrue:",
            "whileFalse:",
            "timesRepeat:",
            "do:",
            "collect:",
            "select:",
            "reject:",
            "anySatisfy:",
            "allSatisfy:",
            "detect:",
            "detect:ifNone:",
            "count:",
            "flatMap:",
            "takeWhile:",
            "dropWhile:",
            "groupBy:",
            "partition:",
            "sort:",
            "doWithKey:",
            "keysAndValuesDo:",
            "ensure:",
            "ifTrue:",
            "ifFalse:",
            "ifNotNil:",
            "ifNil:",
            // BT-3402/BT-3423: the gap this issue closes.
            "and:",
            "or:",
        ] {
            assert_eq!(
                state_threaded_block_arg_indices(sel),
                &[0],
                "expected {sel:?} to thread only its argument at index 0"
            );
            assert!(is_state_threaded_block_arg(sel, 0), "{sel:?} arg 0");
            assert!(!is_state_threaded_block_arg(sel, 1), "{sel:?} arg 1");
        }
    }

    #[test]
    fn threaded_block_arg_two_block_selectors() {
        for sel in ["ifTrue:ifFalse:", "ifNil:ifNotNil:", "ifNotNil:ifNil:"] {
            assert_eq!(state_threaded_block_arg_indices(sel), &[0, 1], "{sel:?}");
            assert!(is_state_threaded_block_arg(sel, 0));
            assert!(is_state_threaded_block_arg(sel, 1));
            assert!(!is_state_threaded_block_arg(sel, 2));
        }
    }

    #[test]
    fn threaded_block_arg_second_arg_selectors() {
        assert_eq!(state_threaded_block_arg_indices("to:do:"), &[1]);
        assert_eq!(state_threaded_block_arg_indices("inject:into:"), &[1]);
        assert_eq!(state_threaded_block_arg_indices("on:do:"), &[1]);
        assert!(!is_state_threaded_block_arg("to:do:", 0));
        assert!(is_state_threaded_block_arg("to:do:", 1));
    }

    #[test]
    fn threaded_block_arg_third_arg_selector() {
        assert_eq!(state_threaded_block_arg_indices("to:by:do:"), &[2]);
        assert!(is_state_threaded_block_arg("to:by:do:", 2));
        assert!(!is_state_threaded_block_arg("to:by:do:", 1));
    }

    #[test]
    fn threaded_block_arg_context_dependent_selectors_are_empty() {
        // eachWithIndex:/do:separatedBy: only thread inside an Actor's own
        // fold (see this function's doc comment) — not unconditionally, so
        // they're excluded from this table rather than claimed incorrectly.
        assert_eq!(
            state_threaded_block_arg_indices("eachWithIndex:"),
            &[] as &[usize]
        );
        assert_eq!(
            state_threaded_block_arg_indices("do:separatedBy:"),
            &[] as &[usize]
        );
    }

    #[test]
    fn threaded_block_arg_non_state_threading_selector_is_empty() {
        assert_eq!(
            state_threaded_block_arg_indices("customLoop:"),
            &[] as &[usize]
        );
        assert!(!is_state_threaded_block_arg("customLoop:", 0));
    }

    /// BT-3423 (ADR 0118 §7) conformance test: enumerates every
    /// [`WellKnownSelector`] variant (via an exhaustive `match` — adding a
    /// new variant without extending this match is a compile error, so this
    /// table can never silently fall behind the enum it classifies) and
    /// asserts [`state_threaded_block_arg_indices`] agrees with the expected
    /// arg-index classification for each one.
    ///
    /// This is the selector-table half of "does the codegen threaded-vars
    /// map and the selector predicate agree on which arg indices thread":
    /// `beamtalk-codegen`'s `get_control_flow_threaded_vars` (`mod.rs`)
    /// reads `state_threaded_block_arg_indices` directly for every
    /// `WellKnownSelector`-backed selector below except the conditional
    /// family (`ifTrue:`/`ifFalse:`/`ifTrue:ifFalse:`/`ifNotNil:`/`ifNil:`/
    /// `ifNil:ifNotNil:`/`ifNotNil:ifNil:`) and `on:do:`, which it instead
    /// routes through `is_conditional_selector`/`is_exception_selector` —
    /// so for those the "agreement" is that this table's indices match what
    /// those two predicates + their dedicated `generate_*_with_mutations`
    /// codegen actually thread (documented per-arm below), not a literal
    /// shared code path. Either way, a selector reaching neither special
    /// case falls through to this table verbatim, so the two representations
    /// cannot drift apart for it.
    #[test]
    fn well_known_selector_arg_indices_are_exhaustively_classified() {
        fn expected_indices(sel: WellKnownSelector) -> &'static [usize] {
            match sel {
                // Two-block conditionals.
                WellKnownSelector::IfTrueIfFalse
                | WellKnownSelector::IfNilIfNotNil
                | WellKnownSelector::IfNotNilIfNil => &[0, 1],
                // One-block conditionals, plus `whileTrue:`/`whileFalse:`
                // (KEYWORD selectors despite the name resemblance to
                // `WhileTrue`/`WhileFalse` — their receiver is the
                // condition block, handled separately by codegen; their one
                // keyword argument, the loop body, is arg0 here) and
                // `ensure:` (its one argument, the cleanup block, is arg0).
                WellKnownSelector::IfTrue
                | WellKnownSelector::IfFalse
                | WellKnownSelector::IfNotNil
                | WellKnownSelector::IfNil
                | WellKnownSelector::WhileTrue
                | WellKnownSelector::WhileFalse
                | WellKnownSelector::Ensure => &[0],
                // `on:do:`: arg0 is the exception class, arg1 the handler block.
                WellKnownSelector::OnDo => &[1],
                // Every other WellKnownSelector variant is either not a
                // keyword selector at all (unary/no block argument:
                // `isNil`, `class`, `repeat`, `hash`, `fieldNames`,
                // `perform:` family, block-`value` family, `isOk`/`isError`
                // family) or a keyword selector whose argument is never a
                // state-threaded block (`isKindOf:`, `respondsTo:`,
                // `error:`, `fieldAt:`/`fieldAt:put:`) — none carry a block
                // argument this table recognizes.
                WellKnownSelector::IsNil
                | WellKnownSelector::NotNil
                | WellKnownSelector::IsKindOf
                | WellKnownSelector::Class
                | WellKnownSelector::RespondsTo
                | WellKnownSelector::Value
                | WellKnownSelector::ValueColon
                | WellKnownSelector::ValueValue
                | WellKnownSelector::ValueValueValue
                | WellKnownSelector::IsOk
                | WellKnownSelector::IsError
                | WellKnownSelector::IsOkColon
                | WellKnownSelector::IsErrorColon
                | WellKnownSelector::Repeat
                | WellKnownSelector::Hash
                | WellKnownSelector::Error
                | WellKnownSelector::FieldAt
                | WellKnownSelector::FieldAtPut
                | WellKnownSelector::FieldNames
                | WellKnownSelector::Perform
                | WellKnownSelector::PerformWithArgs
                | WellKnownSelector::PerformLocallyWithArgs => &[],
            }
        }

        // Mirrors `beamtalk_core::ast::well_known::tests::ALL_VARIANTS` — kept
        // as a literal list here (rather than importing a test-only const
        // across the module boundary) since this test's whole point is to
        // force a compile error in `expected_indices` above when a new
        // variant appears; a shared list wouldn't need the array below to
        // change, but `expected_indices`'s `match` still would.
        const ALL_VARIANTS: &[WellKnownSelector] = &[
            WellKnownSelector::IsNil,
            WellKnownSelector::NotNil,
            WellKnownSelector::IfNil,
            WellKnownSelector::IfNotNil,
            WellKnownSelector::IfNilIfNotNil,
            WellKnownSelector::IfNotNilIfNil,
            WellKnownSelector::IsKindOf,
            WellKnownSelector::Class,
            WellKnownSelector::RespondsTo,
            WellKnownSelector::IfTrue,
            WellKnownSelector::IfFalse,
            WellKnownSelector::IfTrueIfFalse,
            WellKnownSelector::OnDo,
            WellKnownSelector::Value,
            WellKnownSelector::ValueColon,
            WellKnownSelector::ValueValue,
            WellKnownSelector::ValueValueValue,
            WellKnownSelector::IsOk,
            WellKnownSelector::IsError,
            WellKnownSelector::IsOkColon,
            WellKnownSelector::IsErrorColon,
            WellKnownSelector::WhileTrue,
            WellKnownSelector::WhileFalse,
            WellKnownSelector::Repeat,
            WellKnownSelector::Ensure,
            WellKnownSelector::Hash,
            WellKnownSelector::Error,
            WellKnownSelector::FieldAt,
            WellKnownSelector::FieldAtPut,
            WellKnownSelector::FieldNames,
            WellKnownSelector::Perform,
            WellKnownSelector::PerformWithArgs,
            WellKnownSelector::PerformLocallyWithArgs,
        ];

        for &sel in ALL_VARIANTS {
            let name = sel.as_str();
            assert_eq!(
                state_threaded_block_arg_indices(name),
                expected_indices(sel),
                "state_threaded_block_arg_indices({name:?}) disagreed with the \
                 expected WellKnownSelector classification"
            );
        }
    }
}
