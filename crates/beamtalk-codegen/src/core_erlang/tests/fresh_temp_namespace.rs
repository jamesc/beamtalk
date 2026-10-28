// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4140: compiler temporaries never alias a user-bindable name.
//!
//! User `_`-prefixed identifiers map into Core Erlang unchanged
//! (`_Wildcard1` → `_Wildcard1`), so a temp spelled `_<Base><N>` aliased a
//! user variable spelling the same counter value. Two shapes, both pinned
//! here across a sweep of counter values so the repro does not depend on
//! where the allocator's counter happens to stand:
//!
//! - a user `_WildcardN` next to `_` in one pattern became a duplicate
//!   pattern variable (`erlc` rejects the module);
//! - a user `_WildcardN` bound outside a match was shadowed by the `_`
//!   temp in an arm's pattern, so the arm body silently read the matched
//!   element instead of the user's value.
//!
//! Temps now carry [`TEMP_MARKER`](crate::core_erlang::variable_context::TEMP_MARKER),
//! a character no Beamtalk identifier can contain.

use super::{assert_compiles_through_erlc, codegen};
use crate::core_erlang::variable_context::{TEMP_MARKER, VariableContext};

/// Counter values swept by the repros: wide enough to cover every counter a
/// `_` in these small methods can be allocated.
const SWEEP: std::ops::RangeInclusive<usize> = 1..=40;

/// Whether `name` occurs in `out` as a whole Core Erlang variable token.
fn mentions(out: &str, name: &str) -> bool {
    let is_name_char = |c: char| c.is_ascii_alphanumeric() || c == '_' || c == '@';
    out.match_indices(name).any(|(at, _)| {
        let before = out[..at].chars().next_back();
        let after = out[at + name.len()..].chars().next();
        !before.is_some_and(is_name_char) && !after.is_some_and(is_name_char)
    })
}

/// A user `_WildcardN` beside a `_` in the same pattern binds its own Core
/// variable: the module compiles and the arm body returns the user binding.
#[test]
fn user_wildcard_name_beside_wildcard_is_distinct_in_pattern() {
    for n in SWEEP {
        let user = format!("_Wildcard{n}");
        let src = format!(
            "Object subclass: Foo\n  run: x =>\n    x match: [\n      {{{user}, _}} -> {user};\n      _ -> 0\n    ]\n"
        );
        let out = codegen(&src);
        let pattern = format!("{{{user}, {user}}}");
        assert!(
            !out.contains(&pattern),
            "`_` aliased user `{user}` in one pattern.\nCore:\n{out}"
        );
        assert!(
            mentions(&out, &user),
            "user `{user}` must stay bound.\nCore:\n{out}"
        );
        assert_compiles_through_erlc("test", &out);
    }
}

/// A user `_WildcardN` bound before a match is not shadowed by a `_` in an
/// arm's pattern: the arm's tuple pattern never binds the user's name.
#[test]
fn user_wildcard_name_outside_match_is_not_shadowed() {
    for n in SWEEP {
        let user = format!("_Wildcard{n}");
        let src = format!(
            "Object subclass: Foo\n  run: x =>\n    {user} := 5.\n    x match: [\n      {{_}} -> {user};\n      _ -> 0\n    ]\n"
        );
        let out = codegen(&src);
        let pattern = format!("{{{user}}}");
        assert!(
            !out.contains(&pattern),
            "`_` shadowed user `{user}` in an arm pattern.\nCore:\n{out}"
        );
        assert_compiles_through_erlc("test", &out);
    }
}

/// The disjointness argument, checked at both ends: every temp carries the
/// marker, and no Beamtalk identifier can contain it (so no user name, after
/// [`VariableContext::to_core_var`] and a version suffix, can either).
#[test]
fn temps_and_user_names_are_disjoint() {
    let mut ctx = VariableContext::new();
    for base in [
        "Wildcard",
        "Size",
        "Split",
        "Fallthrough",
        "temp",
        "my_var",
        "",
    ] {
        let temp = ctx.fresh_var(base);
        assert!(temp.contains(TEMP_MARKER), "temp `{temp}` lacks the marker");
    }

    // The lexer never folds the marker into an identifier token.
    let src = format!("_Wildcard{TEMP_MARKER}1");
    let tokens = beamtalk_core::source_analysis::lex_with_eof(&src);
    assert!(
        tokens.iter().all(|t| match t.kind() {
            beamtalk_core::source_analysis::TokenKind::Identifier(name) =>
                !name.contains(TEMP_MARKER),
            _ => true,
        }),
        "an identifier token contains `{TEMP_MARKER}`: {tokens:?}"
    );

    // `to_core_var` never introduces it.
    for name in ["_Wildcard1", "_private", "counter", "my_var", "X", ""] {
        assert!(!VariableContext::to_core_var(name).contains(TEMP_MARKER));
    }
}
