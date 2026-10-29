// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! BT-4140, BT-4247: compiler-owned Core Erlang variables never alias a
//! user-bindable name.
//!
//! **BT-4140 (temps).** User `_`-prefixed identifiers used to map into Core
//! Erlang unchanged (`_Wildcard1` → `_Wildcard1`), so a temp spelled
//! `_<Base><N>` aliased a user variable spelling the same counter value. Two
//! shapes, both pinned here across a sweep of counter values so the repro
//! does not depend on where the allocator's counter happens to stand:
//!
//! - a user `_WildcardN` next to `_` in one pattern became a duplicate
//!   pattern variable (`erlc` rejects the module);
//! - a user `_WildcardN` bound outside a match was shadowed by the `_`
//!   temp in an arm's pattern, so the arm body silently read the matched
//!   element instead of the user's value.
//!
//! Temps carry [`TEMP_MARKER`], a character no Beamtalk identifier can
//! contain.
//!
//! **BT-4247 (hand-written names).** Production codegen also emits fixed,
//! hand-written `_`-names (`_Unit`, `_Result`, `_ExtArgs`, …) that a user
//! could spell exactly. One was reachable: a Tier 1 block body discards a
//! non-final statement's value as `let _Unit = <stmt> in <rest>`, so in
//! `[:y | _Unit := y. x + 1. _Unit]` the user's `_Unit` was shadowed and the
//! block answered `x + 1`. Rather than chase each site, user `_`-identifiers
//! now map into their own namespace, `_@<name>`
//! ([`VariableContext::to_core_var`]), which no hand-written name (no marker)
//! and no temp (digits after the marker) can spell. [`HAND_WRITTEN`] is the
//! audit of every hand-written name the corpus emits, and
//! [`hand_written_underscore_names_match_the_audit`] fails on a new one or a
//! user name that reaches Core unmapped.

use super::{assert_compiles_through_erlc, codegen, collect_source_files, repo_root};
use crate::core_erlang::variable_context::{TEMP_MARKER, VariableContext, text_mentions_var};
use std::collections::BTreeSet;

/// Counter values swept by the repros: wide enough to cover every counter a
/// `_` in these small methods can be allocated.
const SWEEP: std::ops::RangeInclusive<usize> = 1..=40;

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
        let core_user = VariableContext::to_core_var(&user);
        assert!(
            text_mentions_var(&out, &core_user),
            "user `{user}` must stay bound (as `{core_user}`).\nCore:\n{out}"
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
/// marker followed by its counter; no Beamtalk identifier contains the
/// marker; and a user `_`-name maps to `_@<name>`, whose text after the
/// marker starts with `_` (never a temp's digits), while any other user name
/// carries no marker (never a temp, and never an `_`-name at all).
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
        let (_, counter) = temp
            .rsplit_once(TEMP_MARKER)
            .unwrap_or_else(|| panic!("temp `{temp}` lacks the marker"));
        assert!(
            !counter.is_empty() && counter.bytes().all(|b| b.is_ascii_digit()),
            "temp `{temp}` must end in marker + counter"
        );
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

    for name in [
        "_Wildcard1",
        "_private",
        "_1",
        "_Unit",
        "counter",
        "my_var",
        "X",
    ] {
        let core = VariableContext::to_core_var(name);
        if name.starts_with('_') {
            assert_eq!(core.strip_prefix("_@"), Some(name), "`{name}` → `{core}`");
            // A version suffix keeps the text after the marker non-numeric.
            let versioned = crate::core_erlang::util::versioned_var(&core, 3);
            let (_, after) = versioned.rsplit_once(TEMP_MARKER).unwrap();
            assert!(after.starts_with('_'), "`{versioned}` could spell a temp");
        } else {
            assert!(!core.contains(TEMP_MARKER), "`{name}` → `{core}`");
            assert!(!core.starts_with('_'), "`{name}` → `{core}`");
        }
    }
}

/// The audit (BT-4247): every hand-written `_`-prefixed Core variable name
/// production codegen emits over the corpus, after [`normalise`] (trailing
/// counter digits and `_ProtoReg_<Protocol>` suffixes dropped).
///
/// None can share a binding with user code: a user `_`-identifier maps to
/// `_@<name>` and none of these contains [`TEMP_MARKER`]. Before BT-4247 the
/// one reachable entry was `_Unit` (see the module doc); the rest are bound
/// in module boilerplate with no user code in scope (`gen_server` callbacks,
/// `register_class`, spawn/`new`, protocol registration, `dispatch`/4
/// fallbacks), in `@primitive` method bodies, which hold no user statements
/// (`_H`, `_T`, `_Value`, `_Reason`), as parameters read only in the
/// prologue before user code (`_ExtArgs`, `_DirectClassSelf`, `_ClassSelf`,
/// `_InitArgs`, `_Selector`), or as tail bindings whose body is generated
/// code only (`_SuperTuple`, `_Result`, `_NewState`, `_ReturnValue`).
///
/// A name added here must be hand-written (never derived from a user
/// identifier) and must not contain [`TEMP_MARKER`].
const HAND_WRITTEN: &[&str] = &[
    "_BuilderState",
    "_CallDeferred",
    "_CallFromInstalled",
    "_CallbackResult",
    "_CastCtxOk",
    "_CastDispatchResult",
    "_CastResult",
    "_CastState",
    "_CastStateChanged",
    "_ChkVal",
    "_ClassRegResult",
    "_ClassSelf",
    "_ClearStack",
    "_ClfBadArgs",
    "_ContDiscarded",
    "_ContErr",
    "_ContSeeded",
    "_Continue",
    "_ContinueResult",
    "_CtxOk",
    "_D4Stack",
    "_D4Type",
    "_DirectClassSelf",
    "_DispS",
    "_DispatchResult",
    "_EReason",
    "_EStack",
    "_EType",
    "_Err",
    "_ExitResult",
    "_Ext",
    "_ExtArgs",
    "_ExtOwner",
    "_Extra",
    "_From",
    "_H",
    "_HfBadArgs",
    "_InfoDispatchResult",
    "_InfoState",
    "_InitArgs",
    "_InitErrClass",
    "_InitErrMsg",
    "_InitErrState",
    "_InitError",
    "_InitReason",
    "_InitResult",
    "_InitRet",
    "_InitStack",
    "_InitType",
    "_InstReg",
    "_IvaBadArgs",
    "_IvapBad",
    // `clean_state_binding`'s committed-state key pattern (BT-4113):
    // `_Key0`, `_Key1`, ..., one per internal key and declared field.
    "_Key",
    "_Marker",
    "_Msg",
    "_NewState",
    "_NotDeferred",
    "_OldPdict",
    "_OldState",
    "_OldVsn",
    "_Other",
    "_OtherSel",
    "_ParentArgs",
    "_PendingActionsCleared",
    "_PerfBadArgs",
    "_Pid",
    "_ProtoRegResult",
    "_ProtoReg_",
    "_PwaBadArgList",
    "_PwaBadSel",
    "_Reason",
    "_Reg",
    "_RegE",
    "_RegOk",
    "_RegS",
    "_RegT",
    "_RestoreOk",
    "_RestorePdict",
    "_Result",
    "_ReturnValue",
    "_RtBadArgs",
    "_Selector",
    "_SpawnC",
    "_SpawnS",
    "_StartOk",
    "_State",
    "_StateChanged",
    "_SuperTuple",
    "_T",
    "_TelPid",
    "_TelStart",
    "_TelStop",
    "_TermDisp",
    "_TermE",
    "_TermOk",
    "_TermS",
    "_TermT",
    "_TrapExit",
    "_UErr",
    "_UErrClass",
    "_UErrMsg",
    "_UFmt",
    "_Unit",
    "_Value",
    "_WireArgs",
    "_WirePropCtx",
    "_WireSelector",
    "_WireVersion",
];

/// `name` with a trailing counter (`_Ext3`, `_InitErrClass0b`) or a
/// protocol suffix (`_ProtoReg_Printable`) dropped, so one audit entry covers
/// every instance.
fn normalise(name: &str) -> &str {
    if name.starts_with("_ProtoReg_") {
        return "_ProtoReg_";
    }
    let trimmed = name
        .strip_suffix(|c: char| c.is_ascii_lowercase())
        .unwrap_or(name);
    let stripped = trimmed.trim_end_matches(|c: char| c.is_ascii_digit());
    if stripped.len() < trimmed.len() && stripped.len() > 1 {
        stripped
    } else {
        name
    }
}

/// Every `_`-prefixed variable token in rendered Core Erlang (string and
/// atom literals, character literals and comments skipped), except the bare
/// wildcard `_`.
fn underscore_vars(core: &str) -> Vec<&str> {
    let b = core.as_bytes();
    let is_name_byte = |c: u8| c == b'_' || c == b'@' || c.is_ascii_alphanumeric();
    let mut out = Vec::new();
    let mut i = 0;
    while i < b.len() {
        match b[i] {
            q @ (b'"' | b'\'') => {
                i += 1;
                while i < b.len() && b[i] != q {
                    i += if b[i] == b'\\' { 2 } else { 1 };
                }
                i += 1;
            }
            b'%' => {
                while i < b.len() && b[i] != b'\n' {
                    i += 1;
                }
            }
            b'$' => i += if b.get(i + 1) == Some(&b'\\') { 3 } else { 2 },
            c if is_name_byte(c) => {
                let start = i;
                while i < b.len() && is_name_byte(b[i]) {
                    i += 1;
                }
                let token = &core[start..i];
                if token.len() > 1 && token.starts_with('_') {
                    out.push(token);
                }
            }
            _ => i += 1,
        }
    }
    out
}

/// The guard (BT-4247), over every `.bt`/`.btscript` file in the stdlib and
/// the examples:
///
/// - every `_`-prefixed Core variable without [`TEMP_MARKER`] is a
///   hand-written name listed in [`HAND_WRITTEN`] — a new one fails until it
///   is audited;
/// - no user `_`-identifier of the source file reaches Core under its own
///   spelling (every user binding went through
///   [`VariableContext::to_core_var`]);
/// - every [`HAND_WRITTEN`] entry is still emitted, so the audit never goes
///   stale.
#[test]
fn hand_written_underscore_names_match_the_audit() {
    let root = repo_root();
    let mut files = Vec::new();
    for dir in [
        "stdlib/src",
        "stdlib/test",
        "stdlib/bootstrap-test",
        "examples",
    ] {
        collect_source_files(&root.join(dir), &mut files);
    }
    assert!(
        files.len() > 100,
        "expected hundreds of corpus files, found {} under {}",
        files.len(),
        root.display()
    );

    let mut emitted: BTreeSet<&'static str> = BTreeSet::new();
    let mut unlisted: BTreeSet<String> = BTreeSet::new();
    let mut unmapped: Vec<String> = Vec::new();
    for (i, path) in files.iter().enumerate() {
        let Ok(source) = std::fs::read_to_string(path) else {
            continue;
        };
        let rel = path
            .strip_prefix(&root)
            .unwrap_or(path)
            .to_string_lossy()
            .replace('\\', "/");
        let tokens = beamtalk_core::source_analysis::lex_with_eof(&source);
        let user_names: BTreeSet<String> = tokens
            .iter()
            .filter_map(|t| match t.kind() {
                beamtalk_core::source_analysis::TokenKind::Identifier(name)
                    if name.len() > 1 && name.starts_with('_') =>
                {
                    Some(name.to_string())
                }
                _ => None,
            })
            .collect();
        let (module, _) = beamtalk_core::source_analysis::parse(tokens);
        let options = crate::core_erlang::CodegenOptions::new(&format!("bt@audit@{i}"))
            .with_stdlib_mode(rel.starts_with("stdlib/src/"));
        // A rejection fixture that fails to compile emits nothing to scan.
        let Ok(core) = crate::core_erlang::generate_module(&module, options) else {
            continue;
        };
        for var in underscore_vars(&core) {
            if var.contains(TEMP_MARKER) {
                continue;
            }
            match HAND_WRITTEN.iter().find(|&&h| h == normalise(var)) {
                Some(h) => {
                    emitted.insert(h);
                }
                // A user spelling of a hand-written name is told apart by
                // `user_names_spelled_like_hand_written_names_stay_distinct`.
                None if user_names.contains(var) => {
                    unmapped.push(format!("{rel}: user `{var}`"));
                }
                None => {
                    unlisted.insert(format!("{var} ({rel})"));
                }
            }
        }
    }

    unmapped.sort();
    unmapped.dedup();
    assert!(
        unmapped.is_empty(),
        "a user `_`-identifier reached Core under its own spelling, bypassing \
         `to_core_var` — it can alias a hand-written name:\n  {}",
        unmapped.join("\n  ")
    );
    assert!(
        unlisted.is_empty(),
        "production codegen emits a hand-written `_`-name missing from \
         `HAND_WRITTEN`. Mint a temp with `fresh_temp_var` instead, or audit \
         it (no `@`, never derived from a user identifier) and list it:\n  {}",
        unlisted.into_iter().collect::<Vec<_>>().join("\n  ")
    );
    let stale: Vec<&&str> = HAND_WRITTEN
        .iter()
        .filter(|h| !emitted.contains(**h))
        .collect();
    assert!(
        stale.is_empty(),
        "`HAND_WRITTEN` lists names the corpus no longer emits; remove them: {stale:?}"
    );
    assert!(
        HAND_WRITTEN.iter().all(|h| !h.contains(TEMP_MARKER)),
        "a hand-written name must not contain `{TEMP_MARKER}`"
    );
}

/// Method shapes a user `_`-binding is tried in: each binds `{n}` to the
/// argument, runs generated code that may bind hand-written names, then
/// answers `{n}`.
const SHAPES: &[&str] = &[
    // Tier 1 block body: a discarded statement between the binding and the read
    // (`_Unit`, the reachable site).
    "Object subclass: Foo\n  run: x =>\n    [:y | {n} := y. x + 1. {n}] value: x\n",
    "Object subclass: Foo\n  run: x =>\n    {n} := x.\n    x + 1.\n    {n}\n",
    "Object subclass: Foo\n  run: x =>\n    [{n} := x. x + 1. {n}] on: Error do: [:e | 0]\n",
    "Object subclass: Foo\n  run: x =>\n    [{n} := x. x + 1. {n}] ensure: [x + 2]\n",
    "Object subclass: Foo\n  run: x =>\n    {n} := x.\n    1 to: 2 do: [:i | x + i].\n    {n}\n",
    "Object subclass: Foo\n  run: x =>\n    x match: [\n      {{n}, _} -> {n};\n      _ -> 0\n    ]\n",
    "Object subclass: Foo\n  class run: x =>\n    {n} := x.\n    x + 1.\n    {n}\n",
    "Actor subclass: Foo\n  state: k = 0\n  run: x =>\n    {n} := x.\n    self.k := x + 1.\n    {n}\n",
    "Actor subclass: Foo\n  state: k = 0\n  run: x =>\n    [:y | {n} := y. self.k := y. {n}] value: x\n",
    "Value subclass: Foo\n  field: k = 0\n  run: x =>\n    {n} := x.\n    x + 1.\n    {n}\n",
];

/// The marker-free `_`-variables of `core`, sorted with repeats.
fn hand_written_vars(core: &str) -> Vec<&str> {
    let mut vars: Vec<&str> = underscore_vars(core)
        .into_iter()
        .filter(|v| !v.contains(TEMP_MARKER))
        .collect();
    vars.sort_unstable();
    vars
}

/// A repro per audited name (BT-4247): a user variable spelled exactly like
/// each [`HAND_WRITTEN`] name (with a counter, as most are emitted), in every
/// [`SHAPES`] method, compiles through `erlc` and leaves the module's
/// marker-free `_`-variables exactly as a neutral user name does — the user
/// binding never takes the hand-written spelling, so it can neither shadow
/// nor be shadowed by one. Runtime: `stdlib/test/hand_written_name_shadowing_test.bt`.
#[test]
fn user_names_spelled_like_hand_written_names_stay_distinct() {
    const NEUTRAL: &str = "_neutral";
    for shape in SHAPES {
        let neutral = codegen(&shape.replace("{n}", NEUTRAL));
        let expected = hand_written_vars(&neutral);
        for &hand in HAND_WRITTEN {
            for name in [format!("{hand}0"), format!("{hand}1"), hand.to_string()] {
                let src = shape.replace("{n}", &name);
                let out = codegen(&src);
                assert_eq!(
                    hand_written_vars(&out),
                    expected,
                    "user `{name}` changed the module's hand-written `_`-names: it \
                     reached Core under its own spelling.\nSource:\n{src}\nCore:\n{out}"
                );
            }
        }
        // `erlc` per shape for the reachable spelling and a counter-suffixed
        // one (a duplicate pattern variable would be rejected here); the
        // comparison above already covers every name.
        for name in ["_Unit", "_BuilderState0"] {
            assert_compiles_through_erlc("test", &codegen(&shape.replace("{n}", name)));
        }
    }
}

/// The one reachable site, pinned by shape: the Tier 1 block body's discarded
/// statement binds `_Unit`, and the user's `_Unit` read after it must not be
/// that binding.
#[test]
fn block_discard_binding_does_not_shadow_user_unit() {
    let out = codegen(
        "Object subclass: Foo\n  run: x =>\n    [:y | _Unit := y. x + 1. _Unit] value: x\n",
    );
    assert!(
        text_mentions_var(&out, "_@_Unit"),
        "user `_Unit` must be bound as `_@_Unit`.\nCore:\n{out}"
    );
    assert!(
        !out.contains("in _Unit"),
        "the block answers the hand-written `_Unit`, not the user's.\nCore:\n{out}"
    );
    assert_compiles_through_erlc("test", &out);
}
