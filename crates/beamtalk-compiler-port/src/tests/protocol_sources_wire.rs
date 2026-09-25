// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `protocol_sources` wire field (ADR 0127 §10a / BT-3593): a `compile`
//! request carrying a cross-file protocol's full `.bt` source lets `uses:`
//! flatten that protocol's provisions, the same way
//! `beamtalk_repl_loader`'s two-stage protocol-reload fan-out feeds each
//! user's own recompile the just-edited protocol's source. Mirrors
//! `trait_expansion::tests::uses_resolves_against_an_externally_carried_protocol`
//! (same fixture), but exercised through the wire (`handle_compile`) instead
//! of calling `expand_module` directly, so this covers the decode +
//! `pre_loaded_protocol_defs` threading this issue actually added.

use super::*;

const COMPARABLE_SOURCE: &str = "Protocol define: Comparable\n\
     \x20 < other :: Self -> Boolean\n\n\
     \x20 max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]";

const VERSION_SOURCE: &str = "Value subclass: Version\n\
     \x20 uses: Comparable\n\
     \x20 field: major :: Integer = 0\n\n\
     \x20 < other :: Version -> Boolean => self.major < other major";

/// Negative control: without `protocol_sources`, `Version`'s `uses:
/// Comparable` cannot resolve — `Comparable` is defined in neither this
/// request's own source nor any external map — so `compile` reports the
/// "unknown protocol" diagnostic `trait_expansion::expand_module` documents,
/// not a successful compile.
#[test]
fn compile_without_protocol_sources_reports_unknown_protocol() {
    let request = Map::from([
        (atom("command"), atom("compile")),
        (atom("source"), binary(VERSION_SOURCE)),
        (atom("module_name"), binary("bt@version")),
    ]);

    let response = handle_compile(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("error"),
        "expected an unresolved `uses: Comparable` to fail compilation: {response:?}"
    );
}

/// Positive case: `protocol_sources => #{"Comparable" => <its .bt source>}`
/// lets `Version`'s `uses: Comparable` flatten `max:` in from the
/// wire-carried protocol source, exactly as a real protocol-reload fan-out
/// recompile would supply it.
#[test]
fn compile_with_protocol_sources_flattens_cross_file_uses() {
    let protocol_sources = Term::from(Map::from([(
        binary("Comparable"),
        binary(COMPARABLE_SOURCE),
    )]));
    let request = Map::from([
        (atom("command"), atom("compile")),
        (atom("source"), binary(VERSION_SOURCE)),
        (atom("module_name"), binary("bt@version")),
        (atom("protocol_sources"), protocol_sources),
    ]);

    let response = handle_compile(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "expected `protocol_sources` to resolve `uses: Comparable`: {response:?}"
    );
    let core_erlang =
        response_field_str(&response, "core_erlang").expect("compile response has core_erlang");
    assert!(
        core_erlang.contains("max"),
        "flattened `max:` should appear in the generated Core Erlang: {core_erlang}"
    );
}

/// A `protocol_sources` entry whose source doesn't actually parse to a
/// protocol named after its own key (a malformed/stale entry) degrades
/// gracefully — `Version` still fails to resolve `uses: Comparable`, exactly
/// as if `protocol_sources` had been omitted, rather than the whole request
/// erroring over one unrelated bad entry.
#[test]
fn compile_with_unparseable_protocol_source_degrades_to_unknown_protocol() {
    let protocol_sources = Term::from(Map::from([(
        binary("Comparable"),
        binary("this is not valid beamtalk source :::"),
    )]));
    let request = Map::from([
        (atom("command"), atom("compile")),
        (atom("source"), binary(VERSION_SOURCE)),
        (atom("module_name"), binary("bt@version")),
        (atom("protocol_sources"), protocol_sources),
    ]);

    let response = handle_compile(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("error"),
        "a malformed protocol_sources entry must not silently succeed: {response:?}"
    );
}
