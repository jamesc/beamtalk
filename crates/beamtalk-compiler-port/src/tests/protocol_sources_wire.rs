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

const RANKED_SOURCE: &str = "Protocol define: Ranked\n\
     \x20 < other :: Self -> Boolean\n\n\
     \x20 max: other :: Self -> Self => (self < other) ifTrue: [other] ifFalse: [self]";

const VERSION_SOURCE: &str = "Value subclass: Version\n\
     \x20 uses: Ranked\n\
     \x20 field: major :: Integer = 0\n\n\
     \x20 < other :: Version -> Boolean => self.major < other major";

/// Negative control: without `protocol_sources`, `Version`'s `uses:
/// Ranked` cannot resolve — `Ranked` is defined in neither this
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
        "expected an unresolved `uses: Ranked` to fail compilation: {response:?}"
    );
}

/// Positive case: `protocol_sources => #{"Ranked" => <its .bt source>}`
/// lets `Version`'s `uses: Ranked` flatten `max:` in from the
/// wire-carried protocol source, exactly as a real protocol-reload fan-out
/// recompile would supply it.
#[test]
fn compile_with_protocol_sources_flattens_cross_file_uses() {
    let protocol_sources = Term::from(Map::from([(binary("Ranked"), binary(RANKED_SOURCE))]));
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
        "expected `protocol_sources` to resolve `uses: Ranked`: {response:?}"
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
/// gracefully — `Version` still fails to resolve `uses: Ranked`, exactly
/// as if `protocol_sources` had been omitted, rather than the whole request
/// erroring over one unrelated bad entry.
#[test]
fn compile_with_unparseable_protocol_source_degrades_to_unknown_protocol() {
    let protocol_sources = Term::from(Map::from([(
        binary("Ranked"),
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

/// BT-3915: the REPL/workspace path resolves the stdlib's own `Comparable`
/// with no `protocol_sources` at all — the stdlib's provision-bearing
/// protocols are the last tier of `uses:` resolution on every surface.
#[test]
fn compile_flattens_a_stdlib_protocol_without_protocol_sources() {
    let source = "Value subclass: Version\n\
         \x20 uses: Comparable\n\
         \x20 field: major :: Integer = 0\n\n\
         \x20 < other :: Version -> Boolean => self.major < other major";
    let request = Map::from([
        (atom("command"), atom("compile")),
        (atom("source"), binary(source)),
        (atom("module_name"), binary("bt@version")),
    ]);

    let response = handle_compile(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "expected `uses: Comparable` to resolve to the stdlib protocol: {response:?}"
    );
    let core_erlang =
        response_field_str(&response, "core_erlang").expect("compile response has core_erlang");
    assert!(
        core_erlang.contains("between:and:"),
        "the stdlib's `between:and:` provision should be flattened in: {core_erlang}"
    );
}

/// BT-3915: on the REPL/workspace path (no package identity), a
/// `uses: stdlib@Comparable` resolves to the stdlib's protocol even when the
/// workspace carries its own `Comparable` on the wire — as `beamtalk build`
/// resolves it.
#[test]
fn compile_resolves_qualified_stdlib_use_beside_a_carried_namesake() {
    let user_comparable = "Protocol define: Comparable\n\
         \x20 < other :: Self -> Boolean\n\n\
         \x20 localOnly -> Integer => 1";
    let source = "Value subclass: Version\n\
         \x20 uses: stdlib@Comparable\n\
         \x20 field: major :: Integer = 0\n\n\
         \x20 < other :: Version -> Boolean => self.major < other major";
    let request = Map::from([
        (atom("command"), atom("compile")),
        (atom("source"), binary(source)),
        (atom("module_name"), binary("bt@version")),
        (
            atom("protocol_sources"),
            Term::from(Map::from([(binary("Comparable"), binary(user_comparable))])),
        ),
    ]);

    let response = handle_compile(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "{response:?}"
    );
    let core_erlang =
        response_field_str(&response, "core_erlang").expect("compile response has core_erlang");
    assert!(
        core_erlang.contains("between:and:") && !core_erlang.contains("localOnly"),
        "expected the stdlib's provisions, not the carried Comparable's: {core_erlang}"
    );
}
