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

/// BT-3915: the workspace's own requirements-only `Comparable`, carried on
/// the wire, shadows the stdlib's for a bare `uses: Comparable` — none of the
/// stdlib's provisions are flattened in.
#[test]
fn compile_carried_requirements_only_protocol_shadows_the_stdlib_one() {
    let user_comparable = "Protocol define: Comparable\n\
         \x20 < other :: Self -> Boolean";
    let request = Map::from([
        (atom("command"), atom("compile")),
        (
            atom("source"),
            binary(VERSION_SOURCE.replace("Ranked", "Comparable").as_str()),
        ),
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
        !core_erlang.contains("between:and:"),
        "the carried Comparable must shadow the stdlib's provisions: {core_erlang}"
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

/// BT-4828: `protocol_source_paths` rides beside `protocol_sources` so the
/// per-file `'__beamtalk_method_lines'/0` table covers an actor method that a
/// protocol in *another* file provides, with the protocol's file and line.
/// Without the path the flattened method has no row (REPL/workspace loads
/// rendered it as `Class>>dispatch`).
#[test]
fn compile_with_protocol_source_paths_gives_cross_file_provision_a_method_lines_row() {
    const PROTOCOL: &str = "// header\n\n\
         Protocol define: Describable\n\
         \x20 describe -> String => 1 printString\n";
    const ACTOR: &str = "Actor subclass: Report\n  uses: Describable\n\n  bump => 1\n";
    let compile = |with_paths: bool| {
        let mut request = Map::from([
            (atom("command"), atom("compile")),
            (atom("source"), binary(ACTOR)),
            (atom("source_path"), binary("a.bt")),
            (atom("module_name"), binary("bt@report")),
            (
                atom("protocol_sources"),
                Term::from(Map::from([(binary("Describable"), binary(PROTOCOL))])),
            ),
        ]);
        if with_paths {
            request.map.insert(
                atom("protocol_source_paths"),
                Term::from(Map::from([(binary("Describable"), binary("b.bt"))])),
            );
        }
        let response = handle_compile(&request);
        assert_eq!(
            response_status(&response).as_deref(),
            Some("ok"),
            "{response:?}"
        );
        response_field_str(&response, "core_erlang").expect("core_erlang")
    };

    let table_of = |code: &str| -> String {
        code.split("'__beamtalk_method_lines'/0 = fun")
            .nth(1)
            .and_then(|rest| rest.split("'has_method'/1 = fun").next())
            .unwrap_or_else(|| panic!("expected a method-lines table, got:\n{code}"))
            .to_string()
    };

    let without = compile(false);
    let table = table_of(&without);
    assert!(
        table.contains("'bump'") && !table.contains("'describe'"),
        "without a protocol path the provision has no row, got:\n{table}"
    );

    let with = compile(true);
    let table = table_of(&with);
    assert!(
        table.contains("'bump'") && table.contains("'describe'"),
        "with the protocol path the provision has a row, got:\n{table}"
    );
    // Each row is `{<<file bytes>>, {Line, Selector}}`; the file is a binary
    // literal, printed byte by byte.
    let row_of = |selector: &str| -> String {
        let marker = format!("{{4, '{selector}'}}");
        let end = table
            .find(&marker)
            .unwrap_or_else(|| panic!("no row for {selector}: {table}"));
        let start = table[..end].rfind("#{").expect("row start");
        table[start..end].to_string()
    };
    let file_bytes = |file: &str| -> String {
        file.bytes()
            .map(|b| format!("#<{b}>(8,1,'integer',['unsigned'|['big']])"))
            .collect::<Vec<_>>()
            .join(",")
    };
    // `describe` is on line 4 of the protocol's file `b.bt`; `bump` is on
    // line 4 of the actor's own `a.bt`.
    assert!(
        row_of("describe").contains(&file_bytes("b.bt")),
        "the provision's row is under the protocol's file and line, got:\n{table}"
    );
    assert!(
        row_of("bump").contains(&file_bytes("a.bt")),
        "the actor's own row stays under its own file, got:\n{table}"
    );
}
