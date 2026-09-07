// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0108 Phase 8 (BT-2902): type Name = ... REPL declarations -- with and without a doc comment, rejecting multiple declarations in one turn, and `known_type_aliases` resolving/round-tripping an alias declared in an earlier turn.

use super::*;

// -------------------------------------------------------------------
// ADR 0108 Phase 8 (BT-2902): `type Name = ...` REPL declarations
// -------------------------------------------------------------------

/// True when a response field is present and is the atom `undefined`
/// (Rust's `None`, e.g. an alias declaration with no doc comment).
fn response_field_is_undefined_atom(term: &Term, key: &str) -> bool {
    let Term::Map(map) = term else {
        return false;
    };
    matches!(map_get(map, key), Some(Term::Atom(a)) if a.name == "undefined")
}

#[test]
fn type_alias_declaration_without_doc_comment() {
    let response = handle_compile_expression(&compile_expression_request(
        "type Direction = #north | #south | #east | #west",
    ));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "resp: {response:?}"
    );
    let Term::Map(m) = &response else {
        panic!("expected a map response, got: {response:?}");
    };
    assert_eq!(map_get(m, "kind"), Some(&atom("type_alias_definition")));
    assert_eq!(
        response_field_str(&response, "alias_name").as_deref(),
        Some("Direction")
    );
    assert_eq!(
        response_field_str(&response, "expansion").as_deref(),
        Some("#north | #south | #east | #west")
    );
    assert!(
        response_field_is_undefined_atom(&response, "doc_comment"),
        "no doc comment on the declaration must round-trip as `undefined`, \
             not an empty string: {response:?}"
    );
}

#[test]
fn type_alias_declaration_with_doc_comment() {
    let response = handle_compile_expression(&compile_expression_request(
        "/// How a supervised child restarts after exit.\n\
             type RestartStrategy = #temporary | #transient | #permanent",
    ));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "resp: {response:?}"
    );
    assert_eq!(
        response_field_str(&response, "alias_name").as_deref(),
        Some("RestartStrategy")
    );
    assert_eq!(
        response_field_str(&response, "expansion").as_deref(),
        Some("#temporary | #transient | #permanent")
    );
    assert_eq!(
        response_field_str(&response, "doc_comment").as_deref(),
        Some("How a supervised child restarts after exit.")
    );
}

#[test]
fn multiple_type_alias_declarations_in_one_turn_is_an_error() {
    let response = handle_compile_expression(&compile_expression_request(
        "type A = Integer\ntype B = String",
    ));
    assert_eq!(
        response_status(&response).as_deref(),
        Some("error"),
        "resp: {response:?}"
    );
}

/// `known_type_aliases` (ADR 0108 Phase 8) makes an alias declared in an
/// *earlier* REPL turn resolvable in the current turn's `::` annotation
/// — the cross-turn persistence mechanism this issue adds, since an
/// alias has no live BEAM artifact for the session to recover it from
/// the way a REPL-declared class is recovered. Asserts on the
/// `matchExhaustive:` diagnostic text (not just "no unresolved-type
/// error") so this also proves `Direction` expanded to the exact
/// closed singleton union — not merely a silently-accepted unknown name.
#[test]
fn known_type_aliases_resolves_alias_from_earlier_turn() {
    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (
            atom("source"),
            binary(
                "scrutinee :: Direction := #north. \
                     scrutinee matchExhaustive: [#north -> 0; #south -> 1; #east -> 2]",
            ),
        ),
        (atom("module"), binary("bt@test_module")),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![binary(
                "type Direction = #north | #south | #east | #west",
            )])),
        ),
    ]);
    let response = handle_compile_expression(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("error"),
        "resp: {response:?}"
    );
    let diags = response_diagnostics(&response).expect("diagnostics");
    let found = diags.elements.iter().any(|d| {
        let Term::Map(m) = d else { return false };
        map_get(m, "message")
            .and_then(term_to_string)
            .is_some_and(|msg| {
                msg.contains("non-exhaustive matchExhaustive:") && msg.contains("#west")
            })
    });
    assert!(
        found,
        "expected a non-exhaustive matchExhaustive: `#west` diagnostic \
             (proves Direction resolved to the closed union, not an unresolved-type \
             fallback), got: {response:?}"
    );
}

/// The `known_type_aliases` round trip (declare → `unparse_type_annotation_display`
/// → resend as `type Name = <expansion>` → reparse) is not limited to simple
/// singleton unions — ADR 0108 Semantics explicitly allows any `TypeAnnotation`
/// on the RHS, including `\`/`&` forms (`type PublicTag = Symbol \ (#reserved |
/// #internal)`). Pins that a `Difference` RHS survives the same declare-then-use
/// two-turn round trip `known_type_aliases_resolves_alias_from_earlier_turn`
/// exercises for a plain union, since the reparse path
/// (`extract_known_type_aliases`) silently drops any alias whose expansion text
/// fails to reparse — a regression here would fail closed (the alias just
/// vanishes) rather than loudly, so it needs its own pin.
#[test]
fn known_type_aliases_round_trips_a_difference_rhs() {
    let declare_response = handle_compile_expression(&compile_expression_request(
        "type PublicTag = Symbol \\ (#reserved | #internal)",
    ));
    assert_eq!(
        response_status(&declare_response).as_deref(),
        Some("ok"),
        "resp: {declare_response:?}"
    );
    let expansion = response_field_str(&declare_response, "expansion")
        .expect("expansion field must be present");

    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (atom("source"), binary("tag :: PublicTag := #anything")),
        (atom("module"), binary("bt@test_module")),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![binary(&format!(
                "type PublicTag = {expansion}"
            ))])),
        ),
    ]);
    let response = handle_compile_expression(&request);
    assert_eq!(
        response_status(&response).as_deref(),
        Some("ok"),
        "a Difference-RHS alias declared in an earlier turn must still resolve \
             (not silently vanish) when referenced in a later turn's `::` annotation: \
             {response:?}"
    );
}
