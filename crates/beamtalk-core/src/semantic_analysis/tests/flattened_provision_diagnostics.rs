// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0127 §3 ("Source locations") / BT-3663:
//! diagnostics raised inside a flattened trait provision are attributed to
//! the protocol and reported once across its users.

use super::*;
use crate::ast::ProtocolDefinition;
use crate::source_analysis::{Diagnostic, lex_with_eof, merge_provision_diagnostics, parse};

fn parse_ok(source: &str) -> Module {
    let (module, diagnostics) = parse(lex_with_eof(source));
    assert!(diagnostics.is_empty(), "{diagnostics:?}\n{source}");
    module
}

fn protocol_defs(source: &str) -> Vec<ProtocolDefinition> {
    parse_ok(source).protocols
}

/// A provision with a type error in its body: `Integer` has no `bogus`.
const BAD_TRAIT: &str = "Protocol define: Broken
  name -> String

  probe -> Integer => 3 bogus
";

fn analyse_user(user: &str, defs: &[ProtocolDefinition]) -> Vec<Diagnostic> {
    let module = parse_ok(user);
    analyse_full(
        &module,
        AnalysisContext::default().with_pre_loaded_protocol_defs(defs.to_vec()),
    )
    .diagnostics
}

fn provision_diagnostics(diagnostics: Vec<Diagnostic>) -> Vec<Diagnostic> {
    diagnostics
        .into_iter()
        .filter(|d| d.provision.is_some())
        .collect()
}

#[test]
fn provision_type_error_is_tagged_with_its_protocol_and_user() {
    let defs = protocol_defs(BAD_TRAIT);
    let diagnostics = analyse_user(
        "Object subclass: Alpha\n  uses: Broken\n  name -> String => \"a\"\n",
        &defs,
    );
    let tagged = provision_diagnostics(diagnostics);
    assert_eq!(tagged.len(), 1, "{tagged:?}");
    let origin = tagged[0].provision.as_ref().unwrap();
    assert_eq!(origin.protocol.as_str(), "Broken");
    assert_eq!(origin.users, vec![ecow::EcoString::from("Alpha")]);
    assert!(
        tagged[0]
            .notes
            .iter()
            .any(|n| n.message == "while flattening into Alpha"),
        "{:?}",
        tagged[0].notes
    );
}

#[test]
fn provision_type_error_shared_by_two_users_is_reported_once() {
    let defs = protocol_defs(BAD_TRAIT);
    let mut all = Vec::new();
    for user in ["Alpha", "Beta"] {
        all.extend(analyse_user(
            &format!("Object subclass: {user}\n  uses: Broken\n  name -> String => \"x\"\n"),
            &defs,
        ));
    }
    assert_eq!(provision_diagnostics(all.clone()).len(), 2);

    let merged = merge_provision_diagnostics(all);
    let tagged = provision_diagnostics(merged);
    assert_eq!(tagged.len(), 1, "{tagged:?}");
    assert_eq!(
        tagged[0].provision.as_ref().unwrap().users,
        vec![
            ecow::EcoString::from("Alpha"),
            ecow::EcoString::from("Beta")
        ]
    );
    let flattening_notes: Vec<_> = tagged[0]
        .notes
        .iter()
        .filter(|n| n.message.starts_with("while flattening into"))
        .collect();
    assert_eq!(flattening_notes.len(), 1, "{:?}", tagged[0].notes);
    assert_eq!(
        flattening_notes[0].message,
        "while flattening into Alpha, Beta"
    );
}

#[test]
fn a_users_own_diagnostic_is_not_tagged() {
    let defs = protocol_defs(BAD_TRAIT);
    let diagnostics = analyse_user(
        "Object subclass: Alpha\n  uses: Broken\n  name -> String => 3 bogus\n",
        &defs,
    );
    let (tagged, own): (Vec<_>, Vec<_>) =
        diagnostics.into_iter().partition(|d| d.provision.is_some());
    assert_eq!(tagged.len(), 1, "{tagged:?}");
    assert!(
        own.iter().any(|d| d.message.contains("bogus")),
        "the user's own type error stays untagged: {own:?}"
    );
}
