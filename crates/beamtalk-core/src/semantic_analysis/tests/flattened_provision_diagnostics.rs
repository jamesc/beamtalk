// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0127 §3 ("Source locations", "Name resolution") / BT-3663:
//! diagnostics raised inside a flattened trait provision are attributed to
//! the protocol, reported once across its users, and the provision's free
//! class names resolve in the protocol's package.

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

// ── Name resolution in the protocol's package (ADR 0127 §3) ──────────────

use crate::semantic_analysis::class_hierarchy::ClassInfo;
use crate::semantic_analysis::{ClassHierarchy, ProtocolSource, ProtocolSourceMap};

/// Class infos of a source file, stamped as belonging to `package`.
fn package_classes(source: &str, package: &str) -> Vec<ClassInfo> {
    let mut infos = ClassHierarchy::extract_class_infos(&parse_ok(source));
    ClassHierarchy::stamp_package_on_infos(&mut infos, package);
    infos
}

fn sources_in_package(protocols: &[&str], package: &str) -> ProtocolSourceMap {
    protocols
        .iter()
        .map(|name| {
            (
                ecow::EcoString::from(*name),
                ProtocolSource {
                    path: Some(format!("{package}/{name}.bt").into()),
                    text: "".into(),
                    package: Some(package.into()),
                },
            )
        })
        .collect()
}

/// Analyses `user` (package `app`) against protocol definitions and the
/// given pre-loaded classes; `sources` carries the protocols' package identity.
fn analyse_in_app(
    user: &str,
    defs: &[ProtocolDefinition],
    classes: Vec<ClassInfo>,
    sources: ProtocolSourceMap,
) -> Vec<Diagnostic> {
    let module = parse_ok(user);
    let options = crate::CompilerOptions {
        current_package: Some("app".into()),
        ..Default::default()
    };
    analyse_full(
        &module,
        AnalysisContext::default()
            .with_options(&options)
            .with_pre_loaded_classes(classes)
            .with_pre_loaded_protocol_defs(defs.to_vec())
            .with_protocol_sources(sources),
    )
    .diagnostics
}

const JSON_PARSER: &str = "Object subclass: Parser
  class parse: text :: String -> Integer => 42
";

const RUNS_TRAIT: &str = "Protocol define: Runs
  run: text :: String -> Integer => Parser parse: text
";

/// The user's package defines its own `Parser` — without `parse:`.
const APP_USER: &str = "Object subclass: Parser
  size -> Integer => 0

Object subclass: Job
  uses: Runs
";

#[test]
fn same_named_user_class_captures_a_provision_reference_without_package_identity() {
    // Control: with no package identity for the protocol, `Parser` in the
    // provision resolves to the user's `Parser` (today's behaviour), which
    // has no class-side `parse:`.
    let diagnostics = analyse_in_app(
        APP_USER,
        &protocol_defs(RUNS_TRAIT),
        package_classes(JSON_PARSER, "json"),
        ProtocolSourceMap::new(),
    );
    assert!(
        diagnostics.iter().any(|d| d.message.contains("parse:")),
        "expected the capture to surface as a `parse:` diagnostic: {diagnostics:?}"
    );
}

#[test]
fn same_named_user_class_does_not_capture_a_provision_reference() {
    let diagnostics = analyse_in_app(
        APP_USER,
        &protocol_defs(RUNS_TRAIT),
        package_classes(JSON_PARSER, "json"),
        sources_in_package(&["Runs"], "json"),
    );
    assert!(
        !diagnostics.iter().any(|d| d.message.contains("parse:")),
        "the provision's `Parser` is json's, not the user's: {diagnostics:?}"
    );
}

fn resolved_receiver_package(package_of_protocol: &str) -> Option<String> {
    let mut protocols: HashMap<ecow::EcoString, ProtocolDefinition> = protocol_defs(RUNS_TRAIT)
        .into_iter()
        .map(|p| (p.name.name.clone(), p))
        .collect();
    crate::semantic_analysis::trait_expansion::resolve_provision_names(
        &mut protocols,
        &sources_in_package(&["Runs"], package_of_protocol),
        &package_classes(JSON_PARSER, package_of_protocol),
        Some("app"),
    );
    let body = &protocols["Runs"].provided_methods[0].body[0].expression;
    let Expression::MessageSend { receiver, .. } = body else {
        panic!("expected a message send, got {body:?}");
    };
    let Expression::ClassReference { package, .. } = receiver.as_ref() else {
        panic!("expected a class reference receiver, got {receiver:?}");
    };
    package.as_ref().map(|p| p.name.to_string())
}

#[test]
fn provision_reference_is_emitted_package_qualified() {
    assert_eq!(resolved_receiver_package("json").as_deref(), Some("json"));
}

#[test]
fn provision_reference_in_the_users_own_package_is_left_unqualified() {
    assert_eq!(resolved_receiver_package("app"), None);
}

// ── `internal` (ADR 0071) is checked against the protocol's package ──────

const INTERNAL_HELPER: &str = "internal Object subclass: Helper
  help -> Integer => 1
";

const USES_HELPER_TRAIT: &str = "Protocol define: Helps
  assist -> Integer => Helper new help
";

const HELPS_USER: &str = "Object subclass: Job
  uses: Helps
";

#[test]
fn provision_may_use_its_own_packages_internal_class() {
    let diagnostics = analyse_in_app(
        HELPS_USER,
        &protocol_defs(USES_HELPER_TRAIT),
        package_classes(INTERNAL_HELPER, "json"),
        sources_in_package(&["Helps"], "json"),
    );
    assert!(
        !diagnostics
            .iter()
            .any(|d| d.message.contains("internal to package")),
        "json's provision may use json's internal Helper: {diagnostics:?}"
    );

    // Control: without the protocol's package the reference is checked
    // against the user's package and is (falsely) a visibility error.
    let control = analyse_in_app(
        HELPS_USER,
        &protocol_defs(USES_HELPER_TRAIT),
        package_classes(INTERNAL_HELPER, "json"),
        ProtocolSourceMap::new(),
    );
    assert!(
        control
            .iter()
            .any(|d| d.message.contains("internal to package")),
        "{control:?}"
    );
}

#[test]
fn provision_may_not_use_another_packages_internal_class() {
    let diagnostics = analyse_in_app(
        HELPS_USER,
        &protocol_defs(USES_HELPER_TRAIT),
        package_classes(INTERNAL_HELPER, "other"),
        sources_in_package(&["Helps"], "json"),
    );
    let visibility: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message.contains("internal to package"))
        .collect();
    assert_eq!(visibility.len(), 1, "{diagnostics:?}");
    assert!(
        visibility[0]
            .message
            .contains("cannot be referenced from 'json'"),
        "{}",
        visibility[0].message
    );
    let origin = visibility[0].provision.as_ref().expect("attributed");
    assert_eq!(origin.protocol.as_str(), "Helps");
}

/// Two carried protocols share a name (e.g. project and a dependency): the
/// first wins, consistent with `ProtocolRegistry::add_pre_loaded`.
#[test]
fn same_named_carried_protocols_resolve_first_wins() {
    let mut defs = protocol_defs(BAD_TRAIT);
    defs.extend(protocol_defs(
        "Protocol define: Broken\n  name -> String\n\n  probe -> Integer => 3\n",
    ));
    let tagged = provision_diagnostics(analyse_user(
        "Object subclass: Alpha\n  uses: Broken\n  name -> String => \"a\"\n",
        &defs,
    ));
    assert!(
        !tagged.is_empty(),
        "the first (broken) definition must be the one flattened"
    );
}

// ── A cross-file trait user's provisions are visible to other files ──────
// (BT-3668)

const TAGGED_TRAIT: &str = "Protocol define: Tagged
  tag -> String => \"tag\"
";

const WIDGET: &str = "Object subclass: Widget
  uses: Tagged
";

const WIDGET_CALLER: &str = "Object subclass: Caller
  describe: w :: Widget -> String => w tag
";

fn dnu_diagnostics(widget_infos: Vec<ClassInfo>) -> Vec<Diagnostic> {
    let module = parse_ok(WIDGET_CALLER);
    analyse_full(
        &module,
        AnalysisContext::default().with_pre_loaded_classes(widget_infos),
    )
    .diagnostics
    .into_iter()
    .filter(|d| d.message.contains("does not understand"))
    .collect()
}

fn tagged_defs() -> HashMap<ecow::EcoString, ProtocolDefinition> {
    protocol_defs(TAGGED_TRAIT)
        .into_iter()
        .map(|p| (p.name.name.clone(), p))
        .collect()
}

#[test]
fn unflattened_class_info_of_a_cross_file_trait_user_reports_dnu() {
    // Control: the class's own body alone has no `tag`, which is the gap
    // `extract_flattened_class_infos` closes.
    let infos = ClassHierarchy::extract_class_infos(&parse_ok(WIDGET));
    assert_eq!(dnu_diagnostics(infos).len(), 1);
}

#[test]
fn flattened_class_info_of_a_cross_file_trait_user_has_its_provisions() {
    let infos = crate::semantic_analysis::trait_expansion::extract_flattened_class_infos(
        &parse_ok(WIDGET),
        &tagged_defs(),
        None,
    );
    let tag = infos[0]
        .methods
        .iter()
        .find(|m| m.selector == "tag")
        .expect("`tag` is flattened into Widget's info");
    assert_eq!(tag.defined_in.as_str(), "Widget");
    assert_eq!(tag.origin.as_deref(), Some("Tagged"));
    assert!(dnu_diagnostics(infos).is_empty());
}

#[test]
fn flattened_class_info_keeps_a_class_body_method_over_the_provision() {
    let widget = "Object subclass: Widget\n  uses: Tagged\n  tag -> String => \"own\"\n";
    let infos = crate::semantic_analysis::trait_expansion::extract_flattened_class_infos(
        &parse_ok(widget),
        &tagged_defs(),
        None,
    );
    let tags: Vec<_> = infos[0]
        .methods
        .iter()
        .filter(|m| m.selector == "tag")
        .collect();
    assert_eq!(tags.len(), 1);
    assert_eq!(tags[0].origin, None);
}
