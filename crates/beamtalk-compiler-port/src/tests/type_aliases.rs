// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `known_type_aliases` threading through `diagnostics/compile/compile_expression` (class and protocol), cross-module alias references, and beamtalk.toml diagnostics-overrides loading.

use super::*;

/// ADR 0108 hot-reload re-check trigger (BT-2899): `diagnostics` now
/// threads `known_type_aliases` through (mirroring `compile_expression`)
/// and reports the resolved compile's `referenced_aliases` — the
/// alias-name → dependent-class index's raw material. Both the alias
/// resolving correctly (proven the same way
/// `known_type_aliases_resolves_alias_from_earlier_turn` proves it: a
/// `matchExhaustive:` non-exhaustive diagnostic naming the residual
/// member) and the new field being populated with the referenced name
/// are pinned here.
#[test]
fn diagnostics_with_known_type_aliases_resolves_and_reports_referenced_aliases() {
    let request = Map::from([
        (atom("command"), atom("diagnostics")),
        (
            atom("source"),
            binary(
                "scrutinee :: Direction := #north. \
                     scrutinee matchExhaustive: [#north -> 0; #south -> 1; #east -> 2]",
            ),
        ),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![binary(
                "type Direction = #north | #south | #east | #west",
            )])),
        ),
    ]);

    let response = handle_diagnostics(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")));

    let Some(Term::List(diagnostics)) = map_get(m, "diagnostics") else {
        panic!("Expected diagnostics list: {response:?}");
    };
    let found = diagnostics.elements.iter().any(|d| {
        let Term::Map(dm) = d else { return false };
        map_get(dm, "message")
            .and_then(term_to_string)
            .is_some_and(|msg| {
                msg.contains("non-exhaustive matchExhaustive:") && msg.contains("#west")
            })
    });
    assert!(
        found,
        "expected Direction to resolve to the closed union via known_type_aliases: {response:?}"
    );

    let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
        panic!("Expected referenced_aliases list: {response:?}");
    };
    let names: Vec<String> = referenced
        .elements
        .iter()
        .filter_map(term_to_string)
        .collect();
    assert_eq!(
        names,
        vec!["Direction".to_string()],
        "expected Direction to be recorded as referenced: {response:?}"
    );
}

/// Without `known_type_aliases`, `referenced_aliases` is simply absent
/// any alias touch — an ordinary compile with no aliases in scope must
/// not report anything.
#[test]
fn diagnostics_without_known_type_aliases_reports_empty_referenced_aliases() {
    let request = Map::from([
        (atom("command"), atom("diagnostics")),
        (atom("source"), binary("1 + 1.")),
    ]);

    let response = handle_diagnostics(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
        panic!("Expected referenced_aliases list: {response:?}");
    };
    assert!(
        referenced.elements.is_empty(),
        "expected no referenced aliases: {response:?}"
    );
}

/// Without `class_hierarchy`, `diagnostics` behaves exactly as before
/// (Counter is unknown, so no diagnostic is produced for the `+ 1` — the
/// checker treats an undeclared receiver type as unresolved-class, not
/// as `Counter`).
#[test]
fn diagnostics_without_class_hierarchy_is_unaffected() {
    let request = Map::from([
        (atom("command"), atom("diagnostics")),
        (atom("source"), binary("1 + 1.")),
    ]);

    let response = handle_diagnostics(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(map_get(m, "status"), Some(&atom("ok")));
    let Some(Term::List(diagnostics)) = map_get(m, "diagnostics") else {
        panic!("Expected diagnostics list: {response:?}");
    };
    assert!(
        diagnostics.elements.is_empty(),
        "expected no diagnostics for a plain valid expression: {response:?}"
    );
}

/// BT-2839 (ADR 0100 Rule 3 surface-parity gap): a project root with a
/// `dnu = "error"` `[diagnostics]` table parses into a table that
/// escalates `Dnu`, mirroring what `beamtalk build` and the LSP
/// (BT-2800) already do for the same `beamtalk.toml`.
#[test]
fn load_diagnostics_overrides_from_parses_project_manifest() {
    use beamtalk_core::compilation::DiagnosticSeverityOverride;
    use beamtalk_core::source_analysis::DiagnosticCategory;

    let dir = tempfile::tempdir().expect("failed to create temp dir");
    std::fs::write(
        dir.path().join("beamtalk.toml"),
        "[package]\nname = \"demo\"\nversion = \"0.1.0\"\n\n[diagnostics]\ndnu = \"error\"\n",
    )
    .expect("failed to write beamtalk.toml");

    let table = load_diagnostics_overrides_from(dir.path());
    assert_eq!(
        table.get(&DiagnosticCategory::Dnu),
        Some(&DiagnosticSeverityOverride::Error),
        "expected dnu = \"error\" to parse into the table, got: {table:?}"
    );
}

/// Lenient by design: a root with no `beamtalk.toml` at all — the common
/// case for an ad-hoc `beamtalk repl` session outside a project — must
/// not block diagnostics. An empty table is a complete no-op (Rule 1
/// defaults), matching the LSP's `load_diagnostics_table_absent_manifest_is_noop`.
#[test]
fn load_diagnostics_overrides_from_missing_manifest_is_empty() {
    let dir = tempfile::tempdir().expect("failed to create temp dir");

    let table = load_diagnostics_overrides_from(dir.path());
    assert!(
        table.is_empty(),
        "expected an empty table with no beamtalk.toml, got: {table:?}"
    );
}

/// A `beamtalk.toml` with an invalid `[diagnostics]` table (unknown
/// severity string) must not panic or block diagnostics — it logs and
/// falls back to an empty table, same as a missing manifest.
#[test]
fn load_diagnostics_overrides_from_malformed_table_is_empty() {
    let dir = tempfile::tempdir().expect("failed to create temp dir");
    std::fs::write(
        dir.path().join("beamtalk.toml"),
        "[diagnostics]\ndnu = \"not-a-real-severity\"\n",
    )
    .expect("failed to write beamtalk.toml");

    let table = load_diagnostics_overrides_from(dir.path());
    assert!(
        table.is_empty(),
        "expected an empty table for a malformed [diagnostics] entry, got: {table:?}"
    );
}

/// ADR 0050 Phase 4: `class_hierarchy` in `compile` request is accepted
/// and does not cause errors (backward-compatible optional key).
#[test]
fn compile_accepts_class_hierarchy_key() {
    use eetf::{FixInteger, List};

    let counter_meta = Map::from([
        (atom("class"), atom("Counter")),
        (atom("superclass"), atom("Actor")),
        (atom("meta_version"), Term::from(FixInteger::from(2))),
        (atom("is_sealed"), atom("false")),
        (atom("is_abstract"), atom("false")),
        (atom("is_value"), atom("false")),
        (atom("is_typed"), atom("false")),
        (atom("fields"), Term::from(List::from(vec![]))),
        (atom("field_types"), Term::from(Map::from([]))),
        (atom("method_info"), Term::from(Map::from([]))),
        (atom("class_method_info"), Term::from(Map::from([]))),
        (atom("class_variables"), Term::from(List::from(vec![]))),
    ]);
    let class_hierarchy_term = Term::from(Map::from([(atom("Counter"), Term::from(counter_meta))]));

    let request = Map::from([
        (atom("command"), atom("compile")),
        (
            atom("source"),
            binary("Object subclass: MyThing\n  hello => 42"),
        ),
        (atom("module_name"), binary("bt@my_thing")),
        (atom("class_hierarchy"), class_hierarchy_term),
    ]);

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "compile with class_hierarchy should succeed: {response:?}"
    );
}

/// ADR 0108 hot-reload re-check trigger (BT-2899): `compile` now threads
/// `known_type_aliases` through too (previously only
/// `compile_expression` did), so a class-defining compile reports which
/// alias names its own annotations referenced — the raw material for
/// `beamtalk_alias_xref`'s alias-name → dependent-class index.
#[test]
fn compile_with_known_type_aliases_reports_referenced_aliases() {
    let request = Map::from([
        (atom("command"), atom("compile")),
        (
            atom("source"),
            binary(
                "Object subclass: Dashboard\n  \
                     heading: h :: Direction -> Integer => 0\n",
            ),
        ),
        (atom("module_name"), binary("bt@dashboard")),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![binary(
                "type Direction = #north | #south | #east | #west",
            )])),
        ),
    ]);

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "expected compile to succeed: {response:?}"
    );
    let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
        panic!("Expected referenced_aliases list: {response:?}");
    };
    let names: Vec<String> = referenced
        .elements
        .iter()
        .filter_map(term_to_string)
        .collect();
    assert_eq!(
        names,
        vec!["Direction".to_string()],
        "expected Direction to be recorded as referenced: {response:?}"
    );
}

/// BT-2917 (BT-2899 follow-up): the sibling of
/// `compile_with_known_type_aliases_reports_referenced_aliases` for a
/// protocol-only `compile` — before this fix, `protocol_definition`'s
/// response had no `referenced_aliases` field at all, so
/// `beamtalk_repl_compiler.erl`'s protocol arm had nothing to register
/// into `beamtalk_alias_xref`, even though the exact same annotation on
/// a class method's signature (the test above) already worked.
#[test]
fn compile_protocol_with_known_type_aliases_reports_referenced_aliases() {
    let request = Map::from([
        (atom("command"), atom("compile")),
        (
            atom("source"),
            binary("Protocol define: Directional\n  heading: d :: Direction -> Boolean\n"),
        ),
        (atom("module_name"), binary("bt@directional")),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![binary(
                "type Direction = #north | #south | #east | #west",
            )])),
        ),
    ]);

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "expected compile to succeed: {response:?}"
    );
    assert_eq!(
        map_get(m, "kind"),
        Some(&atom("protocol_definition")),
        "expected a protocol_definition response: {response:?}"
    );
    let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
        panic!("Expected referenced_aliases list: {response:?}");
    };
    let names: Vec<String> = referenced
        .elements
        .iter()
        .filter_map(term_to_string)
        .collect();
    assert_eq!(
        names,
        vec!["Direction".to_string()],
        "expected Direction to be recorded as referenced: {response:?}"
    );
}

/// BT-2952: the REPL-inline sibling of
/// `compile_with_known_type_aliases_reports_referenced_aliases` for a
/// class defined via `compile_expression` (as opposed to `:load`d from
/// a file via `compile`) — before this fix, `parse_and_check_expression`
/// called `compute_diagnostics_with_known_vars_classes_and_aliases`,
/// which discards `AnalysisResult::referenced_aliases` entirely, so
/// `class_definition_ok_response` had no field to carry it in at all.
#[test]
fn compile_expression_class_with_known_type_aliases_reports_referenced_aliases() {
    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (
            atom("source"),
            binary(
                "Object subclass: Dashboard\n  \
                     heading: h :: Direction -> Integer => 0\n",
            ),
        ),
        (atom("module"), binary("bt@repl_eval_1")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![binary(
                "type Direction = #north | #south | #east | #west",
            )])),
        ),
    ]);

    let response = handle_compile_expression(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "expected compile_expression to succeed: {response:?}"
    );
    assert_eq!(
        map_get(m, "kind"),
        Some(&atom("class_definition")),
        "expected a class_definition response: {response:?}"
    );
    let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
        panic!("Expected referenced_aliases list: {response:?}");
    };
    let names: Vec<String> = referenced
        .elements
        .iter()
        .filter_map(term_to_string)
        .collect();
    assert_eq!(
        names,
        vec!["Direction".to_string()],
        "expected Direction to be recorded as referenced: {response:?}"
    );
}

/// BT-2952: the REPL-inline sibling of
/// `compile_protocol_with_known_type_aliases_reports_referenced_aliases`
/// for a protocol defined via `compile_expression` — before this fix,
/// `handle_compile_expression`'s protocol branch called
/// `handle_inline_protocol_definition` with a hardcoded `&[]` since
/// `parse_and_check_expression` never computed a real set for this path.
#[test]
fn compile_expression_protocol_with_known_type_aliases_reports_referenced_aliases() {
    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (
            atom("source"),
            binary("Protocol define: Directional\n  heading: d :: Direction -> Boolean\n"),
        ),
        (atom("module"), binary("bt@repl_eval_1")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![binary(
                "type Direction = #north | #south | #east | #west",
            )])),
        ),
    ]);

    let response = handle_compile_expression(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "expected compile_expression to succeed: {response:?}"
    );
    assert_eq!(
        map_get(m, "kind"),
        Some(&atom("protocol_definition")),
        "expected a protocol_definition response: {response:?}"
    );
    let Some(Term::List(referenced)) = map_get(m, "referenced_aliases") else {
        panic!("Expected referenced_aliases list: {response:?}");
    };
    let names: Vec<String> = referenced
        .elements
        .iter()
        .filter_map(term_to_string)
        .collect();
    assert_eq!(
        names,
        vec!["Direction".to_string()],
        "expected Direction to be recorded as referenced: {response:?}"
    );
}

/// BT-2941: `handle_inline_protocol_definition` (the protocol-only branch
/// of `handle_compile`) previously never threaded `pre_loaded_aliases`
/// into `CodegenOptions`, so `self.alias_registry` at codegen time was
/// always the empty module-local registry — a protocol source file never
/// declares its own `type Name = ...` (that's a separate top-level
/// declaration), so every alias a protocol method signature could
/// reference is necessarily cross-module/pre-loaded.
///
/// BT-2957: protocol methods have no standalone function to attach a real
/// `-spec` to, so `generate_protocol_registrations`
/// (`gen_server/methods.rs`) now embeds the same `user_type`/abstract-type
/// representation `-spec`s use directly in each `register_protocol`
/// method-requirement map's `param_types`/`return_type` entries, and
/// `actor_codegen.rs::generate_module` marks the alias as referenced
/// before the module header's named `-type` declarations are emitted —
/// so `Wrapper`/`Base` both get a `user_type` reference here, just not
/// via a real Dialyzer `-spec` (protocols have no function to attach one
/// to).
#[test]
fn compile_protocol_cross_module_alias_reference_emits_user_type() {
    let request = Map::from([
        (atom("command"), atom("compile")),
        (
            atom("source"),
            binary("Protocol define: Directional\n  heading: d :: Wrapper -> Boolean\n"),
        ),
        (atom("module_name"), binary("bt@directional")),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![
                binary("type Base = #north | #south | #east | #west"),
                binary("type Wrapper = Base"),
            ])),
        ),
    ]);

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("ok")),
        "expected compile to succeed: {response:?}"
    );
    let core_erlang = map_get(m, "core_erlang")
        .and_then(term_to_string)
        .expect("core_erlang field must be present");
    assert!(
        core_erlang.contains("{'user_type', 0, 'wrapper', []}"),
        "expected the protocol method's Wrapper-typed parameter to embed a \
             user_type reference in its register_protocol metadata: {core_erlang}"
    );
    assert!(
        core_erlang.matches("'wrapper'").count() >= 2,
        "expected the module attribute list to also declare the named -type \
             'wrapper' the user_type reference points at (one occurrence for the \
             -type declaration, at least one more for the reference): {core_erlang}"
    );
}

/// BT-2941 sibling of `compile_protocol_cross_module_alias_reference_emits_user_type`
/// for the OTHER `handle_inline_protocol_definition` caller: the REPL-inline
/// `compile_expression` path (`handle_compile_expression`'s protocol branch).
/// Both call sites needed the same `.with_pre_loaded_aliases(...)` wiring.
/// See the BT-2957 update note on the sibling test above — the same
/// reasoning applies here.
#[test]
fn compile_expression_protocol_cross_module_alias_reference_emits_user_type() {
    let request = Map::from([
        (atom("command"), atom("compile_expression")),
        (
            atom("source"),
            binary("Protocol define: Directional\n  heading: d :: Wrapper -> Boolean\n"),
        ),
        (atom("module"), binary("bt@repl_eval_1")),
        (atom("known_vars"), Term::from(List::from(vec![]))),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![
                binary("type Base = #north | #south | #east | #west"),
                binary("type Wrapper = Base"),
            ])),
        ),
    ]);

    let response = handle_compile_expression(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected a map response, got: {response:?}");
    };
    assert_eq!(
        map_get(m, "status").and_then(term_to_atom).as_deref(),
        Some("ok"),
        "expected compile_expression to succeed: {response:?}"
    );
    let core_erlang = map_get(m, "core_erlang")
        .and_then(term_to_string)
        .expect("core_erlang field must be present");
    assert!(
        core_erlang.contains("{'user_type', 0, 'wrapper', []}"),
        "expected the protocol method's Wrapper-typed parameter to embed a \
             user_type reference in its register_protocol metadata: {core_erlang}"
    );
    assert!(
        core_erlang.matches("'wrapper'").count() >= 2,
        "expected the module attribute list to also declare the named -type \
             'wrapper' the user_type reference points at (one occurrence for the \
             -type declaration, at least one more for the reference): {core_erlang}"
    );
}

/// The concrete BT-2912 repro, exercised through the compiler port
/// exactly as a live REPL turn would present it: turn 1 declares `type
/// Point = Integer` (carried forward via `known_type_aliases`, mirroring
/// how the workspace re-seeds it every turn — ADR 0108 Phase 8); turn 2
/// sends `Object subclass: Point`. Before BT-2899, `compile` never
/// threaded `known_type_aliases` at all, so
/// `AliasRegistry::add_pre_loaded`'s existing collision check
/// (`alias_registry.rs`) never had a chance to see the class — the class
/// compiled clean, silently shadowing the alias in every subsequent `::`
/// annotation. It must now fail with the namespace-collision diagnostic.
#[test]
fn compile_class_over_earlier_turn_alias_is_flagged() {
    let request = Map::from([
        (atom("command"), atom("compile")),
        (
            atom("source"),
            binary("Object subclass: Point\n  hello => 42\n"),
        ),
        (atom("module_name"), binary("bt@point")),
        (
            atom("known_type_aliases"),
            Term::from(List::from(vec![binary("type Point = Integer")])),
        ),
    ]);

    let response = handle_compile(&request);
    let Term::Map(ref m) = response else {
        panic!("Expected map response");
    };
    assert_eq!(
        map_get(m, "status"),
        Some(&atom("error")),
        "expected the class-vs-alias collision to fail the compile: {response:?}"
    );
    let Some(Term::List(diags)) = map_get(m, "diagnostics") else {
        panic!("Expected diagnostics list: {response:?}");
    };
    let found = diags.elements.iter().any(|d| {
        let Term::Map(dm) = d else { return false };
        map_get(dm, "message")
            .and_then(term_to_string)
            .is_some_and(|msg| msg.contains("Point") && msg.contains("collides with class"))
    });
    assert!(
        found,
        "expected a Point-vs-alias collision diagnostic: {response:?}"
    );
}
