// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Type-alias visibility in the project-wide registry (completions,
//! go-to-definition, find-references, hover) and cross-file diagnostics
//! resolution (classes, protocols, extensions, severity overrides, stdlib
//! shadowing).

use super::common::*;

// -----------------------------------------------------------------------
// ADR 0108 Phase 8 (BT-2901): completions, goto-definition, find-
// references, and hover for type aliases via `SimpleLanguageService`
// -----------------------------------------------------------------------

#[test]
fn alias_visible_in_project_wide_registry_after_update_file() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("aliases.bt");
    service.update_file(
        file.clone(),
        "type RestartStrategy = #temporary | #transient | #permanent\n".to_string(),
    );

    assert!(
        service
            .project_index
            .alias_registry()
            .has_alias("RestartStrategy"),
        "alias should be tracked in the project-wide registry after update_file"
    );
}

#[test]
fn alias_removed_from_registry_when_file_removed() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("aliases.bt");
    service.update_file(
        file.clone(),
        "type RestartStrategy = #temporary | #transient | #permanent\n".to_string(),
    );
    assert!(
        service
            .project_index
            .alias_registry()
            .has_alias("RestartStrategy")
    );

    service.remove_file(&file);
    assert!(
        !service
            .project_index
            .alias_registry()
            .has_alias("RestartStrategy"),
        "alias should be dropped from the project-wide registry when its file is removed"
    );
}

#[test]
fn completions_include_alias_name_cross_file() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("aliases.bt");
    let file_b = Utf8PathBuf::from("supervisor.bt");
    service.update_file(
        file_a,
        "type RestartStrategy = #temporary | #transient | #permanent\n".to_string(),
    );
    service.update_file(
        file_b.clone(),
        "Object subclass: Supervisor\n  restart: policy :: \n".to_string(),
    );

    let completions = service.completions(&file_b, Position::new(1, 20));
    assert!(
        completions.iter().any(|c| c.label == "RestartStrategy"),
        "Expected cross-file alias name in completions, got: {:?}",
        completions.iter().map(|c| &c.label).collect::<Vec<_>>()
    );
}

#[test]
fn goto_definition_alias_from_annotation_site_cross_file() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("aliases.bt");
    let file_b = Utf8PathBuf::from("supervisor.bt");
    service.update_file(
        file_a.clone(),
        "type RestartStrategy = #temporary | #transient | #permanent\n".to_string(),
    );
    service.update_file(
        file_b.clone(),
        "Object subclass: Supervisor\n  restart: policy :: RestartStrategy => policy\n".to_string(),
    );

    // "  restart: policy :: RestartStrategy =>" — "RestartStrategy" starts at column 21.
    let def = service.goto_definition(&file_b, Position::new(1, 25));
    let loc = def.expect("goto-def should navigate to the alias declaration");
    assert_eq!(loc.file, file_a);
    assert_eq!(loc.span.start(), 5); // "type " is 5 chars.
}

#[test]
fn find_references_alias_from_definition_site_cross_file() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("aliases.bt");
    let file_b = Utf8PathBuf::from("supervisor.bt");
    service.update_file(
        file_a.clone(),
        "type RestartStrategy = #temporary | #transient | #permanent\n".to_string(),
    );
    service.update_file(
        file_b.clone(),
        "Object subclass: Supervisor\n  restart: policy :: RestartStrategy => policy\n".to_string(),
    );

    // Cursor on the alias declaration name in file_a, col 10.
    let refs = service.find_references(&file_a, Position::new(0, 10));
    assert_eq!(
        refs.len(),
        2,
        "expected 2 refs (decl + annotation site), got {refs:?}"
    );
    assert!(refs.iter().any(|r| r.file == file_a));
    assert!(refs.iter().any(|r| r.file == file_b));
}

#[test]
fn hover_resolves_alias_through_project_wide_registry() {
    // Regression guard for the `hover()` call site: it must thread the
    // project-wide `AliasRegistry` (not `None`) so a cross-file
    // alias-typed reference resolves to `AliasName (expansion)`.
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("aliases.bt");
    let file_b = Utf8PathBuf::from("supervisor.bt");
    service.update_file(
        file_a,
        "type RestartStrategy = #temporary | #transient | #permanent\n".to_string(),
    );
    service.update_file(
        file_b.clone(),
        "Object subclass: Supervisor\n  restart: policy :: RestartStrategy => policy\n".to_string(),
    );

    // Hover on the final `policy` (the alias-typed parameter's use,
    // mirroring `hover_provider`'s flagship test) — line 1, column 40:
    // "  restart: policy :: RestartStrategy => policy".
    let hover = service.hover(&file_b, Position::new(1, 40));
    let hover = hover.expect("should get hover for the alias-typed parameter reference");
    assert!(
        hover
            .contents
            .contains("RestartStrategy (#temporary | #transient | #permanent)"),
        "Expected `AliasName (expansion)` via the project-wide registry, got: {}",
        hover.contents
    );
}

#[test]
fn alias_name_at_identifies_alias_and_rejects_class_name() {
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("mixed.bt");
    service.update_file(
        file.clone(),
        "type RestartStrategy = #temporary | #transient | #permanent\n\nObject subclass: Foo\n  bar => 1\n"
            .to_string(),
    );

    assert_eq!(
        service.alias_name_at(&file, Position::new(0, 10)),
        Some(EcoString::from("RestartStrategy"))
    );
    assert_eq!(service.alias_name_at(&file, Position::new(2, 20)), None);
}

/// BT-2919: a cursor on `Foo` in `policy :: Foo` (parameter-annotation
/// position) must be recognized as an unresolved type reference even
/// though the file declaring `type Foo = ...` is deliberately never
/// passed to `update_file` here — this simulates workspace preload not
/// having reached that file yet (or the preload budget exhausting
/// first). `alias_name_at` alone misses this case because it requires
/// the alias to already be registered in the project-wide index.
#[test]
fn unresolved_type_reference_at_detects_annotation_position_before_declaring_file_indexed() {
    let mut service = SimpleLanguageService::new();
    let file_b = Utf8PathBuf::from("supervisor.bt");
    service.update_file(
        file_b.clone(),
        "Object subclass: Supervisor\n  restart: policy :: Foo => policy\n".to_string(),
    );

    assert!(
        !service.is_project_complete(),
        "preload hasn't run in this test; project_complete defaults to false"
    );
    // "  restart: policy :: Foo => policy" — `Foo` occupies columns 21-23.
    assert_eq!(
        service.unresolved_type_reference_at(&file_b, Position::new(1, 22)),
        Some(EcoString::from("Foo")),
        "cursor on `Foo` in annotation position must be flagged as an \
         unresolved type reference even though no file declares it yet"
    );
    assert_eq!(
        service.alias_name_at(&file_b, Position::new(1, 22)),
        None,
        "alias_name_at only recognizes already-registered aliases"
    );

    // find_references must route through the class-aware walker (which
    // matches by name text, not registry membership) instead of
    // silently falling through to the identifier-only fallback, which
    // never looks at type annotations and would return nothing here.
    let refs = service.find_references(&file_b, Position::new(1, 22));
    assert_eq!(
        refs.len(),
        1,
        "expected the annotation-site reference itself, got {refs:?}"
    );
}

/// BT-2919: `has_incomplete_reference_coverage_at` must agree with the
/// two checks it combines (`alias_name_at` for an already-resolved
/// alias, `unresolved_type_reference_at` for a not-yet-indexed one) —
/// it's a single-AST-walk optimization, not a behavior change.
#[test]
fn has_incomplete_reference_coverage_at_matches_split_checks() {
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("aliases.bt");
    let file_b = Utf8PathBuf::from("supervisor.bt");
    service.update_file(
        file_a,
        "type RestartStrategy = #temporary | #transient | #permanent\n".to_string(),
    );
    service.update_file(
        file_b.clone(),
        "Object subclass: Supervisor\n  restart: policy :: RestartStrategy => policy\n".to_string(),
    );

    // Resolved alias reference (RestartStrategy is indexed via file_a).
    assert!(service.has_incomplete_reference_coverage_at(&file_b, Position::new(1, 22)));

    // Unresolved type-reference position (Foo, at columns 19-21, is
    // never declared anywhere).
    let file_c = Utf8PathBuf::from("unresolved.bt");
    service.update_file(
        file_c.clone(),
        "Object subclass: Widget\n  render: shape :: Foo => shape\n".to_string(),
    );
    assert!(service.has_incomplete_reference_coverage_at(&file_c, Position::new(1, 20)));

    // Method selector (bar, at columns 2-4): neither check fires — method
    // headers are not walked by find_identifier_at_position.
    let file_d = Utf8PathBuf::from("plain.bt");
    service.update_file(
        file_d.clone(),
        "Object subclass: Plain\n  bar => 1\n".to_string(),
    );
    assert!(!service.has_incomplete_reference_coverage_at(&file_d, Position::new(1, 3)));
}

/// BT-2027: `SimpleLanguageService::diagnostics` must hand off the
/// cross-file class set from the `ProjectIndex` to the unified diagnostic
/// pipeline, so that a file referencing a class defined elsewhere does
/// not produce a spurious `UnresolvedClass` diagnostic.
#[test]
fn diagnostics_resolve_cross_file_class_via_project_index() {
    use beamtalk_core::source_analysis::DiagnosticCategory;

    let mut service = SimpleLanguageService::new();
    let src_file = Utf8PathBuf::from("src/Foo.bt");
    let test_file = Utf8PathBuf::from("test/FooTest.bt");

    service.update_file(
        src_file.clone(),
        "Object subclass: Foo\n  class demo => 42\n".to_string(),
    );
    service.update_file(
        test_file.clone(),
        "Object subclass: FooTest\n  class run =>\n    Foo demo\n".to_string(),
    );

    let diags = service.diagnostics(&test_file);
    let unresolved: Vec<_> = diags
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::UnresolvedClass))
        .collect();
    assert!(
        unresolved.is_empty(),
        "cross-file class `Foo` should resolve via ProjectIndex, got: {unresolved:?}"
    );
}

/// BT-2951: `ProjectIndex::cross_file_alias_infos_for` used to return
/// every alias from every indexed file unconditionally, with no
/// `package` stamping and no `current_package` threaded to
/// `AliasRegistry::add_pre_loaded`'s seeding-boundary exclusion — so a
/// dependency's `internal type Foo = ...` (indexed from
/// `_build/deps/<name>/src/`, mirroring `beamtalk-lsp`'s filesystem-driven
/// dependency preload) was visible (and go-to-definition-navigable) from
/// every other indexed file, silently bypassing ADR 0108's `internal`
/// modifier on this surface. A dependency's *public* alias must still
/// resolve — this mirrors `beamtalk-cli`'s
/// `lint_resolves_dependency_protocol_and_public_alias_but_not_internal_alias`.
///
/// Uses `goto_definition` (rather than a diagnostic) as the resolution
/// signal: an annotation naming an alias absent from the project-wide
/// `AliasRegistry` doesn't necessarily produce a diagnostic at all
/// (`check_annotation_for_unresolved_alias` only warns when there's a
/// near-miss *suggestion* — see its doc), whereas `goto_definition`
/// navigating to the alias's declaration is an unambiguous "this name
/// resolved" signal, exactly like
/// `goto_definition_alias_from_annotation_site_cross_file` above uses
/// for the same-project case.
#[test]
fn goto_definition_excludes_dependency_internal_alias_but_resolves_public_one() {
    let mut service = SimpleLanguageService::new();
    let dep_file = Utf8PathBuf::from("_build/deps/http/src/Types.bt");
    let consumer_file = Utf8PathBuf::from("src/Client.bt");

    service.update_file(
        dep_file.clone(),
        "internal type Secret = String\ntype PublicId = Integer\n".to_string(),
    );
    service.update_file(
        consumer_file.clone(),
        "Object subclass: Client\n  useSecret: s :: Secret => s\n  useId: i :: PublicId => i\n"
            .to_string(),
    );

    // "  useSecret: s :: Secret => s" — the annotation's "Secret" starts
    // at column 18.
    let secret_def = service.goto_definition(&consumer_file, Position::new(1, 20));
    assert!(
        secret_def.is_none(),
        "a dependency's internal alias must not resolve (navigate) from a \
         consumer file, got: {secret_def:?}"
    );

    // "  useId: i :: PublicId => i" — the annotation's "PublicId" starts
    // at column 14.
    let public_def = service.goto_definition(&consumer_file, Position::new(2, 18));
    let loc = public_def
        .expect("a dependency's public alias must still resolve (navigate) from a consumer file");
    assert_eq!(loc.file, dep_file);
}

/// BT-2951 sibling: a same-project file's `internal type Foo = ...` must
/// stay visible to every *other* same-project file (ADR 0108: internal
/// aliases are usable throughout their declaring package, not just their
/// declaring file) — only a *different* package's internal alias should
/// ever be excluded. Both files here are stamped with the same
/// same-project marker (`ProjectIndex`'s `CURRENT_PROJECT_PACKAGE_MARKER`),
/// so this must not regress into over-exclusion.
#[test]
fn goto_definition_resolves_same_project_internal_alias_cross_file() {
    let mut service = SimpleLanguageService::new();
    let alias_file = Utf8PathBuf::from("src/Types.bt");
    let consumer_file = Utf8PathBuf::from("src/Client.bt");

    service.update_file(
        alias_file.clone(),
        "internal type Secret = String\n".to_string(),
    );
    service.update_file(
        consumer_file.clone(),
        "Object subclass: Client\n  useSecret: s :: Secret => s\n".to_string(),
    );

    // "  useSecret: s :: Secret => s" — the annotation's "Secret" starts
    // at column 18.
    let def = service.goto_definition(&consumer_file, Position::new(1, 20));
    let loc = def.expect("a same-project internal alias should resolve (navigate) cross-file");
    assert_eq!(loc.file, alias_file);
}

/// BT-2950: `pre_loaded_protocols` was never populated on the LSP
/// surface at all — a protocol declared in a different project file was
/// invisible to LSP diagnostics, so an `extending:` clause naming it
/// produced a false "extends unknown protocol" error even though
/// `beamtalk build`/`beamtalk lint` already resolve it correctly
/// (BT-2910). Mirrors `diagnostics_resolve_cross_file_class_via_project_index`
/// above, but for protocols.
#[test]
fn diagnostics_resolve_cross_file_protocol_via_project_index() {
    let mut service = SimpleLanguageService::new();
    let base_file = Utf8PathBuf::from("src/Base.bt");
    let extending_file = Utf8PathBuf::from("src/Extended.bt");

    service.update_file(
        base_file,
        "Protocol define: BaseProto\n  base => Boolean\n".to_string(),
    );
    service.update_file(
        extending_file.clone(),
        "Protocol define: ExtendedProto\n  extending: BaseProto\n  extra => Boolean\n".to_string(),
    );

    let diags = service.diagnostics(&extending_file);
    let unknown_protocol: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("extends unknown protocol"))
        .collect();
    assert!(
        unknown_protocol.is_empty(),
        "cross-file protocol `BaseProto` should resolve via ProjectIndex, got: {unknown_protocol:?}"
    );
}

/// BT-2950 sibling: a protocol exported by an indexed path dependency
/// (under `_build/deps/<name>/src/`, mirroring how the alias-resolution
/// path already covers dependency files) must resolve in a consumer
/// file — parity with `beamtalk-cli`'s dependency-side test for the same
/// wiring (BT-2910).
#[test]
fn diagnostics_resolve_dependency_protocol_via_project_index() {
    let mut service = SimpleLanguageService::new();
    let dep_file = Utf8PathBuf::from("_build/deps/http/src/Base.bt");
    let consumer_file = Utf8PathBuf::from("src/Extended.bt");

    service.update_file(
        dep_file,
        "Protocol define: BaseProto\n  base => Boolean\n".to_string(),
    );
    service.update_file(
        consumer_file.clone(),
        "Protocol define: ExtendedProto\n  extending: BaseProto\n  extra => Boolean\n".to_string(),
    );

    let diags = service.diagnostics(&consumer_file);
    let unknown_protocol: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("extends unknown protocol"))
        .collect();
    assert!(
        unknown_protocol.is_empty(),
        "a dependency-exported protocol should resolve via ProjectIndex, got: {unknown_protocol:?}"
    );
}

/// BT-2800 (ADR 0100 Rule 3 surface-parity gap): `SimpleLanguageService`
/// must apply the `[diagnostics]` table set via `set_diagnostics_overrides`
/// exactly like `beamtalk build` does — a `dnu = "error"` override
/// promotes the default `Hint` on an unresolved selector to `Error`.
#[test]
fn diagnostics_applies_severity_overrides() {
    use beamtalk_core::compilation::diagnostics_policy::{
        DiagnosticSeverityOverride, DiagnosticsTable,
    };
    use beamtalk_core::source_analysis::{DiagnosticCategory, Severity};

    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("src/Dnu.bt");
    service.update_file(file.clone(), "\"hello\" frobnicate".to_string());

    // Baseline: no overrides set, Rule 1 default is Hint.
    let baseline = service.diagnostics(&file);
    assert!(
        baseline
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::Dnu) && d.severity == Severity::Hint),
        "expected a Dnu Hint before any override: {baseline:?}"
    );

    let mut table = DiagnosticsTable::new();
    table.insert(DiagnosticCategory::Dnu, DiagnosticSeverityOverride::Error);
    service.set_diagnostics_overrides(table);

    let overridden = service.diagnostics(&file);
    assert!(
        overridden
            .iter()
            .any(|d| d.category == Some(DiagnosticCategory::Dnu) && d.severity == Severity::Error),
        "dnu = \"error\" override must promote the Dnu diagnostic to Error: {overridden:?}"
    );
}

/// BT-2795 (ADR 0100 Rule 2 WS1): a standalone extension defined in one
/// file must be visible to another file's diagnostics — the false `Dnu`
/// hint on a same-project cross-file extension disappears.
#[test]
fn diagnostics_resolve_cross_file_extension_via_project_index() {
    let mut service = SimpleLanguageService::new();
    let ext_file = Utf8PathBuf::from("src/StringShout.bt");
    let use_file = Utf8PathBuf::from("src/UseShout.bt");

    service.update_file(
        ext_file.clone(),
        "String >> shoutLouder => self\n".to_string(),
    );
    service.update_file(
        use_file.clone(),
        "Object subclass: UseShout\n  class demo =>\n    \"abc\" shoutLouder\n".to_string(),
    );

    let diags = service.diagnostics(&use_file);
    let dnu: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("shoutLouder"))
        .collect();
    assert!(
        dnu.is_empty(),
        "cross-file extension `String >> shoutLouder` should resolve, got: {dnu:?}"
    );
}

/// BT-2795: removing the defining file makes the extension unresolved again.
#[test]
fn diagnostics_cross_file_extension_gone_after_remove() {
    let mut service = SimpleLanguageService::new();
    let ext_file = Utf8PathBuf::from("src/StringShout.bt");
    let use_file = Utf8PathBuf::from("src/UseShout.bt");

    service.update_file(
        ext_file.clone(),
        "String >> shoutLouder => self\n".to_string(),
    );
    service.update_file(
        use_file.clone(),
        "Object subclass: UseShout\n  class demo =>\n    \"abc\" shoutLouder\n".to_string(),
    );
    service.remove_file(&ext_file);

    let diags = service.diagnostics(&use_file);
    let dnu: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("shoutLouder"))
        .collect();
    assert!(
        !dnu.is_empty(),
        "after removing the defining file the extension should be unresolved again"
    );
}

/// BT-2027: Opening a stdlib source file must not emit "conflicts with
/// stdlib class" diagnostics for every class the file defines. The
/// language service now sets `stdlib_mode = true` for files tracked as
/// stdlib in the `ProjectIndex`.
#[test]
fn diagnostics_skip_stdlib_shadowing_for_stdlib_files() {
    use crate::project_index::ProjectIndex;

    // Pre-index a stdlib file defining `Counter`.
    let stdlib_path = Utf8PathBuf::from("stdlib/src/Counter.bt");
    let stdlib_source = "Object subclass: Counter\n  class zero => 0\n".to_string();
    let (index_result, _) =
        ProjectIndex::with_stdlib(&[(stdlib_path.clone(), stdlib_source.clone())]);
    let index = index_result.unwrap();

    let mut service = SimpleLanguageService::with_project_index(index);
    // Re-register through update_file so `files` has an entry for it.
    service.update_file(stdlib_path.clone(), stdlib_source);

    let diags = service.diagnostics(&stdlib_path);
    let shadowing: Vec<_> = diags
        .iter()
        .filter(|d| d.message.contains("conflicts with a stdlib class"))
        .collect();
    assert!(
        shadowing.is_empty(),
        "stdlib files should not emit stdlib-shadowing diagnostics, got: {shadowing:?}"
    );
}
