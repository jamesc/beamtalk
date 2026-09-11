// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Workspace-binding shadow warnings, fixture-sourced and
//! synthetic protocol name resolution, and ADR 0108 pre-loaded/
//! referenced type-alias seeding.

use super::*;

// ── Workspace binding shadows class ──

#[test]
fn workspace_binding_shadowing_class_emits_warning() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    use crate::source_analysis::DiagnosticCategory;

    // Pre-load a class "Workspace" so the hierarchy knows about it.
    let pre_class = ClassInfo {
        surface_incomplete: false,
        name: EcoString::from("Workspace"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    let src = "42.";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);

    // Analyse with "Workspace" as both a known_var and a pre-loaded class.
    let known_vars = ["Workspace"];
    let result = analyse_full(
        &module,
        AnalysisContext::default()
            .with_known_vars(&known_vars)
            .with_pre_loaded_classes(vec![pre_class]),
    );

    let shadow_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::ShadowedClass))
        .collect();

    assert_eq!(
        shadow_warnings.len(),
        1,
        "Should emit exactly one shadow warning, got: {shadow_warnings:?}"
    );
    assert!(
        shadow_warnings[0]
            .message
            .contains("Workspace binding `Workspace` shadows class `Workspace`")
    );
}

#[test]
fn workspace_binding_not_in_hierarchy_no_shadow_warning() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    use crate::source_analysis::DiagnosticCategory;

    let src = "42.";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);

    // "myCustomVar" is not a class — no shadow warning expected.
    // Use empty pre_loaded_classes so has_cross_file_classes is false,
    // meaning the shadow check doesn't run at all. To test the no-match
    // path, we need at least one pre-loaded class.
    let dummy_class = ClassInfo {
        surface_incomplete: false,
        name: EcoString::from("DummyClass"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    let known_vars = ["myCustomVar"];
    let result = analyse_full(
        &module,
        AnalysisContext::default()
            .with_known_vars(&known_vars)
            .with_pre_loaded_classes(vec![dummy_class]),
    );

    let shadow_warnings: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::ShadowedClass))
        .collect();

    assert!(
        shadow_warnings.is_empty(),
        "Non-class bindings should not trigger shadow warnings, got: {shadow_warnings:?}"
    );
}

// Fixture-sourced protocol names must resolve in downstream modules.
//
// The BUnit test pipeline compiles fixture files, extracts their ProtocolInfo,
// and threads them through as `pre_loaded_protocols`. This test verifies that a
// module referencing a protocol name supplied via that slot (and NOT defined in
// the module itself) does not trigger an `Unresolved class` warning.
#[test]
fn fixture_sourced_protocol_name_is_not_unresolved() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;
    use crate::semantic_analysis::protocol_registry::ProtocolInfo;
    use crate::source_analysis::DiagnosticCategory;

    // Module body references `Displayable` as a class reference, mirroring
    // `Displayable requiredMethods` in `protocol_queries_test.bt`.
    let src = "Displayable requiredMethods.";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);

    // Provide at least one pre-loaded class so `has_cross_file_classes` is
    // true and the unresolved-class validator actually runs. Without this
    // sentinel, the check is suppressed and the test would pass vacuously.
    let dummy_class = ClassInfo {
        surface_incomplete: false,
        name: EcoString::from("DummyClass"),
        superclass: Some(EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![],
        state_types: std::collections::HashMap::new(),
        state_has_default: std::collections::HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    };

    let fixture_protocol = ProtocolInfo {
        name: EcoString::from("Displayable"),
        type_params: vec![],
        type_param_bounds: vec![],
        extending: None,
        methods: vec![],
        class_methods: vec![],
        span: Span::default(),
    };

    let options = crate::CompilerOptions::default();
    let result =
        analyse_full(
            &module,
            AnalysisContext::default()
                .with_options(&options)
                .with_pre_loaded_classes(vec![dummy_class])
                .with_pre_loaded_protocols(vec![fixture_protocol])
                .with_cross_file_extensions(
                    &crate::compilation::extension_index::ExtensionIndex::new(),
                ),
        );

    let unresolved: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| d.category == Some(DiagnosticCategory::UnresolvedClass))
        .collect();

    assert!(
        unresolved.is_empty(),
        "Fixture-sourced protocol name should not trigger unresolved-class warnings, got: {unresolved:?}"
    );
}

// The language service registers every protocol as a synthetic class entry
// (`register_protocol_classes`) and hands those to the checker along with the
// real cross-file classes. A protocol defined in *another* file would
// otherwise reach `analyse_full` as a plain `pre_loaded_classes` entry:
// `has_class` would become true, defeating `is_type_compatible`'s "unknown
// type → compatible" escape hatch, and the nominal walk would flag a false
// "declares return type P, but body returns C" (and "expects P, got C" for
// params) for a class that structurally conforms — while `beamtalk build`,
// which never has such entries, stays silent. Entries named like a protocol
// in the *current* module are already dropped; cross-file protocols must be
// dropped the same way.
#[test]
fn pre_loaded_synthetic_protocol_class_entry_does_not_shadow_protocol() {
    use crate::semantic_analysis::{ClassHierarchy, ProtocolRegistry};

    // The protocol lives in another file — exactly what the language
    // service does per file: build the hierarchy, then register the
    // protocol as a synthetic class entry, then extract its ProtocolInfo.
    let protocol_src =
        "Protocol define: TimeoutToken\n  cancel -> Boolean\n  isActive -> Boolean\n";
    let (protocol_module, _) =
        crate::source_analysis::parse(crate::source_analysis::lex_with_eof(protocol_src));
    let (hierarchy, _) = ClassHierarchy::build(&protocol_module);
    let mut hierarchy = hierarchy.expect("protocol file hierarchy builds");
    hierarchy.register_protocol_classes(&protocol_module);
    let synthetic_entry = hierarchy
        .classes()
        .get("TimeoutToken")
        .cloned()
        .expect("register_protocol_classes adds a synthetic class entry");
    assert!(hierarchy.is_protocol_class("TimeoutToken"));
    let protocol_infos = ProtocolRegistry::extract_protocol_infos(&protocol_module);

    // This file: a conforming class, returned from a `-> TimeoutToken`
    // method and passed to a `:: TimeoutToken` parameter.
    let src = "Value subclass: NullTimer\n\
               \x20 cancel -> Boolean => false\n\
               \x20 isActive -> Boolean => true\n\
               \n\
               typed Object subclass: Pool\n\
               \x20 make -> TimeoutToken => NullTimer new\n\
               \x20 use: t :: TimeoutToken -> Boolean => t cancel\n\
               \x20 go -> Boolean => self use: NullTimer new\n";
    let (module, parse_diags) =
        crate::source_analysis::parse(crate::source_analysis::lex_with_eof(src));
    assert!(
        parse_diags.is_empty(),
        "fixture must parse cleanly: {parse_diags:?}"
    );

    let options = crate::CompilerOptions::default();
    let result =
        analyse_full(
            &module,
            AnalysisContext::default()
                .with_options(&options)
                .with_pre_loaded_classes(vec![synthetic_entry])
                .with_pre_loaded_protocols(protocol_infos)
                .with_cross_file_extensions(
                    &crate::compilation::extension_index::ExtensionIndex::new(),
                ),
        );

    let false_mismatches: Vec<_> = result
        .diagnostics
        .iter()
        .filter(|d| {
            d.message.contains("declares return type TimeoutToken")
                || (d.message.contains("expects TimeoutToken") && d.message.contains("got"))
        })
        .map(|d| d.message.clone())
        .collect();
    assert!(
        false_mismatches.is_empty(),
        "NullTimer structurally conforms to the cross-file protocol TimeoutToken — \
         no nominal mismatch expected (the CLI reports none), got: {false_mismatches:?}"
    );
    assert!(
        !result.class_hierarchy.has_class("TimeoutToken"),
        "a cross-file protocol's synthetic class entry must not reach the checker as a class"
    );
}

// ADR 0108 Phase 5: pre-loaded aliases must be seeded into the alias
// registry the same way pre-loaded protocols are, with current-module
// definitions winning and cross-package `internal` entries excluded at the
// seeding boundary.
#[test]
fn pre_loaded_alias_is_seeded_and_current_module_wins() {
    use crate::semantic_analysis::alias_registry::AliasInfo;

    // Current module declares its own `Id` alias — this must win over the
    // pre-loaded (cross-file) `Id` of the same name.
    let src = "type Id = String";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);

    let cross_file_alias = AliasInfo {
        name: EcoString::from("Timeout"),
        annotation: TypeAnnotation::Simple(Identifier {
            name: EcoString::from("Integer"),
            span: Span::default(),
        }),
        is_internal: false,
        package: Some(EcoString::from("app")),
        span: Span::default(),
    };
    let shadowed_alias = AliasInfo {
        name: EcoString::from("Id"),
        annotation: TypeAnnotation::Simple(Identifier {
            name: EcoString::from("Integer"),
            span: Span::default(),
        }),
        is_internal: false,
        package: Some(EcoString::from("app")),
        span: Span::default(),
    };
    // Internal alias declared in a *different* package — must be excluded.
    let foreign_internal_alias = AliasInfo {
        name: EcoString::from("ParserState"),
        annotation: TypeAnnotation::Simple(Identifier {
            name: EcoString::from("Integer"),
            span: Span::default(),
        }),
        is_internal: true,
        package: Some(EcoString::from("other_pkg")),
        span: Span::default(),
    };

    let options = crate::CompilerOptions {
        current_package: Some("app".to_string()),
        ..Default::default()
    };
    let result =
        analyse_full(
            &module,
            AnalysisContext::default()
                .with_options(&options)
                .with_pre_loaded_aliases(vec![
                    cross_file_alias,
                    shadowed_alias,
                    foreign_internal_alias,
                ])
                .with_cross_file_extensions(
                    &crate::compilation::extension_index::ExtensionIndex::new(),
                ),
        );

    assert!(
        result
            .diagnostics
            .iter()
            .all(|d| d.severity != Severity::Error),
        "unexpected errors: {:?}",
        result.diagnostics
    );

    // Cross-file alias is visible.
    assert!(result.alias_registry.has_alias("Timeout"));
    // Current-module's own definition wins over the same-named pre-loaded one.
    assert_eq!(
        result
            .alias_registry
            .get("Id")
            .unwrap()
            .annotation
            .type_name()
            .as_str(),
        "String",
        "current-module alias definition must win"
    );
    // A different-package internal alias is never seeded — seeding-boundary exclusion.
    assert!(
        !result.alias_registry.has_alias("ParserState"),
        "a different-package internal pre-loaded alias must never be visible in the consumer's table"
    );
}

// An `internal` alias from the *same* package (e.g. another file in
// a same-package multi-file compilation) must still be seeded — ADR 0108
// scopes `internal` to the whole declaring package, not just the declaring
// file, so this is not the seeding-boundary exclusion case above.
#[test]
fn pre_loaded_internal_alias_from_same_package_is_still_seeded() {
    use crate::semantic_analysis::alias_registry::AliasInfo;

    let module = Module::new(vec![], Span::default());

    let same_package_internal_alias = AliasInfo {
        name: EcoString::from("ParserState"),
        annotation: TypeAnnotation::Simple(Identifier {
            name: EcoString::from("Integer"),
            span: Span::default(),
        }),
        is_internal: true,
        package: Some(EcoString::from("json")),
        span: Span::default(),
    };

    let options = crate::CompilerOptions {
        current_package: Some("json".to_string()),
        ..Default::default()
    };
    let result =
        analyse_full(
            &module,
            AnalysisContext::default()
                .with_options(&options)
                .with_pre_loaded_aliases(vec![same_package_internal_alias])
                .with_cross_file_extensions(
                    &crate::compilation::extension_index::ExtensionIndex::new(),
                ),
        );

    assert!(
        result.alias_registry.has_alias("ParserState"),
        "a same-package internal alias from another file must remain visible"
    );
}

// ADR 0108 hot-reload re-check trigger: a cross-package
// `referenced_aliases`/`beamtalk_alias_xref` dependency edge must be
// recorded when a *seeded* (pre-loaded, from a dependency package) alias is
// referenced by the current module — and a foreign `internal` alias must
// never appear as such an edge, since `add_pre_loaded`'s seeding-boundary
// exclusion (see its doc) means it is never seeded into the consumer's
// alias table in the first place, so a reference to it resolves as an
// ordinary unknown-class annotation, not an alias dependency. Both halves
// matter for the live-redefinition re-check: only a *recorded* dependency
// edge lets `beamtalk_alias_xref` find the dependent class when the alias
// is later redefined live.
#[test]
fn referenced_aliases_records_a_seeded_cross_package_alias_and_excludes_a_foreign_internal_one() {
    use crate::semantic_analysis::alias_registry::AliasInfo;

    // Two classes in the *consuming* (`app`) package: one references the
    // dependency's public exported alias `Timeout`, the other attempts to
    // reference a *different* dependency's `internal` alias `ParserState`
    // — which `add_pre_loaded` never seeds because it is `internal` and
    // belongs to a package (`other_pkg`) other than the current compile.
    let src = "Object subclass: TimeoutUser\n  wait: t :: Timeout => t\n\n\
               Object subclass: ParserStateUser\n  parse: s :: ParserState => s\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);

    let public_dependency_alias = AliasInfo {
        name: EcoString::from("Timeout"),
        annotation: TypeAnnotation::Simple(Identifier {
            name: EcoString::from("Integer"),
            span: Span::default(),
        }),
        is_internal: false,
        package: Some(EcoString::from("net")),
        span: Span::default(),
    };
    let foreign_internal_alias = AliasInfo {
        name: EcoString::from("ParserState"),
        annotation: TypeAnnotation::Simple(Identifier {
            name: EcoString::from("Integer"),
            span: Span::default(),
        }),
        is_internal: true,
        package: Some(EcoString::from("other_pkg")),
        span: Span::default(),
    };

    let options = crate::CompilerOptions {
        current_package: Some("app".to_string()),
        ..Default::default()
    };
    let result =
        analyse_full(
            &module,
            AnalysisContext::default()
                .with_options(&options)
                .with_pre_loaded_aliases(vec![public_dependency_alias, foreign_internal_alias])
                .with_cross_file_extensions(
                    &crate::compilation::extension_index::ExtensionIndex::new(),
                ),
        );

    // (a) The public dependency alias is seeded and, once referenced by
    // `TimeoutUser`'s parameter, recorded as a dependency edge — exactly
    // what `beamtalk_alias_xref` needs to find `TimeoutUser` as a dependent
    // when `Timeout` is later redefined live.
    assert!(result.alias_registry.has_alias("Timeout"));
    assert!(
        result
            .referenced_aliases
            .contains(&EcoString::from("Timeout")),
        "expected Timeout in referenced_aliases, got: {:?}",
        result.referenced_aliases
    );

    // (b) The foreign internal alias is never seeded — the seeding-boundary
    // exclusion in `add_pre_loaded` runs before any reference to it is ever
    // resolved, so it can never appear in `referenced_aliases` for any
    // class, including one (`ParserStateUser`) that names it directly.
    assert!(!result.alias_registry.has_alias("ParserState"));
    assert!(
        !result
            .referenced_aliases
            .contains(&EcoString::from("ParserState")),
        "a never-seeded internal alias from another package must never appear \
         as a referenced_aliases entry, got: {:?}",
        result.referenced_aliases
    );
}

// ADR 0108 hot-reload re-check trigger: a protocol's own declared
// method-signature annotations must record `referenced_aliases` dependency
// edges exactly like a class method's do — protocol method signatures have
// no body, so the class-method-body-only call sites that normally populate
// this field would otherwise never visit them. Both an instance-side
// parameter annotation (`heading:`) and a class-side return-type annotation
// (`class default`) are covered, since `beamtalk_alias_xref` needs edges for
// both sides of a protocol's signature.
#[test]
fn protocol_method_signature_records_referenced_aliases() {
    let src = "type Direction = #north | #south | #east | #west\n\n\
               Protocol define: Directional\n  \
               heading: d :: Direction -> Boolean\n  \
               class default -> Direction\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);

    let result = analyse(&module);

    assert!(
        result
            .referenced_aliases
            .contains(&EcoString::from("Direction")),
        "expected Direction in referenced_aliases from the protocol's own \
         method signatures, got: {:?}",
        result.referenced_aliases
    );
}

// End-to-end wiring check — the E0402 alias-leak checks (Phase 8)
// must fire through the full `analyse_full` pipeline, not just when
// the validator functions are called directly in `visibility_validators.rs`.
#[test]
fn analyse_full_pipeline_reports_internal_alias_leaked_in_public_signature() {
    let src = "internal type ParserState = Integer\n\n\
               Object subclass: Parser\n  tokenize: input :: String -> ParserState => nil";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);

    let options = crate::CompilerOptions {
        current_package: Some("json".to_string()),
        ..Default::default()
    };
    let result = analyse_full(&module, AnalysisContext::default().with_options(&options));

    assert!(
        result
            .diagnostics
            .iter()
            .any(|d| d.severity == Severity::Error
                && d.message.contains("Internal type alias 'ParserState'")),
        "expected the full pipeline to report the leaked internal alias, got: {:?}",
        result.diagnostics
    );
}
