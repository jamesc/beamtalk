// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! `NativeTypeRegistry`-backed completions and protocol registration/
//! visibility in the project index, including cross-file protocol
//! go-to-definition and find-references (moved from `lib.rs` per BT-3450).

use super::common::*;

// -----------------------------------------------------------------------
// ADR 0075: NativeTypeRegistry integration
// -----------------------------------------------------------------------

#[test]
fn completions_use_native_type_registry() {
    use beamtalk_core::semantic_analysis::type_checker::{
        FunctionSignature, InferredType, ParamType, TypeProvenance,
    };

    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("test.bt");
    service.update_file(file.clone(), "Erlang lists ".to_string());

    // Without registry: completions should still work (untyped)
    let completions = service.completions(&file, Position::new(0, 13));
    let reverse_untyped = completions.iter().find(|c| c.label == "reverse:");
    assert!(
        reverse_untyped.is_some(),
        "Should find reverse: without registry"
    );

    // With registry: completions should show typed signatures
    let mut registry = NativeTypeRegistry::new();
    registry.register_module(
        "lists",
        vec![FunctionSignature {
            name: "reverse".to_string(),
            arity: 1,
            params: vec![ParamType {
                keyword: Some(ecow::EcoString::from("list")),
                type_: InferredType::known("List"),
            }],
            return_type: InferredType::known("List"),
            provenance: TypeProvenance::Extracted,
            line: None,
        }],
    );
    service.set_native_types(registry);

    let completions = service.completions(&file, Position::new(0, 13));
    let reverse_typed = completions.iter().find(|c| c.label == "reverse:");
    assert!(
        reverse_typed.is_some(),
        "Should find reverse: with registry"
    );
    let detail = reverse_typed.unwrap().detail.as_deref().unwrap_or("");
    assert_eq!(
        detail, "reverse: list :: List -> List",
        "Should show typed signature from NativeTypeRegistry"
    );
}

#[test]
fn set_and_get_native_types() {
    let mut service = SimpleLanguageService::new();
    assert!(service.native_types().is_none());

    service.set_native_types(NativeTypeRegistry::new());
    assert!(service.native_types().is_some());
}

// -----------------------------------------------------------------------
// BT-1933: Protocol class object completions via SimpleLanguageService
// -----------------------------------------------------------------------

#[test]
fn protocol_registered_in_project_index() {
    let mut service = SimpleLanguageService::new();

    let file = Utf8PathBuf::from("proto.bt");
    let source = "Protocol define: Printable\n  asString -> String";
    service.update_file(file.clone(), source.to_string());

    // Protocol should be visible in project index as a class
    assert!(
        service.project_index.hierarchy().has_class("Printable"),
        "Protocol should be registered in project hierarchy"
    );

    // Protocol class should have the expected class methods
    let class = service
        .project_index
        .hierarchy()
        .get_class("Printable")
        .expect("Printable should be a class in the hierarchy");
    assert_eq!(class.superclass.as_deref(), Some("Protocol"));
    assert!(class.is_sealed);
    assert!(class.is_abstract);
    let selectors: Vec<_> = class
        .class_methods
        .iter()
        .map(|m| m.selector.as_str())
        .collect();
    assert!(
        selectors.contains(&"requiredMethods"),
        "Protocol class should have requiredMethods, got: {selectors:?}"
    );
    assert!(
        selectors.contains(&"conformingClasses"),
        "Protocol class should have conformingClasses, got: {selectors:?}"
    );
}

#[test]
fn protocol_visible_cross_file_in_project_index() {
    let mut service = SimpleLanguageService::new();

    // File A defines a protocol
    service.update_file(
        Utf8PathBuf::from("a.bt"),
        "Protocol define: Printable\n  asString -> String".to_string(),
    );

    // File B defines a class
    service.update_file(
        Utf8PathBuf::from("b.bt"),
        "Object subclass: Foo\n  bar => 1".to_string(),
    );

    // Both should be visible in merged hierarchy
    assert!(
        service.project_index.hierarchy().has_class("Printable"),
        "Protocol from file A should be visible in merged hierarchy"
    );
    assert!(
        service.project_index.hierarchy().has_class("Foo"),
        "Class from file B should be visible in merged hierarchy"
    );
}

// -----------------------------------------------------------------------
// BT-1936: Goto-definition and find-references for protocol names
// -----------------------------------------------------------------------

#[test]
fn goto_definition_protocol_from_class_type_param_bound() {
    // Click on `Printable` inside `Logger(T :: Printable)` in another file.
    let mut service = SimpleLanguageService::new();
    let file_a = Utf8PathBuf::from("printable.bt");
    let file_b = Utf8PathBuf::from("logger.bt");

    service.update_file(
        file_a.clone(),
        "Protocol define: Printable\n  asString -> String".to_string(),
    );
    service.update_file(
        file_b.clone(),
        "Actor subclass: Logger(T :: Printable)\n  log: msg => self".to_string(),
    );

    // `Actor subclass: Logger(T :: Printable)`
    //  0         1         2         3
    //  0123456789012345678901234567890123456789
    // "Printable" starts at column 29; click at column 32.
    let def = service.goto_definition(&file_b, Position::new(0, 32));
    let loc = def.expect("goto-def should navigate to protocol declaration");
    assert_eq!(loc.file, file_a);
}

#[test]
fn goto_definition_protocol_from_extending_clause() {
    // Click on `Comparable` inside `extending: Comparable`.
    let mut service = SimpleLanguageService::new();
    let file = Utf8PathBuf::from("protocols.bt");
    service.update_file(
        file.clone(),
        "Protocol define: Comparable\n  < other :: Self -> Boolean\n\n\
         Protocol define: Sortable\n  extending: Comparable\n  sortKey -> Object"
            .to_string(),
    );

    // Line 4 (0-indexed) is `  extending: Comparable`
    //   0         1         2
    //   01234567890123456789012345
    //   "  extending: Comparable"  — "Comparable" starts at col 13.
    let def = service.goto_definition(&file, Position::new(4, 15));
    let loc = def.expect("goto-def should find Comparable protocol");
    assert_eq!(loc.file, file);
}

#[test]
fn find_references_protocol_from_definition_site() {
    // Click on the protocol definition name and find: the definition itself
    // plus all usages (extending target, type-param bounds).
    let mut service = SimpleLanguageService::new();
    let file_proto = Utf8PathBuf::from("printable.bt");
    let file_logger = Utf8PathBuf::from("logger.bt");
    let file_sortable = Utf8PathBuf::from("sortable.bt");

    service.update_file(
        file_proto.clone(),
        "Protocol define: Printable\n  asString -> String".to_string(),
    );
    service.update_file(
        file_logger.clone(),
        "Actor subclass: Logger(T :: Printable)\n  log: msg => self".to_string(),
    );
    service.update_file(
        file_sortable.clone(),
        "Protocol define: Sortable\n  extending: Printable\n  sortKey -> Object".to_string(),
    );

    // "Protocol define: Printable" — `Printable` starts at column 17.
    let refs = service.find_references(&file_proto, Position::new(0, 20));
    // Expect: definition (file_proto) + bound (file_logger) + extending (file_sortable) = 3.
    assert_eq!(
        refs.len(),
        3,
        "expected 3 protocol references, got {refs:?}"
    );
    assert!(refs.iter().any(|r| r.file == file_proto));
    assert!(refs.iter().any(|r| r.file == file_logger));
    assert!(refs.iter().any(|r| r.file == file_sortable));
}
