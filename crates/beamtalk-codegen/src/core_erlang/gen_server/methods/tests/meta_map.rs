// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0098: `__beamtalk_meta` map construction — toolchain provenance,
//! type params, package, kind, and visibility.

use super::*;
use beamtalk_core::ast::{ClassDefinition, ClassKind, Module, TypeParamDecl};
use beamtalk_core::test_helpers::test_support::make_actor_class;

#[test]
fn test_meta_method_info_map_with_type_params() {
    let entries: Vec<MethodInfoEntry> = vec![(
        "unwrap".to_string(),
        0,
        MetaTypeRepr::TypeParam {
            name: "T".to_string(),
            index: 0,
        },
        vec![],
        true,
        false,
    )];
    let doc = CoreErlangGenerator::meta_method_info_map(&entries);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("{'type_param', 'T', 0}"),
        "method_info map should contain type_param tagged tuple. Got: {output}"
    );
}

#[test]
fn test_meta_type_params_in_meta_map() {
    // Build a generic class and verify type_params appears in meta map
    let mut class = make_actor_class("Container");
    class.type_params = vec![
        TypeParamDecl::unbounded(Identifier::new("T", s())),
        TypeParamDecl::unbounded(Identifier::new("E", s())),
    ];
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'type_params' => ['T', 'E']"),
        "meta map should include type_params list. Got: {output}"
    );
}

/// Helper: build a single-class module from an actor class (ADR 0098 tests).
fn module_with(class: ClassDefinition) -> Module {
    Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    }
}

#[test]
fn test_meta_provenance_keys_emitted_when_supplied() {
    // ADR 0098 Phase 3: a known toolchain bakes beamtalk_version + otp_release
    // into __beamtalk_meta as binary string literals (the same compound OTP key
    // the stamp uses — never a runtime system_info call).
    let module = module_with(make_actor_class("Counter"));
    let provenance = MetaProvenance {
        beamtalk_version: Some("0.4.0-dev+abc123"),
        otp_release: Some("28-16.4"),
    };
    let output = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        provenance,
    )
    .to_pretty_string();

    assert!(
        output.contains("'beamtalk_version' => "),
        "meta map should include beamtalk_version key. Got: {output}"
    );
    assert!(
        output.contains("'otp_release' => "),
        "meta map should include otp_release key. Got: {output}"
    );
    // Values are baked verbatim as binary literals.
    assert!(
        output.contains(&beamtalk_cerl_doc::binary::binary_string_literal(
            "0.4.0-dev+abc123"
        )),
        "beamtalk_version value not baked correctly. Got: {output}"
    );
    assert!(
        output.contains(&beamtalk_cerl_doc::binary::binary_string_literal("28-16.4")),
        "otp_release value not baked correctly. Got: {output}"
    );
}

#[test]
fn test_meta_provenance_keys_absent_by_default() {
    // REPL / test / older-toolchain codegen supplies no provenance; the keys
    // must be omitted entirely (readers treat absence as a stale module).
    let module = module_with(make_actor_class("Counter"));
    let output = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    )
    .to_pretty_string();

    assert!(
        !output.contains("beamtalk_version"),
        "meta map must omit beamtalk_version when unknown. Got: {output}"
    );
    assert!(
        !output.contains("otp_release"),
        "meta map must omit otp_release when unknown. Got: {output}"
    );
}

#[test]
fn test_meta_provenance_version_only_when_otp_unknown() {
    // OTP probe failed but the version is known: emit beamtalk_version alone.
    let module = module_with(make_actor_class("Counter"));
    let provenance = MetaProvenance {
        beamtalk_version: Some("1.2.3"),
        otp_release: None,
    };
    let output = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        provenance,
    )
    .to_pretty_string();

    assert!(
        output.contains("'beamtalk_version' => "),
        "beamtalk_version should be present. Got: {output}"
    );
    assert!(
        !output.contains("otp_release"),
        "otp_release must be omitted when OTP is unknown. Got: {output}"
    );
}

#[test]
fn test_meta_type_params_empty_for_non_generic() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'type_params' => []"),
        "non-generic class should have empty type_params. Got: {output}"
    );
}

#[test]
fn test_meta_map_includes_package_name() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        Some("my_counter"),
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'package' => 'my_counter'"),
        "meta map should include package name. Got: {output}"
    );
}

#[test]
fn test_meta_map_package_none_without_package() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'package' => 'none'"),
        "meta map should have 'none' package when no package. Got: {output}"
    );
}

#[test]
fn test_meta_map_includes_kind_actor() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'kind' => 'actor'"),
        "actor class meta should have kind 'actor'. Got: {output}"
    );
}

#[test]
fn test_meta_map_includes_kind_value() {
    let mut class = make_actor_class("Point");
    class.class_kind = ClassKind::Value;
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'kind' => 'value'"),
        "value class meta should have kind 'value'. Got: {output}"
    );
}

#[test]
fn test_extract_package_from_module_name() {
    assert_eq!(
        extract_package_from_module_name("bt@my_counter@counter"),
        Some("my_counter".to_string())
    );
    assert_eq!(
        extract_package_from_module_name("bt@stdlib@integer"),
        Some("stdlib".to_string())
    );
    assert_eq!(extract_package_from_module_name("beamtalk_integer"), None);
    assert_eq!(extract_package_from_module_name("bt@"), None);
    assert_eq!(
        extract_package_from_module_name("bt@pkg@sub@dir@class"),
        Some("pkg".to_string())
    );
}

#[test]
fn test_meta_map_visibility_public_by_default() {
    let class = make_actor_class("Counter");
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'visibility' => 'public'"),
        "non-internal class should have visibility 'public'. Got: {output}"
    );
}

#[test]
fn test_meta_map_visibility_internal() {
    let mut class = make_actor_class("Helper");
    class.is_internal = true;
    let module = Module {
        classes: vec![class],
        method_definitions: Vec::new(),
        protocols: vec![],
        type_aliases: Vec::new(),
        native_declarations: Vec::new(),
        expressions: Vec::new(),
        span: s(),
        file_leading_comments: vec![],
        file_trailing_comments: Vec::new(),
    };
    let doc = CoreErlangGenerator::build_meta_map_doc(
        module.classes.first().unwrap(),
        &module,
        false,
        false,
        None,
        MetaProvenance::default(),
    );
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'visibility' => 'internal'"),
        "internal class should have visibility 'internal'. Got: {output}"
    );
}

#[test]
fn test_method_info_visibility_public_by_default() {
    let entries: Vec<MethodInfoEntry> = vec![(
        "getValue".to_string(),
        0,
        MetaTypeRepr::None,
        vec![],
        false,
        false,
    )];
    let doc = CoreErlangGenerator::meta_method_info_map(&entries);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'visibility' => 'public'"),
        "non-internal method should have visibility 'public'. Got: {output}"
    );
}

#[test]
fn test_method_info_visibility_internal() {
    let entries: Vec<MethodInfoEntry> = vec![(
        "helperMethod".to_string(),
        0,
        MetaTypeRepr::None,
        vec![],
        false,
        true,
    )];
    let doc = CoreErlangGenerator::meta_method_info_map(&entries);
    let output = doc.to_pretty_string();
    assert!(
        output.contains("'visibility' => 'internal'"),
        "internal method should have visibility 'internal'. Got: {output}"
    );
}
