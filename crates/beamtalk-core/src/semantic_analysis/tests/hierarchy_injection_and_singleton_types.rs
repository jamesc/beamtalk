// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! ADR 0050 Phase 4 `analyse_with_known_vars_and_classes`
//! hierarchy injection and end-to-end singleton-union type
//! annotation flow.

use super::*;

// ── ADR 0050 Phase 4: analyse_with_known_vars_and_classes ──

#[test]
fn analyse_with_known_vars_and_classes_injects_user_class_into_hierarchy() {
    use crate::semantic_analysis::class_hierarchy::ClassInfo;

    let pre_class = ClassInfo {
        surface_incomplete: false,
        name: EcoString::from("UserClass"),
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
    let result = analyse_full(
        &module,
        AnalysisContext::default().with_pre_loaded_classes(vec![pre_class]),
    );
    assert!(
        result.class_hierarchy.has_class("UserClass"),
        "UserClass should be visible in the hierarchy after injection"
    );
}

#[test]
fn analyse_with_known_vars_and_classes_empty_is_equivalent_to_base() {
    let src = "1 + 2.";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, _parse_diags) = crate::source_analysis::parse(tokens);
    let result_base = analyse(&module);
    let result_new = analyse_full(
        &module,
        AnalysisContext::default().with_pre_loaded_classes(vec![]),
    );
    // Same number of diagnostics (both should be empty for valid source)
    assert_eq!(result_base.diagnostics.len(), result_new.diagnostics.len());
}

// --- Singleton type annotations end-to-end ---

/// A `:: Integer | #infinity` annotation parses, resolves, and forms
/// the union `InferredType` that flows to the type checker — proven by the
/// union-send diagnostic naming both members. This is the pipeline that
/// env-seeded tests cannot reach from source.
#[test]
fn bt2627_singleton_union_annotation_flows_to_type_checker() {
    let src = "Object subclass: D\n  m: x :: Integer | #infinity =>\n    x size\n";
    let tokens = crate::source_analysis::lex_with_eof(src);
    let (module, parse_diags) = crate::source_analysis::parse(tokens);
    assert!(parse_diags.is_empty(), "should parse: {parse_diags:?}");
    let result = analyse(&module);
    // `Integer` does not understand `size`; the diagnostic names the full union,
    // which is only possible if the singleton-union annotation actually formed.
    assert!(
        result.diagnostics.iter().any(|d| {
            d.message.contains("understand") && d.message.contains("Integer | #infinity")
        }),
        "expected a union-send diagnostic naming `Integer | #infinity`, got: {:?}",
        result.diagnostics
    );
}
