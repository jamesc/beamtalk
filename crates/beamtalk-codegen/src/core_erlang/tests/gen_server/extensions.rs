// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Codegen for class extension methods: same-module typed-field
//! extensions and cross-module (foreign) extensions, including
//! class-side foreign extensions and self-extensions.

use super::*;

/// Builds a `ClassInfo` for a foreign target class named `PriceBand`
/// with a single state field `lo` carrying the given declared type. Used by the
/// extension-method field-type threading tests.
fn price_band_class_info_with_lo_type(
    lo_type: Option<&str>,
) -> beamtalk_core::semantic_analysis::class_hierarchy::ClassInfo {
    use beamtalk_core::semantic_analysis::class_hierarchy::{ClassInfo, DeclaredType};
    use std::collections::HashMap;

    let mut state_types = HashMap::new();
    if let Some(ty) = lo_type {
        state_types.insert(ecow::EcoString::from("lo"), DeclaredType::parse(ty));
    }
    ClassInfo {
        surface_incomplete: false,
        name: ecow::EcoString::from("PriceBand"),
        superclass: Some(ecow::EcoString::from("Object")),
        is_sealed: false,
        is_abstract: false,
        is_typed: false,
        is_internal: false,
        package: None,
        is_value: false,
        is_native: false,
        handle_scope: None,
        state: vec![ecow::EcoString::from("lo")],
        state_types,
        state_has_default: HashMap::new(),
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        type_param_bounds: vec![],
        superclass_type_args: vec![],
    }
}

#[test]
fn test_bt2728_extension_object_typed_field_dispatches() {
    // An extension method comparing an object-typed `self.<field>` must
    // route through the runtime guard so it dispatches to the field type's
    // operator — same as an in-class method. The target class (`PriceBand`) is
    // foreign (declared elsewhere); its `lo :: Money` field type is resolved
    // from the class hierarchy threaded into extension codegen.
    let src = "PriceBand >> below: other => self.lo < other";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@geo@price_band_ext")
            .with_class_hierarchy(vec![price_band_class_info_with_lo_type(Some("Money"))]),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("call 'beamtalk_primitive':'is_object'("),
        "object-typed self.<field> comparison in an extension must be guarded (dispatch); got:\n{code}"
    );
}

#[test]
fn test_bt2728_extension_object_typed_field_arithmetic_dispatches() {
    // The arithmetic guard (`is_number`) follows a parallel path to the
    // comparison guard and shares the same `set_extension_target_field_types`
    // fix. An extension method doing arithmetic on an object-typed `self.<field>`
    // must route through the `is_number` guard so `self.lo + other` dispatches to
    // the field type's `+` instead of `badarith`-ing.
    let src = "PriceBand >> plus: other => self.lo + other";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@geo@price_band_ext")
            .with_class_hierarchy(vec![price_band_class_info_with_lo_type(Some("Money"))]),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("call 'erlang':'is_number'("),
        "object-typed self.<field> arithmetic in an extension must be guarded (dispatch); got:\n{code}"
    );
}

#[test]
fn test_bt2728_extension_untyped_field_stays_bare() {
    // An untyped `self.<field>` in an extension keeps the bare BIF (no
    // regression) — the guard/dispatch path is only taken for object-typed
    // fields.
    let src = "PriceBand >> below: other => self.lo < other";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@geo@price_band_ext")
            .with_class_hierarchy(vec![price_band_class_info_with_lo_type(None)]),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("call 'erlang':'<'(") && !code.contains("is_object"),
        "untyped self.<field> comparison in an extension must stay bare; got:\n{code}"
    );
}

#[test]
fn test_bt2728_extension_primitive_field_stays_bare() {
    // A primitive-typed (`Integer`) `self.<field>` in an extension keeps
    // the bare comparison BIF — parity with in-class primitive fields.
    let src = "PriceBand >> below: other => self.lo < other";
    let tokens = beamtalk_core::source_analysis::lex_with_eof(src);
    let (module, _) = beamtalk_core::source_analysis::parse(tokens);

    let code = generate_module(
        &module,
        CodegenOptions::new("bt@geo@price_band_ext")
            .with_class_hierarchy(vec![price_band_class_info_with_lo_type(Some("Integer"))]),
    )
    .expect("codegen should succeed");

    assert!(
        code.contains("call 'erlang':'<'(") && !code.contains("is_object"),
        "primitive-typed self.<field> comparison in an extension must stay bare; got:\n{code}"
    );
}

// ── Foreign cross-class extension codegen (gen_server/extensions.rs) ──────

/// A unary foreign extension on a stdlib value class generates a
/// `beamtalk_extensions:register/5` call with a 2-arity fun.
///
/// The target class (`String`) is not declared in this module, so the
/// standalone method is foreign and must be registered at load time.
#[test]
fn test_foreign_extension_unary_emits_register_with_2arity_fun() {
    let src = "String >> shout => self uppercase ++ \"!\"\n";
    let code = super::codegen(src);
    assert!(
        code.contains("call 'beamtalk_extensions':'register'"),
        "foreign extension should emit beamtalk_extensions:register. Got:\n{code}"
    );
    assert!(
        code.contains("'String'"),
        "foreign extension should register under the bare class name 'String'. Got:\n{code}"
    );
    assert!(
        code.contains("'shout'"),
        "foreign extension should register the selector atom. Got:\n{code}"
    );
    // Value-type targets use a 2-arity fun (ExtArgs + Self).
    assert!(
        code.contains("fun (_ExtArgs, Self)"),
        "value-type foreign extension fun must be 2-arity. Got:\n{code}"
    );
    assert!(
        !code.contains("fun (_ExtArgs, Self, State)"),
        "value-type foreign extension must NOT use the 3-arity actor fun shape. Got:\n{code}"
    );
}

/// A keyword foreign extension generates `_ExtArgs` list unpacking
/// for each declared parameter.
#[test]
fn test_foreign_extension_keyword_unpacks_ext_args() {
    let src = "String >> wrapWith: edge => edge ++ self ++ edge\n";
    let code = super::codegen(src);
    assert!(
        code.contains("call 'beamtalk_extensions':'register'"),
        "keyword foreign extension should emit register. Got:\n{code}"
    );
    assert!(
        code.contains("_ExtArgs"),
        "keyword foreign extension should reference _ExtArgs for parameter unpacking. Got:\n{code}"
    );
    // The first (and only) parameter is bound via erlang:hd(_ExtArgs).
    assert!(
        code.contains("'erlang':'hd'"),
        "first keyword parameter must be bound via erlang:hd(_ExtArgs). Got:\n{code}"
    );
}

/// A class-side foreign extension (`Target class >> sel`) registers
/// under the metaclass tag `'Target class'` (with a space), not the bare class
/// name. This is the established tag convention for metaclass registration.
#[test]
fn test_foreign_extension_class_side_uses_metaclass_tag() {
    let src = "String class >> banner => \"=== banner ===\"\n";
    let code = super::codegen(src);
    assert!(
        code.contains("call 'beamtalk_extensions':'register'"),
        "class-side foreign extension should emit register. Got:\n{code}"
    );
    // Metaclass tag uses a space: 'String class' (not 'Stringclass' or 'String').
    assert!(
        code.contains("'String class'"),
        "class-side extension must use the metaclass tag 'String class'. Got:\n{code}"
    );
    assert!(
        code.contains("'banner'"),
        "class-side extension should register the selector atom. Got:\n{code}"
    );
}

/// A self-extension (target class declared in the same module) is
/// folded into the host class module and must NOT emit a
/// `beamtalk_extensions:register` call.
#[test]
fn test_self_extension_not_registered_via_beamtalk_extensions() {
    let src = concat!(
        "Actor subclass: Counter\n",
        "  state: value = 0\n",
        "  get => value\n",
        "\n",
        "Counter >> getDouble => value * 2\n",
    );
    let code = super::codegen(src);
    assert!(
        !code.contains("call 'beamtalk_extensions':'register'"),
        "self-extension (Counter >> in the same module as Counter) must NOT emit \
         beamtalk_extensions:register. Got:\n{code}"
    );
}
