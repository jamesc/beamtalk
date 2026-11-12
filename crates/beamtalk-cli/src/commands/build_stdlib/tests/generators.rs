// Copyright 2026 James Casey
// SPDX-License-Identifier: Apache-2.0

//! Tests for the `generators` side of the stdlib build.
//!
//! **DDD Context:** CLI / Build System

use super::super::generators::*;
use super::super::metadata::*;
use super::{sample_class_meta, set_mtime, temp_utf8_dir};
use crate::commands::app_file;
use beamtalk_core::semantic_analysis::class_hierarchy::DeclaredType;
use camino::Utf8PathBuf;
use std::fs;
use std::time::Duration;
use std::time::SystemTime;

#[test]
fn test_generate_alias_sources_section_empty() {
    let mut code = String::new();
    generate_alias_sources_section(&mut code, &[]);
    assert!(
        code.contains("pub(super) fn generated_stdlib_alias_sources() -> Vec<&'static str> {"),
        "Should always emit the function signature. Got: {code}"
    );
    assert!(
        code.contains("vec![\n    ]"),
        "Should emit an empty vec when there are no aliases. Got: {code}"
    );
}

#[test]
fn test_generate_alias_sources_section_preserves_caller_order() {
    // Sorting by alias *name* requires the caller's `AliasInfo` (plain
    // declaration text alone would put `internal type ...` out of order
    // — see this function's doc) — so it emits entries in whatever order
    // it's given, trusting the caller (`build_stdlib()`) to have already
    // sorted by name.
    let mut code = String::new();
    generate_alias_sources_section(
        &mut code,
        &[
            "type Zebra = Integer".to_string(),
            "type Alpha = String".to_string(),
        ],
    );

    let alpha_pos = code.find("type Alpha").unwrap();
    let zebra_pos = code.find("type Zebra").unwrap();
    assert!(
        zebra_pos < alpha_pos,
        "Should preserve caller order (Zebra before Alpha here), not re-sort. Got: {code}"
    );
}

#[test]
fn test_generate_alias_sources_section_escapes_embedded_quotes() {
    // Not valid Beamtalk syntax — this only exercises the generator's
    // Rust-string-literal escaping, not the parser.
    let mut code = String::new();
    generate_alias_sources_section(&mut code, &[r#"contains "a quote""#.to_string()]);

    assert!(
        code.contains(r#"contains \"a quote\""#),
        "Embedded quotes should be escaped for the Rust string literal. Got: {code}"
    );
}

#[test]
fn test_generate_app_file() {
    let (_temp, ebin_dir) = temp_utf8_dir();

    let source_files = vec![
        Utf8PathBuf::from("lib/integer.bt"),
        Utf8PathBuf::from("lib/string.bt"),
    ];

    let erlang_modules = vec!["beamtalk_list".to_string()];

    generate_app_file(&ebin_dir, &source_files, &erlang_modules, &[], &[], &[]).unwrap();

    let app_file = ebin_dir.join("beamtalk_stdlib.app");
    assert!(app_file.exists());

    let content = fs::read_to_string(app_file).unwrap();
    assert!(content.contains("beamtalk_stdlib"));
    // ADR 0016: Module names use bt@stdlib@ prefix. The app's Erlang
    // modules are listed too (BT-4384): this `.app` is the installed one.
    assert!(
        content.contains("{modules, ['beamtalk_list', 'bt@stdlib@integer', 'bt@stdlib@string']}"),
        "Got:\n{content}"
    );
}

#[test]
fn test_generate_app_file_empty() {
    let (_temp, ebin_dir) = temp_utf8_dir();

    generate_app_file(&ebin_dir, &[], &[], &[], &[], &[]).unwrap();

    let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
    assert!(content.contains("{modules, []}"));
    // No type-alias metadata => no `type_aliases` env key at all
    // (matches `format_type_aliases_entry`'s empty-input contract).
    assert!(!content.contains("type_aliases"));
}

#[test]
fn test_generate_app_file_invalid_name_errors() {
    let (_temp, ebin_dir) = temp_utf8_dir();

    let source_files = vec![
        Utf8PathBuf::from("lib/integer.bt"),
        Utf8PathBuf::from("lib/my-bad-name.bt"),
    ];

    let result = generate_app_file(&ebin_dir, &source_files, &[], &[], &[], &[]);
    assert!(result.is_err());
}

#[test]
fn test_generate_app_file_with_protocol_modules() {
    let (_temp, ebin_dir) = temp_utf8_dir();

    let source_files = vec![Utf8PathBuf::from("lib/integer.bt")];
    let protocol_modules = vec!["bt@stdlib@printable".to_string()];

    generate_app_file(&ebin_dir, &source_files, &[], &[], &protocol_modules, &[]).unwrap();

    let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
    assert!(
        content.contains("{protocol_modules, ['bt@stdlib@printable']}"),
        "Should contain protocol_modules env key. Got:\n{content}"
    );
}

#[test]
fn test_generate_app_file_with_type_aliases() {
    let (_temp, ebin_dir) = temp_utf8_dir();

    let source_files = vec![Utf8PathBuf::from("lib/supervisor.bt")];
    let alias_metadata = vec![app_file::AliasMetadata {
        name: "SupervisionStrategy".to_string(),
        expansion: "#oneForOne | #oneForAll | #restForOne".to_string(),
        doc: None,
        source_file: "lib/supervisor.bt".to_string(),
        internal: false,
    }];

    generate_app_file(&ebin_dir, &source_files, &[], &[], &[], &alias_metadata).unwrap();

    let content = fs::read_to_string(ebin_dir.join("beamtalk_stdlib.app")).unwrap();
    assert!(
        content.contains("{type_aliases, [")
            && content.contains("name => 'SupervisionStrategy'")
            && content.contains("expansion => \"#oneForOne | #oneForAll | #restForOne\""),
        "Should contain type_aliases env key with the alias entry. Got:\n{content}"
    );
}

#[test]
fn test_generate_class_entry_emits_correct_structure() {
    let meta = sample_class_meta();
    let mut code = String::new();
    generate_class_entry(&mut code, &meta);

    assert!(code.contains("\"Counter\".into()"));
    assert!(code.contains("Some(\"Actor\".into())"));
    assert!(code.contains("is_sealed: false"));
    assert!(code.contains("is_abstract: false"));
    assert!(code.contains("\"count\".into()"));
    assert!(code.contains("selector: \"increment\".into()"));
    assert!(code.contains("selector: \"add:\".into()"));
    assert!(code.contains("arity: 1"));
    assert!(code.contains("selector: \"default\".into()"));
    // ADR 0103: the generated ClassInfo carries the handle_scope field.
    assert!(code.contains("handle_scope: None"));
    // ADR 0124 A2a: no `initialize` in this fixture's methods, so the
    // definitely-assigned summary is empty.
    assert!(code.contains("initialize_assigns: BTreeSet::new()"));
}

#[test]
fn test_generate_class_entry_sealed_methods() {
    let meta = sample_class_meta();
    let mut code = String::new();
    generate_class_entry(&mut code, &meta);

    // The add: method is sealed
    assert!(code.contains("is_sealed: true"));
}

#[test]
fn test_generate_class_entry_auto_ctors_anchor() {
    // a Value meta emits a populated anchor from the captured
    // keyword decision (`auto_kw`), `keyword_constructor: None` when
    // synthesis produced nothing, and a bare `None` anchor for
    // non-Value classes. The decision must be captured at synthesis
    // time: by emission, `class_methods` also holds the synthesized
    // keyword itself, so re-checking it here would always (wrongly)
    // report a user collision. BT-4132: the gate is the post-writeback
    // kind (`is_value_subclass`), so an indirect Value subclass whose
    // AST kind is `Object` is anchored too.
    let mut meta = sample_class_meta();
    meta.class_kind = beamtalk_core::ast::ClassKind::Object;
    meta.is_value_subclass = true;
    meta.auto_kw = Some("x:y:".to_string());
    let mut code = String::new();
    generate_class_entry(&mut code, &meta);
    assert!(
        code.contains("keyword_constructor: Some(\"x:y:\".into())"),
        "captured keyword must reach the anchor. Got:\n{code}"
    );

    meta.auto_kw = None;
    let mut code = String::new();
    generate_class_entry(&mut code, &meta);
    assert!(
        code.contains("Some(AutoConstructors { keyword_constructor: None })"),
        "a Value with no keyword keeps an empty anchor. Got:\n{code}"
    );

    meta.is_value_subclass = false;
    meta.auto_kw = Some("x:y:".to_string());
    let mut code = String::new();
    generate_class_entry(&mut code, &meta);
    assert!(
        code.contains("auto_ctors: None"),
        "non-Value classes emit a bare None anchor. Got:\n{code}"
    );
}

#[test]
fn test_generate_class_entry_root_class() {
    let meta = ClassMeta {
        module_name: "bt@stdlib@proto_object".to_string(),
        class_name: "ProtoObject".to_string(),
        superclass_name: "none".to_string(),
        modifiers: ClassModifiers {
            is_abstract: true,
            ..ClassModifiers::default()
        },
        class_kind: beamtalk_core::ast::ClassKind::Object,
        state: vec![],
        state_types: vec![],
        state_has_default: vec![],
        state_kinds: vec![],
        initialize_assigns: vec![],
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![],
        type_params: vec![],
        superclass_type_args: vec![],
        handle_scope: None,
        auto_kw: None,
        is_value_subclass: false,
        in_caller_new1: false,
    };
    let mut code = String::new();
    generate_class_entry(&mut code, &meta);

    assert!(code.contains("superclass: None"));
    assert!(code.contains("is_abstract: true"));
    assert!(code.contains("methods: vec![],"));
}

#[test]
fn test_generate_method_list_empty() {
    let mut code = String::new();
    generate_method_list(&mut code, "methods", &[], "Test");
    assert!(code.contains("methods: vec![],"));
}

#[test]
fn test_generate_superclass_type_args_empty() {
    let mut code = String::new();
    generate_superclass_type_args(&mut code, &[], &[]);
    assert!(
        code.contains("superclass_type_args: vec![],"),
        "Should emit empty vec when no args. Got: {code}"
    );
}

#[test]
fn test_generate_superclass_type_args_param_ref() {
    let mut code = String::new();
    generate_superclass_type_args(&mut code, &[DeclaredType::simple("E")], &["E".to_string()]);
    assert!(
        code.contains("superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 0 }]"),
        "Should emit ParamRef for forwarded type params. Got: {code}"
    );
}

#[test]
fn test_generate_superclass_type_args_param_ref_second_position() {
    // `Collection(V) subclass: Dictionary(K, V)` — V is at param_index 1.
    let mut code = String::new();
    generate_superclass_type_args(
        &mut code,
        &[DeclaredType::simple("V")],
        &["K".to_string(), "V".to_string()],
    );
    assert!(
        code.contains("superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 1 }]"),
        "Should compute param_index relative to subclass's own type_params. Got: {code}"
    );
}

#[test]
fn test_generate_superclass_type_args_concrete() {
    // `Collection(Integer) subclass: IntArray` — Integer is not a type param.
    let mut code = String::new();
    generate_superclass_type_args(&mut code, &[DeclaredType::simple("Integer")], &[]);
    assert!(
        code.contains(
            "superclass_type_args: vec![SuperclassTypeArg::Concrete { declared: DeclaredType::simple(\"Integer\") }]"
        ),
        "Non-type-param args should emit Concrete. Got: {code}"
    );
}

#[test]
fn test_generate_superclass_type_args_concrete_escaped() {
    // Guard against unescaped quotes/backslashes breaking the generated Rust.
    let mut code = String::new();
    generate_superclass_type_args(
        &mut code,
        &[DeclaredType::simple("My\\\"Type")],
        &["E".to_string()],
    );
    assert!(
        code.contains(
            "SuperclassTypeArg::Concrete { declared: DeclaredType::simple(\"My\\\\\\\"Type\") }"
        ),
        "Should emit escaped Concrete type_name. Got: {code}"
    );
}

#[test]
fn test_generate_builtins_sorted_deterministic() {
    let meta = [
        ClassMeta {
            module_name: "bt@stdlib@zebra".to_string(),
            class_name: "Zebra".to_string(),
            superclass_name: "Object".to_string(),
            modifiers: ClassModifiers::default(),
            class_kind: beamtalk_core::ast::ClassKind::Object,
            state: vec![],
            state_types: vec![],
            state_has_default: vec![],
            state_kinds: vec![],
            initialize_assigns: vec![],
            has_dynamic_field_writer: false,
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            handle_scope: None,
            auto_kw: None,
            is_value_subclass: false,
            in_caller_new1: false,
        },
        ClassMeta {
            module_name: "bt@stdlib@alpha".to_string(),
            class_name: "Alpha".to_string(),
            superclass_name: "Object".to_string(),
            modifiers: ClassModifiers {
                is_sealed: true,
                ..ClassModifiers::default()
            },
            class_kind: beamtalk_core::ast::ClassKind::Object,
            state: vec![],
            state_types: vec![],
            state_has_default: vec![],
            state_kinds: vec![],
            initialize_assigns: vec![],
            has_dynamic_field_writer: false,
            methods: vec![],
            class_methods: vec![],
            class_variables: vec![],
            type_params: vec![],
            superclass_type_args: vec![],
            handle_scope: None,
            auto_kw: None,
            is_value_subclass: false,
            in_caller_new1: false,
        },
    ];

    let mut code = String::new();
    // Simulate the sorted generation from generate_builtins_rs
    let sorted = sorted_by_class_name(&meta);

    for m in &sorted {
        generate_class_entry(&mut code, m);
    }

    // Alpha should appear before Zebra
    let alpha_pos = code.find("\"Alpha\"").unwrap();
    let zebra_pos = code.find("\"Zebra\"").unwrap();
    assert!(
        alpha_pos < zebra_pos,
        "Classes should be sorted alphabetically"
    );
}

#[test]
fn test_generate_method_list_emits_return_type() {
    let methods = vec![MethodMeta {
        selector: "add:".to_string(),
        arity: 1,
        kind: MethodKindMeta::Primary,
        is_sealed: false,
        is_internal: false,
        spawns_block: false,
        return_type: Some(DeclaredType::simple("Counter")),
        param_types: vec![Some(DeclaredType::simple("Integer"))],
        doc: None,
        primitive: None,
        constant_value: None,
    }];
    let mut code = String::new();
    generate_method_list(&mut code, "methods", &methods, "Counter");
    assert!(
        code.contains("return_type: Some(DeclaredType::simple(\"Counter\"))"),
        "Should emit return type. Got: {code}"
    );
    assert!(
        code.contains("param_types: vec![Some(DeclaredType::simple(\"Integer\"))]"),
        "Should emit param types. Got: {code}"
    );
}

#[test]
fn test_generate_method_list_emits_none_return_type() {
    let methods = vec![MethodMeta {
        selector: "increment".to_string(),
        arity: 0,
        kind: MethodKindMeta::Primary,
        is_sealed: false,
        is_internal: false,
        spawns_block: false,
        return_type: None,
        param_types: vec![],
        doc: None,
        primitive: None,
        constant_value: None,
    }];
    let mut code = String::new();
    generate_method_list(&mut code, "methods", &methods, "Counter");
    assert!(
        code.contains("return_type: None"),
        "Should emit None for untyped methods. Got: {code}"
    );
    assert!(
        code.contains("param_types: vec![]"),
        "Should emit empty param_types. Got: {code}"
    );
}

#[test]
fn test_generate_method_list_emits_spawns_block_true() {
    let methods = vec![MethodMeta {
        selector: "after:do:".to_string(),
        arity: 2,
        kind: MethodKindMeta::Primary,
        is_sealed: false,
        is_internal: false,
        spawns_block: true,
        return_type: None,
        param_types: vec![Some(DeclaredType::simple("Integer")), None],
        doc: None,
        primitive: None,
        constant_value: None,
    }];
    let mut code = String::new();
    generate_method_list(&mut code, "class_methods", &methods, "Timer");
    assert!(
        code.contains("spawns_block: true"),
        "Should emit spawns_block: true. Got: {code}"
    );
}

#[test]
fn test_generate_method_list_emits_is_internal_true() {
    // ADR 0101 Part 4: internal seam helpers must propagate `is_internal:
    // true` into the generated builtin metadata (it was previously
    // hardcoded `false`). Guards the `true` path of the `{internal}` field.
    let methods = vec![MethodMeta {
        selector: "xrefImplementorsOf:".to_string(),
        arity: 1,
        kind: MethodKindMeta::Primary,
        is_sealed: false,
        is_internal: true,
        spawns_block: false,
        return_type: Some(DeclaredType::simple("Dictionary")),
        param_types: vec![Some(DeclaredType::simple("Symbol"))],
        doc: None,
        primitive: None,
        constant_value: None,
    }];
    let mut code = String::new();
    generate_method_list(&mut code, "methods", &methods, "SystemNavigation");
    assert!(
        code.contains("is_internal: true"),
        "Should emit is_internal: true for an internal method. Got: {code}"
    );
}

// --- write_generated_file_if_changed ---
//
// `generate_builtins_rs`/`generate_erlang_builtins_hrl` themselves still
// write to the real hardcoded `GENERATED_BUILTINS_PATH`/
// `GENERATED_BUILTINS_HRL_PATH` constants and aren't unit-tested directly
// (per AGENTS.md's "Generated files" rule; their orchestration is covered
// separately). This shared helper is
// where the "only write if changed" and write-error branches actually
// live, and it takes `dest` as a parameter, so tests exercise it against
// a temp file instead.

#[test]
fn test_write_generated_file_if_changed_creates_missing_file() {
    let (_temp, dir) = temp_utf8_dir();
    let dest = dir.join("generated.rs");

    write_generated_file_if_changed(&dest, "content v1").unwrap();

    assert_eq!(fs::read_to_string(&dest).unwrap(), "content v1");
}

#[test]
fn test_write_generated_file_if_changed_overwrites_when_content_differs() {
    let (_temp, dir) = temp_utf8_dir();
    let dest = dir.join("generated.rs");
    fs::write(&dest, "content v1").unwrap();

    write_generated_file_if_changed(&dest, "content v2").unwrap();

    assert_eq!(fs::read_to_string(&dest).unwrap(), "content v2");
}

#[test]
fn test_write_generated_file_if_changed_skips_write_when_unchanged() {
    let (_temp, dir) = temp_utf8_dir();
    let dest = dir.join("generated.rs");
    fs::write(&dest, "same content").unwrap();
    // Far enough in the past that an unwanted rewrite (which would bump
    // the mtime to "now") is trivially detectable below.
    let original_mtime = SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000);
    set_mtime(&dest, original_mtime);

    write_generated_file_if_changed(&dest, "same content").unwrap();

    assert_eq!(
        fs::metadata(dest.as_std_path())
            .unwrap()
            .modified()
            .unwrap(),
        original_mtime,
        "content was unchanged — the file must not have been rewritten"
    );
}

#[test]
fn test_write_generated_file_if_changed_reports_write_error() {
    let (_temp, dir) = temp_utf8_dir();
    // Parent directory doesn't exist, so the write itself must fail —
    // exercises the write-error path without touching any real file.
    let dest = dir.join("no_such_subdir").join("generated.rs");

    let result = write_generated_file_if_changed(&dest, "content");

    assert!(result.is_err());
}

// --- generate_class_entry multi-item fields (separator branches) ---

#[test]
fn test_generate_class_entry_multi_item_fields_use_separators() {
    let meta = ClassMeta {
        module_name: "bt@stdlib@pair".to_string(),
        class_name: "Pair".to_string(),
        superclass_name: "Object".to_string(),
        modifiers: ClassModifiers::default(),
        class_kind: beamtalk_core::ast::ClassKind::Object,
        state: vec!["first".to_string(), "second".to_string()],
        state_types: vec![
            ("first".to_string(), DeclaredType::simple("Integer")),
            ("second".to_string(), DeclaredType::simple("String")),
        ],
        state_has_default: vec![("first".to_string(), true), ("second".to_string(), false)],
        state_kinds: vec![
            ("first".to_string(), beamtalk_core::ast::SlotKind::Eager),
            ("second".to_string(), beamtalk_core::ast::SlotKind::Late),
        ],
        initialize_assigns: vec!["first".to_string()],
        has_dynamic_field_writer: false,
        methods: vec![],
        class_methods: vec![],
        class_variables: vec![
            ClassVarMeta {
                name: "counterA".to_string(),
                ty: Some(DeclaredType::simple("Integer")),
                has_default: true,
                kind: beamtalk_core::ast::SlotKind::Eager,
            },
            ClassVarMeta {
                name: "counterB".to_string(),
                ty: None,
                has_default: false,
                kind: beamtalk_core::ast::SlotKind::Late,
            },
        ],
        type_params: vec!["T".to_string(), "U".to_string()],
        superclass_type_args: vec![DeclaredType::simple("T"), DeclaredType::simple("Concrete")],
        handle_scope: None,
        auto_kw: None,
        is_value_subclass: false,
        in_caller_new1: false,
    };
    let mut code = String::new();
    generate_class_entry(&mut code, &meta);

    assert!(code.contains(r#"state: vec!["first".into(), "second".into()]"#));
    assert!(code.contains(
        r#"state_types: HashMap::from([("first".into(), DeclaredType::simple("Integer")), ("second".into(), DeclaredType::simple("String"))])"#
    ));
    assert!(code.contains(
        r#"state_has_default: HashMap::from([("first".into(), true), ("second".into(), false)])"#
    ));
    assert!(code.contains(
        r#"state_kinds: HashMap::from([("first".into(), SlotKind::Eager), ("second".into(), SlotKind::Late)])"#
    ));
    assert!(code.contains(r#"initialize_assigns: BTreeSet::from(["first".into()])"#));
    assert!(code.contains(
        r#"class_variables: vec![ClassVarInfo { name: "counterA".into(), ty: Some(DeclaredType::simple("Integer")), has_default: true, kind: SlotKind::Eager }, ClassVarInfo { name: "counterB".into(), ty: None, has_default: false, kind: SlotKind::Late }]"#
    ));
    assert!(code.contains(r#"type_params: vec!["T".into(), "U".into()]"#));
    assert!(code.contains("type_param_bounds: vec![None, None]"));
    assert!(code.contains(
        r#"superclass_type_args: vec![SuperclassTypeArg::ParamRef { param_index: 0 }, SuperclassTypeArg::Concrete { declared: DeclaredType::simple("Concrete") }]"#
    ));
}

// --- declared_type_to_rust_expr — every DeclaredType variant ---

#[test]
fn test_declared_type_to_rust_expr_covers_every_variant() {
    assert_eq!(
        declared_type_to_rust_expr(&DeclaredType::Singleton("nil".into())),
        r#"DeclaredType::singleton("nil")"#
    );
    assert_eq!(
        declared_type_to_rust_expr(&DeclaredType::union(vec![
            DeclaredType::simple("Integer"),
            DeclaredType::simple("Float"),
        ])),
        r#"DeclaredType::union(vec![DeclaredType::simple("Integer"), DeclaredType::simple("Float")])"#
    );
    assert_eq!(
        declared_type_to_rust_expr(&DeclaredType::generic(
            "List",
            vec![DeclaredType::simple("Integer")]
        )),
        r#"DeclaredType::generic("List", vec![DeclaredType::simple("Integer")])"#
    );
    assert_eq!(
        declared_type_to_rust_expr(&DeclaredType::FalseOr(Box::new(DeclaredType::simple(
            "Integer"
        )))),
        r#"DeclaredType::FalseOr(Box::new(DeclaredType::simple("Integer")))"#
    );
    assert_eq!(
        declared_type_to_rust_expr(&DeclaredType::Difference {
            base: Box::new(DeclaredType::simple("Object")),
            excluded: Box::new(DeclaredType::simple("Nil")),
        }),
        r#"DeclaredType::Difference { base: Box::new(DeclaredType::simple("Object")), excluded: Box::new(DeclaredType::simple("Nil")) }"#
    );
    assert_eq!(
        declared_type_to_rust_expr(&DeclaredType::Intersection {
            left: Box::new(DeclaredType::simple("A")),
            right: Box::new(DeclaredType::simple("B")),
        }),
        r#"DeclaredType::Intersection { left: Box::new(DeclaredType::simple("A")), right: Box::new(DeclaredType::simple("B")) }"#
    );
    assert_eq!(
        declared_type_to_rust_expr(&DeclaredType::SelfType),
        "DeclaredType::SelfType"
    );
    assert_eq!(
        declared_type_to_rust_expr(&DeclaredType::SelfClass),
        "DeclaredType::SelfClass"
    );
    assert_eq!(
        declared_type_to_rust_expr(&DeclaredType::ClassOf("Counter".into())),
        r#"DeclaredType::ClassOf("Counter".into())"#
    );
}

// --- generate_method_list doc-string escaping ---

#[test]
fn test_generate_method_list_escapes_doc_special_characters() {
    let methods = vec![MethodMeta {
        selector: "help".to_string(),
        arity: 0,
        kind: MethodKindMeta::Primary,
        is_sealed: false,
        is_internal: false,
        spawns_block: false,
        return_type: None,
        param_types: vec![],
        doc: Some("Line one\nLine \"two\"\tend\r".to_string()),
        primitive: None,
        constant_value: None,
    }];
    let mut code = String::new();
    generate_method_list(&mut code, "methods", &methods, "Help");

    let expected = r#"doc: Some("Line one\nLine \"two\"\tend\r".into())"#;
    assert!(code.contains(expected), "Got: {code}");
}
